/*
 *	websid_module.c
 *
 *	(c) Copyright 2021 Jürgen Wothke <tinyrsid@wothke.ch>,
 *						All Rights Reserved.
 *
 *  Kernel module "websid device driver" for scripted control of a SID chip (e.g. MOS6581). 
 *
 *
 *  API for "userland" use: 
 *  	1) to play a new song "open" the device /dev/websid (this is not reentrant)
 *		2) use "mmap" to establish required memory mapping 
 *		3) see websid_driver_api.h: follow the fetch/feed protocol via the respective
 *         signal flags in the shared memory (see device_driver_handler.cpp for example) 
 *		4) "close" the filehandle to the device when done with a song
 *
 *
 *  Basic operating mode of this driver:
 *  
 *  Simple double buffering scheme: one buffer is used to playback (here in the driver) 
 *  while the other one can be filled (in "userland") with new data at the same time. The 
 *  player drives the process by advertising which buffer it wants to have filled next 
 *  and by signaling if it is ready to receive a new buffer. The kernel part is very 
 *  timing critical, whereas the "userland" side is not (so much). 
 *  
 *  
 *  License  
 *  
 *  Terms of Use: This software is licensed under a CC BY-NC-SA 
 *  (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 *  The author does not admit liability nor provide warranty for any of this software. 
 *  This material is provided "AS-IS". Use it at your own risk.
 */
 
 
// reminder: latest linux kernel has some "new" RT throttling activated! to get rid of that 
// crap, respective configs (/proc/sys/kernel/sched_rt_period_us & 
// /proc/sys/kernel/sched_rt_runtime_us) are now patched on the userland side - see
// device_driver_handler.cpp	=> seems real braindead having to change those
// globally when the rules are only meant for core #3!
 
 
#include <linux/module.h>
#include <linux/fs.h>
#include <linux/miscdevice.h>
#include <linux/errno.h>

#include <linux/kthread.h>
#include <linux/delay.h>

#include "websid_driver_api.h"

#define  DEVICE_NAME "websid"
#define  CLASS_NAME  "websid"


// XXX open issue: supposedly the internal system timer counter glitches when the
// CPU runs at 1.5GHz.. and it might glitch even more when a core idles at
// its default 600MHz. the respective counter behavior should be investigated 
// by setting the clock to some fixed 
// speed: see /boot/config.txt (arm_freq & force_turbo)
// the remaining glitches may be linked to this - or to the mismatched 
// clockrate; 1MHz external SID clock vs "real" PAL/NTSC clock used in emulation



// the below are just C-file snipets used to make this file less bulky
// (while not having to bother with kernel module makefile setup)
#include "websid_mmap.c"
#include "websid_systimer.c"
#include "websid_gpio.c"


static int _playback_core_id = 3;	// CPU to run the playback thread on
static int _dev_open_count = 0;		// device can only be used by one process at a time

// -------------- util to run without any interruptions -------------

static unsigned long _irq_flags;


// Even with all the "high prio" handling of the playback thread, there are still
// interrupts disturbing the timing. Blocking the IRQs seems to be solve that problem -
// any any glitches left should no be timing flaws of the actual emulation.

#define run_undisturbed(undisturbed_func) \
	/* CAUTION: the undisturbed_func MUST NOT use anything depending on */\
	/* interrupts (e.g. printk, etc) or it will freeze! */\
	/* https://kernel.readthedocs.io/en/sphinx-samples/kernel-hacking.html */\
	\
	get_cpu();						/* aka preempt_disable(); prevent thread from being migrated */\
									/* (this should be redundant - see "isolcpus=3") */\
	local_irq_save(_irq_flags);		/* disable hard IRQs	(include/linux/irqflags.h) */\
	local_bh_disable();				/* disable local IRQs	(include/linux/interrupt.h) */\
	\
	undisturbed_func();\
	\
	local_bh_enable();				/* re-enable local IRQs */\
	local_irq_restore(_irq_flags);	/* re-enable hard IRQs */\
	put_cpu();



// ------------ setup of the basic /dev/websid driver API -------------------------

static int	websid_open(   struct inode *inodep, struct file *fp);
static int	websid_release(struct inode *inodep, struct file *fp);
static int websid_mmap(struct file *file, struct vm_area_struct *vma);

// note: respective file ops are never scheduled on CPU core #3 but always on one 
// of the other cores - while the background playback thread always runs on core #3

static struct file_operations websid_fops = {
	.owner =	THIS_MODULE,
	.mmap = 	websid_mmap,
	.open = 	websid_open,
	.release =	websid_release,
};

static struct miscdevice websid_device = {
	.minor =	MISC_DYNAMIC_MINOR,
	.name =		DEVICE_NAME,
	.fops =		&websid_fops,
	.mode  =	0x666		// let everyone use the device
};


// index of the buffer that should be filled next (0 or 1)
static u32 _feed_index = 0; 

// time at which the song actually started playing: must be reset at the beginning of
// a new song. The timestamps in the "play scripts" are treated as offsets to this base.
// KNOWN LIMITATION: 32-bit impl is prone to overflow once every ~70 minutes
static u32 _ts_offset;



// inlined to avoid additional stack allocations
static u8 *_buf_t0;	// tmp var used in push_fetch_buffer macro
#define push_fetch_buffer(force) {\
	if (force || *_fetch_flag_ptr == 0) {\
		_feed_index ^= 1; /* _feed_index ? 0 : 1 */\
		_buf_t0 = (u8*) (_feed_index ? _script_buf1 : _script_buf0);\
		*_fetch_flag_ptr = (u32)_buf_t0 - (u32)_shared_area;\
		_buf_t0[0] = 0;    /* put an end maker just in case "userland" sends it back without recording */\
	} else {\
		/* there is already a buffer installed - that will work just as fine */\
	}\
}

static struct task_struct *_player_thread;
static volatile u32 _run_next_script = 0;	// used to interrupt after completion of a script
static volatile u32 _player_done = 1;		// indicator if playback thread has stopped


// note: the "real" buffer offsets start after the 2*u32-header that contains the flags,
// i.e. anything >0 && <=7 can be used as a special marker - like the below.
#define RESET_SCRIPT_MARKER 1

// built-in script to reset all SID registers to 0 (to mute any beeps when quitting the player )
static u32 _reset_script[49] = {
	1, (0x0100),	// ts = 1, reg<<8 | value = 0x0100
	1, (0x0200),
	1, (0x0300),
	1, (0x0400),
	1, (0x0500),
	1, (0x0600),
	1, (0x0700),
	1, (0x0800),
	1, (0x0900),
	1, (0x0A00),
	1, (0x0B00),
	1, (0x0C00),
	1, (0x0D00),
	1, (0x0E00),
	1, (0x0F00),
	1, (0x1000),
	1, (0x1100),
	1, (0x1200),
	1, (0x1300),
	1, (0x1400),
	1, (0x1500),
	1, (0x1600),
	1, (0x1700),
	1, (0x1800),
	0
};

// perf logging
static u32 _log_enabled = 1;	// enable/disable logging
static u32 _log_script_count;	// nummber of processed scripts
static u32 _log_max_delay;		// maximum timing error that was observed
static u32 _log_start_delay_count;	// number of issues that occured right at the first script entry 
static u32 _log_other_delay_count;	// number of issues that occured later in a script


// "local" vars of "play_script"
static u32 _p_now;
static u32 _p_i, _p_ts;
static volatile u32 *_p_data;
static volatile u32 *_p_buf= 0;

// inlined to avoid additional stack allocations
#define play_script() {\
	_p_i = 0;\
	_p_ts = _p_buf[_p_i];\
	\
	if (_p_ts && !_ts_offset) {\
	 	/* sync playback with current counter pos */\
		_ts_offset = micros() + 1;	/* no harm starting +1 micro later */\
	}\
	\
	while (_p_ts != 0) {\
		\
		_p_data = &(_p_buf[_p_i+1]);\
		\
		_p_ts += _ts_offset;\
		_p_i += 2;			/* 2x u32 per entry.. */\
		\
		_p_now = micros();\
		if (_p_buf == _reset_script) {\
			/* ts in this script are garbage */\
			if (_p_now < 0xfffffff0) /* don't take any chances with overflow  */\
				while (micros()< (_p_now+3)) { /* just wait a bit to give SID some time between updates  */ }\
		\
		} else if (_p_now > _p_ts) {\
			/* problem: we are already to late (maybe the "userland" feed was too slow?) */\
			/* the problem would probably affect all the following timestamps as well, */\
			/* so it is better to just write off the lost time and take it from here */\
			\
			if (_log_enabled) {\
				u32 diff= _p_now - _p_ts;\
				if (diff > _log_max_delay) _log_max_delay = diff;\
				\
				if (_ts_offset <= _p_now+1) {\
					_log_start_delay_count++;\
				} else {\
					_log_other_delay_count++;\
				}\
			}\
			_ts_offset += (_p_now-_p_ts) + 1;\
			\
		} else {\
			/* detected "gaps" might be used to "do other stuff" as an */\
			/* optimization.. but hopefully that won't be necessary*/\
			while (micros() < _p_ts) { /* just wait and poll  */ }\
		}\
		\
		poke_SID(((*_p_data) >> 8) & 0x1f, (*_p_data) & 0xff);\
		\
		_p_ts = _p_buf[_p_i];\
	}\
	_log_script_count++;\
}

void play_loop(void) {
	_log_script_count = 0;
	_log_max_delay = 0;	
	_log_start_delay_count = _log_other_delay_count = 0;

	end_play_loop:		/* silly construct used to ease 1:1 conversion of this function into a macro */
	
	while (_run_next_script) {
		_p_buf = 0;
	
		while (_p_buf == 0) {
			_p_buf = (volatile u32*) (*_feed_flag_ptr);	/* wait for feed from "userland" */
			
			/* at this stage "_p_buf" is still a relative offset (not an absolute address) */
			/* just wait for main to feed some data */
			if (!_run_next_script) goto end_play_loop;
		}
		
		if ((u32)_p_buf == RESET_SCRIPT_MARKER) {
			/* special marker: "userland" closed the connection */
			_p_buf = _reset_script;
			(*_feed_flag_ptr) = 0;
			
			_run_next_script = 0; /* exit the playback thread when done with the reset-script */
		} else {
			/* convert "_p_buf" offset to absolute address */
			_p_buf = (volatile u32*)((u32)_shared_area + (u32)_p_buf);
			
			(*_feed_flag_ptr) = 0;
			push_fetch_buffer(0);	/* immediately request the other buffer to be filled */
									/* so it should be ready by the time it is needed */
		}
		play_script();
	}
}

int sid_player(void *data) {	
	_player_done= 0;
	
	printk(KERN_INFO "websid: player thread start\n");
	
	// FIXME: the IRQ blocking has a high risk of crashing the machine and
	// it might be useful to make it optional via a module startup param
	
//	play_loop();
	run_undisturbed(play_loop);

	if (_log_enabled) {
		printk(KERN_INFO "websid: stats - played scripts:  %u, max delay: %u c1#: %u c2#: %u\n", 
			_log_script_count, _log_max_delay,
			_log_start_delay_count, _log_other_delay_count);
	}
	printk(KERN_INFO "websid: player thread end\n");
	
	_player_done= 1;
	return 0;
}

static void init_player(void) {
	_run_next_script = 1;	
	_ts_offset = 0;			// reset base time for next played scripts
	*_feed_flag_ptr = 0; 	// should be redundant
	push_fetch_buffer(1);	// signal to "userland" that we are "open for business"
}

static void start_player_core3(void) {
	// this code here still may be running on any core (except core #3)
	
	init_player();
	
	// "equivalent" to PlaybackThread of the old "userland" impl 
	// (it just has to cope without C++)
	_player_thread = kthread_create(sid_player, NULL, "sid_player");	
	kthread_bind(_player_thread, _playback_core_id);
	
	// unfortunately this does not pervent all interrupts:
    sched_set_fifo(_player_thread);	// give prio to task (API only available for GPL.. FUCK YOU MORONS!)
	
	wake_up_process(_player_thread);
}


static int websid_open(struct inode *inodep, struct file *filep) {
	// this code here still may be running on any core (except core #3)

	printk(KERN_INFO "websid: open called on /dev/websid\n");

	if (_dev_open_count) return -EBUSY;		// exactely one concurrent user allowed
	_dev_open_count++;

	start_player_core3();

	printk(KERN_INFO "websid: open started playback thread\n");
	
	/* dev/websid is a virtual (and thus non-seekable) filesystem */
	return stream_open(inodep, filep);
}


static int websid_release(struct inode *inodep, struct file *filep) {
	
	if (_dev_open_count > 0) {
		printk(KERN_INFO "websid: release called on /dev/websid\n");
		
		
		if (!_player_done) {
			// trigger playback thread's exit sequence..
			*_feed_flag_ptr = RESET_SCRIPT_MARKER;
//			printk(KERN_INFO "websid: release triggered SID reset\n");
		} else {
//			printk(KERN_INFO "websid: OOPS player already quit..\n");
		}
		
		websid_unmmap(filep);
		
		_dev_open_count--;   // re-enable next "open"

		printk(KERN_INFO "websid: release completed\n");
		
		return 0;
	} else {
		printk(KERN_INFO "websid error: release before open\n");
		return -ENODEV;	// error
	}
}


static u32 _module_registered = 0;

static int __init websid_init(void) {
	printk(KERN_INFO "websid: loading device driver\n");

	if (!setup_shared_mem()) {
		return -ENODEV;	// error
	}

	if (!system_timer_mmap()) {
		teardown_shared_mem();
		return -ENODEV;	// error
	}
			
	if (misc_register(&websid_device)) {
		system_timer_unmmap();
		teardown_shared_mem();
		return -ENODEV;	// error
	}
			
	if (!gpio_init(websid_device.this_device)) {
		misc_deregister(&websid_device);
		system_timer_unmmap();
		teardown_shared_mem();		
		return -ENODEV;	// error
	};
			
	_module_registered = 1;
	printk(KERN_INFO "websid; registered /dev/websid\n");		
	return 0;
}

static void stop_plackback_loop(void) {
	*_feed_flag_ptr = RESET_SCRIPT_MARKER;
}

static void __exit websid_exit(void) {
	if (_module_registered) {
		u32 ts = micros();
		
		printk(KERN_INFO "websid: unloading device driver\n");
		
		if (_dev_open_count) {
			printk(KERN_INFO "websid: waiting for open connection to close\n");
			while (_dev_open_count && ((micros() - ts) < 10000000)) { }	// 10s grace period
			
			if (_dev_open_count) {
				printk(KERN_INFO "websid: connection is still open - this might crash\n");

				// normally the loop should already have been 
				// quit via "release" when the file was closed..
				stop_plackback_loop();
			}
		}	

		gpio_release();
		system_timer_unmmap();	
		teardown_shared_mem();	
		
		misc_deregister(&websid_device);
		_module_registered = 0;
		printk(KERN_INFO "websid: unloading completed\n");
	} else {
		printk(KERN_INFO "websid: unloading fail - wasn't registered\n");
	}
}

module_init(websid_init);
module_exit(websid_exit); 

/*
The required "sched_set_fifo" and "gpio_free" kernel functions by default are only exported to "GPL" 
licensed modules. Before compiling your kernel from source (which you'll have to do anyway as a 
precondition to build this kernel module). Make sure to edit kernel/linux/scripts/mod/modpost.c
and/or kernel/linux/include/linux/license.h to disable the silly GPL checks.

Or you can just comment use of the respective APIs - which should not make much of a difference anyway.
*/
MODULE_LICENSE("CC BY-NC-SA");
MODULE_AUTHOR("Jürgen Wothke");
MODULE_DESCRIPTION("websid device driver");
MODULE_VERSION("0.1"); 

