# WebSid (playback device driver)

################################################################################
Experimental! This driver is still in development and it is not yet running 
in a stable manner. In order to allow for precise timing, it blocks interrupts 
which some parts of the OS do not seem to appreciate.
Sometimes the driver runs fine for a long time and next time it just freezes
the machine after 10secs. Make sure to not have any unsaved work open when 
you start the driver! In spite of its stability issues, the driver allows to 
check which of the normal player's glitches are simply due to the timing 
having been messed up by the RPi4's interrupts.
################################################################################

This driver replays micro-second exact writes to a connected MOS8580 or
MOS6581 SID chip. Respective "playlists" are fed to the driver using a memory 
area that is shared with the device driver via "mmap". This driver is an 
alternative implementation to the respective built-in "userland" 
implementation already provided in the parent "raspi" folder. The respective
userland player will automatically use this driver whenever it detects it.

The cycle exact timing will not make any difference to most "normal" SID songs, 
but it is crucial for timing critical songs (e.g. songs that use "advanced" 
digi-sample techniques). You can check /var/log/kern.log - where the driver will
log the timing performance whenever a song is done playing. (When the c1 & c2 
counts are 0 it means that every GPIO write was performed in exactly the
right micro second.)
	
	
Note #1: Linux must be booted using "isolcpus=3 rcu_nocbs=3 rcu_nocb_poll=3 
nohz_full=3" (see /boot/cmdline.txt) so that the CPU core #3 can be completely 
hijacked by this module without starving other threads that might be vital to the 
system. There will then still be the below threads left on core #3 but it seems 
that those can be safely starved "for a while":

pi@raspberrypi:~ $ ps -eo pid,ppid,ni,comm | grep /3
   25     2   0 cpuhp/3
   26     2   - migration/3
   27     2   0 ksoftirqd/3
   28     2   0 kworker/3:0
   29     2 -20 kworker/3:0H
 (cpuhp=cpu hotplug, migration=distributes workload across CPU cores,
 ksoftirq=thread raised to handle unserved software interrupts)   


 Note #2: The kernel will notice the hijacking of core #3 and would log 
 lengthy "stall" and "hung_task" warnings to  /var/log/kern.log 
 The websid player automatically disables those warnings (only when using 
 the "device driver playback method" by changing the respective configuration 
 files, e.g.

   /sys/module/rcupdate/parameters/rcu_cpu_stall_suppress
   /proc/sys/kernel/hung_task_timeout_secs

 The original settings are restored when rebooting.
	
	
	
## Howto build/install

	sudo make
	
	This kernel module can then be installed/uninstalled via:

		sudo insmod websid_module.ko
	and 
		sudo rmmod websid_module.ko
	
	(check for errors: tail -f  /var/log/kern.log )

	

## Depencencies

You'll need a Raspberry Pi 4B and the Linux kernel source must have been installed and compiled
(see: https://www.stephenwagner.com/2020/03/17/how-to-compile-linux-kernel-raspberry-pi-4-raspbian/ ).

This module uses a "CC BY-NC-SA" license and it will not work on a regular stock kernel due to the
fact that a stock kernel refuses access to APIs like "sched_set_fifo" and "gpio_free" to non-GPL 
modules. So in order to use this module on your machine you'll have to disable the silly GPL 
checks before building your kernel. see: 
kernel/linux/include/linux/license.h  (just "return 1;" in "license_is_gpl_compatible")
(and maybe kernel/linux/scripts/mod/modpost.c (just "return;" in "check_for_gpl_usage"))
PS: rebuilding a previously built kernel with these changes takes less than 5 minutes..


## Background information

A note on timing constraints: a simple "+1 micro" polling loop manages a maximum 
of 19 loops (on RP4/1.5GHz)! probably less in most cases.. i.e. there may not be
much processing time available when the SID write happens immediately after the
script switch. Granted, the shortest SID update sequence will take 4 micros (which
is the fastest write operation of a 6502 CPU) but some time is always lost during the 
GPIO writes.. so there might be 2-3 micros avaiable in the worst case (i.e. when 
a script ends in the middle of a fast sequence..)


SID instructions in playback scripts are typically sparse, i.e. some writes are made,
and then nothing happends for a while, until the next writes are made. Which means that
a script here may end before the "time window" that it covers has completely elapsed (since
there were no additional writes scheduled in that window). The play loop will then already
start to wait for the next script eventhough that script is not yet needed to be played
for a while.

When the play loop waits for the next script, there are different possible senarions:
1) "userland" was faster and the "feed" is already available: The longer the previous
   script took to handle, the more likely this scenario, e.g. "digi-player songs" should fall
   into this category since they keep poking SID registers continually. It is a
   minority of songs.
2) "userland" was slower and the "feed" is not yet available.
   2a) previous script only covered a short portion of the playback interval: this is the
       norm for most songs which just poke some SID registers initially and then wait
       for the next frame
   2b) "userland" is too slow to keep up with the playback speed. Based on the experience
       with the "userland"-only impl this should NOT happen or the "userland" version should
       have experienced the same issue (which it did not). One difference to the "userland"
       impl is that the "buffer toggle" only allows to feed 1 buffer in advance, whereas
       the deque based "userland" impl may have allowed 2 buffers in those special cases
       where the playback was done handling the script but the current playback interval
       was still ongoing (this might have added some "load averaging" in those special cases).



## License
Terms of Use: This software is licensed under a CC BY-NC-SA 
(http://creativecommons.org/licenses/by-nc-sa/4.0/).
