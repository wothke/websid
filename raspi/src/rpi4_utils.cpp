/**
* WebSid (c) 2021 Jürgen Wothke
* version 1.0
*
* Terms of Use: This software is licensed under a CC BY-NC-SA
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#include <stdlib.h>
#include <stdio.h>
#include <iostream>             // std::cout
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include <memory>				// std::shared_ptr

#include <signal.h>				// SIGFPE
#include <exception>                                                                      

#include <sys/mman.h>

#include <sched.h> 

#include "rpi4_utils.h"

#define TIMER_OFFSET 0x3000

volatile uint32_t *timer_regs = 0;

uint32_t getBaseAddress() {
	// see wiringPi.c
	char buf[512];
	FILE * fp = fopen ("/proc/cpuinfo", "r");

	uint32_t addr = 0;

	if (fp != NULL) {
		while (fgets(buf, sizeof(buf), fp) != NULL) {
			if(strstr(buf,"Raspberry Pi 4")) {	// just check "Model".. "Hardware" and/or "Revision" might used instead
				fprintf(stderr, "detected : %s", strchr(buf, ':')+1);
				addr = 0xFE000000;
				break;
			}
		}
		fclose(fp);
	}
	if (!addr) {
		// addresses that might be used for older models: 0x20000000 or 0x3F000000
		// see https://www.raspberrypi.org/app/uploads/2012/02/BCM2835-ARM-Peripherals.pdf
		// vs 
		// https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2711/rpi_DATA_2711_1p0.pdf
		fprintf(stderr, "error: program currently only works for Raspberry Pi 4..\n\
in order to make it work on other devices the correct base address would need\n\
to be setup here (but those devices might be too slow anyway).");
		exit(1);
	}
	return addr;
}


void systemTimerSetup() {
	uint32_t base_addr = getBaseAddress();
	
	int memfd = -1;
	memfd = open("/dev/mem", O_RDWR|O_SYNC);
	if(memfd < 0) {
		fprintf(stderr, "error: memory map setup\n");
		exit(1);
	}
	timer_regs = (uint32_t *)mmap(0, 0x1000, PROT_READ|PROT_WRITE,
				MAP_SHARED|MAP_LOCKED, memfd, base_addr+TIMER_OFFSET);		
}


// simple mapping of signal codes that might be useful in the 
// "unheard of" event of some WebSid bug 

// note:: building with -fno-exceptions is useless here, since used 
// standard libs will still throw exceptions

const char *SIGS[9] = {
	// see http://www.yolinux.com/TUTORIALS/C++Signals.html
	"",
	"SIGHUP",
	"SIGINT",
	"SIGQUIT",
	"SIGILL",
	"SIGTRAP",
	"SIGABRT",
	"SIGBUS",
	"SIGFPE",	// floating point "exception"
	// ..
};


void installSigHandler(callback_function callback) {
	// garbage C++ does not allow to handle float-exceptions (etc) as regular exceptions..
    std::shared_ptr<void(int)> handler(
        signal(SIGFPE, [](int signum) {
			fprintf(stderr, "Error: signal %d: %s\n", signum, SIGS[signum]);
		  
			/* if this happens, using gdb should be used to get a meaningful stacktrace:
			
				$ sudo gdb ./websid              // start gdb on the program
				> run SomeMusic.sid              // run it with command line arguments
				(floating point exception)       // let it run until exception occurs
				> bt                             // bt will show the stack trace
			*/

			throw std::logic_error("SIGFPE"); 
		}),
        [](__sighandler_t f) { signal(SIGFPE, f); });
		
		
	// used to handle ctrl-C
	// handle ctrl-C
	struct sigaction sigIntHandler;

	sigIntHandler.sa_handler = callback;
	sigemptyset(&sigIntHandler.sa_mask);
	sigIntHandler.sa_flags = 0;
	sigaction(SIGINT, &sigIntHandler, NULL);
}


void scheduleRT() {
	int prio = sched_get_priority_max(SCHED_FIFO);	// i.e. 99
	struct sched_param param;
	param.sched_priority = prio;

	sched_setscheduler(0,SCHED_FIFO,&param);		
	system("echo -1 >/proc/sys/kernel/sched_rt_runtime_us");	// let thread use 100% CPU
	nice(-20);	// maximum (just in case)
}

void migrateThreadToCore3() {
	cpu_set_t cpuset;
	CPU_ZERO(&cpuset);
	CPU_SET(3,&cpuset);
	if (sched_setaffinity(0,sizeof(cpu_set_t),&cpuset)) {
		std::cout << "error: could not assign process to core #3" << std::endl;
	}
}

void startHyperdrive() {
	// it looks as if each core is represented as a "cpuX" here.. and it seems reasonable
	// that both, the dedicated core as well as the core used by linux should be boosted to max
	system("sudo cp /sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_max_freq "
		"/sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq");
				
	system("sudo cp /sys/devices/system/cpu/cpu3/cpufreq/cpuinfo_max_freq "
		"/sys/devices/system/cpu/cpu3/cpufreq/scaling_max_freq");
		
	// FIXME clock change might mess up SPI timing and for future A/D measurement 
	// extensions it might be necessary to run those off a dedicated core with some optimized
	// clock speed
}

void stopHyperdrive() {
	// restore to default
	system("sudo cp /sys/devices/system/cpu/cpu0/cpufreq/scaling_min_freq "
		"/sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq");
	system("sudo cp /sys/devices/system/cpu/cpu3/cpufreq/scaling_min_freq "
		"/sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq");
}
