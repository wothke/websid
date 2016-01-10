#ifndef TINYRSID_NANOCIA_H
#define TINYRSID_NANOCIA_H

#define ADDR_CIA1 0xdc00
#define ADDR_CIA2 0xdd00

extern unsigned long STOPPED;	// value bigger than any 16-bit counter for easy comparison
extern unsigned long NO_INT;		// next interrupt not on this screen; value bigger than any 16-bit counter for easy comparison

	
struct timer {
	/*
	* implementation info: 
	*	-D*0D (interrupt control and status): "io_area" contains the "write" version, i.e. the mask - the "read" version is managed below
	*	-D*04 - D*07 (timers): latch values are managed below, "memory" contains "read version" (current counter)
	* always use the access methods below
	*/
	unsigned int 	memoryAddress;
	
    struct timerState {
		unsigned int 	timer_latch;		// initially set wait-time (CPU cycles only for A, CPU cycles or TimerA underflows for CPU cycles or TimerA underflows B)
		unsigned char	timer_suspended;	// supress repeated 0 timers..
	} ts[2];	
	
	unsigned char	timer_interruptStatus;		// read version of the respective DX0D register
};

extern struct timer cia[2];

extern unsigned char TIMER_A;
extern unsigned char TIMER_B;

void resetCiaTimer();
void setInterruptMask(struct timer *t, unsigned char value);
unsigned char getInterruptStatus(struct timer *t);
int isTimerActive(struct timer *t);
void signalTimerUnderflow(struct timer *t, unsigned char timerId);
void setTimer(struct timer *t, unsigned int offset, unsigned char value);

unsigned long forwardToNextCiaInterrupt(struct timer *t, unsigned long timeLimit);
void simTimerPolling(unsigned short ad, unsigned long *cycles, unsigned short progCount);

#endif