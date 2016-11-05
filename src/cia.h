#ifndef TINYRSID_NANOCIA_H
#define TINYRSID_NANOCIA_H

#define ADDR_CIA1 0xdc00
#define ADDR_CIA2 0xdd00

#include "defines.h"


extern uint32_t STOPPED;	// value bigger than any 16-bit counter for easy comparison
extern uint32_t NO_INT;		// next interrupt not on this screen; value bigger than any 16-bit counter for easy comparison

	
struct timer {
	/*
	* implementation info: 
	*	-D*0D (interrupt control and status): "io_area" contains the "write" version, i.e. the mask - the "read" version is managed below
	*	-D*04 - D*07 (timers): latch values are managed below, "memory" contains "read version" (current counter)
	* always use the access methods below
	*/
	uint16_t 	memoryAddress;
	
    struct timerState {
		uint16_t 	timer_latch;		// initially set wait-time (CPU cycles only for A, CPU cycles or TimerA underflows for CPU cycles or TimerA underflows B)
		uint8_t	timer_suspended;	// supress repeated 0 timers..
	} ts[2];	
	
	uint8_t	timer_interruptStatus;		// read version of the respective DX0D register
};

extern struct timer cia[2];

extern uint8_t TIMER_A;
extern uint8_t TIMER_B;

void resetCiaTimer();
void setInterruptMask(struct timer *t, uint8_t value);
uint8_t getInterruptStatus(struct timer *t);
int isTimerActive(struct timer *t);
void signalTimerUnderflow(struct timer *t, uint8_t timerId);
void setTimer(struct timer *t, uint16_t offset, uint8_t value);

uint32_t forwardToNextCiaInterrupt(struct timer *t, uint32_t timeLimit);

#endif