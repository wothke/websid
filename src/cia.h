/*
* Poor man's emulation of the C64's MOS 6526 CIA (Complex Interface Adapter) timers.
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
* 
* Only those features actually observed in RSID files have been implemented, i.e. simple 
* cycle counting and timer B to timer A linking.
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#ifndef WEBSID_CIA_H
#define WEBSID_CIA_H

#include "base.h"

#define CIA1 0
#define CIA2 1
#define TIMER_A 0
#define TIMER_B 1

void 		ciaReset(uint8_t is_rsid);
void 		ciaSetDefaultsPSID(uint8_t timerDrivenPSID);

void 		ciaClock();
uint8_t 	ciaNMI();
uint8_t 	ciaIRQ();

// PSID crap
uint16_t	ciaGetTimerPSID();
void		ciaFakeIrqPSID();

// hacks
void 		ciaUpdateTOD(uint8_t song_speed);
	
// memory access interface (for memory.c)
uint8_t 	ciaReadMem(uint16_t addr);
void 		ciaWriteMem(uint16_t addr, uint8_t value);

#endif