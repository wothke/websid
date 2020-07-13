/*
* Emulates MOS Technology 6510 CPU - as far as needed to play RSID files.
* 
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
 
#ifndef WEBSID_CPU_H
#define WEBSID_CPU_H

#include "base.h"

// global system clock related
uint32_t	cpuCycles();		// system cycles since starting the song
void 		cpuClockSystem();


// setup
void		cpuInit(void);
void 		cpuReset(uint16_t npc, uint8_t na);	// move program counter into new start position


// simulate next clock cycle 
void		cpuClock(void);


// hack used for digi handling only
uint8_t 	cpuIsInNMI();

// PSID crap
uint8_t		cpuIsValidPcPSID();
void		cpuResetToPSID(uint16_t npc);
void		cpuIrqFlagPSID(uint8_t on);

// debug only
uint16_t	cpuGetPC();
uint8_t		cpuGetSP();
#endif