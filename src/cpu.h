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

// system clock related
uint32_t	cpuCycles();		// system cycles since starting the song
void 		cpuClockSystem();


void		cpuInit(void);
int8_t		cpuClock(void);


// move program counter into new start position
void cpuReset(uint16_t npc, uint8_t na);


void cpuRegReset();
uint8_t cpuPcIsValid();


// PSID crap
void cpuResetToIrqPSID(uint16_t npc);

// debug only
uint16_t cpuGetPC();
uint8_t cpuGetSP();
#endif