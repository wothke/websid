/*
* Poor man's emulation of the C64's VIC-II (Video-Interface-Chip).
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef WEBSID_VIC_H
#define WEBSID_VIC_H

#include "base.h"

void		vicReset(uint8_t is_rsid, uint8_t ntsc_mode);
void		vicSetModel(uint8_t ntsc_mode);

void		vicClock();
uint8_t		vicIRQ();

uint8_t		vicStunCPU();	// 0: no stun; 1: allow "bus write"; 2: stun


double		vicFPS();	// frames per second
uint16_t	vicLinesPerScreen();
uint32_t	vicCyclesPerScreen();

// PSID crap
void		vicFakeIrqPSID();

// memory access interface (for memory.c)
void		vicWriteMem(uint16_t addr, uint8_t value);
uint8_t		vicReadMem(uint16_t addr);

#endif