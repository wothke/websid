/*
* Poor man's emulation of the C64's CIA timers.
*
* <p>Tiny'R'Sid (c) 2012 J.Wothke
* <p>version 0.81
* 
* <p>Only those features actually observed in RSID files have been implemented, i.e. simple 
* cycle counting and timer B to timer A linking.
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#ifndef TINYRSID_CIA_H
#define TINYRSID_CIA_H

#include "base.h"

#define CIA1 0
#define CIA2 1
#define TIMER_A 0
#define TIMER_B 1

void ciaReset(uint32_t cyclesPerScreen, uint8_t isRsid, uint32_t failMarker);
void ciaResetPsid60Hz();

// interface used to interact with CIA
int ciaIsActive(uint8_t ciaIdx);
uint32_t ciaForwardToNextInterrupt(uint8_t ciaIdx, uint32_t timeLimit);

// hacks
void ciaSetNmiVectorHack();
void ciaUpdateTOD(uint8_t songSpeed);
void ciaSignalUnderflow(uint8_t ciaIdx, uint8_t timerIdx);
	
// memory access interface (for memory.c)
uint8_t ciaReadMem(uint16_t addr);
void ciaWriteMem(uint16_t addr, uint8_t value);

void ciaPrintDebug();

#endif