/*
 * This contains everything to do with the emulation of the basic CPU (MOS Technology 6502).
 * 
 * <p>Tiny'R'Sid (c) 2015 Jürgen Wothke
 * <p>version 0.81
 * 
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */
 
#ifndef TINYRSID_CPU_H
#define TINYRSID_CPU_H

#include "base.h"


// when recording original digi samples timestamps are 
// marked using the below "producer" specific bits
#define NMI_OFFSET_MASK		(0x1 << 24)
#define IRQ_OFFSET_MASK 	(0x1 << 25)
#define MAIN_OFFSET_MASK 	(0x1 << 26)

void cpuInit(void);

// process one CPU instruction
void cpuParse(void);

// move program counter into new start position
void cpuReset(uint16_t npc, uint8_t na);
void cpuResetToIrq(uint16_t npc);

// state manipulation
void cpuRegRestore(uint8_t i, uint8_t light);
void cpuRegSave(uint8_t i, uint8_t light);
void cpuRegReset();
void cpuResetCycles(uint32_t c);

// access to CPU status
void cpuSetProgramMode(uint32_t p);
uint32_t cpuGetProgramMode();
uint8_t cpuPcIsValid();
uint8_t cpuIrqFlag();
uint32_t cpuCycles();		// relative (e.g. measured from start of NMI, etc)
uint32_t cpuTotalCycles();	// absolute (measured from start of song)
//void cpuReSyncTotalCycles(uint32_t cyclesPerScreen);

// debug only
uint16_t cpuGetPC();
uint8_t cpuGetSP();
#endif