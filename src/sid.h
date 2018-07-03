/*
* Poor man's emulation of the C64's SID.
*
* <p>Tiny'R'Sid (c) 2016 J.Wothke
* <p>version 0.81
* 
* Note: Only the SID impl has meanwhile been migrated to C++ (to ease 
* future multi-SID configurations..) The remaining code is still in C
* and this include file exports the SID functionalities in their old
* form.
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef TINYRSID_SID_H
#define TINYRSID_SID_H

#include "base.h"

// init/reset
void sidReset(uint32_t sampleRate, uint16_t *sidAddrs, uint8_t *sidIs6581, uint8_t compatibility);
void sidResetIO();	// FIXME properly refactor this legacy function

// direct manipulation of SID state (only for SID #1 - exclusively used for digis  )
void sidPoke(uint8_t reg, uint8_t val);		// update SID registers 

// @param voice numbering just continues for 2 or 3 SID configurations
void sidSetMute(uint8_t voice, uint8_t value);

// use current SID state to generate audio samples
void sidSynthRender (int16_t *buffer, uint32_t len, int16_t **synthTraceBufs);
void sidFilterSamples (uint8_t *digiBuffer, uint32_t len, int8_t v);


// special direct access to SID state (for digi.c) - since digi is only implemented
// for SID number 1, these implicitly refer to the SID ad 0xd400!
uint32_t sidGetSampleFreq();
uint16_t sidGetPulse(uint8_t voice);
uint16_t sidGetFreq(uint8_t voice);
uint8_t sidGetWave(uint8_t voice);
uint8_t sidGetAD(uint8_t voice);
uint8_t sidGetSR(uint8_t voice);

// memory access interface (for memory.c)
uint8_t sidReadMem(uint16_t addr);	// incl. special digi handling
void sidWriteMem(uint16_t addr, uint8_t value);


#endif
