/*
* Poor man's emulation of the SID's digi sample playback features.
*
* <p>Basic approach: Samples and timing info is recorded and
* at the end of a frame that info is merged with regular SID output.
*
* <p>Tiny'R'Sid (c) 2012 J.Wothke
* <p>version 0.81
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef TINYRSID_DIGI_H
#define TINYRSID_DIGI_H

#include "base.h"

// setup
void digiReset(uint8_t compatibility, uint8_t isModel6581);

// handling of collected data
void digiMoveBuffer2NextFrame();	// preserve  "overflow" samples
void digiClearBuffer();				// start from 0
uint16_t digiGetOverflowCount();	//  samples for the next frame?
uint16_t digiGetCount();			//  current number of samples

// detection of sample playback
uint8_t digiDetectSample(uint16_t addr, uint8_t value);
	// tag origin (main-prog, IRQ, NMI) of captured digi-samples
void digiTagOrigin(uint32_t mask, uint32_t offset, uint32_t originalDigiCount);

// rendering the captured digi-samples
uint8_t digiRenderSamples(uint8_t * digiBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall);
void digiMergeSampleData(int8_t hasDigi, int16_t *sound_buffer, uint8_t *digi_buffer, uint32_t len);

// utils for the handling of legacy PSID digis
int32_t digiGenPsidSample(int32_t sIn);
#endif