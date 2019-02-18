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
void digiResetModel(uint8_t isModel6581);
void digiReset(uint8_t compatibility, uint8_t isModel6581, uint8_t overflowFrames);

// handling of collected data
void digiFetchOverflowData();		// gets one frame worth of samples from overflow buffer
void digiClearBuffer();				// start from 0
uint16_t digiGetOverflowCount();	//  samples for the next frame?
uint16_t digiGetCount();			//  current number of samples

// detection of sample playback
uint8_t digiDetectSample(uint16_t addr, uint8_t value);
	// tag origin (main-prog, IRQ, NMI) of captured digi-samples
void digiTagOrigin(uint32_t mask, uint32_t offset, uint16_t originalDigiCount, uint16_t originalDigiOverflowCount);

// rendering the captured digi-samples
uint8_t digiRenderSamples(uint8_t * digiBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall);
void digiMergeSampleData(int8_t hasDigi, int16_t *sound_buffer, uint8_t *digi_buffer, uint32_t len);

// utils for the handling of legacy PSID digis
int32_t digiGenPsidSample(int32_t sIn);

uint8_t digiIsMahoneyMode();
uint8_t digiIsIceGuysMode();

void digiBaseOffset(uint32_t base);


/*
* diagnostic information for GUI use
*/
typedef enum {
	DigiNone=0,
	DigiD418=1,			// legacy 4-bit approach
	DigiMahoneyD418=2,	// Mahoney's "8-bit" D418 samples
	DigiFM=3,			// 8-bit frequency modulation; e.g. Vicious_SID_2-15638Hz.sid, LMan - Vortex.sid, etc
	DigiPWM= 4,			// older PWM impls, e.g. Bla_Bla.sid, Bouncy_Balls_RCA_Intro.sid
	DigiPWMTest=5,		// new test-bit based PWM; e.g. Wonderland_XII-Digi_part_1.sid, GhostOrGoblin.sid, etc
	DigiWFM=6,			// 2-bit waveform modulation approach; e.g. IceGuys
} DigiType;

DigiType digiGetType();
const char * digiGetTypeDesc();
uint16_t digiGetRate();

#endif