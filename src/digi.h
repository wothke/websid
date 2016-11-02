#ifndef TINYRSID_DIGI_H
#define TINYRSID_DIGI_H

#include "defines.h"

// setup
void resetDigi(uint8_t compatibility);

// prepare buffers for handling of next frame
void moveDigiBuffer2NextFrame();		// preserve  "overflow" samples
void clearDigiBuffer();					// start from 0

// check if there are samples for the next frame
uint16_t getDigiOverflowCount();

// main hook analyzes SID writes for digi-sample playback
void handleSidWrite(uint16_t addr, uint8_t value);

// number of digi-samples that have been detected in the current frame
uint16_t getDigiCount();

// used to tag origin (main-prog, IRQ, NMI) of captured digi-samples
void markSampleOrigin(uint32_t mask, uint32_t offset, uint32_t originalDigiCount);

// utils used to render the camptured digi-samples
uint8_t renderDigiSamples(uint8_t * digiBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall);
void mergeDigi(int8_t hasDigi, int16_t *sound_buffer, uint8_t *digi_buffer, uint32_t len);

// utils for the handling of legacy PSID digis
int32_t generatePsidDigi(int32_t sIn);
void resetPsidDigi();


#endif