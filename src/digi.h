#ifndef TINYRSID_DIGI_H
#define TINYRSID_DIGI_H

// setup
void resetDigi(unsigned char compatibility);

// prepare buffers for handling of next frame
void moveDigiBuffer2NextFrame();		// preserve  "overflow" samples
void clearDigiBuffer();					// start from 0

// check if there are samples for the next frame
int getDigiOverflowCount();

// main hook analyzes SID writes for digi-sample playback
void handleSidWrite(unsigned short addr, unsigned char value);

// number of digi-samples that have been detected in the current frame
unsigned int getDigiCount();

// used to tag origin (main-prog, IRQ, NMI) of captured digi-samples
void markSampleOrigin(unsigned long mask, unsigned long offset, unsigned long originalDigiCount);

// utils used to render the camptured digi-samples
int renderDigiSamples(unsigned char * digiBuffer, unsigned long cyclesPerScreen, unsigned int samplesPerCall);
void mergeDigi(int hasDigi, short *sound_buffer, unsigned char *digi_buffer, unsigned long len);

// utils for the handling of legacy PSID digis
int generatePsidDigi(int sIn);
void resetPsidDigi();


#endif