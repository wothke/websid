/*
* This provides access to the *.sid file specific environment that 
* the emulation runs in.
* 
* Currently a respective implementation is provided by sidplayer.c
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
 
#ifndef TINYRSID_ENV_H
#define TINYRSID_ENV_H

#include "base.h"

#define MULTI_SID_TYPE 0x4E
#define MAX_SIDS 10					// might eventually need to be increased 

// *.sid file version
uint8_t envIsPSID();
uint8_t envIsRSID();

uint8_t envSidVersion();
uint8_t env2ndOutputChanIdx();

uint32_t envClockRate();

uint8_t envIsSID6581();

uint8_t envIsNTSC();
uint8_t envSetNTSC(uint8_t ntsc);

// PSID stuff
int8_t envIsRasterDrivenPSID();
int8_t envIsTimerDrivenPSID();

uint16_t envGetFreeSpace();

// where it starts
uint16_t envSidPlayAddr();

// playback speed according to meta data
uint8_t envCurrentSongSpeed();


uint16_t envNumberOfSamplesPerCall();

// array with 3 elements, 0 means not available
uint16_t* envSIDAddresses();
uint8_t*  envSID6581s();
uint8_t*  envSIDOutputChannels();

#endif