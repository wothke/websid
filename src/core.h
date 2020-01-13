/*
* This is the interface for using the emulatior.
* 
* implicitly uses the environment provided by env.h
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef WEBSID_CORE_H
#define WEBSID_CORE_H

#include "base.h"


class Core {
public:
	// load the C64 program data into the emulator (just the binary without the .sid file header)
	static void loadSongBinary(uint8_t *src, uint16_t destAddr, uint16_t len, uint8_t basic_mode);

	// then the emulation can be initiated
	static void startupSong(uint32_t sampleRate, uint8_t ntscMode, uint8_t compatibility, uint8_t basic_prog, 
						uint16_t *pInitAddr, uint16_t loadEndAddr, uint16_t playAddr, uint8_t actualSubsong);

	// runs the emulator for the duration of one C64 screen refresh and returns the 
	// respective audio output
	static uint8_t runOneFrame(int16_t * synthBuffer, int16_t **synthTraceBufs, uint16_t samplesPerCall);
	
	static void resetC64();
#ifdef TEST
	static void rsidRunTest();
#endif
};

#endif
