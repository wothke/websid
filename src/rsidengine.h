/*
 * This is the external interface for using the emulatior.
 * 
 * <p>implicitly uses the enviroment provided by env.h
 *
 * <p>Tiny'R'Sid (c) 2015 Jürgen Wothke
 * <p>version 0.81
 * 
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */
#ifndef TINYRSID_RSIDENGINE_H
#define TINYRSID_RSIDENGINE_H

#include "base.h"

// setup/restart
void rsidReset(uint32_t mixfrq, uint8_t compatibility);

// load the C64 program data into the emulator (just the binary without the .sid file header)
void rsidLoadSongBinary(uint8_t *src, uint16_t destAddr, uint32_t len);

// then the emulation can be initiated
void rsidPlayTrack(uint32_t sampleRate, uint8_t compatibility, uint16_t *pInitAddr, 
					uint16_t loadEndAddr, uint16_t playAddr, uint8_t actualSubsong);

// runs the emulator for the duration of one C64 screen refresh and returns the 
// respective audio output
uint8_t rsidProcessOneScreen(int16_t * synthBuffer, uint8_t *digiBuffer, uint32_t 
					cyclesPerScreen, uint16_t samplesPerCall);

uint32_t rsidGetFrameCount(); // FIXME remove/cleanup sid
#endif
