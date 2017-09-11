/*
 * This file provides the interface to the JavaScript world. 
 *  
 * <p>It also handles the *.sid input data and provides the respective 
 * environment exposed in env.h
 *
 * <p>Copyright (C) 2013 Juergen Wothke
 * <p>version 0.81
 *
 * <p>Emulation approach: Each call to computeAudioSamples() delivers some fixed number of audio samples.
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

#include <stdlib.h> 
#include <string.h>
#include <stdio.h>

#include "cia.h"	
#include "vic.h"	
#include "sid.h"	
#include "rsidengine.h"
#include "digi.h"	
#include "hacks.h"	
#include "memory.h"

#ifdef EMSCRIPTEN
#define EMSCRIPTEN_KEEPALIVE __attribute__((used))
#else
#define EMSCRIPTEN_KEEPALIVE
#endif


static uint32_t totalCyclesPerSec;
static uint32_t totalCyclesPerScreen;
static uint8_t cyclesPerRaster;
static uint16_t linesPerScreen;

static uint8_t sidVersion;
static uint8_t ntscMode= 0;
static uint8_t compatibility;
static uint8_t basicProg;

static uint32_t sampleRate;

static uint16_t loadAddr, initAddr, playAddr, loadEndAddr;
static uint8_t actualSubsong, maxSubsong;
static uint32_t playSpeed;

static uint8_t digiEnabled = 1;

// make it large enough for data of 8 PAL screens.. (allocate 
// statically to ease passing to ActionScript)
#define BUFLEN 8*882
static uint32_t soundBufferLen= BUFLEN;
static int16_t soundBuffer[BUFLEN];

static int16_t * synthBuffer= 0;

// only contains the digi data for 1 screen .. 
// and is directly merged into the soundBuffer
static uint8_t * digiBuffer= 0;	

static uint32_t chunkSize; 
static uint16_t numberOfSamplesPerCall;  

static uint8_t isPSID;


static uint32_t numberOfSamplesRendered = 0;
static uint32_t numberOfSamplesToRender = 0;

uint8_t envIsRSID() {
	return (isPSID == 0);
}

void envSetPsidMode(uint8_t m) {
	isPSID= m;
}

uint8_t envIsPSID() {
	return !envIsRSID();
}

uint16_t envSidPlayAddr() {
	return playAddr;
}

uint16_t envLinesPerScreen() {
	return linesPerScreen;
}

uint8_t envCyclesPerRaster() {
	return cyclesPerRaster;
}

uint16_t envNumberOfSamplesPerCall() {
	return numberOfSamplesPerCall;
}

uint32_t envCyclesPerSec() {
	return totalCyclesPerSec;
}

uint32_t envCyclesPerScreen() {
	return totalCyclesPerScreen;
}

static inline uint8_t get_bit(uint32_t val, uint8_t b)
{
    return (uint8_t) ((val >> b) & 1);
}

uint8_t envCurrentSongSpeed() {
	/*
	* PSID V2: songSpeed 0: means screen refresh based, i.e. ignore 
	*                       timer settings an just play 1x per refresh 
	*					 1: means 60hz OR CIA 1 timer A 
	*/	
	return get_bit(playSpeed, actualSubsong > 31 ? 31 : actualSubsong); 
}

int8_t envIsTimerDrivenPSID() {
	return ((envIsPSID() == 1) && (envCurrentSongSpeed() == 1));
}


int8_t envIsRasterDrivenPSID() {
	return ((envIsPSID() == 1) && (envCurrentSongSpeed() == 0));
}

static void resetAudioBuffers() {
	// song with original 60 hz playback.. adjust number of samples to play it faster.	

	if(ntscMode && envIsRasterDrivenPSID()) { 
		numberOfSamplesPerCall= 735; 		// NTSC: 735*60=44100
	} else {
		numberOfSamplesPerCall= 882; 		// PAL: 882*50=44100
	}
	chunkSize= numberOfSamplesPerCall*8; 	// e.g. data for 8 50hz screens (0.16 secs)

	if (synthBuffer)	free(synthBuffer);
	if (digiBuffer)	free(digiBuffer);

	synthBuffer= (int16_t*)malloc(sizeof(int16_t)*numberOfSamplesPerCall + 1);
	digiBuffer= (uint8_t*)malloc(sizeof(uint8_t*)*numberOfSamplesPerCall + 1);	
	
	numberOfSamplesRendered = 0;
	numberOfSamplesToRender = 0;	
}

static void resetTimings() {	
	uint8_t fps;
	if(ntscMode) {		
		fps= 60;
		cyclesPerRaster= 65;			// NTSC
		linesPerScreen = 263;
	} else {
		fps= 50;
		cyclesPerRaster= 63;			// PAL	
		linesPerScreen = 312;
	}
	/*
	hack: correct cycle handling would consider badlines, usage of sprites, 
	bus takeover cycles, etc (without which a screen has too many cpu cycles
	available per frame/screen refresh). This emu does NOT simulate these things
	but instead the following hack is used to fix obvious problems with the 
	playback of THCM's stuff..  without which THCM's player makes too many 
	NMI calls per screen..
	*/
	
	// increased to 4 for the benefit of Kapla_Caves.sid (problem: 
	// due to this hack main loop players will be getting too many cycles)
	uint32_t badlineCycles= cyclesPerRaster*4;	
	
	// NTSC: 17095	/ PAL: 19656		
	totalCyclesPerScreen= cyclesPerRaster*linesPerScreen- badlineCycles;			
	// NTSC: 1025700 (clock would be: 1022727);		/ PAL: 982800 (clock would be; 985248);			
	totalCyclesPerSec= totalCyclesPerScreen*fps; 
}

static uint32_t computeAudioSamples()  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE computeAudioSamples() {
	numberOfSamplesRendered = 0;
			
	uint32_t sampleBufferIdx=0;
	uint32_t hasDigi= 0;
	
	while (numberOfSamplesRendered < chunkSize)
	{
		if (numberOfSamplesToRender == 0) {
			numberOfSamplesToRender = numberOfSamplesPerCall;
			sampleBufferIdx=0;
			hasDigi= rsidProcessOneScreen(synthBuffer, digiBuffer, 
						totalCyclesPerScreen, numberOfSamplesPerCall);
			if (!digiEnabled) hasDigi= 0;
		}
		
		if (numberOfSamplesRendered + numberOfSamplesToRender > chunkSize) {
			uint32_t availableSpace = chunkSize-numberOfSamplesRendered;
			
			memcpy(&soundBuffer[numberOfSamplesRendered], &synthBuffer[sampleBufferIdx], sizeof(int16_t)*availableSpace);
	
			digiMergeSampleData(hasDigi, &soundBuffer[numberOfSamplesRendered], &digiBuffer[sampleBufferIdx], availableSpace);			
			
			sampleBufferIdx += availableSpace;
			numberOfSamplesToRender -= availableSpace;
			numberOfSamplesRendered = chunkSize;
		} else {
			memcpy(&soundBuffer[numberOfSamplesRendered], &synthBuffer[sampleBufferIdx], sizeof(int16_t)*numberOfSamplesToRender);

			digiMergeSampleData(hasDigi, &soundBuffer[numberOfSamplesRendered], &digiBuffer[sampleBufferIdx], numberOfSamplesToRender);
		
			numberOfSamplesRendered += numberOfSamplesToRender;
			numberOfSamplesToRender = 0;
		} 
	}

	return (numberOfSamplesRendered);
}

// bit0=voice0, bit1=voice1,..
static uint32_t enableVoices(uint32_t mask)  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE enableVoices(uint32_t mask) {
	for(uint8_t i= 0; i<3; i++) {
		sidSetMute(i, !(mask&0x1));
		mask = mask >> 1;
	}
	// "voice 4" is digi-output..
	digiEnabled= mask&0x1;
	
	return 0;
}

static uint32_t playTune(uint32_t selectedTrack)  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE playTune(uint32_t selectedTrack) {	
	if (sidVersion <= 2) {
		actualSubsong= selectedTrack & 0xff;
		digiEnabled = 1;

		rsidPlayTrack(sampleRate, compatibility, &initAddr, loadEndAddr, playAddr, actualSubsong);

		resetAudioBuffers();
	} else {
		// there seems to be new version 3 for dual-SID stuff (see Mahoney)
	}
	return 0;
}

static uint16_t loadSIDFromMemory(void *pSidData, 
					uint16_t *load_addr, uint16_t *load_end_addr, 
					uint16_t *init_addr, uint16_t *play_addr, uint8_t *subsongs, 
					uint8_t *startsong, uint32_t *speed, uint32_t file_size)
{
    uint8_t *pData;
    uint8_t data_file_offset;	
	
    pData = (uint8_t*)pSidData;
    data_file_offset = pData[7];

    *load_addr = pData[8]<<8;
    *load_addr|= pData[9];

    *init_addr = pData[10]<<8;
    *init_addr|= pData[11];

    *play_addr = pData[12]<<8;
    *play_addr|= pData[13];

    *subsongs = pData[0xf]-1;
    *startsong = pData[0x11]-1;

	if (*load_addr == 0) {
		// original C64 binary file format
		
		*load_addr = pData[data_file_offset];
		*load_addr|= pData[data_file_offset+1]<<8;
		
		data_file_offset +=2;
	}
	if (*init_addr == 0) {
		*init_addr= *load_addr;	// 0 implies that init routine is at load_addr
	}	
	
    *speed = pData[0x12]<<24;
    *speed|= pData[0x13]<<16;
    *speed|= pData[0x14]<<8;
    *speed|= pData[0x15];
    
	*load_end_addr= *load_addr+file_size-data_file_offset;
	
	long size= file_size-data_file_offset;
	if (size < 0 || size >0xffff) {
		return 0;		// illegal sid file
	}
	
	rsidLoadSongBinary(&pData[data_file_offset], *load_addr, size);

    return *load_addr;
}


	// 0: loadAddr;
	// 1: playSpeed;
	// 2: maxSubSong;
	// 3: actualSubSong;
	// 4: songName;
	// 5: songAuthor;
	// 6: songCopyright;
static void* loadResult [7];

static 	char song_name[32], song_author[32], song_copyright[32];


static uint32_t loadSidFile(void * inBuffer, uint32_t inBufSize)  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE loadSidFile(void * inBuffer, uint32_t inBufSize) {
	uint8_t *inputFileBuffer= (uint8_t *)inBuffer;	

	if (inBufSize < 0x7c) return 1;	// we need at least a header..

	memResetKernelROM();		// read only (only need to do this once)

    sampleRate= 44100;		// TODO: extend API & use actual target sample rate
	initAddr= 0;
	playAddr= 0;
	loadEndAddr= 0;
	actualSubsong= 0;
	maxSubsong= 0;	
	playSpeed= 0;		
	ntscMode= 0;
	
	envSetPsidMode(inputFileBuffer[0x00]==0x50?1:0);
	memResetRAM(envIsPSID());
	
	sidVersion= inputFileBuffer[0x05];
	
	// note: emu is not differenciating between SID chip versions (respective flags
	// are therefore ignored - see bits 4/5)
	
	uint8_t flags= inputFileBuffer[0x77];
	
	basicProg= (envIsRSID() && (flags & 0x2));	// C64 BASIC program need to be started..
	
	compatibility= ( (sidVersion & 0x2) &&  ((flags & 0x2) == 0));	
	ntscMode= (sidVersion == 2) && envIsPSID() && (flags & 0x8); // NTSC bit
	
	uint8_t i;
    for (i=0;i<32;i++) song_name[i] = inputFileBuffer[0x16+i];
    for (i=0;i<32;i++) song_author[i] = inputFileBuffer[0x36+i]; 
    for (i=0;i<32;i++) song_copyright[i] = inputFileBuffer[0x56+i];
	
    if (!loadSIDFromMemory(inputFileBuffer, &loadAddr, &loadEndAddr, &initAddr, 
			&playAddr, &maxSubsong, &actualSubsong, &playSpeed, inBufSize)) {
		
		return 1;	// could not load file
	}

	if (basicProg) rsidStartFromBasic(&initAddr);
	
	// global settings that depend on the loaded music file
	resetTimings();
	
	loadResult[0]= &loadAddr;
	loadResult[1]= &playSpeed;
	loadResult[2]= &maxSubsong;
	loadResult[3]= &actualSubsong;
	loadResult[4]= song_name;
	loadResult[5]= song_author;
	loadResult[6]= song_copyright;

	return 0;
}
static char** getMusicInfo() __attribute__((noinline));
static char** EMSCRIPTEN_KEEPALIVE getMusicInfo() {
	return (char**)loadResult;
}

static uint32_t getSoundBufferLen() __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE getSoundBufferLen() {
	return numberOfSamplesRendered;	// in samples
}

static char* getSoundBuffer() __attribute__((noinline));
static char* EMSCRIPTEN_KEEPALIVE getSoundBuffer() {
	return (char*) soundBuffer;
}

static uint32_t getSampleRate() __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE getSampleRate() {
	return sampleRate;
}

