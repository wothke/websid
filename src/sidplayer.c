/*
 * This file provides the initial entry points into the emulator (e.g. "load music file", etc).
 *
 * <p>version 0.7
 * <p>Copyright (C) 2013 Juergen Wothke
 *
 * <p>Emulation approach: Each call to computeAudioSamples() delivers some fixed number of audio samples.
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

#include <stdlib.h> 
#include <string.h>
#include <stdio.h>

#include "nanocia.h"	
#include "nanovic.h"	
#include "sidengine.h"	
#include "rsidengine.h"
#include "digi.h"	
#include "hacks.h"	

#ifdef EMSCRIPTEN
#define EMSCRIPTEN_KEEPALIVE __attribute__((used))
#else
#define EMSCRIPTEN_KEEPALIVE
#endif

#define CYCLELIMIT 2000000		// if 'init' takes longer than 2 secs then something is wrong (mb in endless loop)

static unsigned long sTotalCyclesPerSec;
static unsigned long sTotalCyclesPerScreen;
static unsigned char sCyclesPerRaster;
static unsigned int sLinesPerScreen;

static unsigned char sSidVersion;
static unsigned char sNtscMode= 0;
static unsigned char sCompatibility;

static unsigned long sSampleRate;

static word sInitAddr, sPlayAddr, sLoadEndAddr;
static byte sActualSubsong, sMaxSubsong;
static unsigned long sPlaySpeed;


#define BUFLEN 8*882
static int sSoundBufferLen= BUFLEN;
static short sSoundBuffer[BUFLEN];		// make it large enough for data of 8 PAL screens.. (allocate statically to ease passing to ActionScript)
static short * sSynthBuffer= 0;
static unsigned char * sDigiBuffer= 0;	// only contains the digi data for 1 screen .. and is directly merged into the sSoundBuffer

static unsigned long sChunkSize; 
static unsigned int sNumberOfSamplesPerCall;  

/*
* snapshot of c64 memory right after loading.. it is restored before playing a new track..
*/
static unsigned char sMemorySnapshot[MEMORY_SIZE];

static int sNumberOfSamplesRendered = 0;
static int sNumberOfSamplesToRender = 0;

word getSidPlayAddr() {
	return sPlayAddr;
}

unsigned int getLinesPerScreen() {
	return sLinesPerScreen;
}

unsigned char getCyclesPerRaster() {
	return sCyclesPerRaster;
}

unsigned long getCyclesPerSec() {
	return sTotalCyclesPerSec;
}

unsigned long getCyclesPerScreen() {
	return sTotalCyclesPerScreen;
}

static inline unsigned char get_bit(unsigned long val, unsigned char b)
{
    return (unsigned char) ((val >> b) & 1);
}

unsigned char getCurrentSongSpeed() {
	/*
	* PSID V2: songSpeed 0: means screen refresh based, i.e. ignore timer settings an just play 1x per refresh 
	*					 1: means 60hz OR CIA 1 timer A 
	*/	
	return get_bit(sPlaySpeed, sActualSubsong > 31 ? 31 : sActualSubsong); 
}

static void resetAudioBuffers() {
	// song with original 60 hz playback.. adjust number of samples to play it faster.	

	if(sNtscMode && isRasterDrivenPsid()) { 
		sNumberOfSamplesPerCall= 735; 										// NTSC: 735*60=44100
	} else {
		sNumberOfSamplesPerCall= 882; 										// PAL: 882*50=44100
	}
	sChunkSize= sNumberOfSamplesPerCall*8; 	// e.g. data for 8 50hz screens (0.16 secs)

	if (sSynthBuffer)	free(sSynthBuffer);
	if (sDigiBuffer)	free(sDigiBuffer);

	sSynthBuffer= (short*)malloc(sizeof(short)*sNumberOfSamplesPerCall + 1);
	sDigiBuffer= (unsigned char*)malloc(sizeof(char*)*sNumberOfSamplesPerCall + 1);	
	
	sNumberOfSamplesRendered = 0;
	sNumberOfSamplesToRender = 0;	
}

static void resetTimings() {	
	int fps;
	if(sNtscMode) {		
		fps= 60;
		sCyclesPerRaster= 65;												// NTSC
		sLinesPerScreen = 263;
	} else {
		fps= 50;
		sCyclesPerRaster= 63;													// PAL	
		sLinesPerScreen = 312;
	}
	// hack: correct cycle handling would consider badlines, usage of sprites, bus takeover cycles, etc
	// but we just use the following hack to fix obvious problems with the playback of THCM's stuff..  without 
	// which THCM's player makes too many NMI calls per screen..
	int badlineCycles= sCyclesPerRaster*4;	// increased to 4 for the benefit of Kapla_Caves.sid
	sTotalCyclesPerScreen= sCyclesPerRaster*sLinesPerScreen- badlineCycles;			// NTSC: 17095	/ PAL: 19656		
	sTotalCyclesPerSec= sTotalCyclesPerScreen*fps; // NTSC: 1025700 (clock would be: 1022727);		/ PAL: 982800 (clock would be; 985248);			
}

static int computeAudioSamples()  __attribute__((noinline));
static int EMSCRIPTEN_KEEPALIVE computeAudioSamples() {
	sNumberOfSamplesRendered = 0;
			
	unsigned long sampleBufferIdx=0;
	int hasDigi= 0;
	
	while (sNumberOfSamplesRendered < sChunkSize)
	{
		if (sNumberOfSamplesToRender == 0) {
			sNumberOfSamplesToRender = sNumberOfSamplesPerCall;
			sampleBufferIdx=0;
			hasDigi= processOneScreen(sSynthBuffer, sDigiBuffer, sTotalCyclesPerScreen, sNumberOfSamplesPerCall);
		}
		
		if (sNumberOfSamplesRendered + sNumberOfSamplesToRender > sChunkSize) {
			int availableSpace = sChunkSize-sNumberOfSamplesRendered;
			
			memcpy(&sSoundBuffer[sNumberOfSamplesRendered], &sSynthBuffer[sampleBufferIdx], sizeof(short)*availableSpace);
	
			mergeDigi(hasDigi, &sSoundBuffer[sNumberOfSamplesRendered], &sDigiBuffer[sampleBufferIdx], availableSpace);			
			
			sampleBufferIdx += availableSpace;
			sNumberOfSamplesToRender -= availableSpace;
			sNumberOfSamplesRendered = sChunkSize;
		} else {
			memcpy(&sSoundBuffer[sNumberOfSamplesRendered], &sSynthBuffer[sampleBufferIdx], sizeof(short)*sNumberOfSamplesToRender);

			mergeDigi(hasDigi, &sSoundBuffer[sNumberOfSamplesRendered], &sDigiBuffer[sampleBufferIdx], sNumberOfSamplesToRender);
		
			sNumberOfSamplesRendered += sNumberOfSamplesToRender;
			sNumberOfSamplesToRender = 0;
		} 
	}

	return (sNumberOfSamplesRendered);
}

static void setupDefaultMemBanks() {
	unsigned char memBankSetting= 0x37;	// default memory config: basic ROM, IO area & kernal ROM visible
	if (!isRsid()) {
		// problem: some PSID init routines want to initialize registers in the IO area while others
		// actually expect to use the RAM in that area.. none of them setup the memory banks accordingly :(

		if ((sInitAddr >= 0xd000) && (sInitAddr < 0xe000)) {
			memBankSetting= 0x34;	// default memory config: all RAM
			
		} else if ((sInitAddr >= 0xe000)) {
			// PSIDv2 songs like IK_plus.sid, Pandora.sid use the ROM below the kernal *without* setting 0x0001
			// so obviously here we must use a default like this:
			memBankSetting= 0x35;	// default memory config: IO area visible, RAM $a000-b000 + $e000-$ffff

		} else if (sLoadEndAddr >= 0xa000) {
			memBankSetting= 0x36;
		} else {
			// normally the kernal ROM should be visible: e.g. A-Maz-Ing.sid uses kernal ROM routines & vectors 
			// without setting $01!
			memBankSetting= 0x37;	// default memory config: basic ROM, IO area & kernal ROM visible			
		}
	}
	memory[0x0001]= memBankSetting;	
}

static void resetPsidMemBanks() {
	if (isPsid()) {
		// some PSID actually switch the ROM back on eventhough their code is located there! (e.g. 
		// Ramparts.sid - the respective .sid even claims to be "C64 compatible" - what a joke) 
		
		if ((sPlayAddr >= 0xd000) && (sPlayAddr < 0xe000)) {
			memory[0x0001]= 0x34;
		} else if (sPlayAddr >= 0xe000) {
			memory[0x0001]= 0x35;
		} else if (sPlayAddr >= 0xa000) {
			memory[0x0001]= 0x36;
		} else if (sPlayAddr == 0x0){
			// keep whatever the PSID init setup
		} else {
			memory[0x0001]= 0x37;
		}		
	}	
}

const static unsigned char sFF48IrqHandler[19] ={0x48,0x8A,0x48,0x98,0x48,0xBA,0xBD,0x04,0x01,0x29,0x10,0xEA,0xEA,0xEA,0xEA,0xEA,0x6C,0x14,0x03};
const static unsigned char sEA7EIrqHandler[9] ={0xAD,0x0D,0xDC,0x68,0xA8,0x68,0xAA,0x68,0x40};
const static unsigned char sFE43NmiHandler[4] ={0x78,0x6c,0x18,0x03};

static void resetKernelROM() {
	// we dont have the complete rom but in order to ensure consistent stack handling (regardless of
	// which vector the sid-program is using) we provide dummy versions of the respective standard 
	// IRQ/NMI routines..
	
    memSet(&kernal_rom[0], 0x0, KERNAL_SIZE);

    memcpy(&kernal_rom[0x1f48], sFF48IrqHandler, 19);	// $ff48 irq routine
    memSet(&kernal_rom[0x0a31], 0xea, 0x4d);			// $ea31 fill some NOPs	
    memcpy(&kernal_rom[0x0a7e], sEA7EIrqHandler, 9);	// $ea31 return sequence
    memcpy(&kernal_rom[0x1e43], sFE43NmiHandler, 4);	// $fe43 nmi handler
	
	kernal_rom[0x1ffe]= 0x48;
	kernal_rom[0x1fff]= 0xff;
		
	kernal_rom[0x1ffa]= 0x43;	// standard NMI vectors (this will point into the void at: 0318/19)
	kernal_rom[0x1ffb]= 0xfe;	

	// basic rom init routines (e.g. used by Soundking_V1.sid)
	kernal_rom[0x1D50]= 0x60;	
	kernal_rom[0x1D15]= 0x60;	
	kernal_rom[0x1F5E]= 0x60;	
		
	// kernal vector: initalise screen and keyboard (e.g. used by Voodoo_People_part_1.sid)
	kernal_rom[0x1F81]= 0x60;	
}


static int playTune(int selectedTrack)  __attribute__((noinline));
static int EMSCRIPTEN_KEEPALIVE playTune(int selectedTrack) {
	sActualSubsong= selectedTrack;

    synth_init(sSampleRate);			// sidengine.c stuff
	resetDigi(sCompatibility);			// digi.c stuff
	resetRSID();			// rsidengine.c stuff
	
	// restore original mem image.. previous "sInitAddr" run may have corrupted the state
	memcpy(memory, sMemorySnapshot, MEMORY_SIZE);		
	hackIfNeeded(&sInitAddr);	
	
	setupDefaultMemBanks();	// PSID crap
	
	// if sInitAddr call does not complete then it is likely in an endless loop / maybe digi player
	setProgramStatus(callMain(sInitAddr, sActualSubsong, 0, CYCLELIMIT));		
		
	resetPsidMemBanks();	// PSID again

	resetAudioBuffers();

	return 0;
}

static void resetRAM() {
    memSet(memory, 0x0, sizeof(memory));

	memory[0x0314]= 0x31;		// standard IRQ vector
	memory[0x0315]= 0xea;

	// Master_Blaster_intro.sid actually checks this:
	memory[0x00cb]= 0x40;		// no key pressed 
	
	// for our PSID friends who don't know how to properly use memory banks lets mirror the kernal ROM into RAM
	if (isPsid()) {
		memcpy(&memory[0xe000], &kernal_rom[0], 0x2000);
	}
}

static unsigned short loadSIDFromMemory(unsigned char *dest, void *pSidData, unsigned short *load_addr, unsigned short *load_end_addr, unsigned short *init_addr, unsigned short *play_addr, unsigned char *subsongs, unsigned char *startsong, unsigned long *speed, unsigned long file_size)
{
    unsigned char *pData;
    unsigned char data_file_offset;	
	
    pData = (unsigned char*)pSidData;
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
	
    memcpy(&dest[*load_addr], &pData[data_file_offset], size);
    
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

static unsigned short sLoad_addr;

static int loadSidFile(void * inBuffer, unsigned long inBufSize)  __attribute__((noinline));
static int EMSCRIPTEN_KEEPALIVE loadSidFile(void * inBuffer, unsigned long inBufSize) {
	unsigned char *inputFileBuffer= (unsigned char *)inBuffer;	

	if (inBufSize < 0x7c) return 1;	// we need at least a header..

	resetKernelROM();		// read only (only need to do this once)

    sSampleRate= 44100;		// TODO: extend API & use actual target sample rate
	sInitAddr= 0;
	sPlayAddr= 0;
	sLoadEndAddr= 0;
	sActualSubsong= 0;
	sMaxSubsong= 0;	
	sPlaySpeed= 0;		
	sNtscMode= 0;
	
	setPsidMode(inputFileBuffer[0x00]==0x50?1:0);
	resetRAM();	// depends on PSID/RSID distinction
	
	sSidVersion= inputFileBuffer[0x05];
	sCompatibility= ( (sSidVersion & 0x2) &&  ((inputFileBuffer[0x77] & 0x2) == 0));	
	sNtscMode= (sSidVersion == 2) && isPsid() && (inputFileBuffer[0x77] & 0x8); // NTSC bit
	
	int i;
    for (i=0;i<32;i++) song_name[i] = inputFileBuffer[0x16+i];
    for (i=0;i<32;i++) song_author[i] = inputFileBuffer[0x36+i]; 
    for (i=0;i<32;i++) song_copyright[i] = inputFileBuffer[0x56+i];
	
    if (!loadSIDFromMemory(memory, inputFileBuffer, &sLoad_addr, &sLoadEndAddr, &sInitAddr, 
			&sPlayAddr, &sMaxSubsong, &sActualSubsong, &sPlaySpeed, inBufSize)) {
		
		return 1;	// could not load file
	}

	memcpy(sMemorySnapshot, memory, MEMORY_SIZE);	// backup initial state for use in 'track change'

	// global settings that depend on the loaded music file
	resetTimings();
	
	loadResult[0]= &sLoad_addr;
	loadResult[1]= &sPlaySpeed;
	loadResult[2]= &sMaxSubsong;
	loadResult[3]= &sActualSubsong;
	loadResult[4]= song_name;
	loadResult[5]= song_author;
	loadResult[6]= song_copyright;

	return 0;
}

static char** getMusicInfo() __attribute__((noinline));
static char** EMSCRIPTEN_KEEPALIVE getMusicInfo() {
	return loadResult;
}

static int getSoundBufferLen() __attribute__((noinline));
static int EMSCRIPTEN_KEEPALIVE getSoundBufferLen() {
	return sNumberOfSamplesRendered;	// in samples
}

static char* getSoundBuffer() __attribute__((noinline));
static char* EMSCRIPTEN_KEEPALIVE getSoundBuffer() {
	return (char*) sSoundBuffer;
}

static int getSampleRate() __attribute__((noinline));
static int EMSCRIPTEN_KEEPALIVE getSampleRate() {
	return sSampleRate;
}
