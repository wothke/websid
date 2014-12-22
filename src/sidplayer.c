/*
 * This file contains the basic glue code which provides the interface between original TinySID C code and the additional
 * RSID emu logic.
 *
 * <p>version 0.7
 * <p>Copyright (C) 2013 Juergen Wothke
 *
 * <p>Emulation approach: Sample data is generated for the interval between two IRQs, e.g. typically one C64 screen refresh 
 * (50 or 60Hz) at a time. The resulting data is then filled into the "sSoundBuffer" below.
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

#include <stdlib.h> 
#include <string.h>

#include "nanocia.h"	
#include "nanovic.h"	
#include "sidengine.h"	
#include "rsidengine.h"
#include "hacks.h"	

#ifdef EMSCRIPTEN
#define EMSCRIPTEN_KEEPALIVE __attribute__((used))
#else
#define EMSCRIPTEN_KEEPALIVE
#endif

#define CYCLELIMIT 2000000		// if 'init' takes longer than 2 secs then something is wrong (mb in endless loop)


// in order to avoid unexpected Alchemy linker issues to do with C library files, the complete C code
// is directly included into this file.. (feel free to search for a proper solution ;-)

static unsigned long sTotalCyclesPerSec;
unsigned long sTotalCyclesPerScreen;
unsigned char sCyclesPerRaster;
unsigned int sLinesPerScreen;
unsigned long sIrqTimeout;

unsigned char sIsPSID;
static unsigned char sSidVersion;
static unsigned char sIsC64compatible= 1;
static unsigned char sNtscMode= 0;

static unsigned long sSampleRate;

int isV2() {
	return (sSidVersion == 2);
}

int isC64compatible() {
	return sIsC64compatible;
}

word sInitAddr, sPlayAddr, sLoadEndAddr;
static byte sActualSubsong, sMaxSubsong;
static unsigned long sPlaySpeed;

unsigned char sMainProgStatus= 0; 				// 0: completed 1: interrupted by cycleLimit

		
dword sSoundBuffer[8*882];		// make it large enough for data of 8 PAL screens.. (allocate statically to ease passing to ActionScript)
dword * sSynthBuffer= 0;
unsigned char * sDigiBuffer= 0;	// only contains the digi data for 1 screen .. and is directly merged into the sSoundBuffer

unsigned long sChunkSize; 
unsigned int sNumberOfSamplesPerCall;  

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

int isTimerDrivenPsid() {
	return ((sIsPSID == 1) && (getCurrentSongSpeed() == 1));
}

int isRasterDrivenPsid() {
	return ((sIsPSID == 1) && (getCurrentSongSpeed() == 0));
}

void initBuffers() {
	// song with original 60 hz playback.. adjust number of samples to play it faster.	

	if(sNtscMode && isRasterDrivenPsid()) { 
		sNumberOfSamplesPerCall= 735; 										// NTSC: 735*60=44100
	} else {
		sNumberOfSamplesPerCall= 882; 										// PAL: 882*50=44100
	}
	sChunkSize= sNumberOfSamplesPerCall*8; 	// e.g. data for 8 50hz screens (0.16 secs)

	if (sSynthBuffer)	free(sSynthBuffer);
	if (sDigiBuffer)	free(sDigiBuffer);

	sSynthBuffer= (dword*)malloc(sizeof(dword)*sNumberOfSamplesPerCall + 1);
	sDigiBuffer= (unsigned char*)malloc(sizeof(char*)*sNumberOfSamplesPerCall + 1);	
}

/*
* snapshot of c64 memory right after loading.. it is restored before playing a new track..
*/
unsigned char sMemorySnapshot[MEMORY_SIZE];

/*
* @param digi   is an unsigned 8-bit sample (i.e. origial $d418 4-bit samples have already been shifted
*/
static int genDigi(int in, unsigned char digi) { 
    // transform unsigned 8 bit range into signed 16 bit (–32,768 to 32,767) range	(
	// shift only 7 instead of 8 because digis are otherwise too loud)	
	signed long value = in + (((digi & 0xff) << 7) - 0x4000); 
	
	const int clipValue = 32767;
	if ( value < -clipValue ) {
		value = -clipValue;
	} else if ( value > clipValue ) {
		value = clipValue;
	}	
    return value;
}

static void mergeDigi(int hasDigi, dword *sound_buffer, unsigned char *digi_buffer, unsigned long len) {
	if (hasDigi) {
		int i;
		for (i= 0; i<len; i++) {
			sound_buffer[i]= genDigi(sound_buffer[i], digi_buffer[i]);
		}
	} 
}

void initClockSpeed() {
	sCyclesPerRaster= 63;													// PAL
	sLinesPerScreen = 312;
	
	// hack: correct cycle handling would consider badlines, usage of sprites, bus takeover cycles, etc
	// but we just use the following hack to fix obvious problems with the playback of THCM's stuff..  without 
	// which THCM's player makes too many NMI calls per screen..
	
	int badlineCycles= sCyclesPerRaster*3;	
	
	sTotalCyclesPerScreen= sCyclesPerRaster*sLinesPerScreen- badlineCycles;				// PAL: 19656	
	sTotalCyclesPerSec= sTotalCyclesPerScreen*50; //982800 (clock would be; 985248);				

	if(sNtscMode) {		
		sCyclesPerRaster= 65;												// NTSC
		sLinesPerScreen = 263;
		sTotalCyclesPerScreen= sCyclesPerRaster*sLinesPerScreen- badlineCycles;			// NTSC: 17095		
		sTotalCyclesPerSec= sTotalCyclesPerScreen*60; //1025700 (clock would be: 1022727);				
	}

	// some IRQ players don't finish within one screen, e.g. A-Maze-Ing.sid and Axel_F.sid 
	// (Sean Connolly) even seems to play digis during multi-screen IRQs (so give them more time)			
	sIrqTimeout= sTotalCyclesPerScreen*4;

}

static int sNumberOfSamplesRendered = 0;
static int sNumberOfSamplesToRender = 0;

static void initSampleBuffer() {
	sNumberOfSamplesRendered = 0;
	sNumberOfSamplesToRender = 0;
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
			hasDigi= processOneScreen(sTotalCyclesPerScreen, sNumberOfSamplesPerCall);
		}
		
		if (sNumberOfSamplesRendered + sNumberOfSamplesToRender > sChunkSize) {
			int availableSpace = sChunkSize-sNumberOfSamplesRendered;
			
			memcpy(&sSoundBuffer[sNumberOfSamplesRendered], &sSynthBuffer[sampleBufferIdx], sizeof(dword)*availableSpace);
	
			mergeDigi(hasDigi, &sSoundBuffer[sNumberOfSamplesRendered], &sDigiBuffer[sampleBufferIdx], availableSpace);			
			
			sampleBufferIdx += availableSpace;
			sNumberOfSamplesToRender -= availableSpace;
			sNumberOfSamplesRendered = sChunkSize;
		} else {
			memcpy(&sSoundBuffer[sNumberOfSamplesRendered], &sSynthBuffer[sampleBufferIdx], sizeof(dword)*sNumberOfSamplesToRender);

			mergeDigi(hasDigi, &sSoundBuffer[sNumberOfSamplesRendered], &sDigiBuffer[sampleBufferIdx], sNumberOfSamplesToRender);
		
			sNumberOfSamplesRendered += sNumberOfSamplesToRender;
			sNumberOfSamplesToRender = 0;
		} 
	}

	return (sNumberOfSamplesRendered);
}

void setIO(unsigned int addr, unsigned char value) {
	io_area[addr - 0xd000]= value;
}

static void setupInitMemoryBank() {
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

static void setupPsidPlayMemoryBank() {
	if (sIsPSID) {
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

static void playTune(int selectedTrack)  __attribute__((noinline));
static void EMSCRIPTEN_KEEPALIVE playTune(int selectedTrack) {
	sActualSubsong= selectedTrack;

	reInitEngine();
	
    synth_init(sSampleRate);	
	initClockSpeed();
	initSampleBuffer();
	initC64Rom();
		
	// reset & init
	memcpy(memory, sMemorySnapshot, MEMORY_SIZE);		// restore original mem content.. previous "sInitAddr" run may have corrupted the state

	// CIA 1 defaults	(by default C64 is configured with CIA1 timer / not raster irq)
	setIO(0xdc0d, 0x81);	// interrupt control	(interrupt through timer A)
	setIO(0xdc0e, 0x01); 	// control timer A: start - must already be started (e.g. Phobia, GianaSisters, etc expect it)
	setIO(0xdc0f, 0x08); 	// control timer B (start/stop) means auto-restart
	setIO(0xdc04, sTotalCyclesPerScreen&0xff); 	// timer A (1x pro screen refresh)
	setIO(0xdc05, sTotalCyclesPerScreen>>8);
	
	if (isRsid()) {	
		// by default C64 is configured with CIA1 timer / not raster irq
		setIO(0xd01a, 0x00); 	// raster irq not active
		setIO(0xd011, 0x1B);
		setIO(0xd012, 0x00); 	// raster at line x

		// CIA 2 defaults
		setIO(0xdd0e, 0x08); 	// control timer 2A (start/stop)
		setIO(0xdd0f, 0x08); 	// control timer 2B (start/stop)		
	}
	
	setupInitMemoryBank();
		
	initCia();
	initVic();
	
	sOverflowDigiCount=0;	
	sDigiCount=0;
	
	hackIfNeeded();
	
	sMainProgStatus= callMain(sInitAddr, sActualSubsong, 0, CYCLELIMIT);		// if it does not complete then it is likely in an endless loop / maybe digi player
		
	initBuffers();
	setupPsidPlayMemoryBank();
	
}

	// 0: loadAddr;
	// 1: playSpeed;
	// 2: maxSubSong;
	// 3: actualSubSong;
	// 4: songName;
	// 5: songAuthor;
	// 6: songCopyright;
void* loadResult [7];

static 	char song_name[32], song_author[32], song_copyright[32];

static void * loadSidFile(void * inBuffer, int inBufSize)  __attribute__((noinline));
static void * EMSCRIPTEN_KEEPALIVE loadSidFile(void * inBuffer, int inBufSize) {
    sSampleRate= 44100;
	sInitAddr= 0;
	sPlayAddr= 0;
	sLoadEndAddr= 0;
	sActualSubsong= 0;
	sMaxSubsong= 0;	
	sPlaySpeed= 0;		
	sNtscMode= 0;

	unsigned char sidmem[65536];
    unsigned char *buffer = sidmem;	

    memcpy(buffer, inBuffer, inBufSize);	
	
	sIsPSID= (buffer[0x00]==0x50?1:0);
	sSidVersion= buffer[0x05];
	sIsC64compatible= (sSidVersion & 0x2) &&  ((buffer[0x77] & 0x2) == 0);	
	sNtscMode= (sSidVersion == 2) && !isRsid() && (buffer[0x77] & 0x8); // NTSC bit
	
	int i;
    for (i=0;i<32;i++) song_name[i] = buffer[0x16+i];
    for (i=0;i<32;i++) song_author[i] = buffer[0x36+i]; 
    for (i=0;i<32;i++) song_copyright[i] = buffer[0x56+i];
	
	unsigned short load_addr;
    LoadSIDFromMemory(sidmem, &load_addr, &sLoadEndAddr, &sInitAddr, &sPlayAddr, &sMaxSubsong, 
		&sActualSubsong, &sPlaySpeed, inBufSize);

	memcpy(sMemorySnapshot, memory, MEMORY_SIZE);
	
	loadResult[0]= &load_addr;
	loadResult[1]= &sPlaySpeed;
	loadResult[2]= &sMaxSubsong;
	loadResult[3]= &sActualSubsong;
	loadResult[4]= song_name;
	loadResult[5]= song_author;
	loadResult[6]= song_copyright;

	return loadResult;
}

static dword* getSoundBuffer() __attribute__((noinline));
static dword* EMSCRIPTEN_KEEPALIVE getSoundBuffer() {
	return sSoundBuffer;
}

