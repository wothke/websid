/*
 * This file provides the interface to the JavaScript world. 
 *  
 * <p>It also handles the *.sid input data and provides the respective 
 * environment exposed in env.h
 *
 * Coding conventions:
 *  - names use camelStyle
 *  - type names start uppercase, vars and functions start lowercase
 *  - there are NO globally visible (project level) vars
 *  - non-local variables start with _ (i.e. "globally" visible within file but NOT visible outside of the file)
 *  - global constants and defines are all uppercase 
 *  - cross C-file APIs start with a prefix that reflects the file that provides them, e.g. 
 *    "sid...()" is provided by "sid.c". API is advertised in respective *.h files.
 *  - everything that is not considered to be part of the API is hidden (e.g. by making it static)
 *
 * <p>Copyright (C) 2011-2018 Juergen Wothke
 * <p>version 0.9
 *
 * <p>Emulation approach: Each call to computeAudioSamples() delivers some fixed number of audio samples.
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

#include <stdlib.h> 
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "cia.h"	
#include "vic.h"	
#include "sid.h"	
#include "rsidengine.h"
#include "digi.h"	
#include "hacks.h"	
#include "memory.h"

#include "compute!.h"

#ifdef EMSCRIPTEN
#define EMSCRIPTEN_KEEPALIVE __attribute__((used))
#else
#define EMSCRIPTEN_KEEPALIVE
#endif


// handling of Compute!'s SidPlayer (.mus files)
#define MUS_HEAD 0x8
#define MUS_BASE_ADDR 0x17b0
#define MUS_PLAYER_START MUS_BASE_ADDR
	// where the .mus music file is loaded
#define MUS_DATA_START 0x281e
	// where the player looks for the pointers into the 3 voice command streams (the same is repeated at pos +9)
//#define MUS_VOICE_PTRS 0x2743
#define MUS_PLAY_INDICATOR 0x269B

const static uint16_t MUS_REL_DATA_START= MUS_DATA_START - MUS_BASE_ADDR;
const static uint16_t MUS_MAX_SIZE= MUS_REL_DATA_START;
const static uint16_t MUS_MAX_SONG_SIZE= 0xA000 - MUS_DATA_START;		// stop at BASIC ROM.. or how big are these songs?

	// buffer used to combine .mus and player
static uint8_t *_musMemBuffer= 0;										// represents memory at MUS_BASE_ADDR
const static uint32_t _musMemBufferSize= 0xA000 - MUS_BASE_ADDR;
static uint8_t _musMode= 0;

static uint32_t _totalCyclesPerSec;
static uint32_t _totalCyclesPerScreen;
static uint8_t _cyclesPerRaster;
static uint16_t _linesPerScreen;

static uint8_t _sidVersion;

static uint16_t _sidAddr[3];	// start addr of installed SID chips (0 means not available)
static uint8_t _sidIs6581[3];		// models of installed SID chips 

static uint8_t _ntscMode= 0;
static uint8_t _compatibility;
static uint8_t _basicProg;
static uint32_t _clockRate;
static double _fps;

static uint32_t _sampleRate;

static uint16_t _loadAddr, _initAddr, _playAddr, _loadEndAddr;
static uint8_t _actualSubsong, _maxSubsong;
static uint32_t _playSpeed;

static uint8_t _digiEnabled = 1;

static uint32_t _traceSID= 0;

//#define BUFLEN 8*882	// make it large enough for data of 8 screens (for NTSC 8*735 would be sufficient)

#define BUFLEN 96000/50	// keep it down to one screen to allow for more direct feedback to WebAudio side..

static uint32_t _soundBufferLen= BUFLEN;
static int16_t _soundBuffer[BUFLEN];
	// SID debug buffers corresponding to _soundBuffer
static int16_t _voice1Buffer[BUFLEN];
static int16_t _voice2Buffer[BUFLEN];
static int16_t _voice3Buffer[BUFLEN];
static int16_t _voice4Buffer[BUFLEN];


// these buffers are "per frame" i.e. 1 screen refresh, e.g. 822 samples
static int16_t * _synthBuffer= 0;

static int16_t * _synthBufferVoice1= 0;
static int16_t * _synthBufferVoice2= 0;
static int16_t * _synthBufferVoice3= 0;
static int16_t ** _synthTraceBuffers= 0;

// only contains the digi data for 1 screen .. 
// and is directly merged into the _soundBuffer
static uint8_t * _digiBuffer= 0;	

static uint32_t _chunkSize; 
static uint16_t _numberOfSamplesPerCall;  

static uint8_t _isPSID;
static uint8_t _isFilePSID;

static uint32_t _numberOfSamplesRendered = 0;
static uint32_t _numberOfSamplesToRender = 0;

static uint8_t _soundStarted, _skipSilenceLoop;

static int8_t _digiDiagnostic;
static int16_t _digiPreviousRate;
static int16_t _digiAverageRate;

// ------ song specific infos
	// 0: _loadAddr;
	// 1: _playSpeed;
	// 2: maxSubSong;
	// 3: actualSubSong;
	// 4: songName;
	// 5: songAuthor;
	// 6: songCopyright;
static void* loadResult[7];

#define MAX_INFO_LEN 32
#define MAX_INFO_LINES 5

static 	char 	_songName[MAX_INFO_LEN+1], 
				_songAuthor[MAX_INFO_LEN+1], 
				_songCopyright[MAX_INFO_LEN+1],
				_songInfoTrash[MAX_INFO_LEN+1];

static char* _infoTexts[MAX_INFO_LINES];


uint16_t* envSIDAddresses() {
	return _sidAddr;
}
uint8_t*  envSID6581s() {
	return _sidIs6581;
}

// FIXME: envIsRSID is also used for certain PSID files that are deemed fit for use of 
// the RSID emulation .. this is rather confusing and error prone!

uint8_t envIsRSID() {
	return (_isPSID == 0);
}

uint8_t envIsFilePSID(){
	return (_isFilePSID == 1);
}

void envSetPsidMode(uint8_t m) {
	_isPSID= m;
}

uint8_t envIsPSID() {
	return !envIsRSID();
}

uint16_t envSidPlayAddr() {
	return _playAddr;
}

uint16_t envLinesPerScreen() {
	return _linesPerScreen;
}

uint8_t envCyclesPerRaster() {
	return _cyclesPerRaster;
}

uint16_t envNumberOfSamplesPerCall() {
	return _numberOfSamplesPerCall;
}

uint32_t envCyclesPerSec() {
	return _totalCyclesPerSec;
}

uint32_t envCyclesPerScreen() {
	return _totalCyclesPerScreen;
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
	return get_bit(_playSpeed, _actualSubsong > 31 ? 31 : _actualSubsong); 
}

uint32_t envClockRate() {
	return _clockRate;
}
double envFPS(){
	return _fps;
}

int8_t envIsTimerDrivenPSID() {
	return ((envIsPSID() == 1) && (envCurrentSongSpeed() == 1));
}


int8_t envIsRasterDrivenPSID() {
	return ((envIsPSID() == 1) && (envCurrentSongSpeed() == 0));
}

static void resetAudioBuffers() {
	
	// number of samples corresponding to one simulated frame/screen
	// (granularity of emulation is 1 screen)
	if(_ntscMode && envIsRasterDrivenPSID()) { 
		_numberOfSamplesPerCall= _sampleRate/60; 		// NTSC: 735*60=44100
	} else {
		_numberOfSamplesPerCall= _sampleRate/50; 		// PAL: 882*50=44100
	}
//	_chunkSize= _numberOfSamplesPerCall*8; 	// e.g. data for 8 50hz screens (0.16 secs)
	_chunkSize= _numberOfSamplesPerCall; 	// WebAudio side can still aggregate these..
	// just to make sure these is no garbage left
	memset(_voice1Buffer, 0, sizeof(int16_t)*BUFLEN);
	memset(_voice2Buffer, 0, sizeof(int16_t)*BUFLEN);
	memset(_voice3Buffer, 0, sizeof(int16_t)*BUFLEN);
	memset(_voice4Buffer, 0, sizeof(int16_t)*BUFLEN);
	
	if (_synthBuffer) free(_synthBuffer);
	if (_digiBuffer)	free(_digiBuffer);

	_synthBuffer= (int16_t*)malloc(sizeof(int16_t)*_numberOfSamplesPerCall + 1);
	_digiBuffer= (uint8_t*)malloc(sizeof(uint8_t)*_numberOfSamplesPerCall + 1);	
	
	// debug output (corresponding to _synthBuffer)
	if (_synthBufferVoice1) free(_synthBufferVoice1);
	if (_synthBufferVoice2) free(_synthBufferVoice2);
	if (_synthBufferVoice3) free(_synthBufferVoice3);
	if (_synthTraceBuffers) free(_synthTraceBuffers);

	if (_traceSID) {	// availability of _synthTraceBuffers controls if SID will generate the respective output
		_synthBufferVoice1= (int16_t*)calloc(sizeof(int16_t),_numberOfSamplesPerCall + 1);	
		_synthBufferVoice2= (int16_t*)calloc(sizeof(int16_t),_numberOfSamplesPerCall + 1);	
		_synthBufferVoice3= (int16_t*)calloc(sizeof(int16_t),_numberOfSamplesPerCall + 1);	
		
		_synthTraceBuffers= (int16_t**)malloc(sizeof(int16_t*)*3);
		
		_synthTraceBuffers[0]= _synthBufferVoice1;
		_synthTraceBuffers[1]= _synthBufferVoice2;
		_synthTraceBuffers[2]= _synthBufferVoice3;
	} else {
		_synthBufferVoice1= 0;	
		_synthBufferVoice2= 0;	
		_synthBufferVoice3= 0;	

		_synthTraceBuffers= 0;	// disables respective SID rendering
	}
	_numberOfSamplesRendered = 0;
	_numberOfSamplesToRender = 0;	
}

static void resetTimings(uint8_t isNTSC) {
	// note: clocking is derived from the targetted video standard.
	// 14.31818MHz (NTSC) respectively of 17.734475MHz (PAL) - divide by 4 for respective 
	// color subcarrier: 3.58Mhz NTSC,  4.43MHz PAL.
	// the 8.18MHz (NTSC) respectively 7.88MHz (PAL) "dot clock" is derived from the above
	// system clock is finally "dot clock" divided by 8
	if(isNTSC) {
		// NTSC
		_fps= 59.826;
		_cyclesPerRaster= 65;
		_linesPerScreen = 263;	// with  520 pixels
		_clockRate= 1022727;	// system clock (14.31818MHz/14)
	} else {
		// PAL
		_fps= 50.125;
		_cyclesPerRaster= 63;	
		_linesPerScreen = 312;	// with 504 pixels
		_clockRate= 985249;		// system clock (17.734475MHz/18)
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
	uint32_t badlineCycles= _cyclesPerRaster*4;	
	
	// NTSC: 17095	/ PAL: 19656		
	_totalCyclesPerScreen= _cyclesPerRaster*_linesPerScreen- badlineCycles;			
	// NTSC: 1025700 (clock would be: 1022727);		/ PAL: 982800 (clock would be: 985248);			
	_totalCyclesPerSec= round(((double)_totalCyclesPerScreen)*_fps);	
}

static uint8_t musIsTrackEnd(uint8_t voice) {
	return memReadRAM(MUS_PLAY_INDICATOR) == 0x0;
	/*
	// check for end of .mus voice 0 (other voices might be shorter & loop)
	uint16_t addr= (memReadRAM(voice + MUS_VOICE_PTRS+3) << 8) + memReadRAM(voice + MUS_VOICE_PTRS) - 2;	// pointer stops past the 0x14f HALT command!
	return (memReadRAM(addr) == 0x1) && (memReadRAM(addr+1) == 0x4f); 	// HALT command 0x14f
	*/
}

static uint32_t computeAudioSamples()  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE computeAudioSamples() {
	if (_musMode && musIsTrackEnd(0)) {
		return -1;
	}
	_numberOfSamplesRendered = 0;
			
	uint32_t sampleBufferIdx=0;
	uint32_t hasDigi= 0;
	
	while (_numberOfSamplesRendered < _chunkSize)
	{
		if (_numberOfSamplesToRender == 0) {
			_numberOfSamplesToRender = _numberOfSamplesPerCall;
			sampleBufferIdx=0;
						
			for (uint8_t i= 0; i<_skipSilenceLoop; i++) {	// too much seeking might block too long?
				hasDigi= rsidProcessOneScreen(_synthBuffer, _digiBuffer, 
						_totalCyclesPerScreen, _numberOfSamplesPerCall, _synthTraceBuffers);
				
				if (hasDigi) {
					DigiType t= digiGetType();
					uint16_t rate= digiGetRate();
					_digiDiagnostic= t;
					_digiAverageRate = (_digiPreviousRate + rate) >> 1;	// average out frame overflow issues
					_digiPreviousRate= rate;
				}
				
				_soundStarted|= hasDigi;
				
				if (!_soundStarted) {
					// check if there is some output this time..
					for (uint16_t j= 0; j<_numberOfSamplesPerCall; j++) {
						if (_synthBuffer[j] != 0) {
							_soundStarted= 1;
							break;
						}
					}
				} else {
					break;
				}			
			}
			if (!_soundStarted) {
				// broken sounds might become irresponsive without this brake
				_skipSilenceLoop = (_skipSilenceLoop>10) ? (_skipSilenceLoop - 10) : 1; 
			}
			
			if (!_digiEnabled) hasDigi= 0;
		}
		
		if (_numberOfSamplesRendered + _numberOfSamplesToRender > _chunkSize) {
			uint32_t availableSpace = _chunkSize-_numberOfSamplesRendered;
			
			memcpy(&_soundBuffer[_numberOfSamplesRendered], &_synthBuffer[sampleBufferIdx], sizeof(int16_t)*availableSpace);
			digiMergeSampleData(hasDigi, &_soundBuffer[_numberOfSamplesRendered], &_digiBuffer[sampleBufferIdx], availableSpace);			
	
			/*
			* In addition to the actual sample data played by WebAudio, buffers containing raw voice data are also
			* created here. These are 1:1 in sync with the sample buffer, i.e. for each sample entry in the sample buffer
			* there are respective there is a corresponding entry in the additional buffers - which are all exactly the
			* same size as the sample buffer. Note: things start to become messy when trying to use (e.g. visualize) 
			* respective add-on data *in-sync* with the actual WebAudio playback (see AbstractTicker).
			*/
	
			if (_traceSID) {
				// do the same for the respecive voice traces
				memcpy(&_voice1Buffer[_numberOfSamplesRendered], &_synthBufferVoice1[sampleBufferIdx], sizeof(int16_t)*availableSpace);
				memcpy(&_voice2Buffer[_numberOfSamplesRendered], &_synthBufferVoice2[sampleBufferIdx], sizeof(int16_t)*availableSpace);
				memcpy(&_voice3Buffer[_numberOfSamplesRendered], &_synthBufferVoice3[sampleBufferIdx], sizeof(int16_t)*availableSpace);
					
				memset(&_voice4Buffer[_numberOfSamplesRendered], 0, availableSpace*sizeof(int16_t)); // clear buffer so that existing "merge" can be reused
				digiMergeSampleData(hasDigi, &_voice4Buffer[_numberOfSamplesRendered], &_digiBuffer[sampleBufferIdx], availableSpace);			
			}
			sampleBufferIdx += availableSpace;
			_numberOfSamplesToRender -= availableSpace;
			_numberOfSamplesRendered = _chunkSize;
		} else {
			memcpy(&_soundBuffer[_numberOfSamplesRendered], &_synthBuffer[sampleBufferIdx], sizeof(int16_t)*_numberOfSamplesToRender);
			digiMergeSampleData(hasDigi, &_soundBuffer[_numberOfSamplesRendered], &_digiBuffer[sampleBufferIdx], _numberOfSamplesToRender);

			if (_traceSID) {
				// do the same for the respecive voice traces
				memcpy(&_voice1Buffer[_numberOfSamplesRendered], &_synthBufferVoice1[sampleBufferIdx], sizeof(int16_t)*_numberOfSamplesToRender);
				memcpy(&_voice2Buffer[_numberOfSamplesRendered], &_synthBufferVoice2[sampleBufferIdx], sizeof(int16_t)*_numberOfSamplesToRender);
				memcpy(&_voice3Buffer[_numberOfSamplesRendered], &_synthBufferVoice3[sampleBufferIdx], sizeof(int16_t)*_numberOfSamplesToRender);
					
				memset(&_voice4Buffer[_numberOfSamplesRendered], 0, _numberOfSamplesToRender*sizeof(int16_t)); // clear buffer so that existing "merge" can be reused
				digiMergeSampleData(hasDigi, &_voice4Buffer[_numberOfSamplesRendered], &_digiBuffer[sampleBufferIdx], _numberOfSamplesToRender);			
			}
			_numberOfSamplesRendered += _numberOfSamplesToRender;
			_numberOfSamplesToRender = 0;
		} 
	}
	return (_numberOfSamplesRendered);
}

// bit0=voice0, bit1=voice1,..
static uint32_t enableVoices(uint32_t mask)  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE enableVoices(uint32_t mask) {
	for(uint8_t i= 0; i<3; i++) {			// FIXME: properly support this for multi SID
		sidSetMute(i, !(mask&0x1));
		mask = mask >> 1;
	}
	// "voice 4" is digi-output..
	_digiEnabled= mask&0x1;
	
	return 0;
}

static uint32_t playTune(uint32_t selectedTrack, uint32_t traceSID)  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE playTune(uint32_t selectedTrack, uint32_t traceSID) {
	_traceSID= traceSID; 
	
	_actualSubsong= (selectedTrack >= _maxSubsong) ? _actualSubsong : selectedTrack;
	_digiEnabled = 1;
	_digiDiagnostic= _digiAverageRate= _digiPreviousRate= 0;
	
	_soundStarted= 0;
	_skipSilenceLoop= 100;

	rsidPlayTrack(_sampleRate, _compatibility, &_initAddr, _loadEndAddr, _playAddr, _actualSubsong);

	resetAudioBuffers();

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

	uint16_t tracks= pData[0xf] | (((uint16_t)pData[0xe])<<8); // pointless using 2 bytes for a max of 256.. when the max of speed flags is 32..
	if (tracks == 0) tracks= 1;
	if (tracks > 0xff) tracks= 0xff;
    *subsongs = tracks & 0xff;

	uint16_t start= pData[0x11] | (((uint16_t)pData[0x10])<<8);
	if (!start) start= 1;
	if (start > tracks) start= tracks;
    *startsong = (start & 0xff) - 1;	// start at index 0

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

static uint16_t musGetOffset(uint8_t* buf) {
	return (((uint16_t)buf[1]) << 8) + buf[0];
}

static void resetInfoText() {
	_infoTexts[0]= _songName;
	_infoTexts[1]= _songAuthor;
	_infoTexts[2]= _songCopyright;
	
	// .mus files may have more lines with unstructured text... ignore 	
	_infoTexts[3]= _songInfoTrash;
	_infoTexts[4]= _songInfoTrash;

	memset(_songName, 0, MAX_INFO_LEN);
	memset(_songAuthor, 0, MAX_INFO_LEN);
	memset(_songCopyright, 0, MAX_INFO_LEN);
}

static void musMapInfoTexts(uint8_t *musSongFile, uint32_t musSongFileLen, uint16_t trackDataLen) {
	if (musSongFileLen <= trackDataLen) return;
	
	uint8_t *buffer= musSongFile + trackDataLen;
	uint16_t maxInfoLen= musSongFileLen - trackDataLen;

	resetInfoText();
		
	uint8_t line = 0;
	uint8_t currentLen = 0;
	for (uint8_t j= 0; j<maxInfoLen; j++) {	// iterate over all the remaining chars in the file 
		uint8_t ch= buffer[j];	
		
		if (!(ch == 0xd) && ((ch < 0x20) || (ch > 0x60))) continue; // remove C64 special chars.. don't have that font anyway 
		
		if (currentLen < MAX_INFO_LEN) {
			char* dest = _infoTexts[line];
			dest[currentLen++]= (ch == 0xd)? 0 : ch;
			
			if (MAX_INFO_LEN == currentLen) currentLen--; // last one wins.. hopefully the 0 terminator..
		} else {
			// ignore: should not be possible..
		}
		if ((ch == 0xd) && (currentLen > 0)) {	// remove empty lines
			// start new line..
			currentLen= 0;
			line++;
			if (line >= MAX_INFO_LINES) break;
		}
	}
}

void musGetSizes(uint8_t *musSongFile, uint16_t *v1len, uint16_t *v2len, uint16_t *v3len, uint16_t *trackDataLen) {
	(*v1len)= musGetOffset(musSongFile+2);
	(*v2len)= musGetOffset(musSongFile+4);
	(*v3len)= musGetOffset(musSongFile+6);

	(*trackDataLen)= MUS_HEAD+ (*v1len)+ (*v2len)+ (*v3len);
}	

uint16_t getSidAddr(uint8_t centerByte) {
	if (((centerByte >= 0x42) && (centerByte <= 0xFE)) && 
		!((centerByte >= 0x80) && (centerByte <= 0xDF)) && 
		!(centerByte & 0x1)) {

		return ((uint16_t)0xD000) | (((uint16_t)centerByte)<<4);
	}
	return 0;
}

static void configureSids(uint16_t flags, uint8_t addr2, uint8_t addr3) {
	_sidAddr[0]= 0xd400;
	_sidIs6581[0]= (flags>>4) & 0x3;	
	_sidIs6581[0]= !((_sidIs6581[0]>>1) & 0x1); 	// only use 8580 when bit is explicitly set
	
	_sidAddr[1]= getSidAddr(addr2);
	_sidIs6581[1]= (flags>>6) & 0x3;
	_sidIs6581[1]= !_sidIs6581[1] ? _sidIs6581[0] : !((_sidIs6581[1]>>1) & 0x1); 
		
	_sidAddr[2]= getSidAddr(addr3);
	_sidIs6581[2]= (flags>>8) & 0x3;
	_sidIs6581[2]= !_sidIs6581[2] ? _sidIs6581[0] : !((_sidIs6581[2]>>1) & 0x1); 
}

// Compute!'s .mus files require an addtional player that must installed with the song file.
static uint16_t loadComputeSidplayerData(uint8_t *musSongFile, uint32_t musSongFileLen) {
	// note: the player can also be used in RSID mode (but for some reason the timing is then much slower..)
	_isPSID= 1;
	_sidVersion= 2;
	_basicProg= 0;
	_compatibility= 1;
	_ntscMode= 1;			// .mus stuff is mostly from the US..
	
	configureSids(0, 0, 0); 	// just use one *old* SID at d400 
	
	_loadAddr= MUS_BASE_ADDR;
	_loadEndAddr= 0x9fff;

	_initAddr= MUS_BASE_ADDR;
	_playAddr=  0x1a07;
	
	uint16_t pSize= COMPUTESIDPLAYER_LENGTH;
	if((pSize > MUS_MAX_SIZE) || (musSongFileLen > MUS_MAX_SONG_SIZE)) return 0; // ERROR
	
	// prepare temp input buffer
	if (_musMemBuffer == 0) {
		_musMemBuffer= (uint8_t*)malloc(_musMemBufferSize);	// represents mem from MUS_BASE_ADDR to $9fff
	}
	memcpy(_musMemBuffer, computeSidplayer, pSize);

	// patch/configure the MUS player
	_musMemBuffer[0x17ca -MUS_BASE_ADDR]= (!_ntscMode) & 0x1;	// NTSC by default.. so this is not really needed
		
		
	// patch "INIT" routine to load the MUS file from other address
//	uint16_t addr= 	MUS_DATA_START+2;					// skip 2-bytes header, e.g. start at $2820 
//	_musMemBuffer[0x17ef -MUS_BASE_ADDR]= addr & 0xff;
//	_musMemBuffer[0x17f1 -MUS_BASE_ADDR]= addr >>8;
	
	memcpy(_musMemBuffer+MUS_REL_DATA_START, musSongFile, musSongFileLen);


	uint16_t v1len, v2len, v3len, trackDataLen;
	musGetSizes(musSongFile, &v1len, &v2len, &v3len, &trackDataLen);
	if (trackDataLen >= musSongFileLen) {
//		fprintf(stderr, "info cannot be retrieved  from corrupt .mus file\n");
		return 0;
	}		
	
 	musMapInfoTexts(musSongFile, musSongFileLen, trackDataLen);

	return 1;
}

static uint32_t loadSidFile(uint32_t isMus, void * inBuffer, uint32_t inBufSize, uint32_t sampleRate)  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE loadSidFile(uint32_t isMus, void * inBuffer, uint32_t inBufSize, uint32_t sampleRate) {
	uint8_t *inputFileBuffer= (uint8_t *)inBuffer;	
	
	if (!isMus && (inBufSize < 0x7c)) return 1;	// we need at least a header..

	memResetKernelROM();		// read only (only need to do this once)

    _sampleRate= sampleRate > 96000 ? 48000 : sampleRate; // see hardcoded BUFLEN
	_initAddr= 0;
	_playAddr= 0;
	_loadEndAddr= 0;
	_actualSubsong= 0;
	_maxSubsong= 0;	
	_playSpeed= 0;		
	_ntscMode= 0;	// default is PAL
	
	_isFilePSID= isMus || (inputFileBuffer[0x00] == 0x50) ? 1 : 0;	
	envSetPsidMode(_isFilePSID);

	memResetRAM(envIsPSID());
	
	if ((_musMode= isMus)) {
		// todo: the same kind of impl could be used for .sid files that contain .mus data.. (see respectice flag)
		if (!loadComputeSidplayerData(inputFileBuffer, inBufSize)) {
			return 1;
		}
		rsidLoadSongBinary(_musMemBuffer, _loadAddr, _musMemBufferSize);		
	} else {
		_sidVersion= inputFileBuffer[0x05];
				
		uint16_t flags= (_sidVersion > 1) ? (((uint16_t)inputFileBuffer[0x77]) | (((uint16_t)inputFileBuffer[0x77])<<8)) : 0x0;
		
		_basicProg= (envIsRSID() && (flags & 0x2));	// C64 BASIC program need to be started..
		
		_compatibility= ( (_sidVersion & 0x2) &&  ((flags & 0x2) == 0));	
		_ntscMode= (_sidVersion == 2) && envIsPSID() && (flags & 0x8); // NTSC bit
		
		configureSids(flags, _sidVersion>2?inputFileBuffer[0x7a]:0, _sidVersion>3?inputFileBuffer[0x7b]:0);
			
		uint8_t i;
		for (i=0;i<32;i++) _songName[i] = inputFileBuffer[0x16+i];
		for (i=0;i<32;i++) _songAuthor[i] = inputFileBuffer[0x36+i]; 
		for (i=0;i<32;i++) _songCopyright[i] = inputFileBuffer[0x56+i];

		if (!loadSIDFromMemory(inputFileBuffer, &_loadAddr, &_loadEndAddr, &_initAddr, 
				&_playAddr, &_maxSubsong, &_actualSubsong, &_playSpeed, inBufSize)) {
			return 1;	// could not load file
		}
	}
		

	if (_basicProg) rsidStartFromBasic(&_initAddr);
	
	// global settings that depend on the loaded music file
	resetTimings(_ntscMode);
	
	loadResult[0]= &_loadAddr;
	loadResult[1]= &_playSpeed;
	loadResult[2]= &_maxSubsong;
	loadResult[3]= &_actualSubsong;
	loadResult[4]= _songName;
	loadResult[5]= _songAuthor;
	loadResult[6]= _songCopyright;

	return 0;
}
static char** getMusicInfo() __attribute__((noinline));
static char** EMSCRIPTEN_KEEPALIVE getMusicInfo() {
	return (char**)loadResult;
}

static uint32_t getSoundBufferLen() __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE getSoundBufferLen() {
	return _numberOfSamplesRendered;	// in samples
}

static char* getSoundBuffer() __attribute__((noinline));
static char* EMSCRIPTEN_KEEPALIVE getSoundBuffer() {
	return (char*) _soundBuffer;
}

static uint32_t getSampleRate() __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE getSampleRate() {
	return _sampleRate;
}

// additional accessors that might be useful for tweaking defaults from the GUI

uint8_t envIsSID6581()  __attribute__((noinline));
uint8_t EMSCRIPTEN_KEEPALIVE envIsSID6581() {
	return _sidIs6581[0];		// only for the 1st chip
}

static uint8_t envSetSID6581(uint8_t is6581)  __attribute__((noinline));
static uint8_t EMSCRIPTEN_KEEPALIVE envSetSID6581(uint8_t is6581) {
	// this minimal update should allow to toggle the used filter without disrupting playback in progress
	_sidIs6581[0]= _sidIs6581[1]= _sidIs6581[2]= is6581;
	sidResetModel(envSID6581s());
	return 0;
}

uint8_t envIsNTSC()  __attribute__((noinline));
uint8_t EMSCRIPTEN_KEEPALIVE envIsNTSC() {
	return _ntscMode;
}

static uint8_t envSetNTSC(uint8_t isNTSC)  __attribute__((noinline));
static uint8_t EMSCRIPTEN_KEEPALIVE envSetNTSC(uint8_t isNTSC) {
	_ntscMode= isNTSC;
	
	resetTimings(isNTSC);
	resetAudioBuffers();

	return 0;
}

static char* getBufferVoice1() __attribute__((noinline));
static char* EMSCRIPTEN_KEEPALIVE getBufferVoice1() {
	return (char*) _voice1Buffer;
}

static char* getBufferVoice2() __attribute__((noinline));
static char* EMSCRIPTEN_KEEPALIVE getBufferVoice2() {
	return (char*) _voice2Buffer;
}

static char* getBufferVoice3() __attribute__((noinline));
static char* EMSCRIPTEN_KEEPALIVE getBufferVoice3() {
	return (char*) _voice3Buffer;
}

static char* getBufferVoice4() __attribute__((noinline));
static char* EMSCRIPTEN_KEEPALIVE getBufferVoice4() {
	return (char*) _voice4Buffer;
}

static uint16_t getRegisterSID(uint16_t reg) __attribute__((noinline));
static uint16_t EMSCRIPTEN_KEEPALIVE getRegisterSID(uint16_t reg) {
	return  memReadIO(0xd400 + reg);
}

static uint16_t getRAM(uint16_t addr) __attribute__((noinline));
static uint16_t EMSCRIPTEN_KEEPALIVE getRAM(uint16_t addr) {
	return  memReadRAM(addr);
}

static uint16_t getDigiType() __attribute__((noinline));
static uint16_t EMSCRIPTEN_KEEPALIVE getDigiType() {
	return (_digiDiagnostic > 0) ? _digiDiagnostic : 0;
}

static const char * getDigiTypeDesc() __attribute__((noinline));
static const char * EMSCRIPTEN_KEEPALIVE getDigiTypeDesc() {
	return (_digiDiagnostic > 0) ? digiGetTypeDesc() : "";
}

uint16_t getDigiRate() __attribute__((noinline));
uint16_t EMSCRIPTEN_KEEPALIVE getDigiRate() {
	return (_digiDiagnostic > 0) ? _digiAverageRate : 0;
}

