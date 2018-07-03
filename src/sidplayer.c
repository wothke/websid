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
#define MUS_BASE_ADDR 0x1800
#define MUS_PLAYER_START MUS_BASE_ADDR
	// where the .mus music file is loaded
#define MUS_DATA_START 0x281e
	// where the player looks for the pointers into the 3 voice command streams
#define MUS_VOICE_PTRS 0x2743

const static uint16_t MUS_REL_DATA_START= MUS_DATA_START - MUS_BASE_ADDR;
const static uint16_t MUS_REL_VOICE_PTRS= MUS_VOICE_PTRS - MUS_BASE_ADDR;	
const static uint16_t MUS_MAX_SIZE= MUS_REL_DATA_START;
const static uint16_t MUS_MAX_SONG_SIZE= 0xA000 - MUS_DATA_START;		// stop at BASIC ROM.. or how big are these songs?

	// buffer used to combine .mus and player
static uint8_t *_musMemBuffer= 0;										// represents memory at MUS_BASE_ADDR
static uint32_t _musMemBufferSize= 0xA000 - MUS_BASE_ADDR;
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

static uint32_t _sampleRate;

static uint16_t _loadAddr, _initAddr, _playAddr, _loadEndAddr;
static uint8_t _actualSubsong, _maxSubsong;
static uint32_t _playSpeed;

static uint8_t _digiEnabled = 1;

static uint32_t _traceSID= 0;

// make it large enough for data of 8 PAL screens.. (allocate 
// statically to ease passing to ActionScript)
#define BUFLEN 8*882
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


uint16_t* envSIDAddresses() {
	return _sidAddr;
}
uint8_t*  envSID6581s() {
	return _sidIs6581;
}

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
int8_t envIsTimerDrivenPSID() {
	return ((envIsPSID() == 1) && (envCurrentSongSpeed() == 1));
}


int8_t envIsRasterDrivenPSID() {
	return ((envIsPSID() == 1) && (envCurrentSongSpeed() == 0));
}

static void resetAudioBuffers() {
	// song with original 60 hz playback.. adjust number of samples to play it faster.	

	if(_ntscMode && envIsRasterDrivenPSID()) { 
		_numberOfSamplesPerCall= 735; 		// NTSC: 735*60=44100
	} else {
		_numberOfSamplesPerCall= 882; 		// PAL: 882*50=44100
	}
	_chunkSize= _numberOfSamplesPerCall*8; 	// e.g. data for 8 50hz screens (0.16 secs)

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

static void resetTimings() {	
	uint8_t fps;
	if(_ntscMode) {		
		fps= 60;
		_cyclesPerRaster= 65;			// NTSC
		_linesPerScreen = 263;
		_clockRate= 1022727;
	} else {
		fps= 50;
		_cyclesPerRaster= 63;			// PAL	
		_linesPerScreen = 312;
		_clockRate= 985248;
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
	_totalCyclesPerSec= _totalCyclesPerScreen*fps; 
}

static uint8_t musIsTrackEnd(uint8_t voice) {
	uint16_t addr= (memReadRAM(voice + MUS_VOICE_PTRS+3) << 8) + memReadRAM(voice + MUS_VOICE_PTRS) - 2;	// pointer stops past the 0x14f HALT command!

	return (memReadRAM(addr) == 0x1) && (memReadRAM(addr+1) == 0x4f); 	// HALT command 0x14f
}
static uint32_t computeAudioSamples()  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE computeAudioSamples() {
	if (_musMode) {	// check for end of .mus song 	
		if (musIsTrackEnd(0) && musIsTrackEnd(1) && musIsTrackEnd(2)) {
			return -1;
		}
	}
	
	_numberOfSamplesRendered = 0;
			
	uint32_t sampleBufferIdx=0;
	uint32_t hasDigi= 0;
	
	while (_numberOfSamplesRendered < _chunkSize)
	{
		if (_numberOfSamplesToRender == 0) {
			_numberOfSamplesToRender = _numberOfSamplesPerCall;
			sampleBufferIdx=0;
			hasDigi= rsidProcessOneScreen(_synthBuffer, _digiBuffer, 
						_totalCyclesPerScreen, _numberOfSamplesPerCall, _synthTraceBuffers);
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
	for(uint8_t i= 0; i<3; i++) {
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
	
	if (1 || (_sidVersion <= 2)) {
		_actualSubsong= selectedTrack & 0xff;
		_digiEnabled = 1;

		rsidPlayTrack(_sampleRate, _compatibility, &_initAddr, _loadEndAddr, _playAddr, _actualSubsong);

		resetAudioBuffers();
	} else {
		// there seems to be new version 3 for dual-SID stuff.. and actually there is not
		// really much use in playing only half the voices - eventhough a standard C64
		// would bahave in the very same way..
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


	// 0: _loadAddr;
	// 1: _playSpeed;
	// 2: maxSubSong;
	// 3: actualSubSong;
	// 4: songName;
	// 5: songAuthor;
	// 6: songCopyright;
static void* loadResult [7];



#define MAX_INFO_LEN 32
#define MAX_INFO_LINES 5

static 	char 	song_name[MAX_INFO_LEN+1], 
				song_author[MAX_INFO_LEN+1], 
				song_copyright[MAX_INFO_LEN+1],
				song_info_trash[MAX_INFO_LEN+1];

static char* info_texts[MAX_INFO_LINES];


static uint16_t musGetOffset(uint8_t* buf) {
	return buf[0] + (((uint16_t)buf[1]) << 8);
}

static void resetInfoText() {
	info_texts[0]= song_name;
	info_texts[1]= song_author;
	info_texts[2]= song_copyright;
	
	// .mus files may have more lines with unstructured text... ignore 	
	info_texts[3]= song_info_trash;
	info_texts[4]= song_info_trash;

	memset(song_name, 0, MAX_INFO_LEN);
	memset(song_author, 0, MAX_INFO_LEN);
	memset(song_copyright, 0, MAX_INFO_LEN);
}


static void musMapInfoTexts(uint8_t *musSongData, uint32_t musSongDataLen, uint16_t trackLen) {		
	uint8_t *infoBuff= musSongData + trackLen;
	uint16_t maxInfo= musSongDataLen - trackLen;

	resetInfoText();
		
	uint8_t k = 0;
	uint8_t currentLen = 0;
	for (uint8_t j= 0; j<maxInfo; j++) {
		uint8_t ch= infoBuff[j];	
		ch= !(ch == 0xd) && ((ch < 0x20) || (ch > 0x60)) ? 0x20 : ch; // remove C64 special chars.. don't have that font anyway
		
		if (MAX_INFO_LEN > currentLen) {
			char* dest = info_texts[k];
			dest[currentLen++]= (ch == 0xd)? 0 : ch;
		} else {
			// ignore
		}
		if (ch == 0xd) {
			currentLen= 0;
			k++;
			if (k > MAX_INFO_LINES) break;
		}
	}
}

void musGetSizes(uint8_t *musSongData, uint16_t *v1len, uint16_t *v2len, uint16_t *v3len, uint16_t *trackLen) {
	(*v1len)= musGetOffset(musSongData+2);
	(*v2len)= musGetOffset(musSongData+4);
	(*v3len)= musGetOffset(musSongData+6);

	(*trackLen)= MUS_HEAD+ (*v1len)+ (*v2len)+ (*v3len);
}	

// Compute!'s .mus files require an addtional player that must installed with the song file.
static uint16_t loadComputeSidplayerData(uint8_t *musSongData, uint32_t musSongDataLen) {
	_sidVersion= 2;
	_basicProg= 0;
	_compatibility= 1;
	_sidAddr[0]= 0xd400; _sidAddr[1]= _sidAddr[2]= 0;
	_sidIs6581[0]= _sidIs6581[1]= _sidIs6581[2]= 1;

	_ntscMode= 1;			// the .mus stuff is mostely from the US..
	_loadAddr= MUS_BASE_ADDR;
	_loadEndAddr= 0x9fff;

	_initAddr= MUS_BASE_ADDR;
	_playAddr= 0x1bf2;
	
	uint16_t pSize= COMPUTESIDPLAYER_LENGTH;
	if((pSize > MUS_MAX_SIZE) || (musSongDataLen > MUS_MAX_SONG_SIZE)) return 0; // ERROR
	
	// prepare temp input buffer
	if (_musMemBuffer == 0) {
		_musMemBuffer= (uint8_t*)malloc(_musMemBufferSize);	// represents mem from $1800-$9fff
	}
	memcpy(_musMemBuffer, computeSidplayer, pSize);
	memcpy(_musMemBuffer+MUS_REL_DATA_START, musSongData, musSongDataLen);
	
	uint16_t v1len, v2len, v3len, trackLen;
	musGetSizes(musSongData, &v1len, &v2len, &v3len, &trackLen);
	if (trackLen >= musSongDataLen) {
//		fprintf(stderr, "info cannot be retrieved  from corrupt .mus file\n");
		return 0;
	}		
		
	musMapInfoTexts(musSongData, musSongDataLen, trackLen);

	uint16_t v1start= MUS_HEAD + MUS_DATA_START;
	uint16_t v2start= v1start+ v1len;
	uint16_t v3start= v2start+ v2len;
	
	// setup player
	_musMemBuffer[MUS_REL_VOICE_PTRS+0]= v1start & 0xff;
	_musMemBuffer[MUS_REL_VOICE_PTRS+1]= v2start & 0xff;
	_musMemBuffer[MUS_REL_VOICE_PTRS+2]= v3start & 0xff;
	_musMemBuffer[MUS_REL_VOICE_PTRS+3]= v1start >> 8;
	_musMemBuffer[MUS_REL_VOICE_PTRS+4]= v2start >> 8;
	_musMemBuffer[MUS_REL_VOICE_PTRS+5]= v3start >> 8;

	return 1;
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

static uint32_t loadSidFile(uint32_t isMus, void * inBuffer, uint32_t inBufSize)  __attribute__((noinline));
static uint32_t EMSCRIPTEN_KEEPALIVE loadSidFile(uint32_t isMus, void * inBuffer, uint32_t inBufSize) {
	uint8_t *inputFileBuffer= (uint8_t *)inBuffer;	

	if (!isMus && (inBufSize < 0x7c)) return 1;	// we need at least a header..

	memResetKernelROM();		// read only (only need to do this once)

    _sampleRate= 44100;		// TODO: extend API & use actual target sample rate
	_initAddr= 0;
	_playAddr= 0;
	_loadEndAddr= 0;
	_actualSubsong= 0;
	_maxSubsong= 0;	
	_playSpeed= 0;		
	_ntscMode= 0;
	
	_isFilePSID= (inputFileBuffer[0x00] == 0x50) || isMus ? 1 : 0;	
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
		for (i=0;i<32;i++) song_name[i] = inputFileBuffer[0x16+i];
		for (i=0;i<32;i++) song_author[i] = inputFileBuffer[0x36+i]; 
		for (i=0;i<32;i++) song_copyright[i] = inputFileBuffer[0x56+i];

		if (!loadSIDFromMemory(inputFileBuffer, &_loadAddr, &_loadEndAddr, &_initAddr, 
				&_playAddr, &_maxSubsong, &_actualSubsong, &_playSpeed, inBufSize)) {
			
			return 1;	// could not load file
		}
	}
		

	if (_basicProg) rsidStartFromBasic(&_initAddr);
	
	// global settings that depend on the loaded music file
	resetTimings();
	
	loadResult[0]= &_loadAddr;
	loadResult[1]= &_playSpeed;
	loadResult[2]= &_maxSubsong;
	loadResult[3]= &_actualSubsong;
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
	_sidIs6581[0]= _sidIs6581[1]= _sidIs6581[2]= is6581;
	
	sidReset(_sampleRate, envSIDAddresses(), envSID6581s(), _compatibility);

	return 0;
}

static uint8_t envIsNTSC()  __attribute__((noinline));
static uint8_t EMSCRIPTEN_KEEPALIVE envIsNTSC() {
	return _ntscMode;
}

static uint8_t envSetNTSC(uint8_t isNTSC)  __attribute__((noinline));
static uint8_t EMSCRIPTEN_KEEPALIVE envSetNTSC(uint8_t isNTSC) {
	_ntscMode= isNTSC;
	
	resetAudioBuffers();
	resetTimings();

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


