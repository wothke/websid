/*
* This file provides the interface to the JavaScript world (i.e. it provides all the 
* APIs expected by the "backend adapter").
*
* It also deals with the specifics of the different supported song formats
* and sets up the emulator environment (see env.h) accordingly.
*
*
* The general coding conventions used in this project are:
*
*  - type names use camelcase & start uppercase
*  - method/function names use camelcase & start lowercase
*  - vars use lowercase and _ delimiters
*  - private vars (instance vars, etc) start with _
*  - global constants and defines are all uppercase 
*  - cross C-file APIs (for those older parts that still use C) start with a prefix 
*    that reflects the file that provides them, e.g. "vic...()" is provided by "vic.c". 
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Emulation approach: Each call to computeAudioSamples() delivers some fixed number of audio samples.
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#include <stdlib.h> 
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <emscripten.h>

extern "C" {
#include "cia.h"	
#include "vic.h"	
#include "core.h"
#include "hacks.h"	
#include "memory.h"
}
#include "sid.h"	

#include "compute!.h"

// ----------------- audio output buffer management ------------------------------------------

//#define BUFLEN 8*882	// make it large enough for data of 8 screens (for NTSC 8*735 would be sufficient)
#define BUFLEN 96000/50	// keep it down to one screen to allow for more direct feedback to WebAudio side..

static uint32_t 		_soundBufferLen= BUFLEN;
static int16_t 			_soundBuffer[BUFLEN];

#define MAX_VOICES 			12					// max 3 sids*4 voices (1 digi channel)
#define MAX_SCOPE_BUFFERS 	12

static int16_t* 		_scope_buffers[MAX_SCOPE_BUFFERS];	// output "scope" streams corresponding to final audio buffer

// these buffers are "per frame" i.e. 1 screen refresh, e.g. 822 samples
static int16_t * 		_synth_buffer= 0;
static int16_t ** 		_synth_trace_buffers= 0;

static uint32_t 		_chunk_size; 
static uint16_t 		_number_of_samples_per_call;  

static uint32_t 		_number_of_samples_rendered = 0;
static uint32_t 		_number_of_samples_to_render = 0;

static uint8_t 			_sound_started, _skip_silence_loop;

static int8_t 			_digi_diagnostic;
static int16_t 			_digiPreviousRate;
static int16_t 			_digi_average_rate;


// ----------------- general .sid file status information ------------------------------------

static uint8_t 	_sid_version;

static uint8_t	_is_rsid;
static uint8_t	_is_psid;			// redundancy to avoid runtime logic

static uint16_t	_sid_addr[3];		// start addr of installed SID chips (0 means NOT available)
static uint8_t 	_sid_is_6581[3];	// models of installed SID chips 

static uint8_t 	_ntsc_mode= 0;
static uint32_t	_clockRate;

static uint32_t	_sample_rate;

static uint8_t 	_compatibility;
static uint8_t 	_basic_prog;
static uint16_t _free_space;

static uint16_t	_load_addr, _init_addr, _play_addr, _load_end_addr;
static uint8_t 	_actual_subsong, _max_subsong;
static uint32_t	_play_speed;

static uint32_t	_trace_sid= 0;

extern "C" uint16_t envGetFreeSpace() {
	return _free_space;
}

static void resetTimings(uint8_t is_ntsc) {
	vicSetModel(is_ntsc);	// see for timing details

	if(is_ntsc) {	// 
		// NTSC
		_clockRate= 1022727;	// system clock (14.31818MHz/14)
	} else {
		// PAL
		_clockRate= 985249;		// system clock (17.734475MHz/18)
	}
}


// song specific infos
	// 0: load_addr;
	// 1: play_speed;
	// 2: max_sub_song;
	// 3: actual_sub_song;
	// 4: song_same;
	// 5: song_author;
	// 6: song_copyright;
static void* _load_result[7];

#define MAX_INFO_LEN 32
#define MAX_INFO_LINES 5

static 	char 	_song_name[MAX_INFO_LEN+1], 
				_song_author[MAX_INFO_LEN+1], 
				_song_copyright[MAX_INFO_LEN+1],
				_song_info_trash[MAX_INFO_LEN+1];

static char* _info_texts[MAX_INFO_LINES];

static void resetInfoText() {
	_info_texts[0]= _song_name;
	_info_texts[1]= _song_author;
	_info_texts[2]= _song_copyright;
	
	// .mus files may have more lines with unstructured text... ignore 	
	_info_texts[3]= _song_info_trash;
	_info_texts[4]= _song_info_trash;

	memset(_song_name, 0, MAX_INFO_LEN);
	memset(_song_author, 0, MAX_INFO_LEN);
	memset(_song_copyright, 0, MAX_INFO_LEN);
}


uint16_t getSidAddr(uint8_t center_byte) {
	if (((center_byte >= 0x42) && (center_byte <= 0xFE)) && 
		!((center_byte >= 0x80) && (center_byte <= 0xDF)) && 
		!(center_byte & 0x1)) {

		return ((uint16_t)0xD000) | (((uint16_t)center_byte)<<4);
	}
	return 0;
}

static void configureSids(uint16_t flags, uint8_t addr2, uint8_t addr3) {
	_sid_addr[0]= 0xd400;
	_sid_is_6581[0]= (flags>>4) & 0x3;	
	_sid_is_6581[0]= !((_sid_is_6581[0]>>1) & 0x1); 	// only use 8580 when bit is explicitly set
	
	_sid_addr[1]= getSidAddr(addr2);
	_sid_is_6581[1]= (flags>>6) & 0x3;
	_sid_is_6581[1]= !_sid_is_6581[1] ? _sid_is_6581[0] : !((_sid_is_6581[1]>>1) & 0x1); 
		
	_sid_addr[2]= getSidAddr(addr3);
	_sid_is_6581[2]= (flags>>8) & 0x3;
	_sid_is_6581[2]= !_sid_is_6581[2] ? _sid_is_6581[0] : !((_sid_is_6581[2]>>1) & 0x1); 
}

extern "C" uint16_t* envSIDAddresses() {
	return _sid_addr;
}
extern "C" uint8_t*  envSID6581s() {
	return _sid_is_6581;
}

extern "C" uint8_t envIsRSID() {
	return _is_rsid;
}

extern "C" uint8_t envIsPSID(){
	return _is_psid;
}

extern "C" uint16_t envSidPlayAddr() {
	return _play_addr;
}

extern "C" uint16_t envNumberOfSamplesPerCall() {
	return _number_of_samples_per_call;
}

static uint8_t get_bit(uint32_t val, uint8_t b)
{
    return (uint8_t) ((val >> b) & 1);
}

extern "C" uint8_t envCurrentSongSpeed() {
	/*
	* PSID V2: songSpeed 0: means screen refresh based, i.e. ignore 
	*                       timer settings an just play 1x per refresh 
	*					 1: means 60hz OR CIA 1 timer A 
	*/	
	return get_bit(_play_speed, _actual_subsong > 31 ? 31 : _actual_subsong); 
}

extern "C" uint32_t envClockRate() {
	return _clockRate;
}

extern "C" int8_t envIsTimerDrivenPSID() {
	return (envIsPSID() && (envCurrentSongSpeed() == 1));
}

extern "C" int8_t envIsRasterDrivenPSID() {
	return (envIsPSID() && (envCurrentSongSpeed() == 0));
}


static void resetAudioBuffers() {
	
	// number of samples corresponding to one simulated frame/screen
	// (granularity of emulation is 1 screen)
	if(_ntsc_mode && envIsRasterDrivenPSID()) { 
		_number_of_samples_per_call= _sample_rate/vicFPS(); 		// NTSC: 735*60=44100		800*60=48000
	} else {
		_number_of_samples_per_call= _sample_rate/vicFPS(); 		// PAL: 882*50=44100		960*50=48000
	}
//	_chunk_size= _number_of_samples_per_call*8; 	// e.g. data for 8 50hz screens (0.16 secs)
	_chunk_size= _number_of_samples_per_call; 	// WebAudio side can still aggregate these..
	// just to make sure these is no garbage left
	
	if (_scope_buffers[0] == 0) {
		// alloc once.. like in old statically allocated impl..
		for (int i= 0; i<MAX_SCOPE_BUFFERS; i++) {
			_scope_buffers[i]= (int16_t*) calloc(sizeof(int16_t), BUFLEN);
		}
	} else {
		for (int i= 0; i<MAX_SCOPE_BUFFERS; i++) {
			memset(_scope_buffers[i], 0, sizeof(int16_t)*BUFLEN);
		}
	}

	if (_synth_buffer) free(_synth_buffer);

	_synth_buffer= (int16_t*)malloc(sizeof(int16_t)*_number_of_samples_per_call + 1);
	
	// trace output (corresponding to _synth_buffer)
	if (_synth_trace_buffers) {
		for (int i= 0; i<MAX_VOICES; i++) {
			if (_synth_trace_buffers[i]) { free(_synth_trace_buffers[i]); _synth_trace_buffers[i]= 0; }
		}
		free(_synth_trace_buffers); _synth_trace_buffers= 0;
	}	

	if (_trace_sid) {	// availability of _synth_trace_buffers controls if SID will generate the respective output
		if (!_synth_trace_buffers) _synth_trace_buffers= (int16_t**)calloc(sizeof(int16_t*), MAX_VOICES);
		
		for (int i= 0; i<MAX_VOICES; i++) {
			_synth_trace_buffers[i]= (int16_t*)calloc(sizeof(int16_t),_number_of_samples_per_call + 1);			
		}		
		
	} else {
		if (_synth_trace_buffers) {
			for (int i= 0; i<MAX_VOICES; i++) {
				if (_synth_trace_buffers[i]) { free(_synth_trace_buffers[i]); _synth_trace_buffers[i]= 0; }
			}		
			free(_synth_trace_buffers); _synth_trace_buffers= 0;	// disables respective SID rendering
		}
	}
	_number_of_samples_rendered = 0;
	_number_of_samples_to_render = 0;	
}

extern "C" uint8_t envSetNTSC(uint8_t is_ntsc)  __attribute__((noinline));	// note: GUI may treak this
extern "C" uint8_t EMSCRIPTEN_KEEPALIVE envSetNTSC(uint8_t is_ntsc) {
	_ntsc_mode= is_ntsc;
	
	resetTimings(is_ntsc);
	resetAudioBuffers();

	return 0;
}


// ----------------- handling of Compute!'s SidPlayer (.mus files) ----------------------------

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
static uint8_t*			_musMemBuffer= 0;										// represents memory at MUS_BASE_ADDR
const static uint16_t	_mus_mem_buffer_size= 0xA000 - MUS_BASE_ADDR;
static uint8_t 			_mus_mode= 0;

static uint16_t musGetOffset(uint8_t* buf) {
	return (((uint16_t)buf[1]) << 8) + buf[0];
}

void musGetSizes(uint8_t *mus_song_file, uint16_t *v1len, uint16_t *v2len, uint16_t *v3len, uint16_t *track_data_len) {
	(*v1len)= musGetOffset(mus_song_file+2);
	(*v2len)= musGetOffset(mus_song_file+4);
	(*v3len)= musGetOffset(mus_song_file+6);

	(*track_data_len)= MUS_HEAD+ (*v1len)+ (*v2len)+ (*v3len);
}	

static uint8_t musIsTrackEnd(uint8_t voice) {
	return memReadRAM(MUS_PLAY_INDICATOR) == 0x0;
	/*
	// check for end of .mus voice 0 (other voices might be shorter & loop)
	uint16_t addr= (memReadRAM(voice + MUS_VOICE_PTRS+3) << 8) + memReadRAM(voice + MUS_VOICE_PTRS) - 2;	// pointer stops past the 0x14f HALT command!
	return (memReadRAM(addr) == 0x1) && (memReadRAM(addr+1) == 0x4f); 	// HALT command 0x14f
	*/
}

static void musMapInfoTexts(uint8_t *mus_song_file, uint32_t mus_song_file_len, uint16_t track_data_len) {
	if (mus_song_file_len <= track_data_len) return;
	
	uint8_t *buffer= mus_song_file + track_data_len;
	uint16_t max_info_len= mus_song_file_len - track_data_len;

	resetInfoText();
		
	uint8_t line = 0;
	uint8_t current_len = 0;
	for (uint8_t j= 0; j<max_info_len; j++) {	// iterate over all the remaining chars in the file 
		uint8_t ch= buffer[j];	
		
		if (!(ch == 0xd) && ((ch < 0x20) || (ch > 0x60))) continue; // remove C64 special chars.. don't have that font anyway 
		
		if (current_len < MAX_INFO_LEN) {
			char* dest = _info_texts[line];
			dest[current_len++]= (ch == 0xd)? 0 : ch;
			
			if (MAX_INFO_LEN == current_len) current_len--; // last one wins.. hopefully the 0 terminator..
		} else {
			// ignore: should not be possible..
		}
		if ((ch == 0xd) && (current_len > 0)) {	// remove empty lines
			// start new line..
			current_len= 0;
			line++;
			if (line >= MAX_INFO_LINES) break;
		}
	}
}

// Compute!'s .mus files require an addtional player that must installed with the song file.
static uint16_t loadComputeSidplayerData(uint8_t *mus_song_file, uint32_t mus_song_file_len) {
	// note: the player can also be used in RSID mode (but for some reason the timing is then much slower..)
	_sid_version= 2;
	_basic_prog= 0;
	_compatibility= 1;
	envSetNTSC(1);	// .mus stuff is mostly from the US..

	
	configureSids(0, 0, 0); 	// just use one *old* SID at d400 
	
	_load_addr= MUS_BASE_ADDR;
	_load_end_addr= 0x9fff;

	_init_addr= MUS_BASE_ADDR;
	_play_addr=  0x1a07;
	
	uint16_t pSize= COMPUTESIDPLAYER_LENGTH;
	if((pSize > MUS_MAX_SIZE) || (mus_song_file_len > MUS_MAX_SONG_SIZE)) return 0; // ERROR
	
	// prepare temp input buffer
	if (_musMemBuffer == 0) {
		_musMemBuffer= (uint8_t*)malloc(_mus_mem_buffer_size);	// represents mem from MUS_BASE_ADDR to $9fff
	}
	memcpy(_musMemBuffer, computeSidplayer, pSize);

	// patch/configure the MUS player
	_musMemBuffer[0x17ca -MUS_BASE_ADDR]= (!_ntsc_mode) & 0x1;	// NTSC by default.. so this is not really needed
		
		
	// patch "INIT" routine to load the MUS file from other address
//	uint16_t addr= 	MUS_DATA_START+2;					// skip 2-bytes header, e.g. start at $2820 
//	_musMemBuffer[0x17ef -MUS_BASE_ADDR]= addr & 0xff;
//	_musMemBuffer[0x17f1 -MUS_BASE_ADDR]= addr >>8;
	
	memcpy(_musMemBuffer+MUS_REL_DATA_START, mus_song_file, mus_song_file_len);


	uint16_t v1len, v2len, v3len, track_data_len;
	musGetSizes(mus_song_file, &v1len, &v2len, &v3len, &track_data_len);
	if (track_data_len >= mus_song_file_len) {
		EM_ASM_({ console.log('info cannot be retrieved  from corrupt .mus file');});	// less mem than inclusion of fprintf
		return 0;
	}		
	
 	musMapInfoTexts(mus_song_file, mus_song_file_len, track_data_len);

	return 1;
}

// ----------------- generic handling --------------------------------------------------------

extern "C" int32_t computeAudioSamples()  __attribute__((noinline));
extern "C" int32_t EMSCRIPTEN_KEEPALIVE computeAudioSamples() {
#ifdef TEST
	return 0;
#endif
	if (_mus_mode && musIsTrackEnd(0)) {
		return -1;
	}
	_number_of_samples_rendered = 0;
			
	uint32_t sample_buffer_idx=0;
	uint8_t has_digi= 0;
	
	while (_number_of_samples_rendered < _chunk_size)
	{
		if (_number_of_samples_to_render == 0) {
			_number_of_samples_to_render = _number_of_samples_per_call;
			sample_buffer_idx=0;
						
			for (uint8_t i= 0; i<_skip_silence_loop; i++) {	// too much seeking might block too long?
				Core::runOneFrame(_synth_buffer, _synth_trace_buffers, _number_of_samples_per_call);
				
				DigiType t= SID::getGlobalDigiType();
				if (t) {
					uint16_t rate= SID::getGlobalDigiRate();
					
					if (rate > 10) {
						has_digi= 1;
						
						_digi_diagnostic= t;
						_digi_average_rate = (_digiPreviousRate + rate) >> 1;	// average out frame overflow issues
						_digiPreviousRate= rate;
					} else {
						_digi_diagnostic= DigiNone;
						_digiPreviousRate= _digi_average_rate = 0;
					}
				}
				
				_sound_started |= has_digi;

				if (!_sound_started) {
					// check if there is some output this time..
					for (uint16_t j= 0; j<_number_of_samples_per_call; j++) {
						if (_synth_buffer[j] > 30) {		// empty signal still returns some small values...
							_sound_started= 1;
							break;
						}
					}
				} else {
					break;
				}			
			}
			if (!_sound_started) {
				// broken sounds might become irresponsive without this brake
				_skip_silence_loop = (_skip_silence_loop>10) ? (_skip_silence_loop - 10) : 1; 
			}			
		}
		
		if (_number_of_samples_rendered + _number_of_samples_to_render > _chunk_size) {
			uint32_t available_space = _chunk_size-_number_of_samples_rendered;
			
			memcpy(&_soundBuffer[_number_of_samples_rendered], &_synth_buffer[sample_buffer_idx], sizeof(int16_t)*available_space);
				
			/*
			* In addition to the actual sample data played by WebAudio, buffers containing raw voice data are also
			* created here. These are 1:1 in sync with the sample buffer, i.e. for each sample entry in the sample buffer
			* there are respective there is a corresponding entry in the additional buffers - which are all exactly the
			* same size as the sample buffer. Note: things start to become messy when trying to use (e.g. visualize) 
			* respective add-on data *in-sync* with the actual WebAudio playback (see AbstractTicker).
			*/
	
			if (_trace_sid) {
				int sid_voices= SID::getNumberUsedChips()*4;			
				// do the same for the respective voice traces
				for (int i= 0; i<sid_voices; i++) {
					memcpy(&(_scope_buffers[i][_number_of_samples_rendered]), &(_synth_trace_buffers[i][sample_buffer_idx]), sizeof(int16_t)*available_space);
				}
			}
			sample_buffer_idx += available_space;
			_number_of_samples_to_render -= available_space;
			_number_of_samples_rendered = _chunk_size;
		} else {
			memcpy(&_soundBuffer[_number_of_samples_rendered], &_synth_buffer[sample_buffer_idx], sizeof(int16_t)*_number_of_samples_to_render);

			if (_trace_sid) {
				int sid_voices= SID::getNumberUsedChips()*4;
				// do the same for the respecive voice traces
				for (int i= 0; i<sid_voices; i++) {
					memcpy(&(_scope_buffers[i][_number_of_samples_rendered]), &(_synth_trace_buffers[i][sample_buffer_idx]), sizeof(int16_t)*_number_of_samples_to_render);
				}
			}
			_number_of_samples_rendered += _number_of_samples_to_render;
			_number_of_samples_to_render = 0;
		} 
	}
	return (_number_of_samples_rendered);
}


extern "C" uint32_t enableVoice(uint8_t sid_idx, uint8_t voice, uint8_t on)  __attribute__((noinline));
extern "C" uint32_t EMSCRIPTEN_KEEPALIVE enableVoice(uint8_t sid_idx, uint8_t voice, uint8_t on) {
	SID::setMute(sid_idx, voice, !on);
	
	return 0;
}

extern "C" uint32_t playTune(uint32_t selected_track, uint32_t trace_sid)  __attribute__((noinline));
extern "C" uint32_t EMSCRIPTEN_KEEPALIVE playTune(uint32_t selected_track, uint32_t trace_sid) {
	_trace_sid= trace_sid; 
	
	_actual_subsong= (selected_track >= _max_subsong) ? _actual_subsong : selected_track;
	_digi_diagnostic= _digi_average_rate= _digiPreviousRate= 0;
	
	_sound_started= 0;
	_skip_silence_loop= 100;

	Core::startupSong(_sample_rate, _ntsc_mode, _compatibility, &_init_addr, _load_end_addr, _play_addr, _actual_subsong);

	resetAudioBuffers();

	return 0;
}

#ifdef TEST
static uint16_t loadTestFromMemory(void *buf, uint32_t buflen)
{
	uint8_t *pdata =(uint8_t*)buf;;
    uint8_t data_file_offset= 0;	
	
	uint16_t load_addr;
	
	// original C64 binary file format	
	load_addr = pdata[0];
	load_addr|= pdata[1]<<8;

	data_file_offset +=2;
	
	int32_t size= buflen-2;
	if (size < 0 || size >0xffff) {
		return 0;		// illegal sid file
	}	
	Core::loadSongBinary(&pdata[2], load_addr, size);
    return load_addr;	
}
#endif

static uint16_t loadSIDFromMemory(void *sid_data, 
					uint16_t *load_addr, uint16_t *load_end_addr, 
					uint16_t *init_addr, uint16_t *play_addr, uint8_t *subsongs, 
					uint8_t *startsong, uint32_t *speed, uint32_t file_size)
{
    uint8_t *pdata;
    uint8_t data_file_offset;	
	
    pdata = (uint8_t*)sid_data;
	
    data_file_offset = pdata[7];

    *load_addr = pdata[8]<<8;
    *load_addr|= pdata[9];

    *init_addr = pdata[10]<<8;
    *init_addr|= pdata[11];

    *play_addr = pdata[12]<<8;
    *play_addr|= pdata[13];

	uint16_t tracks= pdata[0xf] | (((uint16_t)pdata[0xe])<<8); // pointless using 2 bytes for a max of 256.. when the max of speed flags is 32..
	if (tracks == 0) tracks= 1;
	if (tracks > 0xff) tracks= 0xff;
    *subsongs = tracks & 0xff;

	uint16_t start= pdata[0x11] | (((uint16_t)pdata[0x10])<<8);
	if (!start) start= 1;
	if (start > tracks) start= tracks;
    *startsong = (start & 0xff) - 1;	// start at index 0

	if (*load_addr == 0) {
		// original C64 binary file format
		
		*load_addr = pdata[data_file_offset];
		*load_addr|= pdata[data_file_offset+1]<<8;
		
		data_file_offset +=2;
	}
	if (*init_addr == 0) {
		*init_addr= *load_addr;	// 0 implies that init routine is at load_addr
	}	
	
    *speed = pdata[0x12]<<24;
    *speed|= pdata[0x13]<<16;
    *speed|= pdata[0x14]<<8;
    *speed|= pdata[0x15];
    
	*load_end_addr= *load_addr+file_size-data_file_offset;
	
	int32_t size= file_size-data_file_offset;
	if (size < 0 || size >0xffff) {
		return 0;		// illegal sid file
	}
	
	// find a space to put the starter code: the number of hoops you have 
	// to jump through just to find 6 free bytes is a bad joke..
	
	uint8_t start_page= pdata[0x78];
	uint8_t driver_size = 33;	// see memory.c: _driverPSID
	
	_free_space= 0;
	if (start_page == 0xff) {
		// no space available
	} else if (start_page == 0x0) {
		if (((*load_addr) + size) < (0xcfff - driver_size)) {
			_free_space= 0xcfff - driver_size;
		} else if ((*load_addr) >= (0x0400 + driver_size)) {
			_free_space= (0x0400 + driver_size);
		}
	} else {
		_free_space= ((uint16_t)start_page) << 8;
	}
	
	Core::loadSongBinary(&pdata[data_file_offset], *load_addr, size);

    return *load_addr;
}


static uint16_t parseSYS(uint16_t start, uint16_t end) {
	// parse a simple "123 SYS3000" BASIC command
	uint16_t result= 0;
	uint8_t c= 0;
	for (uint16_t i= start; i<=end; i++) {
		c= memReadRAM(i);
		if (!c) break;

		if ((c >= 0x30) && (c <= 0x39) ) { 
			result = result*10 + (c-0x30); 
		} else if (result > 0) {
			break;
		}
	}
	return result;
}	

// extracts the SYS $.... address from trivial BASIC program amd patches (*initAddr) accordingly
void startFromBasic(uint16_t *init_addr) {
	// don't have C64 BASIC ROM if BASIC program is just used to 
	// jump to some address (SYS $....) then try to do that for 
	// simple one line programs..

	if ((*init_addr) == 0x0801) {
		uint16_t nextLine= memReadRAM(*init_addr) | ((memReadRAM((*init_addr)+1)) << 8);
		if (!memReadRAM(nextLine)) {
			// one line program	(bad luck if additional REM etc lines are used..)
			if (memReadRAM(((*init_addr) + 4)) == 0x9e) {	// command is SYS
				 (*init_addr) = parseSYS((*init_addr) + 5, nextLine);
			}			
		}
	}
}

void setRsidMode(uint8_t is_rsid) {
	_is_rsid= is_rsid;
	_is_psid= !is_rsid;
}

extern "C" uint32_t loadSidFile(uint32_t is_mus, void * in_buffer, uint32_t in_buf_size, uint32_t sample_rate, char *filename )  __attribute__((noinline));
extern "C" uint32_t EMSCRIPTEN_KEEPALIVE loadSidFile(uint32_t is_mus, void * in_buffer, uint32_t in_buf_size, uint32_t sample_rate, char *filename ) {
#ifdef TEST
	fprintf(stderr, "starting test %s\n", filename);
	
	_sid_version= 2;
	_basic_prog= 0;
	_compatibility= 1;

	setRsidMode(1);
	
	envSetNTSC(0);
	
	configureSids(0, 0, 0); 	// just use one *old* SID at d400 
	
	memInitTest();
	
	_init_addr= loadTestFromMemory(in_buffer, in_buf_size);
	
	if (_init_addr) {
		if (_init_addr != 0x0801) {
			fprintf(stderr, "ERROR: unexpected start for  test: %s len: %lu addr: %d\n", filename, in_buf_size, _init_addr);
		}
		Core::rsidRunTest();		
	} else {
		fprintf(stderr, "ERROR: cannot load test: %s\n", (const char*)filename);
	}
	return 0;
	
#else
	uint8_t *input_file_buffer= (uint8_t *)in_buffer;	
	
	if (!is_mus && (in_buf_size < 0x7c)) return 1;	// we need at least a header..

	memResetKernelROM();		// read only (only need to do this once)

    _sample_rate= sample_rate > 96000 ? 48000 : sample_rate; // see hardcoded BUFLEN
	_init_addr= 0;
	_play_addr= 0;
	_load_end_addr= 0;
	_actual_subsong= 0;
	_max_subsong= 0;	
	_play_speed= 0;		
	uint8_t ntscMode= 0;	// default is PAL

	
	setRsidMode(is_mus || (input_file_buffer[0x00] == 0x50) ? 0 : 1);

	memResetRAM(envIsPSID());
	
	if ((_mus_mode= is_mus)) {
		// todo: the same kind of impl could be used for .sid files that contain .mus data.. (see respectice flag)
		if (!loadComputeSidplayerData(input_file_buffer, in_buf_size)) {
			return 1;
		}
		Core::loadSongBinary(_musMemBuffer, _load_addr, _mus_mem_buffer_size);		
	} else {
		_sid_version= input_file_buffer[0x05];
				
		uint16_t flags= (_sid_version > 1) ? (((uint16_t)input_file_buffer[0x77]) | (((uint16_t)input_file_buffer[0x77])<<8)) : 0x0;
				
		_basic_prog= (envIsRSID() && (flags & 0x2));	// C64 BASIC program need to be started..
		
		_compatibility= ( (_sid_version & 0x2) &&  ((flags & 0x2) == 0));	
		ntscMode= (_sid_version == 2) && envIsPSID() && (flags & 0x8); // NTSC bit
		
		configureSids(flags, _sid_version>2?input_file_buffer[0x7a]:0, _sid_version>3?input_file_buffer[0x7b]:0);
			
		uint8_t i;
		for (i=0;i<32;i++) _song_name[i] = input_file_buffer[0x16+i];
		for (i=0;i<32;i++) _song_author[i] = input_file_buffer[0x36+i]; 
		for (i=0;i<32;i++) _song_copyright[i] = input_file_buffer[0x56+i];

		if (!loadSIDFromMemory(input_file_buffer, &_load_addr, &_load_end_addr, &_init_addr, 
				&_play_addr, &_max_subsong, &_actual_subsong, &_play_speed, in_buf_size)) {
			return 1;	// could not load file
		}	
	}
		

	if (_basic_prog) startFromBasic(&_init_addr);
	
	// global settings that depend on the loaded music file
	envSetNTSC(ntscMode);
	//resetTimings(_ntsc_mode);
	
	_load_result[0]= &_load_addr;
	_load_result[1]= &_play_speed;
	_load_result[2]= &_max_subsong;
	_load_result[3]= &_actual_subsong;
	_load_result[4]= _song_name;
	_load_result[5]= _song_author;
	_load_result[6]= _song_copyright;

	return 0;
#endif	
}
extern "C" char** getMusicInfo() __attribute__((noinline));
extern "C" char** EMSCRIPTEN_KEEPALIVE getMusicInfo() {
	return (char**)_load_result;
}

extern "C" uint32_t getSoundBufferLen() __attribute__((noinline));
extern "C" uint32_t EMSCRIPTEN_KEEPALIVE getSoundBufferLen() {
	return _number_of_samples_rendered;	// in samples
}

extern "C" char* getSoundBuffer() __attribute__((noinline));
extern "C" char* EMSCRIPTEN_KEEPALIVE getSoundBuffer() {
	return (char*) _soundBuffer;
}

extern "C" uint32_t getSampleRate() __attribute__((noinline));
extern "C" uint32_t EMSCRIPTEN_KEEPALIVE getSampleRate() {
	return _sample_rate;
}

// additional accessors that might be useful for tweaking defaults from the GUI

extern "C" uint8_t envIsSID6581()  __attribute__((noinline));
extern "C" uint8_t EMSCRIPTEN_KEEPALIVE envIsSID6581() {
	return _sid_is_6581[0];		// only for the 1st chip
}

extern "C" uint8_t envSetSID6581(uint8_t is6581)  __attribute__((noinline));
extern "C" uint8_t EMSCRIPTEN_KEEPALIVE envSetSID6581(uint8_t is6581) {
	// this minimal update should allow to toggle the used filter without disrupting playback in progress
	_sid_is_6581[0]= _sid_is_6581[1]= _sid_is_6581[2]= is6581;
	SID::setModels(envSID6581s());
	return 0;
}

extern "C" uint8_t getDigiType()  __attribute__((noinline));
extern "C" uint8_t EMSCRIPTEN_KEEPALIVE getDigiType() {
	return SID::getGlobalDigiType();
}

extern "C" const char* getDigiTypeDesc()  __attribute__((noinline));
extern "C" const char* EMSCRIPTEN_KEEPALIVE getDigiTypeDesc() {
	return SID::getGlobalDigiTypeDesc();
}

extern "C" uint16_t getDigiRate()  __attribute__((noinline));
extern "C" uint16_t EMSCRIPTEN_KEEPALIVE getDigiRate() {
	return SID::getGlobalDigiRate();
}

extern "C" uint8_t envIsNTSC()  __attribute__((noinline));
extern "C" uint8_t EMSCRIPTEN_KEEPALIVE envIsNTSC() {
	return _ntsc_mode;
}

extern "C" uint16_t getRegisterSID(uint16_t reg) __attribute__((noinline));
extern "C" uint16_t EMSCRIPTEN_KEEPALIVE getRegisterSID(uint16_t reg) {
	return  memReadIO(0xd400 + reg);
}

extern "C" uint16_t getRAM(uint16_t addr) __attribute__((noinline));
extern "C" uint16_t EMSCRIPTEN_KEEPALIVE getRAM(uint16_t addr) {
	return  memReadRAM(addr);
}

extern "C" uint16_t getGlobalDigiType() __attribute__((noinline));
extern "C" uint16_t EMSCRIPTEN_KEEPALIVE getGlobalDigiType() {
	return (_digi_diagnostic > 0) ? _digi_diagnostic : 0;
}

extern "C" const char * getGlobalDigiTypeDesc() __attribute__((noinline));
extern "C" const char * EMSCRIPTEN_KEEPALIVE getGlobalDigiTypeDesc() {
	return (_digi_diagnostic > 0) ? SID::getGlobalDigiTypeDesc() : "";
}

extern "C" uint16_t getGlobalDigiRate() __attribute__((noinline));
extern "C" uint16_t EMSCRIPTEN_KEEPALIVE getGlobalDigiRate() {
	return (_digi_diagnostic > 0) ? _digi_average_rate : 0;
}

extern "C" int getNumberTraceStreams() __attribute__((noinline));
extern "C" int EMSCRIPTEN_KEEPALIVE getNumberTraceStreams() {	// always use additional stream for digi samples..
	return SID::getNumberUsedChips()*4;
}
extern "C" const char** getTraceStreams() __attribute__((noinline));
extern "C" const char** EMSCRIPTEN_KEEPALIVE getTraceStreams() {
	return (const char**)_scope_buffers;	// ugly cast to make emscripten happy
}


// ------------------ deprecated stuff that should no longer be used -----------------------

// @deprecated bit0=voice0, bit1=voice1,..
extern "C" uint32_t enableVoices(uint32_t mask)  __attribute__((noinline));
extern "C" uint32_t EMSCRIPTEN_KEEPALIVE enableVoices(uint32_t mask) {
	for(uint8_t i= 0; i<3; i++) {
		SID::setMute(0, i, !(mask&0x1));
		mask = mask >> 1;
	}
	
	return 0;
}

/**
* @deprecated use getTraceStreams instead
*/
extern "C" char* getBufferVoice1() __attribute__((noinline));
extern "C" char* EMSCRIPTEN_KEEPALIVE getBufferVoice1() {
	return (char*) _scope_buffers[0];
}
/**
* @deprecated use getTraceStreams instead
*/
extern "C" char* getBufferVoice2() __attribute__((noinline));
extern "C" char* EMSCRIPTEN_KEEPALIVE getBufferVoice2() {
	return (char*) _scope_buffers[1];
}
/**
* @deprecated use getTraceStreams instead
*/
extern "C" char* getBufferVoice3() __attribute__((noinline));
extern "C" char* EMSCRIPTEN_KEEPALIVE getBufferVoice3() {
	return (char*) _scope_buffers[2];
}
/**
* @deprecated use getTraceStreams instead
*/
extern "C" char* getBufferVoice4() __attribute__((noinline));
extern "C" char* EMSCRIPTEN_KEEPALIVE getBufferVoice4() {
	return (char*) _scope_buffers[3];
}
