/*
* This file provides the interface to the JavaScript world (i.e. it provides all the 
* APIs expected by the "backend adapter").
*
* It also handles the output audio buffer logic.
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
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#include <stdlib.h> 
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <emscripten.h>

extern "C" {
#include "env.h"
#include "vic.h"	
#include "core.h"
#include "memory.h"
}
#include "sid.h"	
#include "loaders.h"	



static FileLoader*		_loader;

// ----------------- audio output buffer management ------------------------------------------


#define BUFLEN 96000/50	// keep it down to one screen to allow for more direct feedback to WebAudio side..

static uint32_t 	_soundBufferLen= BUFLEN;

#define CHANNELS 2
static int16_t 		_soundBuffer[BUFLEN*CHANNELS];

#define MAX_VOICES 			40					// max 10 sids*4 voices (1 digi channel) - fixme: no digi used in multi-SID
#define MAX_SCOPE_BUFFERS 	40

static int16_t* 	_scope_buffers[MAX_SCOPE_BUFFERS];	// output "scope" streams corresponding to final audio buffer

// these buffers are "per frame" i.e. 1 screen refresh, e.g. 822 samples
static int16_t * 	_synth_buffer= 0;
static int16_t ** 	_synth_trace_buffers= 0;

static uint32_t 	_chunk_size; 
static uint16_t 	_number_of_samples_per_call;  

static uint32_t 	_number_of_samples_rendered = 0;
static uint32_t 	_number_of_samples_to_render = 0;

static uint8_t	 	_sound_started;
static uint8_t	 	_skip_silence_loop;

static int8_t 		_digi_diagnostic;
static int16_t 		_digiPreviousRate;
static int16_t 		_digi_average_rate;

static uint32_t		_sample_rate;
static uint32_t		_clockRate;

static uint32_t		_trace_sid= 0;
static uint8_t		_ready_to_play= 0;

extern "C" uint16_t envGetFreeSpace() {
	return FileLoader::getFreeSpace();
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

extern "C" uint16_t* envSIDAddresses() {
	return FileLoader::getSIDAddresses();
}
extern "C" uint8_t*  envSID6581s() {
	return FileLoader::getSID6581s();
}

extern "C" uint8_t*  envSIDOutputChannels() {
	return FileLoader::getTargetChannels();
}

extern "C" uint8_t envIsRSID() {
	return FileLoader::isRSID();
}

extern "C" uint8_t envIsPSID(){
	return FileLoader::isPSID();
}

extern "C" uint16_t envSidPlayAddr() {
	return FileLoader::getSidPlayAddr();
}

extern "C" uint16_t envNumberOfSamplesPerCall() {
	return _number_of_samples_per_call;
}

extern "C" uint8_t envCurrentSongSpeed() {
	return FileLoader::getCurrentSongSpeed(); 
}

extern "C" uint32_t envClockRate() {
	return _clockRate;
}

extern "C" uint8_t env2ndOutputChanIdx() {
	return FileLoader::get2ndOutputChanIdx();
}

static void resetAudioBuffers() {
	
	// number of samples corresponding to one simulated frame/screen
	// (granularity of emulation is 1 screen)
	
	// NTSC: 735*60=44100		800*60=48000  
	// PAL: 882*50=44100		960*50=48000
	_number_of_samples_per_call= _sample_rate/vicFPS();

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

	_synth_buffer= (int16_t*)malloc(sizeof(int16_t)*_number_of_samples_per_call*CHANNELS + 1);
	
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
	resetTimings(is_ntsc);
	resetAudioBuffers();

	return 0;
}

// ----------------- generic handling --------------------------------------------------------

// This is driving the emulation: Each call to computeAudioSamples() delivers some fixed number 
// of audio samples and the necessary emulation timespan is derived from it:

extern "C" int32_t computeAudioSamples()  __attribute__((noinline));
extern "C" int32_t EMSCRIPTEN_KEEPALIVE computeAudioSamples() {
	if(!_ready_to_play) return 0;
	
	int sid_voices= SID::getNumberUsedChips()*4;

#ifdef TEST
	return 0;
#endif
	_number_of_samples_rendered = 0;
			
	uint32_t sample_buffer_idx=0;
	
	while (_number_of_samples_rendered < _chunk_size)
	{
		if (_number_of_samples_to_render == 0) {
			_number_of_samples_to_render = _number_of_samples_per_call;
			sample_buffer_idx=0;
						
			uint8_t is_single_sid=	envSidVersion() != MULTI_SID_TYPE;
			uint8_t speed=	envCurrentSongSpeed();
			
			for (uint16_t i= 0; i<_skip_silence_loop; i++) {	// too much seeking will make the browser unresponsive
				Core::runOneFrame(is_single_sid, speed, _synth_buffer, _synth_trace_buffers, _number_of_samples_per_call);
				
				DigiType t= SID::getGlobalDigiType();
				if (t) {
					uint16_t rate= SID::getGlobalDigiRate();
					
					if (rate > 10) {
						_digi_diagnostic= t;
						_digi_average_rate = (_digiPreviousRate + rate) >> 1;	// average out frame overflow issues
						_digiPreviousRate= rate;
					} else {
						_digi_diagnostic= DigiNone;
						_digiPreviousRate= _digi_average_rate = 0;
					}
				} else {
					_digi_diagnostic= DigiNone;
					_digiPreviousRate= _digi_average_rate = 0;
				}
								
				if (!_sound_started) {
					if (SID::isAudible()) {
						_sound_started= 1;
						break;
					}
				} else {
					break;
				}			
			}			
		}
		
		if (_number_of_samples_rendered + _number_of_samples_to_render > _chunk_size) {
			uint32_t available_space = _chunk_size-_number_of_samples_rendered;
			
			memcpy(&_soundBuffer[_number_of_samples_rendered], &_synth_buffer[sample_buffer_idx], sizeof(int16_t)*available_space*CHANNELS);
				
			/*
			* In addition to the actual sample data played by WebAudio, buffers containing raw voice data are also
			* created here. These are 1:1 in sync with the sample buffer, i.e. for each sample entry in the sample buffer
			* there are respective there is a corresponding entry in the additional buffers - which are all exactly the
			* same size as the sample buffer. Note: things start to become messy when trying to use (e.g. visualize) 
			* respective add-on data *in-sync* with the actual WebAudio playback (see AbstractTicker).
			*/
	
			if (_trace_sid) {
				// do the same for the respective voice traces
				for (int i= 0; i<sid_voices; i++) {
					if ((FileLoader::getSidVersion() != MULTI_SID_TYPE) || (sid_voices % 4) != 3)	// no digi 
						memcpy(&(_scope_buffers[i][_number_of_samples_rendered]), &(_synth_trace_buffers[i][sample_buffer_idx]), sizeof(int16_t)*available_space);
				}
			}
			sample_buffer_idx += available_space;
			_number_of_samples_to_render -= available_space;
			_number_of_samples_rendered = _chunk_size;
		} else {
			memcpy(&_soundBuffer[_number_of_samples_rendered], &_synth_buffer[sample_buffer_idx], sizeof(int16_t)*CHANNELS*_number_of_samples_to_render);

			if (_trace_sid) {
				// do the same for the respecive voice traces
				for (int i= 0; i<sid_voices; i++) {
					if ((FileLoader::getSidVersion() != MULTI_SID_TYPE) || (sid_voices % 4) != 3)	// no digi 
						memcpy(&(_scope_buffers[i][_number_of_samples_rendered]), &(_synth_trace_buffers[i][sample_buffer_idx]), sizeof(int16_t)*_number_of_samples_to_render);
				}
			}
			_number_of_samples_rendered += _number_of_samples_to_render;
			_number_of_samples_to_render = 0;
		} 
	}
	
	if (_loader->isTrackEnd()) {	// "play" must have been called before 1st use of this check
		return -1;
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
	_ready_to_play= 0;
	_trace_sid= trace_sid; 
			
	_digi_diagnostic= _digi_average_rate= _digiPreviousRate= 0;
	
	_sound_started= 0;
		
	// note: crappy BASIC songs like Baroque_Music_64_BASIC take 100sec before they even 
	// start playing.. unfortunately the emulation is NOT fast enough (at least on my 
	// old PC) to just skip that phase in "no time" and if the calculations take too
	// long then they will block the browser complitely
	
	// performing respective skipping-logic directly within INIT is a bad idea since it will 
	// completely block the browser until it is completed. From "UI responsiveness" perspective
	// it is preferable to attempt a "speedup" on limited slices from within the audio-rendering loop.
	
	_skip_silence_loop= 10;	// should keep the UI responsive; means that on a fast machine the above garbage song will still take 10 secs before it plays

	_loader->initTune(_sample_rate, selected_track);

	resetAudioBuffers();

	_ready_to_play= 1;

	return 0;
}

extern "C" uint32_t loadSidFile(uint32_t is_mus, void *in_buffer, uint32_t in_buf_size, uint32_t sample_rate, char *filename, void *basic_ROM, void *char_ROM, void *kernal_ROM)  __attribute__((noinline));
extern "C" uint32_t EMSCRIPTEN_KEEPALIVE loadSidFile(uint32_t is_mus, void *in_buffer, uint32_t in_buf_size, uint32_t sample_rate, char *filename, void *basic_ROM, void *char_ROM, void *kernal_ROM) {
	_ready_to_play= 0;											// stop any emulator use
    _sample_rate= sample_rate > 96000 ? 48000 : sample_rate; 	// see hardcoded BUFLEN

	_loader= FileLoader::getInstance(is_mus, in_buffer, in_buf_size);
	
	if (!_loader) return 1;	// error

	return _loader->load((uint8_t *)in_buffer, in_buf_size, filename, basic_ROM, char_ROM, kernal_ROM);
}

extern "C" char** getMusicInfo() __attribute__((noinline));
extern "C" char** EMSCRIPTEN_KEEPALIVE getMusicInfo() {
	return FileLoader::getInfoStrings();
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
	return FileLoader::isSID6581();
}

extern "C" uint8_t envSetSID6581(uint8_t is6581)  __attribute__((noinline));
extern "C" uint8_t EMSCRIPTEN_KEEPALIVE envSetSID6581(uint8_t is6581) {
	return FileLoader::setSID6581(is6581);
}

extern "C" uint8_t getDigiType()  __attribute__((noinline));
extern "C" uint8_t EMSCRIPTEN_KEEPALIVE getDigiType() {
	return SID::getGlobalDigiType();
}

extern "C" uint8_t envSidVersion()  __attribute__((noinline));
extern "C" uint8_t EMSCRIPTEN_KEEPALIVE envSidVersion() {
	return FileLoader::getSidVersion();
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
	return FileLoader::getNTSCMode();
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

/**
* @deprecated bit0=voice0, bit1=voice1,..
*/
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
