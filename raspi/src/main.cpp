/*
* Driver used to run WebSid as a native program on a "Raspberry Pi 4B" to drive 
* a SidBerry mounted SID chip.
*
* The program either plays using an integrated playback thread (lower quality) or 
* using a separate playback device driver (which must previsouly have been installed as
* a kernel module). Upon startup this program automatically selects the best available
* playback option.
*
* Note: the WebSid emulator uses:
* 
* NTSC:	59.8260895 Hz screen refresh rate 
* 		17095 clock cycles per frame
* 		1022727 Hz clock speed (14.31818MHz/14)
* 		
* PAL:	50.124542  Hz screen refresh rate 
* 		19656 clock cycles per frame
* 		985249 Hz clock speed (17.734475MHz/18)
* 
* Known Limitation: due to different clock speeds, the speed of a "cycle" would 
* normally differ between NTSC and PAL but the used "SidBerry" device always uses 
* the same clock speed of 1MHz, i.e. the WF output calculated by the emulation will
* inevitably get out of sync with the 1MHz clocked real SID chip.. todo: it might be a 
* good idea to save the "script" files and later re-run a 1Mhz clocked SID emulation
* directly based on that script file - and use that for comparisons. Or the emulator 
* could be patched to use a differently clocked SID.
* 
* Issue #2: The timestamps recorded in the emulation are measured using the correct
* clock speed of the song's specified environment (NTSC vs PAL). Whereas the playback
* thread uses the (flawed) 1MHz counter of the Raspberry (which probably is NOT in 
* sync with the real 1MHz crystal used on the device either.).
*
*
* WebSid (c) 2021 Jürgen Wothke
* version 1.0
*
* Terms of Use: This software is licensed under a CC BY-NC-SA
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/


#include <iostream>     // std::cout
#include <string.h>

#include <unistd.h>		// close

// WebSid stuff
#include "../../src/core.h"
#include "../../src/loaders.h"
extern "C" {
#include "../../src/vic.h"	
	// from sidplayer.cpp
uint32_t loadSidFile(uint32_t is_mus, void* in_buffer, uint32_t in_buf_size, uint32_t sample_rate, char* filename, void* basic_ROM, void* char_ROM, void* kernal_ROM);
uint32_t playTune(uint32_t selected_track, uint32_t trace_sid);
char** getMusicInfo();
}

// Raspberry stuff
#include "rpi4_utils.h"
#include "cp1252.h"

#include "fallback_handler.h"
#include "device_driver_handler.h"

using namespace std;


void showHelp(char *argv[]) {
	
	cout << "Usage: " << argv[0] << " <song filename> [Options]" << endl;
	cout << "Options: " << endl;
	cout << " -t, --track   : index of the track to play (if more than 1 is available)" << endl;			
	cout << " -v, --version : Show version and copyright information " << endl;
	cout << " -h, --help    : Show this help message " << endl << endl;
	
	cout << "Note: MOS6581/8580 player specifically designed for use with a" << endl;
	cout << "Raspberry Pi 4 (it cannot be used with older models!) and with" << endl;
	cout << "the SID-chip adapter/GPIO wiring used in the SidBerry project." << endl << endl;
	
	cout << "The player expects to have CPU core #3 dedicated for its exclusive" << endl;
	cout << "use: add 'isolcpus=3 rcu_nocbs=3 rcu_nocb_poll=3 nohz_full=3 ' to" << endl;
	cout << "/boot/cmdline.txt for best results. " << endl;
	
	exit(1);
}

void handleArgs(int argc, char *argv[], string *filename, int *track) {
	for(int i = 1; i < argc; i++) {
		if((filename->length() == 0) && argv[i][0] != '-') {
			*filename = argv[i];			
		} else if(!strcmp(argv[i], "-v") || !strcmp(argv[i], "--version")) {
			cout << "WebSid (RPi4 edition 1.0)" << endl;
			cout << "(C)opyright 2021 by Jürgen Wothke" << endl;
		} else if(!strcmp(argv[i], "-t") || !strcmp(argv[i], "--track")) {
			i++;
			*track = atoi(argv[i]) - 1;
		} else if(!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
			showHelp(argv);			
		} else {
			cout << "Warning: Invalid parameter " << endl;
		}
	}

	if(filename->length() == 0) {
		showHelp(argv);			
	}
}

#define SID_FILE_MAX 0xffff

size_t loadBuffer(string *filename, unsigned char *buffer) {
	FILE *file = fopen(filename->c_str(), "rb");	
	if(file == NULL) {
		cout << "file not found: " << (*filename) << endl;
		return 0;
	}
	
	size_t size = fread(buffer, 1, SID_FILE_MAX, file);
	fclose(file);
	
	return size;
}

void printSongInfo() {
	unsigned char** info = (unsigned char**)getMusicInfo();
	
	cout << endl;	
	cout << "name     : " << cp1252_to_utf(info[4]) << endl;
	cout << "author   : " << cp1252_to_utf(info[5]) << endl;
	cout << "copyright: " << cp1252_to_utf(info[6]) << endl << endl;
}

#define CHANNELS 2

void loadSidFile(int argc, char *argv[]) {
    int track = 0;
	string filename = "";
	handleArgs(argc, argv, &filename, &track);
	
	unsigned char buffer[SID_FILE_MAX];
	size_t size = loadBuffer(&filename, buffer);
	
	if ( loadSidFile(0, buffer, size, 44100, (char*)filename.c_str(), 0, 0, 0)) {
		cout << "error: no valid sid file - " << filename << endl;
		exit(1);
	}
	if ( playTune(track, 0)) {
		cout << "error: cannot select track - " << track << endl;
		exit(1);
	}
	
	printSongInfo();
}

volatile uint32_t max_frames;

void int_callback(int s){
	// SIGINT (ctrl-C) handler so that proper cleanup can be performed
	max_frames = 0;		// make play loop end at next iteration	
	cout << "playback was interrupted" << endl;
}

void init() {
	installSigHandler(int_callback);
	
	systemTimerSetup();		// init access to system timer counter
}


#define CHECK_PERFORMANCE



static PlaybackHandler *_playbackHandler;

// callback triggered in the emulator whenever a SID register is written
void recordPokeSID(uint32_t ts, uint8_t reg, uint8_t value) {
	_playbackHandler->recordPokeSID(ts, reg, value);
}

void initPlaybackHandler() {
	int fd;
	volatile uint32_t *buffer_base;
	detectDeviceDriver(&fd, &buffer_base);
	
	if (fd < 0) {
		cout <<  "using    : local thread based playback" << endl;
		_playbackHandler= new FallbackHandler();
	} else {
		cout <<  "using    : device driver based player" << endl;		
		_playbackHandler = new DeviceDriverHandler(fd, buffer_base);
	}
}

int main(int argc, char *argv[]) {
	init();
	
	initPlaybackHandler();

	_playbackHandler->recordBegin();	
	loadSidFile(argc, argv);// songs already access SID in INIT.. so recording must be ready
	_playbackHandler->recordEnd();
		
	startHyperdrive();
	
	uint32_t	sample_rate = 44100;
	uint8_t		speed =	FileLoader::getCurrentSongSpeed();
	
	uint16_t	chunk_size = sample_rate / vicFramesPerSecond();// number of samples per call
	int16_t* 	synth_buffer = (int16_t*)malloc(sizeof(int16_t)* (chunk_size * CHANNELS + 1));

#ifdef CHECK_PERFORMANCE
	uint32_t max_micro = 0;
	uint32_t min_micro = 0xffffffff;
#endif

	double frame_in_sec = 1.0 / vicFramesPerSecond();
	max_frames =  5*60*vicFramesPerSecond();	// 5 minutes

	uint32_t overflow_limit = ((0xffffffff - micros()) / 1000000)*vicFramesPerSecond();	// secs to next counter overflow

	if (overflow_limit < max_frames) {
		max_frames = overflow_limit;
		cout << "note: limiting playback time to avoid counter overflow" << endl;
	}
	
	for (uint32_t frame_count = 0; frame_count < max_frames; frame_count++) {
		_playbackHandler->recordBegin();
				
		// the output size of the below runOneFrame call is actually defined by the
		// requested chunk_size, i.e. it will typically cover between 17-20ms 
		
#ifndef CHECK_PERFORMANCE
		Core::runOneFrame(1, speed, synth_buffer, 0, chunk_size);	// will trigger recordPokeSID callback
		
		_playbackHandler->recordEnd();
#else
		uint32_t start = micros();
	
		Core::runOneFrame(1, speed, synth_buffer, 0, chunk_size);	// will trigger recordPokeSID callback

		uint32_t diff = micros() - start;

		_playbackHandler->recordEnd();

		if (diff > max_micro) max_micro= diff;
		if (diff < min_micro) min_micro= diff;
#endif		
		uint32_t sec= frame_count*frame_in_sec;
		printf("\rplaying [%02d:%02d]", sec/60, sec%60); fflush(stdout);		
	}
	
#ifdef CHECK_PERFORMANCE
	// debugging: check worst case performance
	cout << "\n\nmin: " <<  min_micro/1000 << " ms max: " << max_micro/1000 << endl;
#endif

	// teardown	
	delete _playbackHandler;
		
	free(synth_buffer);		
	
	stopHyperdrive();
	systemTimerTeardown();
	
	return 0;
}

