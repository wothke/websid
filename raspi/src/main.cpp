/*
* Driver used to run WebSid as a native program on a RaspberryPi4 to drive 
* a SidBerry mounted SID chip.
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
* directly based on that script file - and use that for comparisons.
* 
* Issue #2: The timestamps recorded in the emulation are measured using the correct
* clockspeed of the song's specified environment (NTSC vs PAL). Whereas the playback
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

/*
	ISSUE: it seems the Linux scheduler disrupts the timing with some kind of interrupt that occurs 
	with a 100Hz frequency: introducing average delays of ~7 micros (but which may occasionally 
	peak up to 30 micros).
	
	For regular songs that just expect some register update every 20ms these glitches are 
	irrelevant as long as the register is eventually updated. However these songs would be 
	massively hurt if affected updates went missing (i.e. bad idea to drop delayed writes).

	However there are songs that depend on very accurately timed, fast paced SID writes that
	are typically performed via an NMI. They try to exploit one of several SID effects that 
	all depend on very exact timing:
	The "distance" between SID writes will "normally" be at least 4 cycles - see 6510's 
	"STA/STX/STY instructions" available for writing to SID (so under normal conditions there
	should be plenty of time to correctly perform them in the player - but not then the logic
	is interrupted for several micros :-( ). Delays may hurt respective songs either due to 
	some SID state not being turned on long enough or to it being turned on for too long.
	
	(There are READ_MODIFY_WRITE operations that will trigger even faster SID write 
	sequences - but these are NOT currently triggered in WebSid and they are very rarely 
	used - Soundcheck.sid is one of the exceptions that might be affected.) 
	
	Below an example of a typical digi-player (see LMan - Vortex.sid), where a 4-writes 
	sequence is performed within a 20 micros interval. The routine plays samples at about 
	8kHz, i.e. it runs in a loop that is repeated every ~125 clock cycles:
	
	;2257    A2 11       LDX #$11          TRIANGLE + GATE(i.e. start "attack phase") 
	;2259    8E 12 D4    STX $D412  +0
	;225C    A2 09       LDX #$09		   TEST (i.e. reset phase accu at 0) + GATE ; i.e. no 	
	                                       waveform means that current DAC level is sustained
	;225E    8E 12 D4    STX $D412	+6
	;2261    AE 08 04    LDX $0408
	;2264    8E 0F D4    STX $D40F	+14    set frequency of oscillator for targeted sample output level
	;2267    A2 01       LDX #$01          GATE (i.e. removal of TEST starts the counting 
	                                       in phase accu)
	;2269    8E 12 D4    STX $D412	+20    "start seeking", i.e. "seek for the desired output 
	                                       level" - this state will stay for ~100 clocks before 
										   the above code is restarted

	(see https://codebase64.org/doku.php?id=base:vicious_sid_demo_routine_explained )
										   
	The actual output level is played at ;225E, i.e. whatever state the SID has been
	skillfully manouvered into at this point defines what is output for the next 125 clock 
	cycles - the remaining logic just makes sure to end up in the correct state: The concept 
	is to select a specific output level (see ;2264) using a triangle waveform (i.e. a waveform
	that linearly changes from minimum to maximum output level - a saw waveform 
	could be used just as well) by letting the oscillator count for an exact(!) period of 
	time (i.e. counting starts from 0 with the write at ;2269 and stops with write at ;225E). 
	Counting for too long or not long enough always results in an incorrect output 
	level. Here it is crucial that the duration that the TEST bit has been set is correct to 
	the cycle!
	
	Half of this problem could be compensated for by a hack: If a delay is observed during 
	a write that clears the TEST bit, then the next write that sets the TEST bit could just 
	be delayed by the same amount. However, if the delay strikes in the second write, nothing
	can be done since the SIDs oscillator has already counted too far.. which cannot be 
	undone. Additionally, in the short period between where the actual waveform is turned on
	(see ;2259) to where it is "frozen" (;225E) a delay might still have some effect.
*/

#include <iostream>             // std::cout
#include <unistd.h>             // nice
#include <string.h>
#include <thread>               // std::thread
#include <mutex>                // std::mutex, std::unique_lock
#include <condition_variable>   // std::condition_variable
#include <atomic>               // std::atomic, std::atomic_flag, ATOMIC_FLAG_INIT
#include <deque>


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
#include "gpio.h"
#include "cp1252.h"

using namespace std;

#define MAX_ENTRIES	1500	// must be adapted to selected "chunk_size"; testcase Comaland_tune_3.sid


/**
* MT save management of the buffers available for double-buffering.
* 
* Throttles "buffer producer" thread if necessary. 
*/
class BufferPool {
	mutex _mtx;
	condition_variable _cv;
    deque<uint32_t *> _pool;
public:
	BufferPool(uint32_t *b1, uint32_t *b2) {		
		// initially available buffers (hardcoded to 2)
		_pool.push_back(b1);
		_pool.push_back(b2);
	}
	/**
	* Hands an buffer to the "producer" thread.
	*
	* Blocks caller until a buffer is available.
	*/
	uint32_t *fetchBuffer() {
		// block caller until there is a buffer available..  
		unique_lock<mutex> lck(_mtx);
		
		while (_pool.empty()) {
			_cv.wait(lck);
		}
		uint32_t *result= _pool.back(); // here it doesn't matter which end
		_pool.pop_back();
		
		return result;
	}
	/**
	* Lets "consumer" thread return an unused buffer back to the pool. 
	*/
	void yieldBuffer(uint32_t *buf) {
		// block caller until there is a "fillable" buffer available..  
		unique_lock<mutex> lck(_mtx);
		_pool.push_front(buf);				// doesn't matter which end
		
		_cv.notify_all();	// wake up waiting main if necessarry
	}
};

/**
* MT save "FIFO work queue" containing the buffers that can be played by the PlaybackThread.
*/
class WorkQueue {
	mutex _mtx;
    deque<uint32_t *> _work;
public:
	uint32_t *popBuffer() {
		unique_lock<mutex> lck(_mtx);
		if (_work.empty()) return 0;
		
		uint32_t *result= _work.back(); // remove oldest from back
		_work.pop_back();
		
		return result;
	}
	void pushBuffer(uint32_t *buf) {
		unique_lock<mutex> lck(_mtx);
		
		_work.push_front(buf);	// add new in front
		
		// in worst case the playback thread will just be busy polling 
		// and there is no need to wake it up
	}
};

/**
* Re-plays the "SID poke" instructions fed by the main thread.
*
* Respective instructions are referred to as "scripts" below and they come 
* in ~20ms batches. A simple double buffering is used to let main thread feed 
* instructions while this one is playing. The separate playback thread decouples
* the actual emulator from the timing critical SID access.
*
* known limitation: the 32-bit counter will overflow every ~70 minutes and the below 
* logic will not currently handle this gracefully. fixme todo: check if use of the full 
* 64-bit counter would cause any relevent slowdown
*/
#define TEST_BITMASK 	0x08
#define FIX_TIMING_HACK		minimal improvement.. see Coma_Light_13_tune_4.sid

class PlaybackThread {
	
	// each script buffer entry consists of 2 uint32_t values where the first
	// value is an offset (in micros) to be matched against the system timer
	// and the second value contains the sid-register and value to be written:
	// value 1) bits 31-0; timestamp in micros; a 0 marks the end of the script
	// 			timestamps are relative to the start of the playback and must 
	//          be adjusted for matching with the system timer
	// value 2) bits 31-16: unused; bits 15-8: SID register offset; bits 7-0: value to write
	
	// the below two buffers are always "owned" by one specific thread at a time and 
	// there is no concurrent access to the buffer
	uint32_t _script_buf1[(MAX_ENTRIES+1) * 2]; 
	uint32_t _script_buf2[(MAX_ENTRIES+1) * 2]; 
	
	
	// MT safe stuff concurrently used
	BufferPool* _buffer_pool;	// available buffers that can be filled by the main thread	
	WorkQueue _work_queue;	
	atomic<bool> _running;	// used to end the thread's loop
	
#ifdef FIX_TIMING_HACK
	// partially compensate timing issues
	uint8_t _test_bit[3];	// current state of the SID voice's test bit
	uint32_t _delay[3];		// delay observed for the setting of a SID voice's the test-bit
#endif
	
	void playScript(uint32_t *buf, uint32_t ts_offset) {
		uint32_t i= 0;
		
		uint32_t ts= buf[i];
				
		while(ts != 0) {
			uint32_t &data= buf[i+1];
			uint8_t reg= (data>>8) & 0x1f;
			uint8_t value= data & 0xff;
			
#ifdef FIX_TIMING_HACK
			uint8_t voice_idx = 0;
			uint8_t voice_reg = reg;	
			if (reg < 7) {}
			else if (reg <= 13) { voice_idx = 1; voice_reg = reg - 7; }
			else if (reg <= 20) { voice_idx = 2; voice_reg = reg - 14; }
#endif			
			
			ts+= ts_offset;
			i+= 2;			// 2x uint32_t per entry

			
			uint32_t now = SYS_COUNT_LOW();
			if (now > ts) {
				// problem: the current write has been delayed
				
#ifdef FIX_TIMING_HACK
				if (voice_reg == 0x4) {	// voice's waveform control
					if (value & TEST_BITMASK) {
						// if clearing was delayed then the setting should be equally delayed
						ts += _delay[voice_idx];
						
						// osc probably already counted too long.. nothing that can be done about that
						_delay[voice_idx]= 0;
					} else {
						// if clearing of test bit is delayed, then the next setting should be equally delayed
						_delay[voice_idx] = _test_bit[voice_idx] ? now-ts : 0;
					}
					_test_bit[voice_idx] = value & TEST_BITMASK;
				}
#endif				
			} else {
				
#ifdef FIX_TIMING_HACK
				if (voice_reg == 0x4) {	// voice's waveform control
					if (value & TEST_BITMASK) {
						// if clearing was delayed then the setting should be equally delayed
						ts += _delay[voice_idx];
					}
					_test_bit[voice_idx] = value & TEST_BITMASK;
					_delay[voice_idx] = 0;
				}
#endif				
				// as long as the player has not been delayed by some interrupt, 
				// then ts should always be in the future when we get here..
				while (SYS_COUNT_LOW()< ts) {/* just wait and poll  */}
			}

			pokeSID(reg, value);

			ts= buf[i];			
		}
	}
	
	void playLoop() {
		migrateThreadToCore3();
		
//		cout << "playLoop running" << endl;
				
		uint32_t ts_offset= 0;
		
		while (_running) {
			uint32_t *buf= 0;
			while (buf == 0) { 
				buf = _work_queue.popBuffer();
				/* just wait for main to feed some data */
				if (!_running) break;
			}
			if (!buf) break;
			
			if (!ts_offset) {
				ts_offset= SYS_COUNT_LOW(); 	// sync playback with current counter pos
			}
			
			playScript(buf, ts_offset);
			
			// done with this buffer
			_buffer_pool->yieldBuffer(buf);
			
		}
		cout << "thread exit" << endl;		
	}
	
	
public:
	PlaybackThread() {
		// if core #3 has been proberly configured in /boot/cmdline.txt then 
		// this thread should be pretty "alone" on that core.. but just in case:
		scheduleRT();
		
		_running = true;
		_buffer_pool = new BufferPool(_script_buf1, _script_buf2);
	}
	virtual ~PlaybackThread() {
		delete _buffer_pool;
	}
	void stop() {
		_running= false;
	}
	uint32_t *fetchBuffer() {
		return _buffer_pool->fetchBuffer();
	}
	// mandatory; buf must have been obtained via fetchBuffer()
	void feedBuffer(uint32_t *buf) {
		_work_queue.pushBuffer(buf);
	}
	
	thread start() {
		return thread([=] { playLoop(); });
	}
};


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

uint32_t *_script_buf;
uint16_t _script_len = 0;

void recordBegin(PlaybackThread *p) {
	_script_buf = p->fetchBuffer();	// playback thread (consumer) throttles the feed process
	_script_len = 0;	
}
// callback triggered in the emulator whenever a SID register is written
void recordPokeSID(uint32_t ts, uint8_t reg, uint8_t value) {
	
	if (_script_len < MAX_ENTRIES) {
		_script_buf[_script_len<<1] = ts;
		_script_buf[(_script_len<<1) + 1] = (((uint32_t)reg)<<8) | ((uint32_t)value);
		
		_script_len++;
	} else {
		cout << "fatal error: recording buffer too small" << endl;
	}
}
void recordEnd(PlaybackThread *p) {
	_script_buf[_script_len<<1] = 0;	// end marker
	
	p->feedBuffer(_script_buf);
	_script_buf = 0;	// redundant
	_script_len = 0;	// redundant
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

uint32_t max_frames;

void int_callback(int s){
	// intercept ctrl-C so that proper cleanup can be performed
    max_frames= 0;
}

void init() {
	installSigHandler(int_callback);
	
	systemTimerSetup();		// init access to system timer counter
	gpioInit();
}

void resetSid(PlaybackThread *p) {
	// hack: just feed a script that resets SID to avoid 
	// annoying beeps when interrupting a song..
	
	uint32_t resetScript[49] = {
		// reset all SID registers to 0
		1, (0x0100),
		1, (0x0200),
		1, (0x0300),
		1, (0x0400),
		1, (0x0500),
		1, (0x0600),
		1, (0x0700),
		1, (0x0800),
		1, (0x0900),
		1, (0x0A00),
		1, (0x0B00),
		1, (0x0C00),
		1, (0x0D00),
		1, (0x0E00),
		1, (0x0F00),
		1, (0x1000),
		1, (0x1100),
		1, (0x1200),
		1, (0x1300),
		1, (0x1400),
		1, (0x1500),
		1, (0x1600),
		1, (0x1700),
		1, (0x1800),
		0
	};
	p->feedBuffer(resetScript);
	usleep(40000);			// should be enough for the buffer to be played
}

//#define CHECK_PERFORMANCE

int main(int argc, char *argv[]) {
	init();

	PlaybackThread *p = new PlaybackThread();
		
	recordBegin(p);	
	loadSidFile(argc, argv);// songs already access SID in INIT.. so recording must be ready
	recordEnd(p);
		
	startHyperdrive();

	thread player_thread = p->start();	
	
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

	uint32_t overflow_limit = ((0xffffffff - SYS_COUNT_LOW()) / 1000000)*vicFramesPerSecond();	// secs to next counter overflow

	if (overflow_limit < max_frames) {
		max_frames = overflow_limit;
		cout << "note: limiting playback time to avoid counter overflow" << endl;
	}
	
	for (uint32_t frame_count = 0; frame_count < max_frames; frame_count++) {
		recordBegin(p);
				
		// the output size of the below runOneFrame call is actually defined by the
		// requested chunk_size, i.e. it will typically cover between 17-20ms 
		
#ifndef CHECK_PERFORMANCE
		Core::runOneFrame(1, speed, synth_buffer, 0, chunk_size);	// will trigger recordPokeSID callback
		
		recordEnd(p);
#else
		uint32_t start= SYS_COUNT_LOW();
		Core::runOneFrame(1, speed, synth_buffer, 0, chunk_size);	// will trigger recordPokeSID callback
		uint32_t diff = SYS_COUNT_LOW() - start;

		recordEnd(p);

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

	resetSid(p);
	
	// teardown	
	p->stop();
	player_thread.join();
	delete p;
	
	free(synth_buffer);	
	
	stopHyperdrive();
	
	return 0;
}

