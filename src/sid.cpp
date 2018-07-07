/*
 * This file builds on the remains of sidengine.c from the "TinySid for Linux" distribution -
 * eventhough little remains of the original implementation by now.
 *
 * <p>It contains the logic used to emulate the "MOS Technology SID" (see 6581, 8580 or 
 * 6582), and only the added handling of 'digi samples' is kept separately in digi.c
 *
 * <p>I massively updated/replaced the old "Tinysid" implementation using my own extensions
 * as well as code from other sources. Specifically I added an "envelope generator" (incl. 
 * "ADSR-bug" handling) and support for RSID digi playback (D418, PWM, etc) - which is handled
 * as a postprocessing step.
 *
 * <p>Credits:
 * <ul>
 * <li>TinySid (c) 1999-2012 T. Hinrichs, R. Sinsch (with additional fixes from Markus Gritsch) 
 *             originally provided the starting point
 * <li>Hermit's jsSID.js provided a variant of "resid filter implementation", an "anti aliasing" 
 *             for "pulse" and "saw" waveforms, and a rather clever approach to
 *             generate combined waveforms (see http://hermit.sidrip.com/jsSID.html)
 * </ul> 
 *
 * <p>Tiny'R'Sid add-ons (c) 2011-2018 J.Wothke
 * <p>version 0.9
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */
 
// useful links:
// http://www.waitingforfriday.com/index.php/Commodore_SID_6581_Datasheet
// http://www.sidmusic.org/sid/sidtech2.html
 
// note: only the "sid" prefixed functions are exported outside of this file..
 
 
// NOTE: the change of just moving the old sid.c C code into a C++ SID class (no functional changes - only moving
// of static functions into the class) *increases* the size of the output library from 129kb to 133kb!
// worse Chrome's "prep time" rises von 0 to 200ms

 
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "sid2.h"
#include "envelope.h"
#include "filter.h"

extern "C" {
#include "base.h"
#include "digi.h"
#include "memory.h"
#include "cpu.h"		// cpuGetProgramMode(), etc
#include "env.h"		// envNumberOfSamplesPerCall(), etc
};


/* Get the bit from an uint32_t at a specified position */
static uint8_t getBit(uint32_t val, uint8_t b) { return (uint8_t) ((val >> b) & 1); }

#define SYNC_BITMASK 	0x02		
#define RING_BITMASK 	0x04		
#define TEST_BITMASK 	0x08		

#define TRI_BITMASK 	0x10		
#define SAW_BITMASK 	0x20		
#define PULSE_BITMASK 	0x40		
#define NOISE_BITMASK 	0x80

// -------- keep private structs hidden here - no point to expose these implementation details in the header file -----------
//          these are the structs from old C impl - which could eventually be furter refactored

// SID register definition
struct SidState {
	uint8_t isModel6581;
	uint8_t level_DC;
	uint32_t sampleRate;	// e.g. 44100

	double cycleOverflow;
	double cyclesPerSample;
	
    struct Voice {
        uint8_t wave;
		uint16_t freq;
        uint16_t pulse;
 
		// add-ons snatched from Hermit's implementation
		double prevWavData;				// combined waveform handling
		uint16_t prevWaveFormOut;		// floating DAC handling
		
		uint8_t notMuted;				// player's separate "mute" feature
    } voices[3];
	
};

// hack for main loop polling (would not be needed if CPU and SID emu were more tightly synchronized..)
struct SimOsc3 {
	uint8_t waveform;
	uint32_t baseCycles;
	uint32_t counter;
	uint32_t multiplicator;	
};

// internal oscillator def
struct Oscillator {
    uint32_t freqIncSample;		// osc increment per second 
    uint32_t freqIncCycle;		// osc increment per cycle
	
    uint32_t pulse;
    uint8_t wave;
	
    uint32_t counter;		// 24-bit as in the original
	
	// hard sync handling
    uint32_t prevCounter;
	uint8_t msbRising;
	
    uint32_t noisepos;
    uint32_t noiseval;
    uint8_t noiseout;
};

/**
* This utility class keeps the precalculated "combined waveform" lookup tables. 
*
* There is no point keeping these in each SID instance (since they are the same anyway)
*/
class WaveformTables {
public: 
	WaveformTables();
private:
	static void createCombinedWF(double *wfarray, double bitmul, double bitstrength, double treshold);
public:
	// Hermit's precalculated "combined waveforms" 
	double TriSaw_8580[4096];
	double PulseSaw_8580[4096];
	double PulseTri_8580[4096];			// Hermit's use of PulseSaw_8580 does not convince in Last_Ninja
	double PulseTriSaw_8580[4096];
};

WaveformTables::WaveformTables() {
    createCombinedWF(TriSaw_8580, 0.8, 2.4, 0.64); //precalculate combined waveform
    createCombinedWF(PulseSaw_8580, 1.4, 1.9, 0.68);
	createCombinedWF(PulseTriSaw_8580, 0.8, 2.5, 0.64);
	// far from "correct" but at least a bit better than Hermit's use of PulseSaw_8580 (see Last_Ninja)
    createCombinedWF(PulseTri_8580, 0.8, 1.5, 0.38);	// improved settings are welcome!
}

void WaveformTables::createCombinedWF(double *wfarray, double bitmul, double bitstrength, double treshold) { //I found out how the combined waveform works (neighboring bits affect each other recursively)
	for (uint16_t i = 0; i < 4096; i++) {
		wfarray[i] = 0; //neighbour-bit strength and DAC MOSFET threshold is approximately set by ears'n'trials
		for (uint8_t j = 0; j < 12; j++) {
			double bitlevel = 0;
			for (uint8_t k = 0; k < 12; k++) {
				bitlevel += (bitmul / pow(bitstrength, abs(k - j))) * (((i >> k) & 1) - 0.5);
			}
			wfarray[i] += (bitlevel >= treshold) ? pow(2.0, (double)j) : 0;
		}
		wfarray[i] *= 12;
	}
}

static WaveformTables _wave;		// only need one instance of this


/**
* This class represents one specific MOS SID chip.
*/
SID::SID() {
	_addr= 0;		// e.g. 0xd400
	
	_sid= (SidState*) malloc(sizeof(SidState));

	_osc[0]= (Oscillator*) malloc(sizeof(Oscillator));
	_osc[1]= (Oscillator*) malloc(sizeof(Oscillator));
	_osc[2]= (Oscillator*) malloc(sizeof(Oscillator));
		
	_env[0]= new Envelope(this, 0);
	_env[1]= new Envelope(this, 1);
	_env[2]= new Envelope(this, 2);
		
	_filter= new Filter(this);

	// hack
	_osc3sim = (SimOsc3*) malloc(sizeof(SimOsc3));
}


// ------------------------- convenience accessors ----------------------------
#define isTestBit(voice) _osc[voice]->wave & TEST_BITMASK

uint8_t SID::getPreviousVoice(uint8_t voice) {
	return voice ? voice-1 : 2;
}
uint8_t SID::getNextVoice(uint8_t voice) {
	return (voice == 2) ? 0 : voice+1;
}

uint32_t SID::getRingModCounter(uint8_t voice) {
	// Bob Yannes: "Ring Modulation was accomplished by substituting the accumulator MSB of an oscillator 
	// in the EXOR function of the triangle waveform generator with the accumulator MSB of the 
	// previous oscillator. That is why the triangle waveform must be selected to use Ring Modulation."
	
	if (_osc[voice]->wave & RING_BITMASK) {
		// wtf does he mean? (1) substitute MSB before using it in some EXOR logic OR (2) substitute it 
		// using EXOR?
		
		uint8_t srcVoice = getPreviousVoice(voice);  
//		return (_osc[voice]->counter & 0x7fffff) | (_osc[srcVoice]->counter & 0x800000);	// (1)	
		return _osc[voice]->counter ^ (_osc[srcVoice]->counter & 0x800000);				// (2) judging by the sound of R1D1.sid, this is it..
	} else {
		return _osc[voice]->counter;		
	}
}

void SID::resetEngine(uint32_t sampleRate, uint8_t isModel6581) {	
	// base setting
	memset((uint8_t*)_sid,0,sizeof(SidState));

	_sid->sampleRate = sampleRate;
	
	_sid->cycleOverflow = 0;
	_sid->cyclesPerSample = ((double)envClockRate()) / sampleRate;
		
	_sid->isModel6581= isModel6581;	
	_sid->level_DC= isModel6581 ? 0x38 : 0x80;	// supposedly the DC level for respective chip model
	
	// filter
	_filter->reset(sampleRate);

	// envelope generator
	for (uint8_t i=0;i<3;i++) {
		_env[i]->reset();
	}

	// oscillator / waveform stuff
	memset((uint8_t*)_osc[0], 0, sizeof(Oscillator));
	memset((uint8_t*)_osc[1], 0, sizeof(Oscillator));
	memset((uint8_t*)_osc[2], 0, sizeof(Oscillator));

	for (uint8_t i=0;i<3;i++) {
		// note: by default the rest of _sid, _osc & _filter 
		// above is set to 0
		_osc[i]->noiseval = 0x7ffff8;		
		_sid->voices[i].notMuted= 1;
	}
	
	_nmiVolChangeDisabled= _allowedOnce= 0;
}
/*
* While "_sid" data structure is automatically kept in sync by the emulator's memory access 
* implementation, the "_osc" and "_filter" helper structures are NOT - i.e. they need to 
* be explicitly synced before calculating SID output.
*/
void SID::syncRegisterCache() {
	// step 1: convert the not easily processable sid registers into some
    //        more convenient form
	
	// envelope generators
    for (uint8_t voice=0; voice<3; voice++) {
		_env[voice]->syncState();
	}
	
	// oscillators
    for (uint8_t voice=0; voice<3; voice++) {
        _osc[voice]->pulse= (_sid->voices[voice].pulse & 0xfff) << 4;	// // 16 MSB pulse needed
				
        _osc[voice]->wave= _sid->voices[voice].wave;

        _osc[voice]->freqIncSample= round(_sid->cyclesPerSample * _sid->voices[voice].freq);	// per 1-sample interval (e.g. ~22 cycles)
        _osc[voice]->freqIncCycle= ((uint32_t)_sid->voices[voice].freq);		
    }
}

// ------------------------- wave form generation ----------------------------

void SID::syncOscillator(uint8_t voice) {
	// Hard Sync is accomplished by clearing the accumulator of an Oscillator  
	// based on the accumulator MSB of the previous oscillator.

	// tests for hard sync:  Ben Daglish's Wilderness music from The Last Ninja  
	// (https://www.youtube.com/watch?v=AbBENI8sHFE) .. seems to be used on voice 2 early on..
	// for some reason the instrument that starts on voice 1 at about 45 secs (come combined waveform?)
	// does not sound as crisp as it should.. (neither in Hermit's player)
		
	// intro noise in Martin Galway's Roland's Rat Race (https://www.youtube.com/watch?v=Zc91S1lrU1I) music.
	
	// the below logic is from the "previous oscillator" perspective
	
	if (!_osc[voice]->freqIncCycle) return;
	
	uint8_t msbRising= _osc[voice]->msbRising;				// base trigger condition
	
	uint8_t destVoice = getNextVoice(voice);  
	uint8_t destSync= _osc[destVoice]->wave & SYNC_BITMASK;	// sync requested?

	// exception: when sync source is itself synced in the same cycle then destination 
	// is NOT synced (based on analysis performed by reSID)
	uint8_t srcSync= _osc[voice]->wave & SYNC_BITMASK;		// for special case handling 
	uint8_t srcMsbRising= _osc[getPreviousVoice(voice)]->msbRising;

	if (msbRising && destSync && !(srcSync && srcMsbRising)) {	  
		_osc[destVoice]->counter = 0;
	}
}
void SID::advanceOscillators() {
/*	
	// this original HARD SYNC impl from TinySID was quite wrong .. but actually
	// one doesn't hear much of a difference to the correct one!
	uint8_t ctrl= _osc[voice]->wave;
	if ((ctrl & SYNC_BITMASK) && (_osc[srcVoice]->counter < _osc[srcVoice]->freq)) {
		// sync oscillator to srcVoice if sync bit set 
		_osc[voice]->counter = _osc[srcVoice]->counter * _osc[voice]->freq / _osc[srcVoice]->freq;
	}	
	*/
	// forwards time by ONE sample - which corresponds to about 22 cycles.. (todo: optimize this brute force impl later)
	double c= _sid->cycleOverflow + _sid->cyclesPerSample;
	uint16_t cycles= (uint16_t)c;		
	_sid->cycleOverflow= c-cycles;

	for (uint8_t t= 0; t<cycles; t++) {
		// forward oscillators one CYCLE (required to properly time HARD SYNC)
		for (uint8_t voice=0; voice<3; voice++) {
			_osc[voice]->prevCounter= _osc[voice]->counter;
			
			// note: TEST (Bit 3): The TEST bit, when set to one, resets and locks oscillator 1 at zero 
			// until the TEST bit is cleared. The noise waveform output of oscillator 1 is also 
			// reset and the pulse waveform output is held at a DC level; test bit has no influence 
			// on the envelope generator whatsoever!
			if (isTestBit(voice)) {
				_osc[voice]->counter  = 0;
				_osc[voice]->noisepos = 0;
				_osc[voice]->noiseval = 0x7ffff8;
			} else {
				// update wave counter
				_osc[voice]->counter = (_osc[voice]->counter + _osc[voice]->freqIncCycle) & 0xFFFFFF;				
			}

			// base for hard sync
			_osc[voice]->msbRising = (_osc[voice]->counter & 0x800000) > (_osc[voice]->prevCounter & 0x800000);
		}
		
		// handle oscillator HARD SYNC (quality wise it isn't worth the trouble to use this correct impl..)
		for (uint8_t voice=0; voice<3; voice++) {
			syncOscillator(voice);
		}	
	}
}

// ------------------------- wave form generation ----------------------------

// Hermit's impl to calculate combined waveforms (check his jsSID-0.9.1-tech_comments in 
// commented jsSID.js for background info): I did not thoroughly check how well this really 
// works (it works well enough for Kentilla and Clique_Baby (apparently has to sound 
// as shitty as it does)
uint16_t SID::combinedWF(uint8_t channel, double *wfarray, uint16_t index, uint8_t differ6581) { //on 6581 most combined waveforms are essentially halved 8580-like waves
	if (differ6581 && _sid->isModel6581) index &= 0x7FF;
	double combiwf = (wfarray[index] + _sid->voices[channel].prevWavData) / 2;
	_sid->voices[channel].prevWavData = wfarray[index];
	
	return (uint16_t)round(combiwf);
}

uint16_t SID::createTriangleOutput(uint8_t voice) {
	uint32_t tmp = getRingModCounter(voice);
    uint32_t wfout = (tmp ^ (tmp & 0x800000 ? 0xFFFFFF : 0)) >> 7;
	return wfout & 0xFFFF;
}			
uint16_t SID::createSawOutput(uint8_t voice) {	// test with Alien or Kawasaki_Synthesizer_Demo
	// Hermit's "anti-aliasing"
	uint32_t wfout = _osc[voice]->counter >> 8;	// top 16-bits
	double step = ((double)_osc[voice]->freqIncSample) / 0x1200000;
	wfout += round(wfout * step);
	if (wfout > 0xFFFF) wfout = 0xFFFF - round (((double)(wfout - 0x10000)) / step);
	return wfout;
}
void SID::calcPulseBase(uint8_t voice, uint32_t *tmp, uint32_t *pw) {
	// based on Hermit's impl
	(*pw) = _osc[voice]->pulse;
	(*tmp) = _osc[voice]->freqIncSample >> 9;	// 15 MSB needed
	
	if (0 < (*pw) && (*pw) < (*tmp)) { (*pw) = (*tmp); }
	(*tmp) ^= 0xFFFF;
	if ((*pw) > (*tmp)) (*pw) = (*tmp);
	(*tmp) = _osc[voice]->counter >> 8;			// 16 MSB needed
}
uint16_t SID::createPulseOutput(uint8_t voice, uint32_t tmp, uint32_t pw) {	// elementary pulse
	if (isTestBit(voice)) return 0xFFFF;	// pulse start position
	
	// Hermit's "anti-aliasing"
	double step = 256.0 / (_osc[voice]->freqIncSample >> 16); //simple pulse, most often used waveform, make it sound as clean as possible without oversampling

	int32_t wfout;
	if (tmp < pw) {
		wfout = round((0xFFFF - pw) * step);
		if (wfout > 0xFFFF) { wfout = 0xFFFF; }
		wfout = wfout - round((pw - tmp) * step);
		if (wfout < 0) { wfout = 0; }
	} //rising edge
	else {
		wfout = pw * step;
		if (wfout > 0xFFFF) { wfout = 0xFFFF; }
		wfout = round((0xFFFF - tmp) * step) - wfout;
		if (wfout >= 0) { wfout = 0xFFFF; }
		wfout &= 0xFFFF;
	} //falling edge
	return wfout;
}	
uint16_t SID::createNoiseOutput(uint8_t voice) {
	// generate noise waveform exactly as the SID does.
	
	// "random values are output through the waveform generator according to the 
	// frequency setting" (http://www.ffd2.com/fridge/blahtune/SID.primer)
	
	// testcase for calibration: see Hermit's noisewfsweep.sid
	uint32_t p= _osc[voice]->counter>>20;		//  top 4-bit seem to be about right.. (didn't find specific specs unfortunately..)
	if (_osc[voice]->noisepos != p) {
		_osc[voice]->noisepos = p;
		
		// impl consistent with: http://www.sidmusic.org/sid/sidtech5.html
		// doc here is probably wrong: http://www.oxyron.de/html/registers_sid.html
		_osc[voice]->noiseout = 
				(getBit(_osc[voice]->noiseval,22) << 7) |
				(getBit(_osc[voice]->noiseval,20) << 6) |
				(getBit(_osc[voice]->noiseval,16) << 5) |
				(getBit(_osc[voice]->noiseval,13) << 4) |
				(getBit(_osc[voice]->noiseval,11) << 3) |
				(getBit(_osc[voice]->noiseval, 7) << 2) |
				(getBit(_osc[voice]->noiseval, 4) << 1) |
				(getBit(_osc[voice]->noiseval, 2) << 0);
				
		_osc[voice]->noiseval = (_osc[voice]->noiseval << 1) |
				(getBit(_osc[voice]->noiseval,22) ^ getBit(_osc[voice]->noiseval,17));	
	}
	return ((uint16_t)_osc[voice]->noiseout) << 8;
}

/* 
* Render a buffer of n samples using the current SID register contents.
*
* KNOWN LIMITATIONS: This impl is based on a specific starting point "snapshot" of the SID registers
* and it is NOT aware of how this SID state was reached - nor any related timing information. At this
* point the CPU/CIA/VIC emulation has already been completed for the respective time interval.
* Conversely the SID's oscillators are only updated within the below function, i.e. the previously run 
* CPU/CIA/VIC emulation will NOT correctly see respective oscillator state (the above "SimOsc3" hack 
* was introduced to address resulting problems in selected scenarios, i.e. "main loop polling for 
* voice 3 oscillator"). 
*
* To properly avoid respective issues the complete emulation would need to be performed on a per 
* sample (like Hermit does) or better per cycle basis. But that would mean that the currently used 
* "predictive" emulation logic would need to be completely replaced..  and so far that does 
* not seem to be worth the trouble.
*/
uint16_t SID::createWaveOutput(int8_t voice) {
	uint16_t outv= 0xFFFF;
	uint8_t ctrl= _osc[voice]->wave;
	
	if (_sid->voices[voice].notMuted) {
		int8_t combined= 0;
		
		// use special handling for certain combined waveforms				
		uint16_t plsout;
		if ((ctrl & PULSE_BITMASK)) {
			uint32_t tmp, pw;	// 16 bits used
			calcPulseBase(voice, &tmp, &pw);
			
			if (((ctrl&0xf0) == PULSE_BITMASK)) {
				// pulse only 
				plsout= createPulseOutput(voice, tmp, pw);
			} else {
				// combined waveforms with pulse
				plsout=  ((tmp >= pw) || isTestBit(voice)) ? 0xFFFF : 0; //(this would be enough for simple but aliased-at-high-pitches pulse)

				if ((ctrl & TRI_BITMASK) && ++combined)  {
					if (ctrl & SAW_BITMASK) {	// PULSE & TRIANGLE & SAW	- like in Lenore.sid
						outv = plsout ? combinedWF(voice, _wave.PulseTriSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
					} else { // PULSE & TRIANGLE - like in Kentilla, Convincing, Clique_Baby, etc							
						// a good test is Last_Ninja:6 voice 1 at 35secs; here Hermit's original PulseSaw settings seem to 
						// be lacking: the respective sound has none of the crispness nor volume of the original
						
						tmp = getRingModCounter(voice); 
						outv = plsout ? combinedWF(voice, _wave.PulseTri_8580, (tmp ^ (tmp & 0x800000 ? 0xFFFFFF : 0)) >> 11, 0) : 0;	// either on or off						
					}				
				} else if ((ctrl & SAW_BITMASK) && ++combined)  {	// PULSE & SAW - like in Defiler.sid
					outv = plsout ? combinedWF(voice, _wave.PulseSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
				}
			}
		} else if ((ctrl & TRI_BITMASK) && (ctrl & SAW_BITMASK) && ++combined) {		// TRIANGLE & SAW - like in Garden_Party.sid
			uint32_t tmp = _osc[voice]->counter >> 12;
			outv = combinedWF(voice, _wave.TriSaw_8580, tmp, 1);	// tmp 12 MSB
		} 
		if (!combined) {
			/* for the rest mix the oscillators with an AND operation as stated in
				the SID's reference manual - even if this is quite wrong. */
		
			if (ctrl & TRI_BITMASK)  outv &= createTriangleOutput(voice);					
			if (ctrl & SAW_BITMASK)  outv &= createSawOutput(voice);
			if (ctrl & PULSE_BITMASK) outv &= plsout;
			if (ctrl & NOISE_BITMASK)  outv &= createNoiseOutput(voice);
			
			// emulate waveform 00 floating wave-DAC 
			if (ctrl & 0xf0) {	// TODO: find testcase song where this is relevant.. 
				_sid->voices[voice].prevWaveFormOut= outv;
			} else {
				// no waveform set						
				outv= _sid->voices[voice].prevWaveFormOut;		// old impl: outv &= _sid->level_DC;			
			}	
		}
	} else {
		outv= ((uint16_t)_sid->level_DC) << 8;
	}
	return outv;
}

/**************************************************************************************************
	below add-on HACK for "main loop ocsillator polling"
	
	This is a hack to support specific ocsillator polling from main loop as done by PollyTracker
	e.g. see Instantfunk.sid (the usage pattern is very specific and the below hack 
	covers exactly that - and nothing more).
***************************************************************************************************/

uint8_t SID::useOscPollingHack() {
	// so far it has only been tested for main-loop
	return cpuGetProgramMode() == MAIN_OFFSET_MASK;
}
void SID::simStartOscillatorVoice3(uint8_t voice, uint8_t val) {
	if ((voice == 2) && useOscPollingHack()) {
		_osc3sim->waveform= val & 0xf0;

		// hack: use only for main/pulse cases like Instantfunk.sid
		_osc3sim->baseCycles= cpuTotalCycles();
		_osc3sim->counter= 0; 
		
		// for some reason the playback is slightly slower than in ACID64
		_osc3sim->multiplicator= round(_sid->cyclesPerSample * envNumberOfSamplesPerCall() / envCyclesPerScreen());
	}
}
uint32_t SID::simOsc3Counter() {
	// voice 3 oscillator counter based on elapsed time
	uint32_t diff= cpuTotalCycles() - _osc3sim->baseCycles;
	_osc3sim->baseCycles= cpuTotalCycles();

	uint32_t f= ((uint32_t)_sid->voices[2].freq) * _osc3sim->multiplicator * diff;		
	_osc3sim->counter= (_osc3sim->counter + f) & 0xffffff;
	return _osc3sim->counter;
}

uint8_t SID::simReadSawtoothD41B() {
	// simulate sawtooth voice 3 oscillator level based on elapsed time	
	// (handle busy polling for sid oscillator3 - e.g. Ring_Ring_Ring.sid)

	return (uint8_t) (simOsc3Counter() >> 16);
}
uint8_t SID::simReadPulsedD41B() {
	// simulate pulse voice 3 oscillator level based on elapsed time	
	uint32_t p= (((uint32_t)_sid->voices[2].pulse) & 0xfff) << 12;
	return (simOsc3Counter() > p) ? 0 : 1;
}
uint8_t SID::simReadD41B() {
	if (_osc3sim->waveform == 0x40) {
		return  simReadPulsedD41B();
	}
	return simReadSawtoothD41B();
}

// ------------------------- public API ----------------------------
uint16_t SID::getBaseAddr() {
	return _addr;
}

// hack
void SID::resetVolumeChangeCount() {
	_volUpdates= 0;
}
uint8_t SID::getNumberOfVolumeChanges() {
	return _volUpdates;
}
void SID::disableVolumeChangeNMI(uint8_t mode) {
	// test cases: Ferrari_Formula_One resets volume from main before starting NMI digis..
	// test cases: Great_Giana_Sisters activates "filter" from NMI (without which the melody stays silent)
	_nmiVolChangeDisabled |= mode; // once NMI mode is active it cannot be undone (see Ferrari_Formula_One - where IRQ sets D418 only sometimes);
	
	if (_nmiVolChangeDisabled) {
		// tune depends on the settings made by the NMI
		uint8_t v= _filter->getVolume();
				
		// keep the volume part (needed in Ferrari_Formula_One) but use the filter (need in Great_Giana_Sisters)
		v= (v&0xf) | (memReadIO(0xd418) & 0xf0);	// propagate filter setting made in NMI 
		
		_filter->poke(0xd418 & 0x1f, v);
	}
}

void SID::reset(uint16_t addr, uint32_t sampleRate, uint8_t isModel6581) {
	_addr= addr;
	
	resetEngine(sampleRate, isModel6581);
	
	Envelope::resetConfiguration(sampleRate);
}

uint8_t SID::isModel6581() {
	return _sid->isModel6581;
}

uint8_t SID::readMem(uint16_t addr) {
	uint16_t offset= addr-_addr;
	switch (offset) {
	case 0x1c:					
		/* 
		used by Alien.sid to set filter cutoff freq(hi): unfortunately the 
		filter impl seems to be rather shitty.. and if the actual envelopeOutput
		is used, then the filter will almost mute the signal (refers to original TinySid filter)
		*/
		return _env[2]->getOutput()*4/5+20;	// use hack to avoid the worst
	case 0x1b:
		if (useOscPollingHack()) { return simReadD41B(); }					
		return memReadIO(addr);
	}
	return memReadIO(addr);
}

void SID::poke(uint8_t reg, uint8_t val) {
    uint8_t voice=0;
	if (reg < 7) {}
    if ((reg >= 7) && (reg <=13)) {voice=1; reg-=7;}
    if ((reg >= 14) && (reg <=20)) {voice=2; reg-=14;}

	
	// writes that impact the envelope generator
	if ((reg >= 0x4) && (reg <= 0x6)) {
		_env[voice]->poke(reg, val);
	}
	// writes that impact the filter
	if ((reg >= 0x15) && (reg <= 0x18)) {		
		if (!(_nmiVolChangeDisabled && (cpuGetProgramMode() == NMI_OFFSET_MASK) && (reg == 0x18))) {	// ignore NMI digis		
			_filter->poke(reg, val);
		}
	}

    switch (reg) {		
        case 0x0: { // Set frequency: Low byte
			_sid->voices[voice].freq = (_sid->voices[voice].freq&0xff00) | val;
            break;
        }
        case 0x1: { // Set frequency: High byte
            _sid->voices[voice].freq = (_sid->voices[voice].freq&0xff) | (val<<8);
            break;
        }
        case 0x2: { // Set pulse width: Low byte
            _sid->voices[voice].pulse = (_sid->voices[voice].pulse&0x0f00) | val;
            break;
        }
        case 0x3: { // Set pulse width: High byte
            _sid->voices[voice].pulse = (_sid->voices[voice].pulse&0xff) | ((val & 0xf)<<8);
            break;
        }
        case 0x4: {
			simStartOscillatorVoice3(voice, val);
			
			_sid->voices[voice].wave = val;			
            break;
        }
		case 0x18: {
			// multi purpose detector
			_volUpdates++;
			
			// hack: FutureComposer v1.0 in some cases (see Luca's songs) seems to perform 2x
			// updates of filter (etc) from same IRQ (seems to be used for the hi-hat) and even 
			// with respective handling the songs sound terrible (probably some flaw of the 
			// filter impl). Disable filter in respective scenarios:
			if ((_volUpdates > 1) && (!_nmiVolChangeDisabled) && ((val>>4) != 0x0)) {				
				// trying to avoid false positives (some digis may still get in here.. but that doesn't hurt)
				
				// this hack might easily conflict with songs like Neverending_Story!
				
				// the voice3enable flag AND the volume here lead to massive up/downs. it seems				
				// there is no point to keep anything from the original settings.. everything 
				// sounds worse than this hard coded setting..
				if(!_allowedOnce) {
					_allowedOnce= 1;	// some songs come here during init
				} else {
					_filter->poke(reg, 0b01111111 );	// FIXME retry if filter impl should ever be changed
				} 
			}
			break;
		}
    }
    return;
}

void SID::writeMem(uint16_t addr, uint8_t value) {
	if (!digiDetectSample(addr, value)) {
		poke(addr&0x1f, value);							
//		memWriteIO((addr&0xfc1f), value);		// XXX check for other SID locations	
		memWriteIO(addr, value);	// forget the mirroring stuff.. just messes with additional SIDs	
	}
}

void SID::synthRender(int16_t *buffer, uint32_t len, int16_t **synthTraceBufs, double scale, uint8_t doClear) {	
	syncRegisterCache();
    
	double cutoff, resonance;	// calc once here as an optimization
	_filter->setupFilterInput(&cutoff, &resonance);

	// now render the buffer
	for (uint32_t bp=0; bp<len; bp++) {		
		advanceOscillators();

		int32_t outo= 0, outf= 0;	// outf and outo here end up with the sum of 3 voices..
		
		// generate the two output signals (filtered / non-filtered)
		for (uint8_t voice=0; voice<3; voice++) {						
			uint16_t outv = createWaveOutput(voice);

			_env[voice]->updateEnvelope();
			
			// envelopeOutput has 8-bit and and outv 16	(Hermit's impl based on 16-bit wave output)		
			// => scale back to signed 16bit
			int32_t voiceOut= scale*_env[voice]->getOutput()/0xff*(((int32_t)outv)-0x8000) ;	
		
			// now route the voice output to either the non-filtered or the
			// filtered channel (with disabled filter outo is used)	
			_filter->routeSignal(&voiceOut, &outf, &outo, voice, &(_sid->voices[voice].notMuted));

			// trace output (always make it 16-bit)		
			if (synthTraceBufs) {
				int16_t *voiceTraceBuffer= synthTraceBufs[voice];
				*(voiceTraceBuffer+bp)= (int16_t)(voiceOut);			
			}
		}
		int32_t finalSample= _filter->getOutput(&outf, &outo, cutoff, resonance);		
		finalSample=  digiGenPsidSample(finalSample);		// recorded PSID digis are merged in directly 
		
		if (!doClear) finalSample+= *(buffer+bp);
		
		// clipping (filter, multi-SID as well as PSID digi may bring output over the edge)
		const int32_t clipValue = 32767;
		if ( finalSample < -clipValue ) {
			finalSample = -clipValue;
		} else if ( finalSample > clipValue ) {
			finalSample = clipValue;
		}				
		*(buffer+bp)= (int16_t)finalSample;
    }
	
	// temp variables used during ADSR-bug detection
	for (int i= 0; i<3; i++) { _env[i]->snapshotLFSR();}
}

uint8_t SID::getWave(uint8_t voice) {
	return _sid->voices[voice].wave;
}

uint8_t SID::getAD(uint8_t voice) {
	return _env[voice]->getAD();
}
uint8_t SID::getSR(uint8_t voice) {
	return _env[voice]->getSR();
}

void SID::filterSamples(uint8_t *digiBuffer, uint32_t len, int8_t voice) {
	_filter->filterSamples(digiBuffer, len, voice);
}

const uint8_t MAX_SID_CHIPS= 3;
static uint8_t _usedSIDs= 0; 
static SID _sids[MAX_SID_CHIPS];	// allocate the maximum.. some may then just remain unused..

/**
* Use a simple map to later find which IO access matches which SID (saves annoying if/elses later):
*/
const int MEM_MAP_SIZE = 0xbff;
static uint8_t _mem2sid[MEM_MAP_SIZE];	// maps memory area d400-dfff to available SIDs 

// who knows what people might use to wire up the output signals of their
// multi-SID configurations... it probably depends on the song what might be "good" 
// settings here.. alternatively I could just let the clipping do its work (without any 
// rescaling). but for now I'll use Hermit's settings - this will make for a better user 
// experience in DeepSID when switching players..

static double _volMap[]= { 1.0f, 0.6f, 0.4f };	


/******************************* old C interface to the remaining emulation code ************************************/

	// APIs exclusively used for digis, i.e. always use the "built-in" standard SID chip
extern "C" void sidPoke(uint8_t reg, uint8_t val) {
	_sids[0].poke(reg, val);
}
extern "C" uint8_t sidGetWave(uint8_t voice) {
	return _sids[0].getWave(voice);
}
extern "C" uint8_t sidGetAD(uint8_t voice) {
	return _sids[0].getAD(voice);	
}
extern "C" uint8_t sidGetSR(uint8_t voice) {
	return _sids[0].getSR(voice);	
}
extern "C" uint16_t sidGetFreq(uint8_t voice) {
	return _sids[0]._sid->voices[voice].freq;
}
extern "C" uint16_t sidGetPulse(uint8_t voice) {
	return _sids[0]._sid->voices[voice].pulse;
}
extern "C" void sidFilterSamples (uint8_t *digiBuffer, uint32_t len, int8_t voice) {
	_sids[0].filterSamples(digiBuffer, len, voice);
}

	// the sample frequency is the same for all SIDs
extern "C" uint32_t sidGetSampleFreq() {
	return _sids[0]._sid->sampleRate;
}

extern "C" void sidResetVolumeChangeCount() {
	 _sids[0].resetVolumeChangeCount();
}
extern "C" uint8_t sidGetNumberOfVolumeChanges() {
	return _sids[0].getNumberOfVolumeChanges();
}
extern "C" void sidDisableVolumeChangeNMI(uint8_t mode) {
	_sids[0].disableVolumeChangeNMI(mode);
}

extern "C" void sidSetMute(uint8_t voice, uint8_t value) {
	// FIXME currently not implemented for more than 1 SID	
	_sids[0]._sid->voices[voice%3].notMuted= !value;
}

extern "C" void sidSynthRender(int16_t *buffer, uint32_t len, int16_t **synthTraceBufs) {	

	double scale= _volMap[_usedSIDs-1];
	
	for (uint8_t i= 0; i<_usedSIDs; i++) {
		SID &sid= _sids[i];			
		_sids[i].synthRender(buffer, len, synthTraceBufs, scale, !i);
	}
}

extern "C" void sidReset(uint32_t sampleRate, uint16_t *sidAddrs, uint8_t *sidIs6581, uint8_t compatibility, uint8_t resetVol) {
	_usedSIDs= 0;
	memset(_mem2sid, 0, MEM_MAP_SIZE); // default is SID #0

	// setup the configured SID chips & make map where to find them
	for (uint8_t i= 0; i<MAX_SID_CHIPS; i++) {
		if (sidAddrs[i]) {
			SID &sid= _sids[_usedSIDs];			
			sid.reset(sidAddrs[i], sampleRate, sidIs6581[i]);
			
			if (resetVol) {
				memWriteIO(sid.getBaseAddr()+0x18, 0xf);		// turn on full volume	
				sid.poke(0x18, 0xf);  	
			}
			
			if (i) {	// 1st entry is always the regular default SID
				memset((void*)(_mem2sid+sidAddrs[i]-0xd400), _usedSIDs, 0x1f);
			}
			_usedSIDs++;
		}
	}
	// digis are rarely used in multi-SID configurations (Mahoney did it but then that 
	// song crashes the emu anyways) .. only support digi for 1st SID:
	digiReset(compatibility, sidIs6581[0]);
}

extern "C" uint8_t sidReadMem(uint16_t addr) {
	uint8_t sidId= _mem2sid[addr-0xd400];
	return _sids[sidId].readMem(addr);
}
extern "C" void sidWriteMem(uint16_t addr, uint8_t value) {		// used by memory.c .. for memSet into d400ff
	uint8_t sidId= _mem2sid[addr-0xd400];
	_sids[sidId].writeMem(addr, value);
}


