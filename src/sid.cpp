/*
* This emulates the C64's "MOS Technology SID" (see 6581, 8580 or 6582).
*
* It handles the respective SID voices and filters. Envelope generator with
* ADSR-delay bug support, waveform generation including combined waveforms. 
* Supports all SID features like ring modulation, hard-sync, etc.
*
* Emulation is performed on a system clock cycle-by-cycle basis.
*
* Anything related to the handling of  'digi samples' is kept separately in digi.c
*
*
* Known limitations: 
*
*  - effects of the digital-to-analog conversion (e.g. respective 6581 flaws) are NOT handled
*
* Credits:
* 
*  - TinySid (c) 1999-2012 T. Hinrichs, R. Sinsch (with additional fixes from Markus Gritsch) 
*             originally provided the starting point - though very little of that code remains here
* 
*  - Hermit's jsSID.js provided a variant of "resid filter implementation", an "anti aliasing" 
*             for "pulse" and "saw" waveforms, and a rather neat approach to
*             generate combined waveforms (see http://hermit.sidrip.com/jsSID.html)
*
* Links:
*
*  - http://www.waitingforfriday.com/index.php/Commodore_SID_6581_Datasheet
*  - http://www.sidmusic.org/sid/sidtech2.html
*
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
 
// todo: run performance tests to check how cycle based oversampling fares as compared to 
// current once-per-sample approach

// todo: with the added external filter Hermit's antialiasing might no longer make sense and the 
// respective handling may be the source of high-pitched noise during PWM/FM digis..

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "sid.h"
#include "envelope.h"
#include "filter.h"
#include "digi.h"

extern "C" {
#include "base.h"
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


const uint8_t MAX_SID_CHIPS= MAX_SIDS;
static uint8_t _used_sids= 0;
static SID _sids[MAX_SID_CHIPS];	// allocate the maximum

// globally shared by all SIDs
static double		_cycles_per_sample;	
static uint32_t		_sample_rate;				// target playback sample rate

// ----- keeping private structs hidden here - no point to expose these 
// ----- implementation details in the header file these are the structs from 
// ----- the old C impl - which could eventually be further refactored

struct SidState {
	uint8_t is_6581;
	
	// SID model specific distortions (based on resid's analysis)
	int32_t wf_zero;
	int32_t dac_offset;
		
    struct Voice {
//        uint8_t		wave;			// redundant: see Oscillator
		uint16_t	freq;
        uint16_t	pulse;				// 12-bit "pulse width" from respective SID registers
		uint8_t		enabled;			// player's separate "mute" feature
 
		// add-ons snatched from Hermit's implementation
		double		prev_wav_data;		// combined waveform handling
		uint16_t 	prev_wave_form_out;	// floating DAC handling		
    } voices[3];
};

// internal oscillator def
struct Oscillator {
    double		freq_inc_sample;		// osc increment per sample (for Hermit's anti-aliasing)
    uint32_t	freq_inc_cycle;			// osc increment per cycle

		// optimization: derived from freq_inc_sample
    double		freq_saw_step;
    uint32_t	freq_pulse_base;
	double		freq_pulse_step;

	
    uint32_t	pulse;					// 16-bit "pulse width" replicated from Voice struct (<<4 shifted for convenience)
    uint8_t		wave;
	
    uint32_t	counter;				// 24-bit as in the original
	
	// hard sync handling
    uint32_t	prev_counter;
	uint8_t		msb_rising;
	
    uint32_t	noisepos;
    uint32_t	noiseval;
    uint8_t		noiseout;
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
	static void createCombinedWF(double *wfarray, double bitmul, double bitstrength, double threshold);
public:
	// Hermit's precalculated "combined waveforms" 
	double TriSaw_8580[4096];
	double PulseSaw_8580[4096];
	double PulseTri_8580[4096];			// Hermit's use of PulseSaw_8580 does not convince in Last_Ninja
	double PulseTriSaw_8580[4096];
};

WaveformTables::WaveformTables() {
    createCombinedWF(TriSaw_8580, 		0.8, 2.4, 0.64);
    createCombinedWF(PulseSaw_8580, 	1.4, 1.9, 0.68);
	createCombinedWF(PulseTriSaw_8580, 	0.8, 2.5, 0.64);
	// far from "correct" but at least a bit better than Hermit's use of PulseSaw_8580 (see Last_Ninja)
    createCombinedWF(PulseTri_8580, 	0.8, 1.5, 0.38);	// improved settings are welcome!
}

void WaveformTables::createCombinedWF(double *wfarray, double bitmul, double bitstrength, double threshold) { 
	// Hermit: "I found out how the combined waveform works (neighboring bits affect each other recursively)"
	for (uint16_t i = 0; i < 4096; i++) {
		wfarray[i] = 0; //neighbour-bit strength and DAC MOSFET threshold is approximately set by ears'n'trials
		for (uint8_t j = 0; j < 12; j++) {
			double bitlevel = 0;
			for (uint8_t k = 0; k < 12; k++) {
				bitlevel += (bitmul / pow(bitstrength, abs(k - j))) * (((i >> k) & 1) - 0.5);
			}
			wfarray[i] += (bitlevel >= threshold) ? pow(2.0, (double)j) : 0;
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
	
	_sid= (struct SidState*) malloc(sizeof(struct SidState));

	_osc[0]= (struct Oscillator*) malloc(sizeof(struct Oscillator));
	_osc[1]= (struct Oscillator*) malloc(sizeof(struct Oscillator));
	_osc[2]= (struct Oscillator*) malloc(sizeof(struct Oscillator));
		
	_env[0]= new Envelope(this, 0);
	_env[1]= new Envelope(this, 1);
	_env[2]= new Envelope(this, 2);
		
	_filter= new Filter(this);
	_digi= new DigiDetector(this);
}


// ------------------------- convenience accessors ----------------------------
#define isTestBit(voice) _osc[voice]->wave & TEST_BITMASK

static uint8_t getPreviousVoice(uint8_t voice) {
	return voice ? voice-1 : 2;
}
static uint8_t getNextVoice(uint8_t voice) {
	return (voice == 2) ? 0 : voice+1;
}

uint32_t SID::getRingModCounter(uint8_t voice) {
	// Bob Yannes: "Ring Modulation was accomplished by substituting the accumulator MSB of an oscillator 
	// in the EXOR function of the triangle waveform generator with the accumulator MSB of the 
	// previous oscillator. That is why the triangle waveform must be selected to use Ring Modulation."
	
	if (_osc[voice]->wave & RING_BITMASK) {
		// wtf does he mean? (1) substitute MSB before using it in some EXOR logic OR (2) substitute it 
		// using EXOR?
		
		uint8_t src_voice = getPreviousVoice(voice);  
//		return (_osc[voice]->counter & 0x7fffff) | (_osc[src_voice]->counter & 0x800000);	// (1)	
		return _osc[voice]->counter ^ (_osc[src_voice]->counter & 0x800000);				// (2) judging by the sound of R1D1.sid, this is it..
	} else {
		return _osc[voice]->counter;		
	}
}
void SID::resetEngine(uint32_t sample_rate, uint8_t is_6581) {	
	// note: structs are NOT packed and contain additional padding..
	
	// reset _sid
	memset((uint8_t*)_sid, 0, sizeof(struct SidState));

	resetModel(is_6581);
	
	_sample_rate = sample_rate;
	_cycles_per_sample = ((double)envClockRate()) / sample_rate;	// corresponds to Hermit's clk_ratio

	for (uint8_t i=0; i<3; i++) {
//		memset((uint8_t*)&_sid->voices[i], 0, sizeof(struct SidState::Voice));	// already included in _sid		
		_sid->voices[i].enabled= 1;
		_sid->voices[i].prev_wave_form_out= 0x7fff;	// use center to avoid distortions through envelope scaling
	}
	
	// reset _osc
	for (uint8_t i=0; i<3; i++) {
		memset((uint8_t*)_osc[i], 0, sizeof(struct Oscillator));
		// note: by default the rest of _sid, _osc & _filter above is set to 0
		_osc[i]->noiseval = 0x7ffff8;
	}
	
	// reset envelope generator
	for (uint8_t i=0;i<3;i++) {
		_env[i]->reset();
	}

	// reset filter
	_filter->reset(sample_rate);
	
// very bad idea! test-case: Banditti_2SID.sid
//	for (uint8_t i= 0; i<0x17; i++) poke(i, 0xff);	// this seems to be what ACID64 uses
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
	
	if (!_osc[voice]->freq_inc_cycle) return;
	
	uint8_t msb_rising= _osc[voice]->msb_rising;				// base trigger condition
	
	uint8_t dest_voice = getNextVoice(voice);  
	uint8_t dest_sync= _osc[dest_voice]->wave & SYNC_BITMASK;	// sync requested?

	// exception: when sync source is itself synced in the same cycle then destination 
	// is NOT synced (based on analysis performed by reSID team)
	
	uint8_t src_sync= _osc[voice]->wave & SYNC_BITMASK;		// for special case handling 
	uint8_t src_msb_rising= _osc[getPreviousVoice(voice)]->msb_rising;

	if (msb_rising && dest_sync && !(src_sync && src_msb_rising)) {	  
		_osc[dest_voice]->counter = 0;
	}
}

void SID::clockOscillators() {
	// forward oscillators one CYCLE (required to properly time HARD SYNC)
	for (uint8_t voice=0; voice<3; voice++) {
		_osc[voice]->prev_counter= _osc[voice]->counter;
		
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
			_osc[voice]->counter = (_osc[voice]->counter + _osc[voice]->freq_inc_cycle) & 0xffffff;				
		}

		// base for hard sync
		_osc[voice]->msb_rising = (_osc[voice]->counter & 0x800000) > (_osc[voice]->prev_counter & 0x800000);
	}
	
	// handle oscillator HARD SYNC (quality wise it isn't worth the trouble to use this correct impl..)
	for (uint8_t voice=0; voice<3; voice++) {
		syncOscillator(voice);
	}	
}

double SID::getCyclesPerSample() {
	return _cycles_per_sample; 
}

// ------------------------- wave form generation ----------------------------

// Hermit's impl to calculate combined waveforms (check his jsSID-0.9.1-tech_comments in 
// commented jsSID.js for background info): I did not thoroughly check how well this really 
// works (it works well enough for Kentilla and Clique_Baby (apparently this one has to sound 
// as shitty as it does)

uint16_t SID::combinedWF(uint8_t channel, double *wfarray, uint16_t index, uint8_t differ6581) { 
	//on 6581 most combined waveforms are essentially halved 8580-like waves
	
	if (differ6581 && _sid->is_6581) index &= 0x7ff;
	/* orig
	double combiwf = (wfarray[index] + _sid->voices[channel].prev_wav_data) / 2;
	_sid->voices[channel].prev_wav_data = wfarray[index];	
	return (uint16_t)round(combiwf);
	*/
	
	// optimization?
	uint32_t combiwf = ((uint32_t)(wfarray[index] + _sid->voices[channel].prev_wav_data)) >> 1;	// missing rounding might not make much of a difference
	_sid->voices[channel].prev_wav_data = wfarray[index];
	return (uint16_t)combiwf;
	
}

uint16_t SID::createTriangleOutput(uint8_t voice) {
	uint32_t tmp = getRingModCounter(voice);
    uint32_t wfout = (tmp ^ (tmp & 0x800000 ? 0xffffff : 0)) >> 7;
	return wfout & 0xffff;
}

uint16_t SID::createSawOutput(uint8_t voice) {	// test-case: Alien.sid, Kawasaki_Synthesizer_Demo.sid
	// Hermit's "anti-aliasing"

	double wfout = _osc[voice]->counter >> 8;	// top 16-bits
	const double step = _osc[voice]->freq_saw_step;
	
	if (step != 0) {
		wfout += wfout * step;
		if (wfout > 0xffff) wfout = 0xffff - (wfout - 0x10000) / step;
	}
	return wfout;
}

void SID::calcPulseBase(uint8_t voice, uint32_t *tmp, uint32_t *pw) {
	// based on Hermit's impl
	(*pw) = _osc[voice]->pulse;								// 16 bit
	(*tmp) = _osc[voice]->freq_pulse_base;					// 15 MSB needed
	
	if ((0 < (*pw)) && ((*pw) < (*tmp))) { (*pw) = (*tmp); }
	(*tmp) ^= 0xffff;
	if ((*pw) > (*tmp)) { (*pw) = (*tmp); }
	(*tmp) = _osc[voice]->counter >> 8;						// 16 MSB needed
}

uint16_t SID::createPulseOutput(uint8_t voice, uint32_t tmp, uint32_t pw) {	// elementary pulse
	if (isTestBit(voice)) return 0xffff;	// pulse start position: this is the output value PEEKed in this scenario 
	
//	1) int32_t wfout = ((_osc[voice]->counter>>8 >= _osc[voice]->pulse))? 0 : 0xffff; // plain impl - inverted compared to resid
//	2) int32_t wfout = ((_osc[voice]->counter>>8 < _osc[voice]->pulse))? 0 : 0xffff; // plain impl
	
	// Hermit's "anti-aliasing" pulse
	
	// note: the smaller the step, the slower the phase shift, e.g. ramp-up/-down rather than
	// immediate switch (current setting does not cause much of an effect - use "0.1*" to make it obvious )
	// larger steps cause "sizzling noise"
		
	// test-case: Touch_Me.sid - for some reason avoiding the "division by zero" actually breaks this song..
	
//	double step= ((double)256.0) / (_osc[voice]->freq_inc_sample / (1<<16));	// Hermit's original shift-impl was prone to division-by-zero
    const double step = _osc[voice]->freq_pulse_step; 	// simple pulse, most often used waveform, make it sound as clean as possible without oversampling
	
	double lim;
	int32_t wfout;
	if (tmp < pw) {	// rising edge (i.e. 0x0 side)	(in scope this is: TOP)
		lim = (0xffff - pw) * step;
		if (lim > 0xffff) { lim = 0xffff; }
		wfout = lim - (pw - tmp) * step;
		if (wfout < 0) { wfout = 0; }
	} else { //falling edge (i.e. 0xffff side)		(in scope this is: BOTTOM)
		lim = pw * step;
		if (lim > 0xffff) { lim = 0xffff; }
		wfout = (0xffff - tmp) * step - lim;
		if (wfout >= 0) { wfout = 0xffff; }
		wfout &= 0xffff;
	}
	return wfout;	// like "plain 2)"; NOTE: Hermit's original may actually be flipped due to filter.. which might cause problems with deliberate clicks.. see Geir Tjelta's feedback
//	return 0xffff - wfout; // like "plain 1)"
}

// combined noise-waveform will feed back into noiseval shift-register potentially clearing
// bits that are used for the "noiseout" (no others.. and not setting!)

static const uint32_t COMBINED_NOISE_MASK = ~((1<<22)|(1<<20)|(1<<16)|(1<<13)|(1<<11)|(1<<7)|(1<<4)|(1<<2));

uint16_t SID::createNoiseOutput(uint8_t voice) {
	// generate noise waveform (FIXME flaw: "noiseval" shift register is only updated when noise 
	// is actually used..)
	
	// "random values are output through the waveform generator according to the 
	// frequency setting" (http://www.ffd2.com/fridge/blahtune/SID.primer)
	
	// test-case for calibration: see Hermit's noisewfsweep.sid
	uint32_t p= _osc[voice]->counter>>20;		//  top 4-bit seem to be about right.. (didn't find specific specs unfortunately..)
	if (_osc[voice]->noisepos != p) {
		_osc[voice]->noisepos = p;
		
		// impl consistent with: http://www.sidmusic.org/sid/sidtech5.html
		// doc here is probably wrong: http://www.oxyron.de/html/registers_sid.html
		
		// performance: compared to rand() this impl is about 2x slower! 
	/*	_osc[voice]->noiseout = 
				(getBit(_osc[voice]->noiseval,22) << 7) |	// FIXME: the extra back shifts could be eliminated 
				(getBit(_osc[voice]->noiseval,20) << 6) |
				(getBit(_osc[voice]->noiseval,16) << 5) |
				(getBit(_osc[voice]->noiseval,13) << 4) |
				(getBit(_osc[voice]->noiseval,11) << 3) |
				(getBit(_osc[voice]->noiseval, 7) << 2) |
				(getBit(_osc[voice]->noiseval, 4) << 1) |
				(getBit(_osc[voice]->noiseval, 2) << 0);
		*/		
		// same as above but without unnecessary shifting
		uint32_t * n= &(_osc[voice]->noiseval);
		_osc[voice]->noiseout = (((*n) >> (22-7) ) & 0x80 ) |
								(((*n) >> (20-6) ) & 0x40 ) |
								(((*n) >> (16-5) ) & 0x20 ) |
								(((*n) >> (13-4) ) & 0x10 ) |
								(((*n) >> (11-3) ) & 0x08 ) |
								(((*n) >> (7-2) ) & 0x04 ) |
								(((*n) >> (4-1) ) & 0x02 ) |
								(((*n) >> (2) ) & 0x01 );

		_osc[voice]->noiseval = (_osc[voice]->noiseval << 1) |
				(getBit(_osc[voice]->noiseval,22) ^ getBit(_osc[voice]->noiseval,17));	
		
	}
	return ((uint16_t)_osc[voice]->noiseout) << 8;
}

uint16_t SID::createWaveOutput(int8_t voice) {
	uint16_t outv= 0xffff;
	const uint8_t ctrl= _osc[voice]->wave;
		
	if (_sid->voices[voice].enabled) {
		int8_t combined= 0;	// flags some specifically handled "combined waveforms" (not all)
		
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
				plsout=  ((tmp >= pw) || isTestBit(voice)) ? 0xffff : 0; //(this would be enough for simple but aliased-at-high-pitches pulse)

				if ((ctrl & TRI_BITMASK) && ++combined)  {
					if (ctrl & SAW_BITMASK) {	// PULSE & TRIANGLE & SAW	- like in Lenore.sid
						outv = plsout ? combinedWF(voice, _wave.PulseTriSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
					} else { // PULSE & TRIANGLE - like in Kentilla, Convincing, Clique_Baby, etc
						// a good test is Last_Ninja:6 voice 1 at 35secs; here Hermit's original PulseSaw settings seem to
						// be lacking: the respective sound has none of the crispness nor volume of the original
						
						tmp = getRingModCounter(voice); 
						outv = plsout ? combinedWF(voice, _wave.PulseTri_8580, (tmp ^ (tmp & 0x800000 ? 0xffffff : 0)) >> 11, 0) : 0;	// either on or off						
					}				
				} else if ((ctrl & SAW_BITMASK) && ++combined)  {	// PULSE & SAW - like in Defiler.sid, Neverending_Story.sid
					outv = plsout ? combinedWF(voice, _wave.PulseSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
				}
			}
		} else if ((ctrl & TRI_BITMASK) && (ctrl & SAW_BITMASK) && ++combined) {		// TRIANGLE & SAW - like in Garden_Party.sid
			uint32_t tmp = _osc[voice]->counter >> 12;
			outv = combinedWF(voice, _wave.TriSaw_8580, tmp, 1);	// tmp 12 MSB
		} 
		
		if (ctrl & NOISE_BITMASK)   {
			if (ctrl & 0x70) {	// combined waveform with noise
				// test-case: Hollywood_Poker_Pro.sid (also Wizax_demo.sid, Billie_Jean.sid)
				
				// The wave-output of the SOASC recording of the above song (at the very beginning) shows a pulse-wave that is 
				// overlaid with a combined "triangle-noise" waveform (without the below there would be strong noise
				// that doesn't belong there)
				
				// according to resid team's analysis "waveform bits are and-ed into the shift register via the 
				// shift register outputs" and thereby the noise zeros-out after a few cycles"
				
				// (as long as noise is not actually used there does not seem to be a point to
				// calculate the respective feedback loop.. eventhough other combined waveforms could 
				// squelch the noise by the same process, requiring a GATE to re-activate it..
				// have yet to find a song that would use that approach)
				outv &= createNoiseOutput(voice);
				
				uint32_t feedback=	(getBit(outv,15) << 22) |
									(getBit(outv,14) << 20) |
									(getBit(outv,13) << 16) |
									(getBit(outv,12) << 13) |
									(getBit(outv,11) << 11) |
									(getBit(outv,10) << 7) |
									(getBit(outv,9) << 4) |
									(getBit(outv,8) << 2);
			
				_osc[voice]->noiseval &= COMBINED_NOISE_MASK | feedback;	// feed back into shift register
			} else {
				outv &= createNoiseOutput(voice);
			}
		} else if (!combined) {
			/* for the rest mix the oscillators with an AND operation as stated in
				the SID's reference manual - even if this is absolutely wrong. */
		
			if (ctrl & TRI_BITMASK)  outv &= createTriangleOutput(voice);					
			if (ctrl & SAW_BITMASK)  outv &= createSawOutput(voice);
			if (ctrl & PULSE_BITMASK) outv &= plsout;
		}
		
		// emulate waveform 00 floating wave-DAC 
		if (ctrl & 0xf0) {	// TODO: find testcase song where this is relevant.. 
			_sid->voices[voice].prev_wave_form_out= outv;
		} else {
			// no waveform set						
			outv= _sid->voices[voice].prev_wave_form_out;
		}	
	} else {
		outv= 0;	// no signal
	}
	return outv;
}

// ------------------------- public API ----------------------------
uint16_t SID::getBaseAddr() {
	return _addr;
}

void SID::resetModel(uint8_t is_6581) {
	_sid->is_6581= is_6581;	
		
	if (is_6581) {
		_sid->wf_zero= -0x3800;
		_sid->dac_offset= 0x8000*0xff;
		
		_filter->resetInput6581(0);
	} else {
		_sid->wf_zero= -0x8000;
		_sid->dac_offset= -0x1000*0xff;
		
		_filter->resetInput8580();
	}
	_digi->resetModel(is_6581);
}

void SID::reset(uint16_t addr, uint32_t sample_rate, uint8_t is_6581, uint8_t compatibility, uint8_t output_channel) {
	_addr= addr;
	_dest_channel= output_channel;
	
	resetEngine(sample_rate, is_6581);
	
	Envelope::resetConfiguration(sample_rate);
	
	_digi->reset(compatibility, is_6581);
}

uint8_t SID::isModel6581() {
	return _sid->is_6581;
}

uint8_t SID::readMem(uint16_t addr) {
	uint16_t offset= addr-_addr;
	switch (offset) {
	case 0x1b:									// "oscillator"		
//		if(_osc[2]->wave & SAW_BITMASK) 		// obsolete
//			return _osc[2]->counter >> 16;		// for IceGuys.sid
		return createWaveOutput(2) >> 8;
		
	case 0x1c:									// envelope
		return _env[2]->getOutput();
	}
	return memReadIO(addr);
}

void SID::updateFreqCache(uint8_t voice) {
	_osc[voice]->freq_inc_sample= _cycles_per_sample * _sid->voices[voice].freq;	// per 1-sample interval (e.g. ~22 cycles)

	// optimization for Hermit's waveform generation:
	_osc[voice]->freq_saw_step = _osc[voice]->freq_inc_sample / 0x1200000;	// for SAW
	_osc[voice]->freq_pulse_base= ((uint32_t)_osc[voice]->freq_inc_sample) >> 9;	// for PULSE: 15 MSB needed
	_osc[voice]->freq_pulse_step= 256 / (((uint32_t)_osc[voice]->freq_inc_sample) >> 16);
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
    switch (reg) {		
		case 0x15:
		case 0x16:
		case 0x17:
		case 0x18:
			_filter->poke(reg, val);
            break;
	}
	
    switch (reg) {		
        case 0x0: { // Set frequency: Low byte
			_sid->voices[voice].freq = (_sid->voices[voice].freq&0xff00) | val;
			
			// convenience:
			updateFreqCache(voice);
			
			_osc[voice]->freq_inc_cycle= ((uint32_t)_sid->voices[voice].freq);		
            break;
        }
        case 0x1: { // Set frequency: High byte
            _sid->voices[voice].freq = (_sid->voices[voice].freq&0xff) | (val<<8);
			
			// convenience:
			updateFreqCache(voice);
			_osc[voice]->freq_inc_cycle= ((uint32_t)_sid->voices[voice].freq);		
            break;
        }
        case 0x2: { // Set pulse width: Low byte
            _sid->voices[voice].pulse = (_sid->voices[voice].pulse&0x0f00) | val;
			_osc[voice]->pulse= ((uint32_t)_sid->voices[voice].pulse) << 4;	// convenience: 16 MSB pulse needed (input is 12-bit)
            break;
        }
        case 0x3: { // Set pulse width: High byte
            _sid->voices[voice].pulse = (_sid->voices[voice].pulse&0xff) | ((val & 0xf)<<8);
			_osc[voice]->pulse= ((uint32_t)_sid->voices[voice].pulse) << 4;	// convenience: 16 MSB pulse needed (input is 12-bit)
            break;
        }
        case 0x4: {
	//		_sid->voices[voice].wave = val;
			_osc[voice]->wave= val; //_sid->voices[voice].wave;
			break;
		}
    }
    return;
}

void SID::writeMem(uint16_t addr, uint8_t value) {
	_digi->detectSample(addr, value);
	
	// no reason anymore to NOT always write (unlike old/un-synced version)
	
	const uint16_t reg= addr&0x1f;
	
	poke(reg, value);
	memWriteIO(addr, value);

	// some crappy songs like Aliens_Symphony.sid actually use d5xx instead of d4xx for their SID
	// settings.. i.e. they use mirrored addresses that might actually be used by additional SID chips.
	// always map to standard address to ease debug output handling, e.g. visualization in DeepSid
	if (_used_sids == 1) {
		addr= 0xd400 | reg;
		memWriteIO(addr, value);
	}
}

void SID::clock() {
	clockOscillators();		// for all 3 voices
				
	for (uint8_t voice=0; voice<3; voice++) {
		_env[voice]->clockEnvelope();
	}
}

void SID::synthSample(int16_t *buffer, int16_t **synth_trace_bufs, uint32_t offset, double *scale, uint8_t do_clear) {	
	// just try the cycle based logic to be used in cleaned up emu
		
	// generate the two output signals (filtered / non-filtered)
	int32_t outf= 0, outo= 0;	// outf and outo here end up with the sum of 3 voices..
	
	// sim duration of 1 sample (might try higher sampling for better quality - see filter adjustments)
	
	// create output sample based on current SID state
	for (uint8_t voice=0; voice<3; voice++) {
		int32_t outv = createWaveOutput(voice);	// at this point an unsigned 16-bit value (see Hermit's impl)	
												// envelopeOutput has 8-bit

		// the ideal voiceOut would be signed 16-bit - but more bits are actually needed particularily
		// for the massively shifted 6581 signal (needed for D418 digis to work correctly)
		int32_t voiceOut= (*scale) * ( _env[voice]->getOutput() * (outv + _sid->wf_zero) + _sid->dac_offset);
		
		// now route the voice output to either the non-filtered or the
		// filtered channel (with disabled filter outo is used)
		
		_filter->routeSignal(&voiceOut, &outf, &outo, voice, &(_sid->voices[voice].enabled));

		// trace output (always make it 16-bit)
		if (synth_trace_bufs) {
			int16_t *voice_trace_buffer= synth_trace_bufs[voice];

			int32_t o= 0, f= 0;	// isolated from other voices
			uint8_t is_filtered= _filter->routeSignal(&voiceOut, &f, &o, voice, &(_sid->voices[voice].enabled));	// redundant.. see above
			
			*(voice_trace_buffer+offset)= (int16_t)_filter->simOutput(voice, is_filtered, &f, &o);
		}
	}
	
	int32_t digi_out= 0;
	_digi->routeDigiSignal(_filter, &digi_out, &outf, &outo);
	
	if (synth_trace_bufs) {
		int16_t *voice_trace_buffer= synth_trace_bufs[3];	// digi track
		*(voice_trace_buffer+offset)= digi_out;				// save the trouble of filtering
	}
	
	int32_t final_sample= _filter->getOutput(&outf, &outo, _cycles_per_sample);	// note: external filter is included here
	
	final_sample=  _digi->genPsidSample(final_sample);	// recorded PSID digis are merged in directly

	int16_t *dest= buffer+(offset<<1)+_dest_channel; 	// always use interleaved stereo buffer
	
	if (!do_clear) final_sample+= *(dest);

	// clipping (filter, multi-SID as well as PSID digi may bring output over the edge)
	const int32_t clip_value = 32767;
	if ( final_sample < -clip_value ) {
		final_sample = -clip_value;
	} else if ( final_sample > clip_value ) {
		final_sample = clip_value;
	}
	*(dest)= (int16_t)final_sample;
}

// same as above but without digi & no filter for trace buffers - once faster PCs are
// more widely in use, then this optimization may be ditched..
void SID::synthSampleStripped(int16_t *buffer, int16_t **synth_trace_bufs, uint32_t offset, double *scale, uint8_t do_clear) {	
	int32_t outf= 0, outo= 0;	// outf and outo here end up with the sum of 3 voices..
	
	for (uint8_t voice=0; voice<3; voice++) {
		uint16_t outv = createWaveOutput(voice);
		
		int32_t voiceOut= (*scale) * ( _env[voice]->getOutput() * (outv + _sid->wf_zero) + _sid->dac_offset);
//		int32_t voiceOut= (*scale) * _env[voice]->getOutput()*(((int32_t)outv)-0x8000);
		_filter->routeSignal(&voiceOut, &outf, &outo, voice, &(_sid->voices[voice].enabled));
		
		// trace output (always make it 16-bit)
		if (synth_trace_bufs) {
			// performance note: specially for multi-SID (e.g. 8SID) songs use of the filter for trace buffers
			// has a significant runtime cost (e.g. 8*3= 24 additional filter calculations - per sample - instead of just 1).
			// switching to display of non-filtered data may make the difference between "still playing" and
			// "much too slow" -> like on my old machine
			
			int16_t *voice_trace_buffer= synth_trace_bufs[voice];
			*(voice_trace_buffer+offset)= (int16_t)(voiceOut);
		}
	}
	
	int32_t final_sample= _filter->getOutput(&outf, &outo, _cycles_per_sample);
	int16_t *dest= buffer+(offset<<1)+_dest_channel; 	// always use interleaved stereo buffer
	
	if (!do_clear) final_sample+= *(dest);

	// clipping (filter, multi-SID as well as PSID digi may bring output over the edge)
	const int32_t clip_value = 32767;
	if ( final_sample < -clip_value ) {
		final_sample = -clip_value;
	} else if ( final_sample > clip_value ) {
		final_sample = clip_value;
	}
	*(dest)= (int16_t)final_sample;
}

// "friends only" accessors
uint8_t SID::getWave(uint8_t voice) {
	return _osc[voice]->wave;
//	return _sid->voices[voice].wave;
}
uint8_t SID::getAD(uint8_t voice) {
	return _env[voice]->getAD();
}
uint8_t SID::getSR(uint8_t voice) {
	return _env[voice]->getSR();
}
uint16_t SID::getFreq(uint8_t voice) {
	return _sid->voices[voice].freq;
}
uint16_t SID::getPulse(uint8_t voice) {
	return _sid->voices[voice].pulse;
}
uint32_t SID::getSampleFreq() {
	return _sample_rate;
}

DigiType  SID::getDigiType() {
	return _digi->getType();
}
const char *  SID::getDigiTypeDesc() {
	return _digi->getTypeDesc();
}
uint16_t  SID::getDigiRate() {
	return _digi->getRate();
}
		

void SID::setMute(uint8_t voice, uint8_t is_muted) {
	if (voice > 3) voice= 3; 	// no more than 4 voices per SID
	
	if (voice == 3) {
		_digi->setEnabled(!is_muted);
		
	} else {
		_sid->voices[voice].enabled= !is_muted;
	}
}

void SID::resetStatistics() {
	_digi->resetCount();
}


/**
* Use a simple map to later find which IO access matches which SID (saves annoying if/elses later):
*/
const int MEM_MAP_SIZE = 0xbff;
static uint8_t _mem2sid[MEM_MAP_SIZE];	// maps memory area d400-dfff to available SIDs 

void SID::setMute(uint8_t sid_idx, uint8_t voice, uint8_t is_muted) {
	if (sid_idx > 9) sid_idx= 9; 	// no more than 10 sids supported

	_sids[sid_idx].setMute(voice, is_muted);
}

DigiType SID::getGlobalDigiType() {
	return _sids[0].getDigiType();
}
const char* SID::getGlobalDigiTypeDesc() {
	return _sids[0].getDigiTypeDesc();
}

uint16_t SID::getGlobalDigiRate() {
	return _sids[0].getDigiRate();
}

void SID::clockAll() {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid= _sids[i];			
		sid.clock();
	}
}


void SID::synthSample(int16_t *buffer, int16_t **synth_trace_bufs, double* scale, uint32_t offset) {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid= _sids[i];					
		int16_t **sub_buf= !synth_trace_bufs ? 0 : &synth_trace_bufs[i<<2];	// synthSample uses 4 entries..
		sid.synthSample(buffer, sub_buf, offset, scale, !i);
	}
}

void SID::synthSampleStripped(int16_t *buffer, int16_t **synth_trace_bufs, double* scale, uint32_t offset) {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid= _sids[i];
		int16_t **sub_buf= !synth_trace_bufs ? 0 : &synth_trace_bufs[i<<2];	// synthSample uses 4 entries..
		sid.synthSampleStripped(buffer, sub_buf, offset, scale, (i==0) || (i==env2ndOutputChanIdx()));
	}
}

void SID::resetGlobalStatistics() {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid= _sids[i];			
		sid.resetStatistics();
	}
}

void SID::setModels(uint8_t *is_6581) {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid= _sids[i];			
		sid.resetModel(is_6581[i]);
	}
}

uint8_t SID::getNumberUsedChips() {
	return _used_sids;
}

void SID::resetAll(uint32_t sample_rate, uint16_t *addrs, uint8_t *is_6581, uint8_t *output_chan, uint8_t compatibility, uint8_t resetVol) {
	_used_sids= 0;
	memset(_mem2sid, 0, MEM_MAP_SIZE); // default is SID #0

	// determine the number of used SIDs
	for (uint8_t i= 0; i<MAX_SID_CHIPS; i++) {
		if (addrs[i]) {
			_used_sids++;
		}
	}
	// setup the configured SID chips & make map where to find them
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid= _sids[i];
		sid.reset(addrs[i], sample_rate, is_6581[i], compatibility, output_chan[i]);	// stereo only used for my extended sid-file format
		
		if (resetVol) {
			memWriteIO(sid.getBaseAddr()+0x18, 0xf);		// turn on full volume
			sid.poke(0x18, 0xf);
		}
		
		if (i) {	// 1st entry is always the regular default SID
			memset((void*)(_mem2sid+addrs[i]-0xd400), i, 0x1f);
		}
	}
}

// -------------- API used by C code --------------

extern "C" uint8_t sidReadMem(uint16_t addr) {
	uint8_t sid_idx= _mem2sid[addr-0xd400];
	return _sids[sid_idx].readMem(addr);
}
extern "C" void sidWriteMem(uint16_t addr, uint8_t value) {
	uint8_t sid_idx= _mem2sid[addr-0xd400];
	_sids[sid_idx].writeMem(addr, value);
}
