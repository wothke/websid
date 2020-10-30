/*
* Handles the wave output of a single voice of the C64's "MOS Technology SID" (see 6581, 8580 or 6582).
*
* This includes handling of the involved oscillators and the actual output generation 
* based on the selected waveform(s) and operating mode. (The resulting wave-output is later - not 
* here - scaled using the output of the envelope generator and may be optionally processed via the SIDs
* internal filter.)
*
* WebSid (c) 2020 Jürgen Wothke
* version 0.93
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "sid.h"

#include "wavegenerator.h"


// waveform control register flags
	// other flags
#define GATE_BITMASK 	0x01
#define SYNC_BITMASK 	0x02
#define RING_BITMASK 	0x04
#define TEST_BITMASK 	0x08
	// waveform selection
#define TRI_BITMASK 	0x10
#define SAW_BITMASK 	0x20
#define PULSE_BITMASK 	0x40
#define NOISE_BITMASK 	0x80

#define NOISE_TRIGGER 0x80000		// threshold that triggers shift of the noise register
#define NOISE_RESET 0x7ffffc		// which one is correct? 0x7ffff8?

const double SCALE_12_16 = ((double)0xffff) / 0xfff;

/**
* This utility class keeps the precalculated "combined waveform" lookup tables.
*
* There is no point keeping these in each SID instance (since they are the same anyway)
*/
class WaveformTables {
public:
	WaveformTables();
private:
	static void createCombinedWF(double* wfarray, double bitmul, double bitstrength, double threshold);
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

void WaveformTables::createCombinedWF(double* wfarray, double bitmul, double bitstrength, double threshold) {
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

static WaveformTables _wave_table;		// only need one instance of this


// ----------------------- utils -----------------------------

#define PREV_IDX(voice_idx) \
	(voice_idx ? voice_idx - 1 : 2)

#define NEXT_IDX(voice_idx) \
	((voice_idx == 2) ? 0 : voice_idx + 1)

// Get the bit from an uint32_t at a specified position
#define GET_BIT(val, b) \
	((uint8_t) ((val >> b) & 1))

// ------------------------ WaveGenerator impl ----------------------------------------

WaveGenerator::WaveGenerator(SID* sid, uint8_t voice_idx) {
	_sid = sid;
	_voice_idx = voice_idx;

	reset();
}

void WaveGenerator::reset() {
    _counter = _freq = _msb_rising = _ctrl = _test_bit = _sync_bit = _ring_bit = 0;
	_pulse_width = _pulse_out = _freq_pulse_base = 0;
	_freq_pulse_step = 0;
	_noise_oversample = _noisepos = _noiseout = 0;
    _freq_saw_step = _prev_wav_data = 0;
	
#ifdef PSID_DEBUG_ADSR
	setMute(i != PSID_DEBUG_VOICE);
#else
	setMute(0);
#endif
	
	_prev_wave_form_out = 0x7fff;	// use center to avoid distortions through envelope scaling
	_noise_LFSR = NOISE_RESET;
}

void WaveGenerator::setMute(uint8_t is_muted) {
	_is_muted = is_muted;
}

uint8_t	WaveGenerator::isMuted() {
	return _is_muted;
}

void WaveGenerator::setWave(uint8_t val) {
	_ctrl = val;
	
	// performance optimization: read much more often then written
	_test_bit = val & TEST_BITMASK;
	_sync_bit = val & SYNC_BITMASK;
	_ring_bit = val & RING_BITMASK;
}

void WaveGenerator::setPulseWidthLow(uint8_t val) {
	_pulse_width = (_pulse_width & 0x0f00) | val;
	
	// 16 MSB pulse needed (input is 12-bit)
	_pulse_out = (uint32_t)(_pulse_width * SCALE_12_16);
}

void WaveGenerator::setPulseWidthHigh(uint8_t val) {
	_pulse_width = (_pulse_width & 0xff) | ((val & 0xf) << 8);

	// 16 MSB pulse needed (input is 12-bit)
	_pulse_out = (uint32_t)(_pulse_width * SCALE_12_16);
}

void WaveGenerator::updateFreqCache(double cycles_per_sample) {
	double freq_inc_sample = cycles_per_sample * _freq;	// per 1-sample interval (e.g. ~22 cycles)

	// oversample high freq noise to avoid excessive loudness (testcase: Empty.sid)
	// at ~22 cycles per sample, frequencies above 0xBA2E should trigger noise recalc
	// 2x within same sample and with 0xFFFF it should be almost 3x..
	_noise_oversample = (uint8_t)round(freq_inc_sample / NOISE_TRIGGER);
	if (!_noise_oversample) _noise_oversample = 1;
	
	// optimization for Hermit's waveform generation:
	_freq_saw_step = freq_inc_sample / 0x1200000;			// for SAW
	_freq_pulse_base = ((uint32_t)freq_inc_sample) >> 9;	// for PULSE: 15 MSB needed
	
	// testcase: Dirty_64, Ice_Guys
	_freq_pulse_step = 256 / (((uint32_t)freq_inc_sample) >> 16);
}

void WaveGenerator::setFreqLow(uint8_t val, double cycles_per_sample) {
	_freq = (_freq & 0xff00) | val;
	updateFreqCache(cycles_per_sample);	// perf opt
}

void WaveGenerator::setFreqHigh(uint8_t val, double cycles_per_sample) {
	_freq = (_freq & 0xff) | (val << 8);
	updateFreqCache(cycles_per_sample);	// perf opt
}

uint8_t WaveGenerator::getWave() {
	return _ctrl;
}
uint16_t WaveGenerator::getFreq() {
	return _freq;
}
uint16_t WaveGenerator::getPulse() {
	return _pulse_width;
}

void WaveGenerator::clockOscillator() {
	// note: TEST (Bit 3): The TEST bit, when set to one, resets and locks
	// oscillator at zero until the TEST bit is cleared. The noise waveform
	// output is also reset and the pulse waveform output is
	// held at a DC level; test bit has no influence on the envelope generator
	// whatsoever!
	
	if (_test_bit) {
		_counter = 0;
		
		// known limitation: reset would correctly occur AFTER a certain delay
		// before which the original _noise_LFSR would just "decay" towards 0..
		_noisepos = 0;
		_noise_LFSR = NOISE_RESET;
	} else {
		uint32_t prev_counter = _counter;
		_counter = (_counter + _freq) & 0xffffff;
		
		// base for hard sync
		_msb_rising = (_counter & 0x800000) > (prev_counter & 0x800000);
	}
}

void WaveGenerator::syncOscillator() {
	// Hard Sync is accomplished by clearing the accumulator of an oscillator
	// based on the accumulator MSB of the previous oscillator.

	// tests for hard sync:  Ben Daglish's Wilderness music from The Last Ninja
	// (https://www.youtube.com/watch?v=AbBENI8sHFE) .. seems to be used on
	// voice 2 early on.. for some reason the instrument that starts on voice 1
	// at about 45 secs (some combined waveform?) does not sound as crisp as it
	// should.. (neither in Hermit's player)
		
	// intro noise in Martin Galway's Roland's Rat Race
	// (https://www.youtube.com/watch?v=Zc91S1lrU1I) music.
	
	// the below logic is from the "previous oscillator" perspective
	
	if (!_freq) return;
	
	if (_msb_rising) {	// perf opt: use nested "if" to avoid unnecessary calcs up front
		WaveGenerator *dest_voice = _sid->getWaveGenerator(NEXT_IDX(_voice_idx));
		uint8_t dest_sync = dest_voice->_sync_bit;	// sync requested?
		
		if (dest_sync && !_sync_bit) {
			// exception: when sync source is itself synced in the same cycle then
			// destination is NOT synced (based on analysis performed by reSID team)
			
			WaveGenerator *src_voice = _sid->getWaveGenerator(PREV_IDX(_voice_idx));
			uint8_t src_msb_rising = src_voice->_msb_rising;
			
			if (!src_msb_rising) {
				dest_voice->_counter = 0;
			}
		}
	}
}

uint32_t WaveGenerator::getRingModCounter() {
	// Bob Yannes: "Ring Modulation was accomplished by substituting
	// the accumulator MSB of an oscillator in the EXOR function of
	// the triangle waveform generator with the accumulator MSB of the
	// previous oscillator. That is why the triangle waveform must be
	// selected to use Ring Modulation."
	
	if (_ring_bit) {
		// wtf does he mean? (1) substitute MSB before using it in some
		// EXOR logic OR (2) substitute it using EXOR?
		
		uint8_t src_voice = PREV_IDX(_voice_idx);
//		return (_counter & 0x7fffff) | (_sid->getWaveGenerator(src_voice)->_counter & 0x800000);// (1)
		return _counter ^ (_sid->getWaveGenerator(src_voice)->_counter & 0x800000);				// (2) judging by the sound of R1D1.sid, this is it..
	} else {
		return _counter;
	}
}

// ------------------------- wave form generation ----------------------------

// Hermit's impl to calculate combined waveforms (check his jsSID-0.9.1-tech_comments
// in commented jsSID.js for background info): I did not thoroughly check how well
// this really works (it works well enough for Kentilla and Clique_Baby (apparently
// this one has to sound as shitty as it does)

uint16_t WaveGenerator::combinedWF(double* wfarray, uint16_t index, uint8_t differ6581) {
	//on 6581 most combined waveforms are essentially halved 8580-like waves
	
	if (differ6581 && _sid->_is_6581) index &= 0x7ff;	// todo: add getter for _sid var or replicate into WaveGenerator
	/* orig
	double combiwf = (wfarray[index] + _prev_wav_data) / 2;
	_prev_wav_data = wfarray[index];
	return (uint16_t)round(combiwf);
	*/
	
	// optimization?
	uint32_t combiwf = ((uint32_t)(wfarray[index] + _prev_wav_data)) >> 1;	// missing rounding might not make much of a difference
	_prev_wav_data = wfarray[index];
	return (uint16_t)combiwf;
}

uint16_t WaveGenerator::createTriangleOutput() {
	uint32_t tmp = getRingModCounter();
    uint32_t wfout = (tmp ^ (tmp & 0x800000 ? 0xffffff : 0)) >> 7;
	return wfout & 0xffff;
}

uint16_t WaveGenerator::createSawOutput() {	// test-case: Alien.sid, Kawasaki_Synthesizer_Demo.sid
	// Hermit's "anti-aliasing"

	double wfout = _counter >> 8;	// top 16-bits
	
	if (_freq_saw_step != 0) {
		wfout += wfout * _freq_saw_step;
		if (wfout > 0xffff) wfout = 0xffff - (wfout - 0x10000) / _freq_saw_step;
	}
	return wfout;
}

void WaveGenerator::calcPulseBase(uint32_t* tmp, uint32_t* pw) {
	// based on Hermit's impl
	(*pw) = _pulse_out;				// 16 bit
	(*tmp) = _freq_pulse_base;		// 15 MSB needed
	
	if ((0 < (*pw)) && ((*pw) < (*tmp))) { (*pw) = (*tmp); }
	(*tmp) ^= 0xffff;
	if ((*pw) > (*tmp)) { (*pw) = (*tmp); }
	(*tmp) = _counter >> 8;				// 16 MSB needed
}

uint16_t WaveGenerator::createPulseOutput(uint32_t tmp, uint32_t pw) {	// elementary pulse
	if (_test_bit) return 0xffff;	// pulse start position
	
//	1) int32_t wfout = ((_counter>>8 >= _pulse_width))? 0 : 0xffff; // plain impl - inverted compared to resid
//	2) int32_t wfout = ((_counter>>8 < _pulse_width))? 0 : 0xffff; // plain impl
	
	// Hermit's "anti-aliasing" pulse
	
	// note: the smaller the step, the slower the phase shift, e.g. ramp-up/-down
	// rather than immediate switch (current setting does not cause much of an
	// effect - use "0.1*" to make it obvious ) larger steps cause "sizzling noise"
		
	// test-case: Touch_Me.sid - for some reason avoiding the "division by zero"
	// actually breaks this song..
	
//	double step = ((double)256.0) / (v->freq_inc_sample / (1<<16));	// Hermit's original shift-impl was prone to division-by-zero
	
	// simple pulse, most often used waveform, make it sound as clean as possible without oversampling
	double lim;
	int32_t wfout;
	if (tmp < pw) {	// rising edge (i.e. 0x0 side)	(in scope this is: TOP)
		lim = (0xffff - pw) * _freq_pulse_step;
		if (lim > 0xffff) { lim = 0xffff; }
		wfout = lim - (pw - tmp) * _freq_pulse_step;
		if (wfout < 0) { wfout = 0; }
	} else { //falling edge (i.e. 0xffff side)		(in scope this is: BOTTOM)
		lim = pw * _freq_pulse_step;
		if (lim > 0xffff) { lim = 0xffff; }
		wfout = (0xffff - tmp) * _freq_pulse_step - lim;
		if (wfout >= 0) { wfout = 0xffff; }
		wfout &= 0xffff;
	}
	return wfout;	// like "plain 2)"; NOTE: Hermit's original may actually be flipped due to filter.. which might cause problems with deliberate clicks.. see Geir Tjelta's feedback
//	return 0xffff - wfout; // like "plain 1)"
}

// combined noise-waveform will feed back into _noise_LFSR shift-register potentially
// clearing bits that are used for the "_noiseout" (no others.. and not setting!)

static const uint32_t COMBINED_NOISE_MASK = ~((1<<22)|(1<<20)|(1<<16)|(1<<13)|(1<<11)|(1<<7)|(1<<4)|(1<<2));

uint16_t WaveGenerator::createNoiseOutput(uint16_t outv, uint8_t is_combined) {
	// "random values are output through the waveform generator according to the
	// frequency setting" (http://www.ffd2.com/fridge/blahtune/SID.primer)
	
	// known limitation of current impl: 1) "_noise_LFSR" shift register is only
	// updated when noise is actually used (correctly it should always be clocked -
	// but given the rather expensive _noiseout calculation it seems acceptable to
	// do both on demand only)
			
	uint32_t p = _counter >> 19;	// check complete "prefix" (not just bit 19) so as not to miss 2x updates..
		
	if (_noisepos != p) {
		_noisepos = p;
		
		// impl consistent with: http://www.sidmusic.org/sid/sidtech5.html
		// doc here is probably wrong: http://www.oxyron.de/html/registers_sid.html
		
		uint32_t noiseout = 0;
		for (uint8_t i= 0; i<_noise_oversample; i++) {
			// clock the noise shift register
			_noise_LFSR = (_noise_LFSR << 1) | (GET_BIT(_noise_LFSR, 22) ^ GET_BIT(_noise_LFSR, 17));
			
			// extract bits used for wave-output
			//                          src-bit-pos - dest-bit-pos
			uint32_t o = ((_noise_LFSR >> (22          - 7) ) & 0x80 ) |
						 ((_noise_LFSR >> (20          - 6) ) & 0x40 ) |
						 ((_noise_LFSR >> (16          - 5) ) & 0x20 ) |
						 ((_noise_LFSR >> (13          - 4) ) & 0x10 ) |
						 ((_noise_LFSR >> (11          - 3) ) & 0x08 ) |
						 ((_noise_LFSR >> (7           - 2) ) & 0x04 ) |
						 ((_noise_LFSR >> (4           - 1) ) & 0x02 ) |
						 ((_noise_LFSR >> (2           - 0) ) & 0x01 );

			noiseout += o;			
						
			if (is_combined) {
				// test-case: Hollywood_Poker_Pro.sid (also Wizax_demo.sid, Billie_Jean.sid)
				
				// The wave-output of the SOASC recording of the above song (at the very
				// beginning) shows a pulse-wave that is overlayed with a combined "triangle-
				// noise" waveform (without the below there would be strong noise that
				// doesn't belong there)
				
				// according to resid team's analysis "waveform bits are and-ed into the
				// shift register via the shift register outputs" and thereby the noise
				// zeros-out after a few cycles"
				
				// (as long as noise is not actually used there does not seem to be a point
				// to calculate the respective feedback loop.. eventhough other combined
				// waveforms could squelch the noise by the same process, requiring a GATE
				// to re-activate it.. have yet to find a song that would use that approach)
				
				o &= outv >> 8;	// result of the combined waveform (here the bits are cleared)
				
				uint32_t feedback = (GET_BIT(o, 7) << 22) |
									(GET_BIT(o, 6) << 20) |
									(GET_BIT(o, 5) << 16) |
									(GET_BIT(o, 4) << 13) |
									(GET_BIT(o, 3) << 11) |
									(GET_BIT(o, 2) << 7) |
									(GET_BIT(o, 1) << 4) |
									(GET_BIT(o, 0) << 2);
				
				_noise_LFSR &= COMBINED_NOISE_MASK | feedback;	// feed back into shift register
			}
		}		
		_noiseout = (noiseout * 0x101) / _noise_oversample;	// scale 8-bit to 16-bit
	}
	return _noiseout;
}

uint16_t WaveGenerator::getOutput() {
	uint16_t outv = 0xffff;

	/* XXX this seems to be redundant - see test in calling method (except for stripped multi-SID impl - which doesn't matter)
	if (!_is_muted) {
	*/
		int8_t combined = 0;	// flags specifically handled "combined waveforms" (not all)
		
		// use special handling for certain combined waveforms
		uint16_t plsout;
		if ((_ctrl & PULSE_BITMASK)) {
			uint32_t tmp, pw;	// 16 bits used
			calcPulseBase(&tmp, &pw);
			
			if (((_ctrl & 0xf0) == PULSE_BITMASK)) {
				// pulse only
				plsout = createPulseOutput(tmp, pw);
			} else {
				// combined waveforms with pulse (all except noise)
				plsout =  ((tmp >= pw) || _test_bit) ? 0xffff : 0; //(this would be enough for simple but aliased-at-high-pitches pulse)
				
				if ((_ctrl & TRI_BITMASK) && ++combined)  {
					if (_ctrl & SAW_BITMASK) {	// PULSE & TRIANGLE & SAW	- like in Lenore.sid
						outv = plsout ? combinedWF(_wave_table.PulseTriSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
					} else {
						// PULSE & TRIANGLE - like in Kentilla, Convincing, Clique_Baby, etc
						// a good test is Last_Ninja:6 voice 1 at 35secs; here Hermit's
						// original PulseSaw settings seem to be lacking: the respective
						// sound has none of the crispness nor volume of the original
						
						tmp = getRingModCounter();
						outv = plsout ? combinedWF(_wave_table.PulseTri_8580, (tmp ^ (tmp & 0x800000 ? 0xffffff : 0)) >> 11, 0) : 0;	// either on or off						
					}
				} else if ((_ctrl & SAW_BITMASK) && ++combined)  {	// PULSE & SAW - like in Defiler.sid, Neverending_Story.sid
					outv = plsout ? combinedWF(_wave_table.PulseSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
				}
			}
		} else if ((_ctrl & TRI_BITMASK) && (_ctrl & SAW_BITMASK) && ++combined) {	// TRIANGLE & SAW - like in Garden_Party.sid
			uint32_t tmp = _counter >> 12;
			outv = combinedWF(_wave_table.TriSaw_8580, tmp, 1);	// tmp 12 MSB
		}
				
		// for the rest mix the oscillators with an AND operation as stated
		// in the SID's reference manual - even if this is absolutely wrong
		if (!combined) {			
			if (_ctrl & TRI_BITMASK)	outv &= createTriangleOutput();			
			if (_ctrl & SAW_BITMASK)	outv &= createSawOutput();
			if (_ctrl & PULSE_BITMASK)	outv &= plsout;
		}
		
		// handle noise last - since it may depend on the effect of the other waveforms
		if (_ctrl & NOISE_BITMASK)   {
			outv &= createNoiseOutput(outv, _ctrl & 0x70);
		}
		
		// emulate waveform 00 floating wave-DAC
		if (_ctrl & 0xf0) {	// TODO: find testcase song where this is relevant..
			_prev_wave_form_out = outv;
		} else {
			// no waveform set
			outv = _prev_wave_form_out;
		}
	/*
	} else {
		outv = 0;	// no signal
	}*/
	return outv;
}
