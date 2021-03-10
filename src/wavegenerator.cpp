/*
* Handles the wave output of a single voice of the C64's "MOS Technology SID" (see 6581, 8580 or 6582).
*
* This includes handling of the involved oscillators and the actual output generation
* based on the selected waveform(s) and operating mode. (The resulting wave-output is later - not
* here - scaled using the output of the envelope generator and may be optionally processed via the SIDs
* internal filter.)
*
* Limitation: Updates that are performed within the current sample (e.g. oscillator reset, WF, pulsewidth
* or frequency change) are not handled correctly. The wave output calculation does not currently
* consider the part which correctly should still be based on the previous setting. The flaw is most
* relevant in FM/PWM digis (see Soundcheck.sid) but should not matter much in regular playback.
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

// note: noise is only ever reset via the TEST-bit but on the real HW it would then be
// updated regardless of the selected WF, i.e. quite a bit of updating may
// have occured even BEFORE the NOISE output is activated. However in this impl here,
// respective updates are only performed while NOISE output is active (as a performance 
// optimization) and the output always reflects the same values right from the start of 
// the "random"-sequence. It seems that when starting from the correct reset value
// 0x7ffff8 the output sequence is at a point that does not sound nice when used for "drum
// instruments". The below NOISE_RESET value tries to "fast forward" to some point in the 
// random-number sequence that produces a more pleasing audio result.
#define NOISE_RESET 0x73f5f8

#ifdef USE_HERMIT_ANTIALIAS
const double SCALE_12_16 = ((double)0xffff) / 0xfff;
#endif

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

// Bob Yannes: "Ring Modulation was accomplished by substituting
// the accumulator MSB of an oscillator in the EXOR function of
// the triangle waveform generator with the accumulator MSB of the
// previous oscillator. That is why the triangle waveform must be
// selected to use Ring Modulation."

// what this means is to substitute the MSB of the current accumulator
// by EXORing it with the MSB of the respective previous oscillator:
#define GET_RINGMOD_COUNTER() \
	_ring_bit ? _counter ^ (_sid->getWaveGenerator(PREV_IDX(_voice_idx))->_counter & 0x800000) : _counter;


// ------------------------ WaveGenerator impl ----------------------------------------


// single waveforms
uint16_t WaveGenerator::nullOutput() {
	// emulate waveform 00 floating wave-DAC - TODO: find testcase song where this is relevant..
	return _prev_wave_form_out;
}

uint16_t WaveGenerator::triangleOutput() {
	_prev_wave_form_out = createTriangleOutput();
	return _prev_wave_form_out;
}

uint16_t WaveGenerator::sawOutput() {
	_prev_wave_form_out = createSawOutput();
	return _prev_wave_form_out;
}

uint16_t WaveGenerator::pulseOutput() {
#ifdef USE_HERMIT_ANTIALIAS
	uint32_t tmp, pw;	// 16 bits used
	calcPulseBase(&tmp, &pw);

	_prev_wave_form_out = createPulseOutput(tmp, pw);
#else
	_prev_wave_form_out = createPulseOutput();
#endif
	return _prev_wave_form_out;
}

uint16_t WaveGenerator::noiseOutput() {
	_prev_wave_form_out = createNoiseOutput(0xffff, 0);
	return _prev_wave_form_out;
}

// combined waveforms
uint16_t WaveGenerator::triangleSawOutput() {
	// TRIANGLE & SAW - like in Garden_Party.sid
	_prev_wave_form_out = combinedWF(_wave_table.TriSaw_8580, _counter >> 12, 1);			// 12 MSB needed
	return _prev_wave_form_out;
}

uint16_t WaveGenerator::pulseTriangleOutput() {
	uint16_t plsout;
#ifdef USE_HERMIT_ANTIALIAS
	uint32_t tmp, pw;	// 16 bits used
	calcPulseBase(&tmp, &pw);
	plsout =  ((tmp >= pw) || _test_bit) ? 0xffff : 0; //(this would be enough for simple but aliased-at-high-pitches pulse)
#else
//	plsout = createPulseOutput();	// plain should be good enough
	plsout =  ((_counter >> 12 >= _pulse_width) || _test_bit) ? 0xffff : 0;
#endif

	// PULSE & TRIANGLE - like in Kentilla, Convincing, Clique_Baby, etc
	// a good test is Last_Ninja:6 voice 1 at 35secs; here Hermit's
	// original PulseSaw settings seem to be lacking: the respective
	// sound has none of the crispness nor volume of the original

	uint32_t c = GET_RINGMOD_COUNTER();
	_prev_wave_form_out = plsout ? combinedWF(_wave_table.PulseTri_8580, (c ^ (c & 0x800000 ? 0xffffff : 0)) >> 11, 0) : 0;	// either on or off
	return _prev_wave_form_out;
}

uint16_t WaveGenerator::pulseTriangleSawOutput() {
	// PULSE & TRIANGLE & SAW	- like in Lenore.sid
	uint16_t plsout;
#ifdef USE_HERMIT_ANTIALIAS
	uint32_t tmp, pw;	// 16 bits used
	calcPulseBase(&tmp, &pw);
	plsout =  ((tmp >= pw) || _test_bit) ? 0xffff : 0; //(this would be enough for simple but aliased-at-high-pitches pulse)
	_prev_wave_form_out = plsout ? combinedWF(_wave_table.PulseTriSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
#else
//	plsout = createPulseOutput();	// plain should be good enough
	plsout =  ((_counter >> 12 >= _pulse_width) || _test_bit) ? 0xffff : 0;
	_prev_wave_form_out = plsout ? combinedWF(_wave_table.PulseTriSaw_8580, _counter >> 12, 1) : 0;	// 12 MSB needed
#endif
	return _prev_wave_form_out;
}

uint16_t WaveGenerator::pulseSawOutput() {
	// PULSE & SAW - like in Defiler.sid, Neverending_Story.sid
	uint16_t plsout;
#ifdef USE_HERMIT_ANTIALIAS
	uint32_t tmp, pw;	// 16 bits used
	calcPulseBase(&tmp, &pw);
	plsout =  ((tmp >= pw) || _test_bit) ? 0xffff : 0; //(this would be enough for simple but aliased-at-high-pitches pulse)
	_prev_wave_form_out = plsout ? combinedWF(_wave_table.PulseSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
#else
//	plsout = createPulseOutput();	// plain should be good enough
	plsout =  ((_counter >> 12 >= _pulse_width) || _test_bit) ? 0xffff : 0;
	_prev_wave_form_out = plsout ? combinedWF(_wave_table.PulseSaw_8580, _counter >> 12, 1) : 0;	// 12 MSB needed
#endif
	return _prev_wave_form_out;
}


WaveGenerator::WaveGenerator(SID* sid, uint8_t voice_idx) {
	_sid = sid;
	_voice_idx = voice_idx;

//	reset();
}

void WaveGenerator::reset(double cycles_per_sample) {
	_cycles_per_sample = cycles_per_sample;

    _counter = _freq = _msb_rising = _ctrl = _test_bit = _sync_bit = _ring_bit = 0;
	_pulse_width = _pulse_width12 = 0;

#ifdef USE_HERMIT_ANTIALIAS
	_pulse_out = _freq_pulse_base = _freq_pulse_step = _freq_saw_step = 0;
#else
	_freq_inc_sample_inv = _ffff_freq_inc_sample_inv = _ffff_cycles_per_sample_inv = 0;
	_pulse_width12_neg = _pulse_width12_plus = 0;
#endif

	_noisepos = _noiseout = 0;
	_freq_inc_sample = _prev_wav_data = 0;

#ifdef PSID_DEBUG_ADSR
	setMute(i != PSID_DEBUG_VOICE);
#else
	setMute(0);
#endif

	_prev_wave_form_out = 0x7fff;	// use center to avoid distortions through envelope scaling
	_noise_LFSR = NOISE_RESET;

	getOutput = &WaveGenerator::nullOutput;
}

void WaveGenerator::setMute(uint8_t is_muted) {
	_is_muted = is_muted;
}

uint8_t	WaveGenerator::isMuted() {
	return _is_muted;
}

void WaveGenerator::setWave(uint8_t val) {
	_ctrl = val;

	if ((val & NOISE_BITMASK) && !(_ctrl & NOISE_BITMASK)) {
		// seems resonable in order to avoid excessive noise updates..
		_noisepos = _counter >> 20;
	}	
	
	// performance optimization: read much more often then written
	_test_bit = val & TEST_BITMASK;
	_sync_bit = val & SYNC_BITMASK;
	_ring_bit = val & RING_BITMASK;

	// rather dispatch once here than repeat the checks for each
	// sample.. note: once again the effects of the browser's "random performance behavior"
	// seem to be bigger than the eventual optimization effects achieved here, i.e.
	// the exact same version may run 30% faster/slower between successive runs (due to whatever
	// other shit the browser might be doing in the background - or maybe due to other Win10
	// processes interfering with the measurements..)

	switch (val & 0xf0) {
		case 0:
			getOutput = &WaveGenerator::nullOutput;
			break;
		case TRI_BITMASK:
			getOutput = &WaveGenerator::triangleOutput;
			break;
		case SAW_BITMASK:
			getOutput = &WaveGenerator::sawOutput;
			break;
		case PULSE_BITMASK:
			getOutput = &WaveGenerator::pulseOutput;
			break;
		case NOISE_BITMASK:
			getOutput = &WaveGenerator::noiseOutput;
			break;

		// commonly used combined waveforms
		case TRI_BITMASK|SAW_BITMASK:
			getOutput = &WaveGenerator::triangleSawOutput;
			break;
		case PULSE_BITMASK|TRI_BITMASK:
			getOutput = &WaveGenerator::pulseTriangleOutput;
			break;
		case PULSE_BITMASK|TRI_BITMASK|SAW_BITMASK:
			getOutput = &WaveGenerator::pulseTriangleSawOutput;
			break;
		case PULSE_BITMASK|SAW_BITMASK:
			getOutput = &WaveGenerator::pulseSawOutput;
			break;

		// fallback.. old all-in-one
		default:
			getOutput = &WaveGenerator::exoticCombinedOutput;
			break;
	}
}

void WaveGenerator::setPulseWidthLow(uint8_t val) {
	_pulse_width = (_pulse_width & 0x0f00) | val;

#ifdef USE_HERMIT_ANTIALIAS
	// 16 MSB pulse needed (input is 12-bit)
	_pulse_out = (uint32_t)(_pulse_width * SCALE_12_16);
#else
	updatePulseCache();
#endif
}

void WaveGenerator::setPulseWidthHigh(uint8_t val) {
	_pulse_width = (_pulse_width & 0xff) | ((val & 0xf) << 8);
	_pulse_width12 = ((uint32_t)_pulse_width) << 12;	// for direct comparisons with 24-bit osc accumulator

#ifdef USE_HERMIT_ANTIALIAS
	// 16 MSB pulse needed (input is 12-bit)
	_pulse_out = (uint32_t)(_pulse_width * SCALE_12_16);
#else
	updatePulseCache();
#endif
}

#ifndef USE_HERMIT_ANTIALIAS
void WaveGenerator::updatePulseCache() {
	_pulse_width12_neg = 0xfff000 - _pulse_width12;
	_pulse_width12_plus = _pulse_width12 + _freq_inc_sample; // used to check PW condition for previous sample
}
#endif

void WaveGenerator::updateFreqCache() {
	_freq_inc_sample = _cycles_per_sample * _freq;	// per 1-sample interval (e.g. ~22 cycles)
#ifndef USE_HERMIT_ANTIALIAS
	_freq_inc_sample_inv = 1.0 / _freq_inc_sample; 	// use faster multiplications later
	_ffff_freq_inc_sample_inv = ((double)0xffff) / _freq_inc_sample;
	_ffff_cycles_per_sample_inv = ((double)0xffff) / _cycles_per_sample;

	// optimization for saw
	uint16_t saw_sample_step = ((uint32_t)_freq_inc_sample) >> 8;	// less than 16-bits
	_saw_range = 0xffff - saw_sample_step;
	_saw_base = saw_sample_step + _saw_range;		// includes  +saw_sample_step/2 error (same as in regular signal)
#endif
#ifdef USE_HERMIT_ANTIALIAS
	// optimization for Hermit's waveform generation:
	_freq_saw_step = _freq_inc_sample / 0x1200000;			// for SAW

	_freq_pulse_base = ((uint32_t)_freq_inc_sample) >> 9;	// for PULSE: 15 MSB needed
	_freq_pulse_step = 256 / (((uint32_t)_freq_inc_sample) >> 16);	// testcase: Dirty_64, Ice_Guys
#else
	updatePulseCache();
#endif
}

void WaveGenerator::setFreqLow(uint8_t val) {
	_freq = (_freq & 0xff00) | val;
	updateFreqCache();	// perf opt
}

void WaveGenerator::setFreqHigh(uint8_t val) {
	_freq = (_freq & 0xff) | (val << 8);
	updateFreqCache();	// perf opt
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

		if (dest_sync) {
			// exception: when sync source is itself synced in the same cycle then
			// destination is NOT synced (based on analysis performed by reSID team)

			if (!(_sync_bit && _sid->getWaveGenerator(PREV_IDX(_voice_idx))->_msb_rising)) {
				dest_voice->_counter = 0;
			}
		}
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
	uint32_t tmp = GET_RINGMOD_COUNTER();
    uint32_t wfout = (tmp ^ (tmp & 0x800000 ? 0xffffff : 0)) >> 7;
	return wfout & 0xffff;
}

uint16_t WaveGenerator::createSawOutput() {	// test-case: Super_Huey.sid (2 saw voices), Alien.sid, Kawasaki_Synthesizer_Demo.sid
	// WebSid renders output at the END of a (e.g. ~22 cycles) sample-interval (more correctly the output
	// should actually use the average value between start/end - or actually oversample)

	// the raw output valid at this cycle will be systematically too high most of the time (since it only takes
	// the end-point of a usually rising curve).. since the error is a fixed offset this should not make any audible
	// difference... but due to the fact that the curve's discontinuity (end of tooth) may occur
	// anywhere *within* the sample-interval some kind of anti-aliasing should be added to avoid undesirable
	// aliasing effects at the end of each saw-tooth
	// return _counter >> 8;	// plain saw: top 16-bits (actual saw output valid at this exact clock cycle)


#ifdef USE_HERMIT_ANTIALIAS
	// Hermit's "anti-aliasing" seems to be quite a hack compared to the output of a "correctly averaged" result
	// based on cycle-by-cycle oversampling: Hermit makes the curve steeper than it should be and his anti-aliasing
	// at the discuntinuity is then also quite off (as compared to the oversampled curve).

	double wfout = _counter >> 8;	// top 16-bits (actual saw output valid at this exatc clock cycle)
	wfout += wfout * _freq_saw_step;	// Hermit uses a lookahead scheme! i.e. his output is off by 22 cycles/1 sample..
	if (wfout > 0xffff) {
		wfout = 0xffff - (wfout - 0x10000) / _freq_saw_step;	// with _freq_saw_step=0 it should never get in here
	}
	return wfout;
#else
	// this should also be slightly faster than Hermit's impl
	if (_counter < _freq_inc_sample) {
		// we are at the end of 1st step of a new sawtooth: but this sample still contains a part of the end
		// of the previous sawtooth (i.e. the reset of the saw signal occured within this sample).. interpolate
		// the respective parts to avoid aliasing effects

		double prop_down = _counter * _freq_inc_sample_inv; 	// proportion after reset

		// the range that does not require interpolation is [_saw_sample_step/2 to 0xffff-_saw_sample_step/2],
		// i.e. the available interpolation range is 0xffff-_saw_sample_step and the correct base would be
		// _saw_sample_step/2

		// the below calc contains a deliberate excess offset of _saw_sample_step/2 so that the antialiased
		// sample here uses the same error as the regular samples generated in the "else" branch

//		return _saw_sample_step + (0xffff-_saw_sample_step)*(1-prop_down);
		return  _saw_base - _saw_range * prop_down;     // same as above just using precalculated stuff

	} else {
		// regular step-up of rising curve.. (result could be adjusted
		// via freq specific offset to get to the "correct" average, i.e. half
		// the change affected during a sample-interval)
		return _counter >> 8;
	}
#endif
}

#ifdef USE_HERMIT_ANTIALIAS
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

//	1) int32_t wfout = ((_counter>>12 >= _pulse_width))? 0 : 0xffff; // plain impl - inverted compared to resid
//	2) int32_t wfout = ((_counter>>12 < _pulse_width))? 0 : 0xffff; // plain impl

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
#else
uint16_t WaveGenerator::createPulseOutput() {
	if (_test_bit) return 0xffff;	// pulse start position

	// note: at the usual sample-rates (<50kHz) the shortest
	// pulse repetition interval takes about 10 samples, i.e.
	// a sample depends on data from (usually) one or at most two such
	// intervals. The smallest spikes that can be selected via
	// the pulse-width are significantly shorter, i.e. they may last
	// just one clock-cycle and easily fit within a sample
	// (a respective spike may then be surrounded on both
	// sides by the respective inverted signal). The below impl
	// tries to generate output that mimicks the results that would
	// be achived when brute-force oversampling the output cycle-by-cycle.

	// there still are some relatively small errors as compared to the
	// resampled signal (maybe due to the variing integer cycles per sample
	// e.g. alternating 22/23 cycles, or some other rounding and/or precision
	// issue.

	// handle start of a new pulse (i.e. high to low toggle)

	if (_counter < _freq_inc_sample) {
		// pulse was reset less than _freq_inc_sample ago:
		// i.e. transition from high to low within this sample

		if(_counter < _pulse_width12) {
			// 1a) new pulse is still low (i.e. it was high for some time
			// during the previous pulse and until now still has been low
			// for the current pulse

			// _counter reflects the freq increments that have already been
			// applied so far:
			uint32_t np = _counter / _freq;		// number of clocks spent in the new pulse
			double op = _cycles_per_sample - np;// number of clocks spent in the old pulse

			// how many clocks can the PULSE be HIGH with the current FREQ?
			double highc = _pulse_width12_neg / _freq;
			if (highc > op) highc = op;

			return highc * _ffff_cycles_per_sample_inv;
		} else {
			// 1b) pulse already toggeled back to HIGH, i.e. there was but a
			// short LOW-spike during the start of this sample (since even the
			// shortest PULSE-duration is multipe samples long, the signal must have
			// been HIGH for all of the previous sample)
			// (testcase: _freq = 0xffff;_pulse_width = 0xff;)

			uint32_t lowc = _pulse_width12 / _freq;
			double highc = _cycles_per_sample - lowc;

			return highc * _ffff_cycles_per_sample_inv;
		}
	}

	// handle lo/hi transition within the current pulse (the 2 "new pulse" scenarios
	// are already covered above, i.e. the only scenario left here is the signal toggle
	// from low to hi based on the pulse-width.. (this also means that here the
	// signal was already low during the previous sample)
	else if ((_counter > _pulse_width12) && (_counter  < _pulse_width12_plus)) {

		double highc = (_counter - _pulse_width12) / _freq;
		return highc * _ffff_cycles_per_sample_inv;
	}

//	return (_counter >> 12) < _pulse_width ? 0 : 0xffff;	// plain pulse
	return _counter < _pulse_width12 ? 0 : 0xffff;	// plain pulse
}
#endif

// combined noise-waveform will feed back into _noise_LFSR shift-register potentially
// clearing bits that are used for the "_noiseout" (no others.. and not setting!)

static const uint32_t COMBINED_NOISE_MASK = ~((1<<22)|(1<<20)|(1<<16)|(1<<13)|(1<<11)|(1<<7)|(1<<4)|(1<<2));


#define NOISE_OVERSAMPLE(...) \
	uint8_t noise_oversample = 0;\
	uint32_t p = _counter >> 20;	/* testcase: Kettle.sid >> 19 is to fast */\
	if (_noisepos != p) {\
		/* make sure to handle the correct number of updates - otherwise
		high frequency noise would be too loud (testcase: Empty.sid):
		p is a 4-bit counter that signals the number of overflows!*/ \
		if (p > _noisepos) {\
			noise_oversample = p - _noisepos;\
		} else {\
			noise_oversample = (0xf - _noisepos) + p;\
		}\
		_noisepos = p;\
		__VA_ARGS__ \
	}

uint16_t WaveGenerator::createNoiseOutput(uint16_t outv, uint8_t is_combined) {
	// "random values are output through the waveform generator according to the
	// frequency setting" (http://www.ffd2.com/fridge/blahtune/SID.primer)

	// known limitation of current impl: "_noise_LFSR" shift register is only
	// updated when noise is actually used (correctly it should always be clocked -
	// but given the rather expensive _noiseout calculation it seems acceptable to
	// do both on demand only - as an attempt to compensate for this limitation the
	// "noise reset" default value is somewhat adjusted)
	
	NOISE_OVERSAMPLE({
		// impl consistent with: http://www.sidmusic.org/sid/sidtech5.html
		// doc here is probably wrong: http://www.oxyron.de/html/registers_sid.html

		uint32_t noiseout = 0;
		for (uint8_t i= 0; i<noise_oversample; i++) {
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

			noiseout += o;		// sum of i 8-bit-values

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
		_noiseout = (noiseout * 0x101) / noise_oversample;	// scale 8-bit to 16-bit
	});
	

	return _noiseout;
}

uint8_t	WaveGenerator::getOsc() {
	// What is commonly/incorrectly referred to as the "value of the oscillator" (which is
	// returned in register D41B).. this value is NOT the oscillator but it is actually WF dependent!
	// For practical purposes combined WFs seem to be irrelevant here and the below incorrect
	// approximation seems to be good enough.

	uint16_t outv = 0xffff;

	if (_ctrl & TRI_BITMASK) {
		outv &= createTriangleOutput();
	}
	// CAUTION: anti-aliasing MUST NOT be used here  (testcase; we_are_demo_tune_2)
	if (_ctrl & SAW_BITMASK) {
		outv &= _counter >> 8;							// plain saw
	}
	if (_ctrl & PULSE_BITMASK) {
		outv &= _counter < _pulse_width12 ? 0 : 0xffff;	// plain pulse
	}

	if (_ctrl & NOISE_BITMASK)   {
		outv &= createNoiseOutput(outv, _ctrl & 0x70);
	}

	return outv >> 8;
}

uint16_t WaveGenerator::exoticCombinedOutput() {
	// fallback: the most relevant waveform combinations are already handled using
	// dedicated methods and this here is only the fallback to handle more unusual combined
	// waveforms - the most relevant of which are probably those involving NOISE

	int8_t combined = 0;
	if ((_ctrl & PULSE_BITMASK)) {
		// combined waveforms with pulse (all except noise)
		if ((_ctrl & TRI_BITMASK) && ++combined)  {
			if (_ctrl & SAW_BITMASK) {											// PULSE & TRIANGLE & SAW
				_prev_wave_form_out = pulseTriangleSawOutput();
			} else {
				_prev_wave_form_out = pulseTriangleOutput();					// PULSE & TRIANGLE
			}
		} else if ((_ctrl & SAW_BITMASK) && ++combined)  {						// PULSE & SAW
			_prev_wave_form_out = pulseSawOutput();
		} else {
			_prev_wave_form_out = 0xffff;
		}
	} else if ((_ctrl & TRI_BITMASK) && (_ctrl & SAW_BITMASK) && ++combined) {	// TRIANGLE & SAW
		_prev_wave_form_out = triangleSawOutput();
	} else {
		_prev_wave_form_out = 0xffff;
	}

	if (!combined) {
		// for the rest mix the oscillators with an AND operation as stated
		// in the SID's reference manual - even if this is absolutely wrong

		if (_ctrl & TRI_BITMASK)	_prev_wave_form_out &= createTriangleOutput();
		if (_ctrl & SAW_BITMASK)	_prev_wave_form_out &= (uint16_t)(_counter >> 8);
		if (_ctrl & PULSE_BITMASK)	_prev_wave_form_out &= (uint16_t)(_counter < _pulse_width12 ? 0 : 0xffff);
	}

	// handle noise last - since it may depend on the effect of the other waveforms
	if (_ctrl & NOISE_BITMASK)   {
		_prev_wave_form_out &= createNoiseOutput(_prev_wave_form_out, _ctrl & 0x70);
	}
	return _prev_wave_form_out;
}
