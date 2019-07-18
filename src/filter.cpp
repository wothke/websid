/*
* This file contains everything to do with the emulation of the SID chip's filter.
*
* Credits:
*  - the main filter implementation is based on Hermit's work
*  - the "external filter" is based on the analysis of the resid team
* 
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#include "filter.h"

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

extern "C" {
#include "env.h"
#include "cpu.h"
}
#include "sid.h"

// switch to completely disable use of the filter
#define USE_FILTER

// internal filter def
struct FilterState {
    uint8_t  low_ena;
	uint8_t  band_ena;
    uint8_t  hi_ena;
    uint8_t  v3ena;
    uint8_t  vol;
	
	uint8_t filter_enabled[3];	// activation per voice
	
	// derived from Hermit's filter implementation: see http://hermit.sidrip.com/jsSID.html	
	double low_pass;		// previous "low pass" output
    double band_pass;	// previous "band pass" output
	
	// filter output for simulated individual voice data (not used for actual playback)
	// allows to visualize the approximate effect the filter had on the respective voices (if any)
	double sim_low_pass[3];
    double sim_band_pass[3];

	double cutoff_ratio_8580;
    double cutoff_ratio_6581;
	
    uint8_t cutoff_low;			// filter cutoff low (3 bits)
    uint8_t cutoff_high;		// filter cutoff high (8 bits)
    uint8_t res_filt;			// resonance (4bits) / Filt
    uint8_t ftp_vol;			// mode (hi/band/lo pass) / volume
	double cutoff, resonance; 	// convenience calculated from the above

	
	// external filter (using "double" to avoid issues with denormals)
	double low_pass_ext;		// previous "low pass" output external filter
	double high_pass_ext;		// previous "high pass" output external filter
	double cutoff_low_pass_ext;	// cutoff "low pass" external filter
	double cutoff_high_pass_ext;// cutoff "high pass" external filter
};

// When looking at real-HW recordings of pulse-wave based songs (e.g. see Superman-Man_of_Steel.sid) 
// I noticed characteristic distortions like those caused by single-time-constant high-pass filters (see 
// https://global.oup.com/us/companion.websites/fdscontent/uscompanion/us/static/companion.websites/9780199339136/Appendices/Appendix_E.pdf
// - figure E.14 ) an effect that my emulator so far had not reproduced at all.. While investigating the 
// origin of the effect I found that investigators at reSid had already identified the culprit as what they called the SID's "external filter".
// I don't know how accurate their analysis really is, but the additional low-/high-pass filter that they propose creates distortions that 
// are pretty much in line with what can be observed in the SOASC recordings. What they say sounds like a very plausible explanation and
// I therefore added the same kind of filter configuration here (thanks to the resid team for their impressive reverse-engineering efforts!).

inline double extFilter(struct FilterState* state, double in, double cycles) {
	double out;
	for (int i= 0; i<cycles; i++) {
		out= state->low_pass_ext - state->high_pass_ext;
		state->high_pass_ext+= state->cutoff_high_pass_ext * out;
		state->low_pass_ext+= state->cutoff_low_pass_ext * (in - state->low_pass_ext);
	}
	return out;
}

struct FilterState* getState(Filter *e) {	// this should rather be static - but "friend" wouldn't work then
	return (struct FilterState*)e->_state;
}

Filter::Filter(SID *sid) {
	_sid= sid;
	
	_state= (void*) malloc(sizeof(struct FilterState));
}

void Filter::reset(uint32_t sample_rate) {
	struct FilterState* state= getState(this); 

	// init "filter" structures
	memset((uint8_t*)state, 0, sizeof(struct FilterState));
	
    state->cutoff_ratio_8580 = ((double)-2.0) * 3.1415926535897932385 * (12500.0 / 256) / sample_rate;
    state->cutoff_ratio_6581 = ((double)-2.0) * 3.1415926535897932385 * (20000.0 / 256) / sample_rate;
//	state->band_pass = 0;	// redundant
//	state->low_pass = 0;

//	state->low_pass_ext= state->high_pass_ext= 0;

	// could use the real clock frequency here.. but with the sample based rounding 
	// the imprecision is already so big that a fake precision here would be overkill..
	const double frequency= 1000000; 
	
	// cutoff: w0=1/RC  // capacitor: 1 F=1000000 uF; 1 uF= 1000000 pF (i.e. 10e-12 F)	
	state->cutoff_high_pass_ext = ((double)100) / frequency;	// hi-pass: R=1kOhm, C=10uF; i.e. w0=100
	state->cutoff_low_pass_ext = ((double)100000) / frequency;	// lo-pass: R=10kOhm, C=1000pF; i.e. w0=100000
}

/* Get the bit from an uint32_t at a specified position */
static uint8_t getBit(uint32_t val, uint8_t b) { return (uint8_t) ((val >> b) & 1); }

void Filter::poke(uint8_t reg, uint8_t val) {
	struct FilterState* state= getState(this); 
	switch (reg) {
        case 0x15: { state->cutoff_low = val & 0x7; resetInput(); break; }
        case 0x16: { state->cutoff_high = val; resetInput(); break; }
        case 0x17: { 
				state->res_filt = val & 0xf7;	// ignore FiltEx		
				resetInput();
				
				for (uint8_t voice=0; voice<3; voice++) {
					state->filter_enabled[voice]  = getBit(val, voice);
				}
	
			break; 
			}
        case 0x18: { 
				state->ftp_vol = val;
#ifdef USE_FILTER
				state->low_ena = getBit(val,4);	// lowpass
				state->band_ena = getBit(val,5);	// bandpass
				state->hi_ena = getBit(val,6);		// highpass
				state->v3ena = !getBit(val,7);	// chan3 off
#endif  
				state->vol   = (val & 0xf);		// main volume	
			break;
			}
	};
}

uint8_t Filter::getVolume() {
	struct FilterState* state= getState(this);
	return state->ftp_vol;
}

// voice output visualization works better if the effect of the filter is included
int32_t Filter::simOutput(uint8_t voice, uint8_t is_filtered, int32_t *filter_in, int32_t *out) {
	struct FilterState* state= getState(this);
#ifdef USE_FILTER	
	double output=	runFilter((double)-(*filter_in), (double)(*out), &(state->sim_band_pass[voice]), &(state->sim_low_pass[voice]));
		
	// filter volume is 4 bits/ outo is ~16bits (16bit from 3 voices + filter effects)
	// (using a reduced OUTPUT_SCALEDOWN risks integer overflow - see A_Desire_Welcome.sid - 
	// but allows to amplify otherwise badly visible graphs..)
	double OUTPUT_SCALEDOWN = 0x6 * 0xf / (is_filtered ? 1.3 : 4.0);	// 
	return round(output * state->vol / OUTPUT_SCALEDOWN); // SID output
#else
	int32_t OUTPUT_SCALEDOWN = 0x6 * 0xf;
	return (*out)* state->vol / OUTPUT_SCALEDOWN;
#endif
}

int32_t Filter::getOutput(int32_t *filter_in, int32_t *out, double cycles_per_sample) {

	struct FilterState* state= getState(this);
	int32_t OUTPUT_SCALEDOWN = 0x6 * 0xf;	// hand tuned with "424"
#ifdef USE_FILTER

	// note: filter impl seems to invert the data (see 2012_High-Score_Power_Ballad where two voices playing
	// the same notes cancelled each other out..)
	double output= 	runFilter((double)-(*filter_in), (double)(*out), &(state->band_pass), &(state->low_pass));
	
	output *= state->vol;

	output= extFilter(state, output, cycles_per_sample);
	
	// filter volume is 4 bits/ outo is ~16bits (16bit from 3 voices + filter effects)
	return round(output / OUTPUT_SCALEDOWN); // SID output
#else
	return (*out)* state->vol / OUTPUT_SCALEDOWN;
#endif
}

//#define USE_C_MATH

// for some reason Hermit's JavaScript impl does seem to sound different (in some situations)
// though the filter impl should be identical (maybe some border-base in math impls?)

#ifndef USE_C_MATH
#include <emscripten.h>
// note: direct use of the original JavaScript Math functions does NOT seem to make any
// difference - nor does the use of "-s BINARYEN_TRAP_MODE='js'": yet for some reason (not 
// identified) Hermit's filter/player seems to sound differently (e.g. 7D_Funkt) and SID model
// switches do not seem to make an audible difference here..
#endif

static double myExp(double in) {
#ifdef USE_C_MATH
	return exp(in);
#else
	double out= EM_ASM_DOUBLE({
		return Math.exp($0);
	}, in);
	return out;
#endif
}

static double myPow(double i1, double i2) {
#ifdef USE_C_MATH
	return pow(i1, i2);
#else
	double out= EM_ASM_DOUBLE({
		return Math.pow($0, $1);
	}, i1, i2);
	return out;
#endif
}

void Filter::resetInput() {// FIXME this would probably need to be re-adjusted for higher sampling rate
#ifdef USE_FILTER
	struct FilterState* state= getState(this);
	
	// weird scale.. - using the "lo" register as a fractional part..
	state->cutoff = ((double)(state->cutoff_low)) / 8 +  state->cutoff_high + 0.2;	// why the +0.2 ? should max not be 256?
		
	if (!_sid->isModel6581()) {
		state->cutoff = 1.0 - myExp(state->cutoff * state->cutoff_ratio_8580);
		state->resonance = myPow(2.0, ((4.0 - (state->res_filt >> 4)) / 8));				// i.e. 1.41 to 0.39
				
	} else {
		if (state->cutoff < 24.0) { state->cutoff = 0.035; }
		else { state->cutoff = 1.0 - 1.263 * myExp(state->cutoff * state->cutoff_ratio_6581); }
		state->resonance = (state->res_filt > 0x5F) ? 8.0 / (state->res_filt >> 4) : 1.41;	// i.e. 1.41 to 0.53
	}
#endif
}

uint8_t Filter::routeSignal(int32_t *voice_out, int32_t *outf, int32_t *outo, uint8_t voice, uint8_t *not_muted) {
	// note: compared to other emus output volume is quite high.. maybe better reduce it a bit?
	struct FilterState* state= getState(this);

#ifdef USE_FILTER
	if ((!state->v3ena && (voice == 2) && !state->filter_enabled[2]) || !(*not_muted)) {
		// voice 3 not silenced by !v3ena if routed through filter!
	} else {
		// regular routing
		if (state->filter_enabled[voice]) {
			// route to filter
			(*outf)+= (*voice_out);
			return 1;
		} else {
			// route directly to output
			(*outo)+= (*voice_out);
			return 0;
		}		
	}
#else
	// Don't use filters, just mix all voices together
	if (*not_muted) { 
		(*outo)+= (*voice_out); 
	}
#endif
	return 0;
}

double Filter::runFilter(double filter_in, double output, double *band_pass, double *low_pass) {
	// derived from Hermit's filter implementation:
	//	"FILTER: two integrator loop bi-quadratic filter, workings learned from resid code, but I kindof simplified the equations
	//	 The phases of lowpass and highpass outputs are inverted compared to the input, but bandpass IS in phase with the input signal.
	//	 The 8580 cutoff frequency control-curve is ideal, while the 6581 has a threshold, and below it outputs a constant lowpass frequency."
	
	// Filter creates/amplifies ugly effect on Vortex's digi channel - the root cause might be a flawed timing of the emulation
	// causing the first samples (of each frame) of that song to be rendered somewhat off.. and the filter probably just
	// amplifies that error
	struct FilterState* state= getState(this);
	double tmp = filter_in + (*band_pass) * state->resonance + (*low_pass);
	
	if (!(state->ftp_vol & 0x70)) { 
		// FIX bug in Hermit's impl: when neither high, band, nor lowpass is active then the 
		// 'output'(i.e. unfiltered signal) was returned and 'filter_in' was completely ignored!
		output+= filter_in; 	// see Dancing_in_the_Moonlight
	} else {		
		if (state->hi_ena) { output -= tmp;} 
		tmp = (*band_pass) - tmp * state->cutoff;
		(*band_pass) = tmp;
		
		if (state->band_ena) { output -= tmp; }
		tmp = (*low_pass) + tmp * state->cutoff;
		(*low_pass) = tmp;
		
		if (state->low_ena) { output += tmp; }
	}
	return output;
}
