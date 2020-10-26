/*
* This file contains everything to do with the emulation of the SID chip's filter.
*
* Credits:
*  - The main filter implementation is based on Hermit's work (this impl still
*    seems to be flawed.. e.g. see voice 1 in Giana_Sisters.sid). This impl is
*    certainly an improvement as compared to the original filter impl from TinySid
*    but it is likely THE most important root cause of remaining output quality issues.
*  - the "external filter" is based on the analysis by the resid team
* 
* Known limitation: In the current implementation the filter is fed with the output 
* sample rate (which may be 22x slower than the clock speed actually used by the SID), 
* i.e. higher frequency changes are totally ignored - which may lead to Moiré-effects 
* and it may distort what the filter would correctly be doing (see fast changing 
* waveforms like "noise" or certain combined-waveforms). However given the fact that 
* certain users already complain that the existing implementation is "too slow" (on
* their old smartphone or iPad) the quality-tradeoff seems to be preferable at this
* point.
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

// note: playback of D418 digi samples may radically change the "volume" that the
// filter uses to calculate one complete sample (e.g. for a 22-CPU-cycle interval).
// Especially when this volume-change occurs towards the very end of the interval
// the used volume may be very incorrect - as compared to the average volume that
// corresponds to the complete interval.
// However for practical purposes the use of a "more realistic" average does not
// seem to have any noticable effect.. so there is no point in making the emulation
// more expensive and I again removed the respective experimental impl.

#include "filter.h"

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// hand tuned with "424", etc (6581 specific songs are
// critical here since their signal is not symetrically distributed around 0)
#define OUTPUT_SCALEDOWN (0x6 * 0xf)	
#define SCOPE_SCALE (0xb * 0xf)	// trial & error: to avoid (most) overflows

const double SCOPE_SCALEDOWN =  ((double)0xf) / SCOPE_SCALE;
const double FILTERED_SCOPE_SCALEDOWN =  ((double)0xf * 4.0) / SCOPE_SCALE;

#include "sid.h"

// switch to completely disable use of the filter
#define USE_FILTER

// note: there doesn't seem to be any peformance difference ..
#define USE_C_MATH

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

/*
// Hermit's 6581 filter distortion impl - unused
#define VCR_SHUNT_6581 1500 //kOhm //cca 1.5 MOhm Rshunt across VCR FET drain and source (causing 220Hz bottom cutoff with 470pF integrator capacitors in old C64)
#define VCR_FET_TRESHOLD 192 //Vth (on cutoff numeric range 0..2048) for the VCR cutoff-frequency control FET below which it doesn't conduct
#define CAP_6581 0.470 //nF //filter capacitor value for 6581
#define CAP_6581_RECIPROCAL (-1000000 / CAP_6581)
#define FILTER_DARKNESS_6581 22.0 //the bigger the value, the darker the filter control is (that is, cutoff frequency increases less with the same cutoff-value)
#define FILTER_DISTORTION_6581 0.0016 //the bigger the value the more of resistance-modulation (filter distortion) is applied for 6581 cutoff-control
*/

// internal filter def
struct FilterState {
    uint8_t  low_ena;
	uint8_t  band_ena;
    uint8_t  hi_ena;
    uint8_t  v3ena;
    uint8_t  do_filter;			// redundancy: some filter mode selected
	
    uint8_t  vol;
		
	uint8_t filter_enabled[3];	// activation per voice
	
	// derived from Hermit's filter implementation: see http://hermit.sidrip.com/jsSID.html	
	double low_pass;	// previous "low pass" output
    double band_pass;	// previous "band pass" output
	
	// filter output for simulated individual voice data (not used for actual playback)
	// allows to visualize the approximate effect the filter had on the respective voices (if any)
	double sim_low_pass[3];
    double sim_band_pass[3];

	double cutoff_ratio_8580;
    double cutoff_ratio_6581, cutoff_bias_6581;

// Hermit's 6581 filter distortion impl - unused
//	double cutoff_steepness_6581, rDS_VCR_FET;
	
	uint32_t sample_rate;
	
    uint8_t cutoff_low;			// filter cutoff low (3 bits)
    uint8_t cutoff_high;		// filter cutoff high (8 bits)
    uint8_t res_filt;			// resonance (4bits) / Filt
    uint8_t ftp_vol;			// mode (hi/band/lo pass) / volume
	
	double cutoff, resonance; 	// convenience calculated from the above

	
	// external filter (using "double" to avoid issues with denormals)
	double low_pass_ext;		// previous "low pass" output external filter
	double high_pass_ext;		// previous "high pass" output external filter
	
//	double cutoff_low_pass_ext;	// cutoff "low pass" external filter
//	double cutoff_high_pass_ext;// cutoff "high pass" external filter
};

// When looking at real-HW recordings of pulse-wave based songs (e.g. see Superman-Man_of_Steel.sid) 
// I noticed characteristic distortions like those caused by single-time-constant high-pass filters (see 
// https://global.oup.com/us/companion.websites/fdscontent/uscompanion/us/static/companion.websites/9780199339136/Appendices/Appendix_E.pdf
// - figure E.14 ) an effect that my emulator so far had not reproduced at all.. While investigating the 
// origin of the effect I found that investigators at resid had already identified the culprit as what they called the SID's "external filter".
// I don't know how accurate their analysis really is, but the additional low-/high-pass filter that they propose creates distortions that 
// are pretty much in line with what can be observed in the SOASC recordings. What they say sounds like a very plausible explanation and
// I therefore added the same kind of filter configuration here (thanks to the resid team for their impressive reverse-engineering efforts!).

// external filter config
#define FREQUENCY 1000000 // // could use the real clock frequency here.. but  a fake precision here would be overkill

inline double extFilter(struct FilterState* state, double in, double cycles) {
/*
	// note: the cycle-by-cycle loop-impl is a performance killer and seems to have been the
	// main reason why some multi-SID songs stated to stutter on my old PC..	
	double out;
	for (int i= 0; i<cycles; i++) {
		out= state->low_pass_ext - state->high_pass_ext;
		state->high_pass_ext+= state->cutoff_high_pass_ext * out;
		state->low_pass_ext+= state->cutoff_low_pass_ext * (in - state->low_pass_ext);
	}
	return out;
*/	

	// cutoff: w0=1/RC  // capacitor: 1 F=1000000 uF; 1 uF= 1000000 pF (i.e. 10e-12 F)
	const double cutoff_high_pass_ext = ((double)100) / state->sample_rate;	// hi-pass: R=1kOhm, C=10uF; i.e. w0=100	=> causes the "saw look" on pulse WFs
//	const double cutoff_low_pass_ext = ((double)100000) / FREQUENCY;	// lo-pass: R=10kOhm, C=1000pF; i.e. w0=100000  .. no point at low sample rate
	
	double out = state->low_pass_ext - state->high_pass_ext;
	state->high_pass_ext += cutoff_high_pass_ext * out;
	state->low_pass_ext += (in - state->low_pass_ext);
//	state->low_pass_ext += cutoff_low_pass_ext * (in - state->low_pass_ext);

	return out;
}

struct FilterState* getState(Filter* e) {	// this should rather be static - but "friend" wouldn't work then
	return (struct FilterState*)e->_state;
}

Filter::Filter(SID* sid) {
	_sid = sid;
	
	_state = (void*) malloc(sizeof(struct FilterState));
}

void Filter::reset(uint32_t sample_rate) {
	struct FilterState* state= getState(this); 

	// init "filter" structures
	memset((uint8_t*)state, 0, sizeof(struct FilterState));

	state->sample_rate = sample_rate;

	// 8580 
    state->cutoff_ratio_8580 = ((double) -2.0) * 3.1415926535897932385 * (12500.0 / 2048) / sample_rate;

	// 6581: old cSID impl
    state->cutoff_ratio_6581 = ((double)-2.0) * 3.1415926535897932385 * (20000.0 / 2048) / sample_rate;
	state->cutoff_bias_6581 = 1 - myExp(-2 * 3.14 * 220 / sample_rate); //around 220Hz below treshold

// Hermit's 6581 filter distortion impl - unused
//	state->cutoff_steepness_6581 = FILTER_DARKNESS_6581 * (2048.0 - VCR_FET_TRESHOLD); //pre-scale for 0...2048 cutoff-value range //lighten CPU-load in sample-callback

//	state->band_pass = 0;	// redundant
//	state->low_pass = 0;

//	state->low_pass_ext= state->high_pass_ext= 0;
}

/* Get the bit from an uint32_t at a specified position */
static uint8_t getBit(uint32_t val, uint8_t b) { return (uint8_t) ((val >> b) & 1); }

void Filter::poke(uint8_t reg, uint8_t val) {
	struct FilterState* state = getState(this); 
	switch (reg) {
        case 0x15: { state->cutoff_low = val & 0x7; _sid->isModel6581() ? resetInput6581(0) : resetInput8580();  break; }
        case 0x16: { state->cutoff_high = val; _sid->isModel6581() ? resetInput6581(0) : resetInput8580();  break; }
        case 0x17: { 
				state->res_filt = val & 0xf7;	// ignore FiltEx

				_sid->isModel6581() ? resetInput6581(0) : resetInput8580(); 
				
				for (uint8_t voice= 0; voice<3; voice++) {
					state->filter_enabled[voice] = getBit(val, voice);
					
					// ditch cached stuff when filter is turned off
					if (!state->filter_enabled[voice]) {
						state->sim_low_pass[voice] = state->sim_band_pass[voice] = 0;
					}
				}
	
			break; 
			}
        case 0x18: {
				//  test-case Kapla_Caves and Immigrant_Song: song "randomly switches between 
				// filter modes which will also occationally disable all filters:
								
//				if ((val & 0x70) != (state->ftp_vol & 0x70)) {	// it seems to be a bad idea to reset filter state in this scenario.. 
				if (!(val & 0x70)) {
					state->low_pass = state->band_pass = 0;
					state->sim_low_pass[0] = state->sim_low_pass[1] = state->sim_low_pass[2] = 0;
					state->sim_band_pass[0] = state->sim_band_pass[1] = state->sim_band_pass[2] = 0;
				}
				state->ftp_vol = val;
#ifdef USE_FILTER
				state->low_ena = getBit(val, 4);		// lowpass
				state->band_ena = getBit(val, 5);	// bandpass
				state->hi_ena = getBit(val, 6);		// highpass
				state->v3ena = !getBit(val, 7);		// chan3 off
				
				state->do_filter = !!(val & 0x70);	// optimization
#endif  
				state->vol = (val & 0xf);			// main volume
			break;
			}
	};
}

// voice output visualization works better if the effect of the filter is included
int32_t Filter::simOutput(uint8_t voice, uint8_t is_filtered, int32_t* filter_in, int32_t* out) {
	struct FilterState* state= getState(this);
#ifdef USE_FILTER	
	double output=	runFilter((double)(*filter_in), (double)(*out), &(state->sim_band_pass[voice]), &(state->sim_low_pass[voice]));

	// not using real volume here: "original" voice output is more interesting without potential distortions
	// caused by the D418 digis.. (which are already tracked as a dedicated 4th voice)
	return is_filtered ? 
				output * SCOPE_SCALEDOWN :
				output * FILTERED_SCOPE_SCALEDOWN;
#else
	return (*out) * SCOPE_SCALEDOWN;
#endif
}

int32_t Filter::getOutput(int32_t* filter_in, int32_t* out, double cycles_per_sample) {

	struct FilterState* state = getState(this);
#ifdef USE_FILTER

	// note: filter impl seems to invert the data (see 2012_High-Score_Power_Ballad where two voices playing
	// the same notes cancelled each other out..)
	
// orig	double output = runFilter((double)-(*filter_in), (double)(*out), &(state->band_pass), &(state->low_pass));
	double output = runFilter((double)(*filter_in), (double)(*out), &(state->band_pass), &(state->low_pass));
	
	
	// note on performance: see http://nicolas.limare.net/pro/notes/2014/12/12_arit_speed/

	// example x86-64 (may differ depending on machine architecture):
	// 278 Mips integer multiplication								=> 1
	// 123 Mips integer division									=> 2.26 slower
	// 138 Mips double multiplication (32-bit is actually slower)	=> 2.01 slower
	// 67 Mips double division (32-bit is actually slower)			=> 4.15 slower
	
	// example: non-digi song, means per frame volume is set 1x and then 882 samples (or more) are rendered
	// 			- cost without prescaling: 882 * (1 + 2.26) =>	2875.32
	//			- cost with prescaling: 4.15 + 882 *(4.15)	=>	3664.45
	// 			=> for a digi song the costs for prescaling would be even higher
	
	// => repeated integer oparations are probably cheaper when floating point divisions are the alternative
		
	output *= state->vol;
	output /= OUTPUT_SCALEDOWN;
	
	output = extFilter(state, output, cycles_per_sample);
	
	// filter volume is 4 bits/ outo is ~16bits (16bit from 3 voices + filter effects)
	return output;
#else
	return (*out) * state->vol / OUTPUT_SCALEDOWN;
#endif
}


void Filter::resetInput8580() {
	// since this only depends on the sid regs, it is sufficient to update this after reg updates
#ifdef USE_FILTER
	struct FilterState* state = getState(this);
	
	// derived from Hermit's cSID-light impl	
	// NOTE: +1 is meant to model that even a 0 cutoff will still let through some signal..
	state->cutoff = ((double)state->cutoff_low) +  state->cutoff_high * 8 + 1;	// cutoff_low is already 3 bits
		
	state->cutoff = 1.0 - myExp(state->cutoff * state->cutoff_ratio_8580);
	state->resonance = myPow(2.0, ((4.0 - (state->res_filt >> 4)) / 8));				// i.e. 1.41 to 0.39
#endif
}
void Filter::resetInput6581(int32_t filtin) {
	// note: this method would need to be called between samples if Hermit's distortion impl ever was to be used 
#ifdef USE_FILTER
	struct FilterState* state = getState(this);
	
	// derived from Hermit's cSID-light impl	
	state->cutoff = ((double)state->cutoff_low) +  state->cutoff_high * 8 + 1;	// cutoff_low is already 3 bits
		
	// Hermit's sSID-light filter distortion impl must be updated with every sample, 
	// see filtin below - i.e. much more expensive and it doesn't seem to be worth the trouble
	// see additional clicks in John Ames's "Teen Spirit 1 main guitar"
/*
	state->cutoff += round(filtin * FILTER_DISTORTION_6581); //MOSFET-VCR control-voltage-modulation (resistance-modulation aka 6581 filter distortion) emulation

	state->rDS_VCR_FET = state->cutoff <= VCR_FET_TRESHOLD ? 
					100000000.0 //below Vth treshold Vgs control-voltage FET presents an open circuit
					: state->cutoff_steepness_6581 / (state->cutoff - VCR_FET_TRESHOLD); // rDS ~ (-Vth*rDSon) / (Vgs-Vth)  //above Vth FET drain-source resistance is proportional to reciprocal of cutoff-control voltage
	state->cutoff = (1 - exp(CAP_6581_RECIPROCAL / (VCR_SHUNT_6581 * state->rDS_VCR_FET / (VCR_SHUNT_6581 + state->rDS_VCR_FET)) / state->sample_rate)); //curve with 1.5MOhm VCR parallel Rshunt emulation
*/	
	
	// old cSID impl
	state->cutoff = (state->cutoff_bias_6581 + ((state->cutoff < 192) ? 0 : 1 - myExp((state->cutoff - 192) * state->cutoff_ratio_6581)));
	
	state->resonance = ((state->res_filt > 0x5F) ? 8.0 / (state->res_filt >> 4) : 1.41);
#endif
}

uint8_t Filter::isSilencedVoice3(uint8_t voice) {
	struct FilterState* state = getState(this);
	return ((voice == 2) && (!state->v3ena) && (!state->filter_enabled[2]));
}

uint8_t Filter::routeSignal(int32_t* voice_out, int32_t* outf, int32_t* outo, uint8_t voice) {
#ifdef USE_FILTER
	// note: compared to other emus output volume is quite high.. maybe better reduce it a bit?
	struct FilterState* state = getState(this);

	// regular routing
	if (state->filter_enabled[voice]) {
		// route to filter
		(*outf) += (*voice_out);
		return 1;
	} else {
		// route directly to output
		(*outo) += (*voice_out);
		return 0;
	}
#else
	// Don't use filters, just mix all voices together
	if (*not_muted) { 
		(*outo) += (*voice_out); 
	}
	return 0;
#endif
}

double Filter::runFilter(double filter_in, double output, double* band_pass, double* low_pass) {
	// derived from Hermit's filter implementation:
	//	"FILTER: two integrator loop bi-quadratic filter, workings learned from resid code, but I kindof simplified the equations.
	//	The phases of lowpass and highpass outputs are inverted compared to the input, but bandpass IS in phase with the input signal.
	//	The 8580 cutoff frequency control-curve is ideal, while the 6581 has a threshold, and below it outputs a constant lowpass frequency."
	
	struct FilterState* state = getState(this);
	
	if (!state->do_filter) {
		// fixed bug in Hermit's impl: when neither high, band, nor lowpass is active then the 
		// 'output'(i.e. unfiltered signal) was returned and 'filter_in' was completely ignored!
		output += filter_in; 	// see Dancing_in_the_Moonlight
	} else {		
		double tmp = filter_in + (*band_pass) * state->resonance + (*low_pass);
		
		if (state->hi_ena) { output -= tmp;} 
		
		tmp = (*band_pass) - tmp * state->cutoff;
		(*band_pass) = tmp;
		
// 		if (state->band_ena) { output -= tmp; }	// orig: supposedly this is what Hermit observed on his real HW
		if (state->band_ena) { output += tmp; }	// make it look like reSID (even if it might be wrong) fixme: check if 6581 and 8580 do this differently
		
		tmp = (*low_pass) + tmp * state->cutoff;
		(*low_pass) = tmp;
		
		if (state->low_ena) { output += tmp; }
	}
	return output;
}
