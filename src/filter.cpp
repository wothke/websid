/*
 * This file contains everything to do with the emulation of the SID chip's filter.
 *
 * There is nothing left in here from the original "TinySid for Linux" implementation.
 *
 * <p>Credits:
 * <ul>
 * <li>the current implementation is based on Hermit's work
 * </ul>
 * <p>Tiny'R'Sid (c) 2011-2018 J.Wothke
 * <p>version 0.9
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
#include "sid2.h"

// switch to completely disable use of the filter
#define USE_FILTER

// there used to be some problem with certain (badly timed) samples (see Vortex) - but 
// for some reason these seem to be gone now (maybe a clipping issue) 
// however now songs like "Hunters_Moon" no longer produce ANY output..
#define USE_DIGIFILTER	

// internal filter def
struct FilterState {
    uint8_t  lowEna;
	uint8_t  bandEna;
    uint8_t  hiEna;
    uint8_t  v3ena;
    uint8_t  vol;
	
	uint8_t filter[3];	// activation per voice
	
	// derived from Hermit's filter implementation: see http://hermit.sidrip.com/jsSID.html
	double prevlowpass;
    double prevbandpass;
#ifdef USE_DIGIFILTER	
	double prevlowpassDigi;
    double prevbandpassDigi;
#endif	
	double cutoff_ratio_8580;
    double cutoff_ratio_6581;
	
    uint8_t ffreqlo;	// filter cutoff low (3 bits)
    uint8_t ffreqhi;	// filter cutoff high (8 bits)
    uint8_t resFtv;		// resonance (4bits) / Filt
    uint8_t ftpVol;		// mode (hi/band/lo pass) / volume
};

struct FilterState* getState(Filter *e) {	// this should rather be static - but "friend" wouldn't work then
	return (struct FilterState*)e->_state;
}

Filter::Filter(SID *sid) {
	_sid= sid;
	
	_state= (void*) malloc(sizeof(FilterState));
}

void Filter::reset(uint32_t sampleRate) {
	struct FilterState* state= getState(this); 

	// init "filter" structures
	memset((uint8_t*)state,0,sizeof(FilterState));

    state->cutoff_ratio_8580 = ((double)-2.0) * 3.1415926535897932385 * (12500.0 / 256) / sampleRate,
    state->cutoff_ratio_6581 = ((double)-2.0) * 3.1415926535897932385 * (20000.0 / 256) / sampleRate;
//	state->prevbandpass = 0;	// redundant
//	state->prevlowpass = 0;	
}

/* Get the bit from an uint32_t at a specified position */
static uint8_t getBit(uint32_t val, uint8_t b) { return (uint8_t) ((val >> b) & 1); }

void Filter::poke(uint8_t reg, uint8_t val) {
	struct FilterState* state= getState(this); 
	switch (reg) {
        case 0x15: { state->ffreqlo = val; break; }
        case 0x16: { state->ffreqhi = val; break; }
        case 0x17: { 
				state->resFtv = val;		
		
				for (uint8_t voice=0; voice<3; voice++) {
					state->filter[voice]  = getBit(val, voice);
				}
	
			break; 
			}
        case 0x18: { 
				state->ftpVol = val;

#ifdef USE_FILTER
				state->lowEna = getBit(val,4);	// lowpass
				state->bandEna = getBit(val,5);	// bandpass
				state->hiEna = getBit(val,6);		// highpass
				state->v3ena = !getBit(val,7);	// chan3 off
#endif  
				state->vol   = (val & 0xf);		// main volume	
			break;
			}
	};
}

uint8_t Filter::getVolume() {
	struct FilterState* state= getState(this);
	return state->ftpVol;
}

int32_t Filter::getOutput(int32_t *in, int32_t *out, double cutoff, double resonance) {
	struct FilterState* state= getState(this);
	int32_t OUTPUT_SCALEDOWN = 0x6 * 0xf;	// hand tuned with "424"
#ifdef USE_FILTER	
	// save the trouble to run any filter calcs when no filters are activated..
	double output=  !(state->ftpVol & 0x70) ? (double)(*out) :  
		runFilter((double)(*in), (double)(*out), &(state->prevbandpass), &(state->prevlowpass), cutoff, resonance);
	
	// filter volume is 4 bits/ outo is ~16bits (16bit from 3 voices + filter effects)		
	return round(output * state->vol / OUTPUT_SCALEDOWN); // SID output
#else
	return (*out)* state->vol / OUTPUT_SCALEDOWN;
#endif
}

uint8_t Filter::isActive(uint8_t voice) {	
	// NOTE: Voice 3 is not silenced by !v3ena if it is routed through the filter!
	struct FilterState* state= getState(this);
	return state->v3ena || (!state->v3ena && state->filter[voice]);
}	

void Filter::setupFilterInput(double *cutoff, double *resonance) {
#ifdef USE_FILTER
	struct FilterState* state= getState(this);
	
	// weird scale.. - using the "lo" register as a fractional part..	
	(*cutoff) = ((double)(state->ffreqlo & 0x7)) / 8 +  state->ffreqhi + 0.2;	// why the +0.2 ?
		
	if (!_sid->isModel6581()) {
		(*cutoff) = 1.0 - exp((*cutoff) * state->cutoff_ratio_8580);
		(*resonance) = pow(2.0, ((4.0 - (state->resFtv >> 4)) / 8));
				
	} else {
		if ((*cutoff) < 24.0) { (*cutoff) = 0.035; }
		else { (*cutoff) = 1.0 - 1.263 * exp((*cutoff) * state->cutoff_ratio_6581); }
		(*resonance) = (state->resFtv > 0x5F) ? 8.0 / (state->resFtv >> 4) : 1.41;
	}
#endif
}

void Filter::routeSignal(int32_t *voiceOut, int32_t *outf, int32_t *outo, uint8_t voice, uint8_t *notMuted) {
// note: compared to other emus output volume is quite high.. maybe better reduce it a bit?
	struct FilterState* state= getState(this);

#ifdef USE_FILTER
	if (((voice<2) || isActive(voice)) && (*notMuted)) {
		if (state->filter[voice]) {
			// route to filter
			(*outf)+= (*voiceOut);	
		} else {
			// route directly to output
			(*outo)+= (*voiceOut);
		}
	}
#else
	// Don't use filters, just mix all voices together
	if (*notMuted) { 
		(*outo)+= (*voiceOut); 
	}
#endif
}


double Filter::runFilter(double in, double output, double *prevbandpass, double *prevlowpass, double cutoff, double resonance) {
	// derived from Hermit's filter implementation:
	//	"FILTER: two integrator loop bi-quadratic filter, workings learned from resid code, but I kindof simplified the equations
	//	 The phases of lowpass and highpass outputs are inverted compared to the input, but bandpass IS in phase with the input signal.
	//	 The 8580 cutoff frequency control-curve is ideal, while the 6581 has a threshold, and below it outputs a constant lowpass frequency."
	
	// Filter creates/amplifies ugly effect on Vortex's digi channel - the root cause might be a flawed timing of the emulation
	// causing the first samples (of each frame) of that song to be rendered somewhat off.. and the filter probably just
	// amplifies that error
	struct FilterState* state= getState(this);
	double tmp = in + (*prevbandpass) * resonance + (*prevlowpass);
	
	if (state->hiEna) { output -= tmp;} 
	tmp = (*prevbandpass) - tmp * cutoff;
	(*prevbandpass) = tmp;
	
	if (state->bandEna) { output -= tmp; }
	tmp = (*prevlowpass) + tmp * cutoff;
	(*prevlowpass) = tmp;
	
	if (state->lowEna) { output += tmp; }	
	return output;
}

void Filter::filterSamples(uint8_t *digiBuffer, uint32_t len, int8_t voice) {
	// depending on the used sample playback implementation, respective digi samples 
	// should be processed by the filter  (e.g. for PWM - but not for D418)
	// respective sample data would need to be merged into the regular SID output before 
	// the filter is applied... this emulator does NOT support this yet - the below 
	// workaround separately runs the imaginary "digi channel" through the filter 
#ifdef USE_FILTER
#ifdef USE_DIGIFILTER
	struct FilterState* state= getState(this);
	if (state->filter[voice]) {
		uint32_t unsignedOut;
		double in, output;

		double volMul= ((double)state->vol)/0xf * 0.66;	// volume reduction tuned according to LMan's feedback
		
		double cutoff, resonance;
		setupFilterInput(&cutoff, &resonance);
		
		for (uint32_t i= 0; i<len; i++) {			
			
			// filter logic is designed for 16-bit signed	
			in= ((int32_t)(digiBuffer[i] * 0x101) - 0x8000);	// rescale to 0xffff max, then convert to signed
			
			// output signal seems to be "inverted"
			output= runFilter(-in, 0, &(state->prevbandpassDigi), &(state->prevlowpassDigi), cutoff, resonance); // invert in to compensate for filter's inversion	
						
			// the timing errors at the frame start may lead to ugly spikes when using the filter (see "Vortex")
			// the below hack tries to limit that effect by discarding the respective filter output:
			if (i<5) output= in;
			
			output *= volMul;		// apply filter volume
			output += 0x8000;		// back  to unsigned			

			unsignedOut= output<0 ? 0 : (output>0xffff ? 0xffff : output);			
			
			// 0x101 only makes sense when >>8 is used here..
//			digiBuffer[i]= (unsignedOut / 0x101) & 0xff; // unsigned 8-bit 
			digiBuffer[i]= (unsignedOut >>8) & 0xff; // unsigned 8-bit 
		}
	}
#else
	double volAdjust= 0.66;	 // tuned according to LMan's feedback
	for (uint32_t i= 0; i<len; i++) {
		digiBuffer[i]= round((((int16_t)digiBuffer[i])-128) * volAdjust)+128;	
	}
#endif
#endif
}