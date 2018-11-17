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

// use of the filter seems to introduce annoying klicks/noise that ISN'T audible in when
// just using the raw data (see My Life, File Deleted, Vortex). It might be due to 
// slight timing error that are then amplified by the filter - maybe some clipping issue..
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
	double lowPass;		// previous "low pass" output
    double bandPass;	// previous "band pass" output
#ifdef USE_DIGIFILTER	
	double lowPassDigi;		// previous "low pass" output (specifically used for digis)
    double bandPassDigi;	// previous "band pass" output
#endif	
	
	// filter output for simulated individual voice data (not used for actual playback)
	// allows to visualize the approximate effect the filter had on the respective voices (if any)
	double simLowPass[3];
    double simBandPass[3];

	double cutoffRatio8580;
    double cutoffRatio6581;
	
    uint8_t cutoffLow;	// filter cutoff low (3 bits)
    uint8_t cutoffHigh;	// filter cutoff high (8 bits)
    uint8_t resFtv;		// resonance (4bits) / Filt
    uint8_t ftpVol;		// mode (hi/band/lo pass) / volume
	
	// external filter (using "double" to avoid issues with denormals)
	double lowPassExt;	// previous "low pass" output external filter
	double highPassExt;	// previous "high pass" output external filter
	double cutoffLowPassExt;	// cutoff "low pass" external filter
	double cutoffHighPassExt;	// cutoff "high pass" external filter
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
		out= state->lowPassExt - state->highPassExt;
		state->highPassExt+= state->cutoffHighPassExt * out;
		state->lowPassExt+= state->cutoffLowPassExt * (in - state->lowPassExt);
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

void Filter::reset(uint32_t sampleRate) {
	struct FilterState* state= getState(this); 

	// init "filter" structures
	memset((uint8_t*)state, 0, sizeof(struct FilterState));
	
    state->cutoffRatio8580 = ((double)-2.0) * 3.1415926535897932385 * (12500.0 / 256) / sampleRate,
    state->cutoffRatio6581 = ((double)-2.0) * 3.1415926535897932385 * (20000.0 / 256) / sampleRate;
//	state->bandPass = 0;	// redundant
//	state->lowPass = 0;

//	state->lowPassExt= state->highPassExt= 0;

	// could use the real clock frequency here.. but with the sample based rounding 
	// the imprecision is already so big that a fake precision here would be overkill..
	const double frequency= 1000000; 
	
	// cutoff: w0=1/RC  // capacitor: 1 F=1000000 uF; 1 uF= 1000000 pF (i.e. 10e-12 F)	
	state->cutoffHighPassExt = ((double)100) / frequency;	// hi-pass: R=1kOhm, C=10uF; i.e. w0=100
	state->cutoffLowPassExt = ((double)100000) / frequency;	// lo-pass: R=10kOhm, C=1000pF; i.e. w0=100000
}

/* Get the bit from an uint32_t at a specified position */
static uint8_t getBit(uint32_t val, uint8_t b) { return (uint8_t) ((val >> b) & 1); }

void Filter::poke(uint8_t reg, uint8_t val) {
	struct FilterState* state= getState(this); 
	switch (reg) {
        case 0x15: { state->cutoffLow = val; break; }
        case 0x16: { state->cutoffHigh = val; break; }
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

// voice output visualization works better if the effect of the filter is included
int32_t Filter::simOutput(uint8_t voice, int32_t *in, int32_t *out, double cutoff, double resonance) {
	struct FilterState* state= getState(this);
	int32_t OUTPUT_SCALEDOWN = 0x6 * 0xf / 4;
#ifdef USE_FILTER	
	// save the trouble to run any filter calcs when no filters are activated..
	double output=  !(state->ftpVol & 0x70) ? (double)(*out) :  
		runFilter((double)(*in), (double)(*out), &(state->simBandPass[voice]), &(state->simLowPass[voice]), cutoff, resonance);
		
	// filter volume is 4 bits/ outo is ~16bits (16bit from 3 voices + filter effects)		
	return round(output * state->vol / OUTPUT_SCALEDOWN); // SID output
#else
	return (*out)* state->vol / OUTPUT_SCALEDOWN;
#endif
}

int32_t Filter::getOutput(int32_t *in, int32_t *out, double cutoff, double resonance, double cyclesPerSample) {

	struct FilterState* state= getState(this);
	int32_t OUTPUT_SCALEDOWN = 0x6 * 0xf;	// hand tuned with "424"
#ifdef USE_FILTER	
	// save the trouble to run any filter calcs when no filters are activated..
		
	double output=  !(state->ftpVol & 0x70) ? (double)(*out) :  
		runFilter((double)(*in), (double)(*out), &(state->bandPass), &(state->lowPass), cutoff, resonance);
	
	output *= state->vol;

	output= extFilter(state, output, cyclesPerSample);
	
	// filter volume is 4 bits/ outo is ~16bits (16bit from 3 voices + filter effects)		
	return round(output / OUTPUT_SCALEDOWN); // SID output
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
	(*cutoff) = ((double)(state->cutoffLow & 0x7)) / 8 +  state->cutoffHigh + 0.2;	// why the +0.2 ?
		
	if (!_sid->isModel6581()) {
		(*cutoff) = 1.0 - exp((*cutoff) * state->cutoffRatio8580);
		(*resonance) = pow(2.0, ((4.0 - (state->resFtv >> 4)) / 8));
				
	} else {
		if ((*cutoff) < 24.0) { (*cutoff) = 0.035; }
		else { (*cutoff) = 1.0 - 1.263 * exp((*cutoff) * state->cutoffRatio6581); }
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

double Filter::runFilter(double in, double output, double *bandPass, double *lowPass, double cutoff, double resonance) {
	// derived from Hermit's filter implementation:
	//	"FILTER: two integrator loop bi-quadratic filter, workings learned from resid code, but I kindof simplified the equations
	//	 The phases of lowpass and highpass outputs are inverted compared to the input, but bandpass IS in phase with the input signal.
	//	 The 8580 cutoff frequency control-curve is ideal, while the 6581 has a threshold, and below it outputs a constant lowpass frequency."
	
	// Filter creates/amplifies ugly effect on Vortex's digi channel - the root cause might be a flawed timing of the emulation
	// causing the first samples (of each frame) of that song to be rendered somewhat off.. and the filter probably just
	// amplifies that error
	struct FilterState* state= getState(this);
	double tmp = in + (*bandPass) * resonance + (*lowPass);
	
	if (state->hiEna) { output -= tmp;} 
	tmp = (*bandPass) - tmp * cutoff;
	(*bandPass) = tmp;
	
	if (state->bandEna) { output -= tmp; }
	tmp = (*lowPass) + tmp * cutoff;
	(*lowPass) = tmp;
	
	if (state->lowEna) { output += tmp; }	
	return output;
}

void Filter::filterSamples(uint8_t *digiBuffer, uint32_t len, int8_t voice) {
	// depending on the used sample playback implementation, respective digi samples 
	// should be processed by the filter  (e.g. for PWM - but not for D418)
	// respective sample data would need to be merged into the regular SID output before 
	// the filter is applied... this emulator does NOT support this yet - the below 
	// workaround separately runs the imaginary "digi channel" through the filter 
	
	// problem: normally the SID filter works on a cycle basis and the "sample" steps used 
	// in this emulator are likely introducing all kinds of problems..
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
			output= runFilter(-in, 0, &(state->bandPassDigi), &(state->lowPassDigi), cutoff, resonance); // invert in to compensate for filter's inversion	
						
			// digi timing errors (the source has not been analyzed in detail and maybe some bug in the 
			// recording logic plays some role in this) may amplify to ugly spikes when using the filter 
			// (see "My Life", "Vortex", etc)
			// the below hack tries to limit that effect by damping the respective filter output (use of the 
			// filter still seems to benefit the songs by removing noise..):
//			if (i<5) { output= in;}	// old
			output= (output+in)/2;
			
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