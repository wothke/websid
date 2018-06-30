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

 // FIXME: might be a good idea to migrate everything to C++ so further cleanup the original C code mess..
 
// useful links:
// http://www.waitingforfriday.com/index.php/Commodore_SID_6581_Datasheet
// http://www.sidmusic.org/sid/sidtech2.html
 
// note: only the "sid" prefixed functions are exported outside of this file..
 
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "sid.h"

#include "digi.h"
#include "memory.h"
#include "cpu.h"		// cpuGetProgramMode(), etc
#include "env.h"		// envNumberOfSamplesPerCall(), etc
#include "rsidengine.h" // rsidGetFrameCount() 


// switch to completely disable use of the filter
#define USE_FILTER

//#define USE_DIGIFILTER	// as long as samples are not perfectly timed, this will introduce additional distortions (no point in wasting the extra cycles..)
//#define USE_JS_MATH		// doesn't make any difference


// SID register definition
struct MosSid {
	uint8_t isModel6581;
	uint8_t level_DC;
	uint32_t sampleRate;	// e.g. 44100

	double cycleOverflow;
	double cyclesPerSample;
	
	uint8_t voiceEnableMask;	// allows to mute certain voices (3 low bits mask)

    struct Voice {
        uint16_t freq;
        uint16_t pulse;
        uint8_t wave;
        uint8_t ad;
        uint8_t sr;
		
		// add-ons snatched from Hermit's implementation
		double prevWavData;					// combined waveform handling
		uint16_t prevWaveFormOut;		// floating DAC handling
    } voices[3];
	
	// filter
    uint8_t ffreqlo;	// filter cutoff low (3 bits)
    uint8_t ffreqhi;	// filter cutoff high (8 bits)
    uint8_t resFtv;		// resonance (4bits) / Filt
    uint8_t ftpVol;		// mode (hi/band/lo pass) / volume
	
	struct EnvGenerator {	// envelope generator stuff
		int32_t limitLFSR; // the original cycle counter would be 15-bit (here samples are counted & counter is rescaled accordingly)
		int32_t counterPeriod[16];
		uint8_t exponentialDelays[256];
    } env;
	
	// Hermit's precalculated "combined waveforms" 
	double TriSaw_8580[4096];
	double PulseSaw_8580[4096];
	double PulseTri_8580[4096];			// Hermit's use of PulseSaw_8580 does not convince in Last_Ninja
	double PulseTriSaw_8580[4096];
};

static struct MosSid _sid;

/**************************************************************************************************
	below add-on HACK for "main loop ocsillator polling"
	
	This is a hack to support specific ocsillator polling from main loop as done by PollyTracker
	e.g. see Instantfunk.sid (the usage pattern is very specific and the below hack 
	covers exactly that - and nothing more).
***************************************************************************************************/

static uint8_t useOscPollingHack() {
	// so far it has only been tested for main-loop
	return cpuGetProgramMode() == MAIN_OFFSET_MASK;
}

struct SimOsc3 {
	uint8_t waveform;
	
	// more recent hack for main loop polling
	uint32_t baseCycles;
	uint32_t counter;
	uint32_t multiplicator;	
};
static struct SimOsc3 _osc3sim;

static void simStartOscillatorVoice3(uint8_t voice, uint8_t val) {
	if ((voice == 2) && useOscPollingHack()) {
		_osc3sim.waveform= val & 0xf0;

		// hack: use only for main/pulse cases like Instantfunk.sid
		_osc3sim.baseCycles= cpuTotalCycles();
		_osc3sim.counter= 0; 
		
		// for some reason the playback is slightly slower than in ACID64
		_osc3sim.multiplicator= round(_sid.cyclesPerSample * envNumberOfSamplesPerCall() / envCyclesPerScreen());
	}
}
static uint32_t simOsc3Counter() {
	// voice 3 oscillator counter based on elapsed time
	uint32_t diff= cpuTotalCycles() - _osc3sim.baseCycles;
	_osc3sim.baseCycles= cpuTotalCycles();

	uint32_t f= ((uint32_t)_sid.voices[2].freq) * _osc3sim.multiplicator * diff;		
	_osc3sim.counter= (_osc3sim.counter + f) & 0xffffff;
	return _osc3sim.counter;
}

static uint8_t simReadSawtoothD41B() {
	// simulate sawtooth voice 3 oscillator level based on elapsed time	
	// (handle busy polling for sid oscillator3 - e.g. Ring_Ring_Ring.sid)

	return (uint8_t) (simOsc3Counter() >> 16);
}

static uint8_t simReadPulsedD41B() {
	// simulate pulse voice 3 oscillator level based on elapsed time	
	uint32_t p= (((uint32_t)_sid.voices[2].pulse) & 0xfff) << 12;
	return (simOsc3Counter() > p) ? 0 : 1;
}

static uint8_t simReadD41B() {
	if (_osc3sim.waveform == 0x40) {
		return  simReadPulsedD41B();
	}
	return simReadSawtoothD41B();
}

/**************************************************************************************************/

typedef enum {
    Attack=0,
    Decay=1,
    Sustain=2,
    Release=3,
} EnvelopePhase;

// internal oscillator def
struct SidOscillator {
    uint32_t freqS;		// osc increment per second 
    uint32_t freqC;		// osc increment per cycle
	
    uint32_t pulse;
    uint8_t wave;
    uint8_t filter;
    uint32_t attack;	// for 255 steps
    uint32_t decay;		// for 255 steps
    uint32_t sustain;
    uint32_t release;
    uint32_t counter;		// 24-bit as in the original
	
	// hard sync handling
    uint32_t prevCounter;		// 24-bit
	uint8_t msbRising;

	// updated envelope generation based on findings from reSID team
	uint8_t envelopeOutput;
	int16_t currentLFSR;	// sim counter	(continuously counting / only reset by AD(S)R match)
	uint8_t zeroLock;  
	uint8_t exponentialCounter;    
	
    uint8_t envphase;
    uint32_t noisepos;
    uint32_t noiseval;
    uint8_t noiseout;
	
	// detection of ADSR-bug conditions
//	uint32_t adsrBugFrameCount;
};

// internal filter def
struct SidFilter {
    uint8_t  lowEna;
	uint8_t  bandEna;
    uint8_t  hiEna;
    uint8_t  v3ena;
    uint8_t  vol;
		
	// derived from Hermit's filter implementation: see http://hermit.sidrip.com/jsSID.html
	double prevlowpass;
    double prevbandpass;
#ifdef USE_DIGIFILTER	
	double prevlowpassDigi;
    double prevbandpassDigi;
#endif	
	double cutoff_ratio_8580;
    double cutoff_ratio_6581;
};

uint32_t sidGetSampleFreq() {
	return _sid.sampleRate;
}
uint8_t sidGetWave(uint8_t voice) {
	return _sid.voices[voice].wave;
}
uint8_t sidGetAD(uint8_t voice) {
	return _sid.voices[voice].ad;	
}
uint8_t sidGetSR(uint8_t voice) {
	return _sid.voices[voice].sr;	
}
uint16_t sidGetFreq(uint8_t voice) {
	return _sid.voices[voice].freq;
}
uint16_t sidGetPulse(uint8_t voice) {
	return _sid.voices[voice].pulse;
}

static struct SidOscillator _osc[3];
static struct SidFilter _filter;

/* Get the bit from an uint32_t at a specified position */
static inline uint8_t getBit(uint32_t val, uint8_t b) { return (uint8_t) ((val >> b) & 1); }

#define SYNC_BITMASK 	0x02		
#define RING_BITMASK 	0x04		
#define TEST_BITMASK 	0x08		

#define TRI_BITMASK 	0x10		
#define SAW_BITMASK 	0x20		
#define PULSE_BITMASK 	0x40		
#define NOISE_BITMASK 	0x80
	
void sidSetMute(uint8_t voice, uint8_t value) {
	if (value)
		_sid.voiceEnableMask &= ~(1 << voice);	// disable voice
	else 
		_sid.voiceEnableMask |= (1 << voice);	// enable voice
}


/*
* check if LFSR threshold was reached 
*/
static uint8_t triggerLFSR_Threshold(uint16_t threshold, int16_t *end) {
	if (threshold == (*end)) {
		(*end)= 0; // reset counter
		return 1;
	}
	return 0;
}

static uint8_t handleExponentialDelay(uint8_t voice) {
	_osc[voice].exponentialCounter+= 1;
	
	uint8_t result= (_osc[voice].exponentialCounter >= _sid.env.exponentialDelays[_osc[voice].envelopeOutput]);
	if (result) {
		_osc[voice].exponentialCounter= 0;	// reset to start next round
	}
	return result;
}

static void simOneEnvelopeCycle(uint8_t voice) {
	/* 
	now process the volume according to the phase and adsr values (explicit switching of ADSR 
	phase is handled in sidPoke() so there is no need to handle that here)

	advance envelope LFSR counter (originally this would be a 15-bit cycle counter.. but we 
	are counting samples here)

	ADSR bug scenario: normally the maximum thresholds used for the original 15-bit counter 
	would have been around 0x7a13 (i.e. somewhat below the 7fff range that can be handled by 
	the counter). For certain bug scenarios it is possible that the threshold is missed and 
	the counter keeps counting until it again reaches the threshold after a wrap-around.. 
	(see sidPoke() for specific ADSR-bug handling)
	*/
	if (++_osc[voice].currentLFSR >= _sid.env.limitLFSR) {
		_osc[voice].currentLFSR= 0;
	}
	
	uint8_t previousEnvelopeOutput = _osc[voice].envelopeOutput;
				
	switch (_osc[voice].envphase) {
		case Attack: {                          // Phase 0 : Attack
			if (triggerLFSR_Threshold(_osc[voice].attack, &_osc[voice].currentLFSR)) {	
				// inc volume when threshold is reached						
				if (!_osc[voice].zeroLock) {
					if (_osc[voice].envelopeOutput < 0xff) {
						/* see Alien.sid: "full envelopeOutput level" GATE off/on sequences 
						   within same IRQ will cause undesireable overflow.. this might not 
						   be a problem in cycle accurate emulations.. but here it is (we 
						   only see a 20ms snapshot)
						*/
						_osc[voice].envelopeOutput= (_osc[voice].envelopeOutput + 1) & 0xff;	// increase volume
					}							
				
					_osc[voice].exponentialCounter = 0;

					if (_osc[voice].envelopeOutput == 0xff) {
						_osc[voice].envphase = Decay;
					}							
				}
			}
			break;
		}
		case Decay: {                   	// Phase 1 : Decay      
			if (triggerLFSR_Threshold(_osc[voice].decay, &_osc[voice].currentLFSR) 
				&& handleExponentialDelay(voice)) { 	// dec volume when threshold is reached
				
				if (!_osc[voice].zeroLock) {
					if (_osc[voice].envelopeOutput != _osc[voice].sustain) {
						_osc[voice].envelopeOutput= (_osc[voice].envelopeOutput - 1) & 0xff;	// decrease volume
					} else {
						_osc[voice].envphase = Sustain;
					}
				}	
			}
			break;
		}
		case Sustain: {                        // Phase 2 : Sustain
			triggerLFSR_Threshold(_osc[voice].decay, &_osc[voice].currentLFSR);	// keeps using the decay threshold!
		
			if (_osc[voice].envelopeOutput != _osc[voice].sustain) {
				_osc[voice].envphase = Decay;
			}
			break;
		}					
		case Release: {                          // Phase 3 : Release
			// this phase must be explicitly triggered by clearing the GATE bit..
			if (triggerLFSR_Threshold(_osc[voice].release, &_osc[voice].currentLFSR) 
				&& handleExponentialDelay(voice)) { 		// dec volume when threshold is reached

				if (!_osc[voice].zeroLock) {				
					_osc[voice].envelopeOutput= (_osc[voice].envelopeOutput - 1) & 0xff;	// decrease volume
				}
			}						
			break;
		}
	}
	if ((_osc[voice].envelopeOutput == 0) && (previousEnvelopeOutput > _osc[voice].envelopeOutput)) {
		_osc[voice].zeroLock = 1;	// new "attack" phase must be started to unlock
	}			
}	  

// Hermit's impl to calculate combined waveforms (check his jsSID-0.9.1-tech_comments in commented jsSID.js for background info): 
// I did not thoroughly check how well this really works (it works well enough for Kentilla and Clique_Baby apparently has to sound as shitty as it does)

static uint16_t combinedWF(uint8_t channel, double *wfarray, uint16_t index, uint8_t differ6581) { //on 6581 most combined waveforms are essentially halved 8580-like waves
	if (differ6581 && _sid.isModel6581) index &= 0x7FF;
	double combiwf = (wfarray[index] + _sid.voices[channel].prevWavData) / 2;
	_sid.voices[channel].prevWavData = wfarray[index];
	
	return (uint16_t)round(combiwf);

}

static void createCombinedWF(double *wfarray, double bitmul, double bitstrength, double treshold) { //I found out how the combined waveform works (neighboring bits affect each other recursively)
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


#include <emscripten.h>

static double jsPow(double a, double b) {
	// tests suggest that the 2 impls are actually equivalent.. just for comparison
#ifdef USE_JS_MATH	
	return EM_ASM_DOUBLE({
			return Math.pow($0, $1);
		}, a, b);
#else
	return pow(a, b);
#endif
}
static double jsExp(double a) {	
#ifdef USE_JS_MATH	
	return EM_ASM_DOUBLE({
			return Math.exp($0);
		}, a);
#else
	return exp(a);
#endif
}

static void getFilterInput(double *cutoff, double *resonance) {
	// weird scale.. - using the "lo" register as a fractional part..	
	(*cutoff) = ((double)(_sid.ffreqlo & 0x7)) / 8 +  _sid.ffreqhi + 0.2;	// why the +0.2 ?
		
	if (!_sid.isModel6581) {
		(*cutoff) = 1.0 - jsExp((*cutoff) * _filter.cutoff_ratio_8580);
		(*resonance) = jsPow(2.0, ((4.0 - (_sid.resFtv >> 4)) / 8));
	} else {
		if ((*cutoff) < 24.0) { (*cutoff) = 0.035; }
		else { (*cutoff) = 1.0 - 1.263 * jsExp((*cutoff) * _filter.cutoff_ratio_6581); }
		(*resonance) = (_sid.resFtv > 0x5F) ? 8.0 / (_sid.resFtv >> 4) : 1.41;
	}
}

static double runFilter(double in, double output, double *prevbandpass, double *prevlowpass, double cutoff, double resonance) {
	// derived from Hermit's filter implementation:
	//	"FILTER: two integrator loop bi-quadratic filter, workings learned from resid code, but I kindof simplified the equations
	//	 The phases of lowpass and highpass outputs are inverted compared to the input, but bandpass IS in phase with the input signal.
	//	 The 8580 cutoff frequency control-curve is ideal, while the 6581 has a threshold, and below it outputs a constant lowpass frequency."
	
	// Filter creates/amplifies ugly effect on Vortex's digi channel - the root cause might be a flawed timing of the emulation
	// causing the first samples (of each frame) of that song to be rendered somewhat off.. and the filter probably just
	// amplifies that error
	double tmp = in + (*prevbandpass) * resonance + (*prevlowpass);
			
	if (_filter.hiEna) { output -= tmp;} 
	tmp = (*prevbandpass) - tmp * cutoff;
	(*prevbandpass) = tmp;
	
	if (_filter.bandEna) { output -= tmp; }
	tmp = (*prevlowpass) + tmp * cutoff;
	(*prevlowpass) = tmp;
	
	if (_filter.lowEna) { output += tmp; }	
	return output;
}

void sidFilterSamples (uint8_t *digiBuffer, uint32_t len, int8_t voice) {
	// depending on the used sample playback implementation, respective digi samples 
	// should be processed by the filter  (e.g. for PWM - but not for D418)
	// respective sample data would need to be merged into the regular SID output before 
	// the filter is applied... this emulator does NOT support this yet - the below 
	// workaround separately runs the imaginary "digi channel" through the filter 

	
#if defined(USE_FILTER)
	if (((voice<2) || _filter.v3ena || (!_filter.v3ena && _osc[voice].filter)) && _osc[voice].filter) {
#ifdef USE_DIGIFILTER
	
		uint32_t unsignedOut;
		double in, output;

		double volMul= ((double)_filter.vol)/0xf * 0.66;	// volume reduction tuned according to LMan's feedback
		
		double cutoff, resonance;
		getFilterInput(&cutoff, &resonance);
		
		for (uint32_t i= 0; i<len; i++) {			
			
			// filter logic is designed for 16-bit signed	
			in= ((int32_t)(digiBuffer[i] * 0x101) - 0x8000);	// rescale to 0xffff max, then convert to signed
			
			// output signal seems to be "inverted"
			output= runFilter(-in, 0, &(_filter.prevbandpassDigi), &(_filter.prevlowpassDigi), cutoff, resonance); // invert in to compensate for filter's inversion	
						
			// the timing errors at the frame start may lead to ugly spikes when using the filter (see "Vortex")
			// the below hack tries to limit that effect by discarding the respective filter output:
			if (i<5) output= in;
			
			output *= volMul;		// apply filter volume
			output += 0x8000;		// back  to unsigned			
	
			unsignedOut= output<0 ? 0 : (output>0xffff ? 0xffff : output);			
			
			digiBuffer[i]= (unsignedOut / 0x101) & 0xff; // unsigned 8-bit 
		}
#else
		double volAdjust= 0.66;	 // tuned according to LMan's feedback
		for (uint32_t i= 0; i<len; i++) {
			digiBuffer[i]= round((((int16_t)digiBuffer[i])-128) * volAdjust)+128;	
		}
#endif
	}
#endif
}

#define isTestBit(voice) _osc[voice].wave&TEST_BITMASK

static uint8_t getPreviousVoice(uint8_t voice) {
	return voice ? voice-1 : 2;
}

static uint8_t getNextVoice(uint8_t voice) {
	return (voice == 2) ? 0 : voice+1;
}

static uint32_t getRingModCounter(uint8_t voice) {
	// Bob Yannes: "Ring Modulation was accomplished by substituting the accumulator MSB of an oscillator 
	// in the EXOR function of the triangle waveform generator with the accumulator MSB of the 
	// previous oscillator. That is why the triangle waveform must be selected to use Ring Modulation."
	
	if (_osc[voice].wave & RING_BITMASK) {
		// wtf does he mean? (1) substitute MSB before using it in some EXOR logic OR (2) substitute it 
		// using EXOR?
		
		uint8_t srcVoice = getPreviousVoice(voice);  
//		return (_osc[voice].counter & 0x7fffff) | (_osc[srcVoice].counter & 0x800000);	// (1)	
		return _osc[voice].counter ^ (_osc[srcVoice].counter & 0x800000);				// (2) judging by the sound of R1D1.sid, this is it..
	} else {
		return _osc[voice].counter;		
	}
}

static uint16_t createTriangleOutput(uint8_t voice) {
	uint32_t tmp = getRingModCounter(voice);
    uint32_t wfout = (tmp ^ (tmp & 0x800000 ? 0xFFFFFF : 0)) >> 7;
	return wfout & 0xFFFF;
}			
static uint16_t createSawOutput(uint8_t voice) {	// test with Alien or Kawasaki_Synthesizer_Demo
	// Hermit's "anti-aliasing"
	uint32_t wfout = _osc[voice].counter >> 8;	// top 16-bits
	double step = ((double)_osc[voice].freqS) / 0x1200000;
	wfout += round(wfout * step);
	if (wfout > 0xFFFF) wfout = 0xFFFF - round (((double)(wfout - 0x10000)) / step);
	return wfout;
}

static void calcPulseBase(uint8_t voice, uint32_t *tmp, uint32_t *pw) {
	// based on Hermit's impl
	(*pw) = _osc[voice].pulse;
	(*tmp) = _osc[voice].freqS >> 9;	// 15 MSB needed
	
	if (0 < (*pw) && (*pw) < (*tmp)) { (*pw) = (*tmp); }
	(*tmp) ^= 0xFFFF;
	if ((*pw) > (*tmp)) (*pw) = (*tmp);
	(*tmp) = _osc[voice].counter >> 8;			// 16 MSB needed
}

static uint16_t createPulseOutput(uint8_t voice, uint32_t tmp, uint32_t pw) {	// elementary pulse
	if (isTestBit(voice)) return 0xFFFF;	// pulse start position
	
	// Hermit's "anti-aliasing"
	double step = 256.0 / (_osc[voice].freqS >> 16); //simple pulse, most often used waveform, make it sound as clean as possible without oversampling

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
static uint16_t createNoiseOutput(uint8_t voice) {
	// generate noise waveform exactly as the SID does.
	
	// "random values are output through the waveform generator according to the 
	// frequency setting" (http://www.ffd2.com/fridge/blahtune/SID.primer)
	
	// testcase for calibration: see Hermit's noisewfsweep.sid
	uint32_t p= _osc[voice].counter>>20;		//  top 4-bit seem to be about right.. (didn't find specific specs unfortunately..)
	if (_osc[voice].noisepos != p) {
		_osc[voice].noisepos = p;
		
		// impl consistent with: http://www.sidmusic.org/sid/sidtech5.html
		// doc here is probably wrong: http://www.oxyron.de/html/registers_sid.html
		_osc[voice].noiseout = 
				(getBit(_osc[voice].noiseval,22) << 7) |
				(getBit(_osc[voice].noiseval,20) << 6) |
				(getBit(_osc[voice].noiseval,16) << 5) |
				(getBit(_osc[voice].noiseval,13) << 4) |
				(getBit(_osc[voice].noiseval,11) << 3) |
				(getBit(_osc[voice].noiseval, 7) << 2) |
				(getBit(_osc[voice].noiseval, 4) << 1) |
				(getBit(_osc[voice].noiseval, 2) << 0);
				
		_osc[voice].noiseval = (_osc[voice].noiseval << 1) |
				(getBit(_osc[voice].noiseval,22) ^ getBit(_osc[voice].noiseval,17));	
	}
	return ((uint16_t)_osc[voice].noiseout) << 8;
}

/*
* While "sid" data structure is automatically kept in sync by the emulator's memory access 
* implementation, the "_osc" and "_filter" helper structures are NOT - i.e. they need to 
* be explicitly synced before calculating SID output.
*/
static void syncRegisterCache() {
    /* 
	"step 1: convert the not easily processable sid registers into some
            more convenient and fast values (makes the thing much faster
            if you process more than 1 sample value at once)"
	*/
	struct EnvGenerator *env= &(_sid.env);

    for (uint8_t voice=0;voice<3;voice++) {
        _osc[voice].pulse   = (_sid.voices[voice].pulse & 0xfff) << 4;	// // 16 MSB pulse needed
        _osc[voice].filter  = getBit(_sid.resFtv, voice);
		// threshold to be reached before incrementing volume
        _osc[voice].attack  = env->counterPeriod[_sid.voices[voice].ad >> 4];
        _osc[voice].decay   = env->counterPeriod[_sid.voices[voice].ad & 0xf];
		uint8_t sustain= _sid.voices[voice].sr >> 4;
        _osc[voice].sustain = sustain<<4 | sustain;
        _osc[voice].release = env->counterPeriod[_sid.voices[voice].sr & 0xf];
        _osc[voice].wave    = _sid.voices[voice].wave;

        _osc[voice].freqS    = round(_sid.cyclesPerSample * _sid.voices[voice].freq);	// per 1-sample interval (e.g. ~22 cycles)
        _osc[voice].freqC    = ((uint32_t)_sid.voices[voice].freq);
		
    }
#ifdef USE_FILTER
	_filter.lowEna = getBit(_sid.ftpVol,4);	// lowpass
	_filter.bandEna = getBit(_sid.ftpVol,5);	// bandpass
	_filter.hiEna = getBit(_sid.ftpVol,6);	// highpass
	_filter.v3ena = !getBit(_sid.ftpVol,7);	// chan3 off
	_filter.vol   = (_sid.ftpVol & 0xf);		// main volume
#endif  
}


static void syncOscillator(uint8_t voice) {
	// Hard Sync is accomplished by clearing the accumulator of an Oscillator  
	// based on the accumulator MSB of the previous oscillator.

	// tests for hard sync:  Ben Daglish's Wilderness music from The Last Ninja  
	// (https://www.youtube.com/watch?v=AbBENI8sHFE) .. seems to be used on voice 2 early on..
	// for some reason the instrument that starts on voice 1 at about 45 secs (come combined waveform?)
	// does not sound as crisp as it should.. (neither in Hermit's player)
		
	// intro noise in Martin Galway's Roland's Rat Race (https://www.youtube.com/watch?v=Zc91S1lrU1I) music.
	
	// the below logic is from the "previous oscillator" perspective
	
	if (!_osc[voice].freqC) return;
	
	uint8_t msbRising= _osc[voice].msbRising;				// base trigger condition
	
	uint8_t destVoice = getNextVoice(voice);  
	uint8_t destSync= _osc[destVoice].wave & SYNC_BITMASK;	// sync requested?

	
	// exception: when sync source is itself synced in the same cycle then destination 
	// is NOT synced (based on analysis performed by reSID)
	uint8_t srcSync= _osc[voice].wave & SYNC_BITMASK;		// for special case handling 
	uint8_t srcMsbRising= _osc[getPreviousVoice(voice)].msbRising;

	if (msbRising && destSync && !(srcSync && srcMsbRising)) {	  
		_osc[destVoice].counter = 0;
	}
}
static void advanceOscillators() {
	// forwards time by ONE sample - which corresponds to about 22 cycles.. (FIXME: optimize this brute force impl later)

/*	
	// this original HARD SYNC impl from TinySID was quite wrong .. but actually
	// one doesn't hear much of a difference to the correct one!
	if ((ctrl & SYNC_BITMASK) && (_osc[srcVoice].counter < _osc[srcVoice].freq)) {
		// sync oscillator to srcVoice if sync bit set 
		_osc[voice].counter = _osc[srcVoice].counter * _osc[voice].freq / _osc[srcVoice].freq;
	}	
*/	
	
	double c= _sid.cyclesPerSample + _sid.cycleOverflow;
	uint16_t cycles= (uint16_t)c;		
	_sid.cycleOverflow= c-cycles;

	for (uint8_t t= 0; t<cycles; t++) {
		// forward oscillators one CYCLE (required to properly time HARD SYNC)
		for (uint8_t voice=0; voice<3; voice++) {
			_osc[voice].prevCounter= _osc[voice].counter;
			
			// reset counter / noise generator if TEST bit set (blocked at 0 as long as set)
			if (isTestBit(voice)) {
				// note: test bit has no influence on the envelope generator whatsoever
				_osc[voice].counter  = 0;
				_osc[voice].noisepos = 0;
				_osc[voice].noiseval = 0x7ffff8;
			} else {
				// update wave counter
				_osc[voice].counter = (_osc[voice].counter + _osc[voice].freqC) & 0xFFFFFF;				
			}

			// base for hard sync
			_osc[voice].msbRising = (_osc[voice].counter & 0x800000) > (_osc[voice].prevCounter & 0x800000);
// old	_osc[voice].msbRising = !(_osc[voice].prevCounter & 0x800000) && (_osc[voice].counter & 0x800000);
		}
		
		// handle oscillator HARD SYNC (quality wise it isn't worth the trouble to use this correct impl..)
		for (uint8_t voice=0; voice<3; voice++) {
			syncOscillator(voice);
		}	
	}
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
* sample or better per cycle basis. But that would mean that the currently used "predictive" emulation 
* logic would need to be completely replaced..  and so far that does not seem to be worth the trouble.
*/

void sidSynthRender (int16_t *buffer, uint32_t len, int16_t **synthTraceBufs) {	
	/*
	note: TEST (Bit 3): The TEST bit, when set to one, resets and locks oscillator 1 at zero 
	until the TEST bit is cleared. The noise waveform output of oscillator 1 is also 
	reset and the pulse waveform output is held at a DC level
	*/
	syncRegisterCache();
    
#ifdef USE_FILTER
	double cutoff, resonance;
	getFilterInput(&cutoff, &resonance);	// calc once here as an optimization
#endif

	// now render the buffer
	for (uint32_t bp=0; bp<len; bp++) {		
		int32_t outo= 0, outf= 0;
		advanceOscillators();
		
		/* 
		step 2 : generate the two output signals (for filtered and non-
		         filtered) from the osc/eg sections
		*/
		for (uint8_t voice=0; voice<3; voice++) {
			uint8_t ctrl= _osc[voice].wave;
						
			uint16_t outv= 0xFFFF;
			uint8_t voiceMute= !((0x1 << voice) & _sid.voiceEnableMask);
			
			if (!voiceMute) {
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
								outv = plsout ? combinedWF(voice, _sid.PulseTriSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
							} else { // PULSE & TRIANGLE - like in Kentilla, Convincing, Clique_Baby, etc							
								// a good test is Last_Ninja:6 voice 1 at 35secs; here Hermit's original PulseSaw settings seem to 
								// be lacking here... the respective sound has none of the crispness nor volume of the original
								
								tmp = getRingModCounter(voice); 
								outv = plsout ? combinedWF(voice, _sid.PulseTri_8580, (tmp ^ (tmp & 0x800000 ? 0xFFFFFF : 0)) >> 11, 0) : 0;	// either on or off						
							}				
						} else if ((ctrl & SAW_BITMASK) && ++combined)  {	// PULSE & SAW - like in Defiler.sid
							outv = plsout ? combinedWF(voice, _sid.PulseSaw_8580, tmp >> 4, 1) : 0;	// tmp 12 MSB
						}
					}
				} else if ((ctrl & TRI_BITMASK) && (ctrl & SAW_BITMASK) && ++combined) {		// TRIANGLE & SAW - like in Garden_Party.sid
					uint32_t tmp = _osc[voice].counter >> 12;
					outv = combinedWF(voice, _sid.TriSaw_8580, tmp, 1);	// tmp 12 MSB
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
						_sid.voices[voice].prevWaveFormOut= outv;
					} else {
						// no waveform set						
						outv= _sid.voices[voice].prevWaveFormOut;		// old impl: outv &= _sid.level_DC;			
					}	
				}
			} else {
				outv= ((uint16_t)_sid.level_DC) << 8;
			}
			
			// using samples (trashed the alternative cycle based envelope-generator counter it ran into issues with Clique_Baby.sid)
			simOneEnvelopeCycle(voice);
			
			// now route the voice output to either the non-filtered or the
			// filtered channel and dont forget to blank out osc3 if desired	

			// outf and outo here end up with the sum of 3 voices..
			// envelopeOutput has 8-bit and and outv 16	(Hermit's impl based on 16-bit wave output)		
			// 16-bit wave * 8 bit envelope	-> 24bit >>8 = 16bit

// note: compared to other emus output volume it quite high.. maybe better reduce it a bit?
#define RESCALE	8				// rescale by envelopeOutput to get 16-bit output
#define RESCALE_NO_FILTER 8		
#define BASELINE 0x8000	

			int32_t voiceOut= ( (((int32_t)outv)-BASELINE) * _osc[voice].envelopeOutput );

#ifdef USE_FILTER
			// NOTE: Voice 3 is not silenced by !v3ena if it is routed through the filter!

			if (((voice<2) || _filter.v3ena || (!_filter.v3ena && _osc[voice].filter)) && !voiceMute) {
				if (_osc[voice].filter) {
					// route to filter
					outf+= voiceOut >> RESCALE;	
				} else {
					// route directly to output
					outo+= voiceOut >> RESCALE;
				}
			}
#else
			// Don't use filters, just mix all voices together
			if (!voiceMute) { 
				outf+= voiceOut >> RESCALE_NO_FILTER; 
			}
#endif
			// trace output (always make it 16-bit - in case somebody wants to play the voices separately)
			
			if (synthTraceBufs) {
				int16_t *voiceTraceBuffer= synthTraceBufs[voice];
				*(voiceTraceBuffer+bp)= (int16_t)(voiceOut >> 8);			
			}
		}

#ifdef USE_FILTER
		double output= runFilter((double)outf, (double)outo, &(_filter.prevbandpass), &(_filter.prevlowpass), cutoff, resonance);

    //    int32_t OUTPUT_SCALEDOWN = 3 * 16;	// need signed 16-bit here 
        int32_t OUTPUT_SCALEDOWN = 6 * 16;	// need signed 16-bit here 
		
		// filter volume is 4 bits/ outo is 16bits		
        int32_t finalSample= round(output * _filter.vol / OUTPUT_SCALEDOWN); // SID output
#else
		int32_t finalSample = outf;
#endif

		finalSample= digiGenPsidSample(finalSample);	// PSID stuff

		// Clipping
		const int32_t clipValue = 32767;
		if ( finalSample < -clipValue ) {
			finalSample = -clipValue;
		} else if ( finalSample > clipValue ) {
			finalSample = clipValue;
		}

		int16_t out= finalSample;
		*(buffer+bp)= out;
    }
}

/*
* Notes regarding ADSR-bug:
*
* Information about how *exactly* the original envolope generator works is still somewhat 
* sketchy (e.g. see links here: https://sourceforge.net/p/sidplay-residfp/wiki/Links/)
* The way it is implemented in resid (and here as well) is NOT necessarily "100% correct" but 
* it seems to be an adequate approximation. The infamous ADSR-bug seems to be a good illustration
* of an area that is still not 100% understood - eventhough there are fairly good theories 
* what is causing the bug and when it is likely to strike (see link above).
*
* The basic setup is this: There is a 15-bit LFSR that "increments" with each cycle and 
* eventually overflows. The user specified "attack", "decay" and "release" settings translate into 
* respective reference thresholds. The threshold of the active phase (e.g. attack) is compared 
* against the current LFSR and in case there is an *exact* match, then the envelope counter is 
* updated (i.e. increased or decreased once) and the LFSR is reset to 0 (other than the 
* automatic overflow, this is the *only* way the LFSR can ever be reset to 0).
* 
* One important aspect is that "counting and comparing" *never* stops - i.e. even when some 
* goal has been achieved (e.g. "sustain" level for "decay" phase or 0 for "release" phase). There
* is no such thing as an idle mode and either the "attack", "decay" or "release" counter is 
* always active (e.g. the "release" threshold stays active until the phase is manually switched
* to a new "attack" by setting the GATE bit).
*
* The ADSR-bug may be encountered whenever the currently active threshold is manually changed
* to a *lower* value (e.g. by setting the AD or SR registers for an active phase or by changing 
* the GATE and thereby replacing the currently used threshold): Whenever the new threshold is 
* already lower than the current LFSR content then the bug occurs: In order to reach the threshold 
* the LFSR has to first overflow and go "full cicle". In the worst case that amounts to 32k clock ticks,  
* i.e. 32ms by which the selected phase is delayed.
*
* Within a cycle-correct CPU/SID emulation respective situations can be easily identified (i.e. it is 
* just a matter of properly updating the LSFR counter for each cycle and generating the resulting
* SID output). But the challenge within this emulator is that the CPU is simulated on a cycle exact basis 
* for a certain time slot (e.g. 5ms) but the SID output synthesis then is done afterwards (not in 
* sync with the CPU emulation. This means that that eventhough the SID emulation is using a 
* cycle exact handling for its stuff (The implementation used here mimicks the approach taken by resid.)
* it can not differenciate the CPU interactions that have just been performed within the current time
* slot. This means that the SID side emulation alone will NOT detect ADSR-bug situations that are 
* directly triggered if there where multiple interactions. (e.g. when short thresholds are involved 
* the runtime of the CPU interactions within the time slot may be larger than the used threshold values, 
* e.g. the time it takes to update the SR and then switch to AD may be much longer than the new A 
* threshold and if the set R was larger, this will directly result in a triggered bug.)
*
* The below add-on hack in handleAdsrBug() tries to mitigate that blind-spot - at least for some of the
* most relevant scenarios.
*/
static uint16_t getCurrentThreshold(uint8_t voice) {
	uint16_t threshold;
	switch (_osc[voice].envphase) {
		case Attack: 
			{ threshold = _sid.env.counterPeriod[_sid.voices[voice].ad >> 4]; break; }
		case Decay: 
		case Sustain:	// keeps using the decay limit
			{ threshold = _sid.env.counterPeriod[_sid.voices[voice].ad & 0xf]; break; }
		case Release: 
			{ threshold = _sid.env.counterPeriod[_sid.voices[voice].sr & 0xf]; break; }
	}
	return threshold;
}

// util for envelope generator LFSR counter
static int32_t clocksToSamples(int32_t clocks) {
	return round(((float)clocks)/_sid.cyclesPerSample);
}

static void simGateAdsrBug(uint8_t voice, uint8_t scenario, uint16_t newRate) {
//	if (_osc[voice].adsrBugFrameCount == rsidGetFrameCount()) {		// FIXME what's the test case here? when was this used?
		uint16_t oldThreshold= getCurrentThreshold(voice);
		uint16_t newThreshold= _sid.env.counterPeriod[newRate];

		// problem/limitation of the current impl is that the internal SID state ISN'T updated in sync with
		// the CPU emulation, i.e. first the CPU is emulated for some interval (e.g. an IRQ) and then afterwards
		// the SID emulation is run for that same interval: i.e. the CPU emulation DOESN'T see the current/up-to-date
		// state of the SID and vice-versa the SID emulation does not see CPU induced changes at the correct time.
		// in the ADSR-bug context this means:
		
		// problem 1: _osc[voice].currentLFSR only reflects the state at the end of the last sid output renderung
		// but to correctly detect the bug the up-to-date counter is needed here. Workaround: supposing this here 
		// happends from an IRQ (the 80% case) then the previous SID rendering covered the time just up to the IRQ 
		// call and cpuCycles() measured the cycles that have since passend within the IRQ.
		// (note: cpuCycles() refers to the local context, i.e. it is reset for each new IRQ, etc.) 
		
		// problem 2: (correctly) the ADSR bug will occur "elapsed" (see var below) samples into the next SID output 
		// rendering, i.e. NOT right from the start. Before that the old counter would still be used. The overflow 
		// at that point would mean that a total of "limitLFSR-(currentLFSR-newThreshold)" increment steps would 
		// *then* be needed to reach the newThreshold, i.e. the "elapsed" time here would be relevant as a "correct 
		// timing" offset.

		
		if (oldThreshold > newThreshold ) {	// only a reduction may lead to an overflow

			uint32_t elapsed = clocksToSamples(cpuCycles());
			uint16_t simLSFR = (_osc[voice].currentLFSR + elapsed) % _sid.env.limitLFSR;	// forward looking position of the counter
						
			if (simLSFR >= newThreshold ) {		// ADSR BUG activated!
				// by setting currentLFSR to something equal or higher than newThreshold it is forced into "overflow territory"):
				
				// try to trigger bug for correct "overall duration" (see problem 2): when set to newThreshold then 
				// the maximum of _sid.env.limitLFSR steps would be needed to get out of the bug, any higher value will still 
				// tigger the bug but reduce the steps needed to get out of it..
				
				if (scenario != 2) {	// hack: todo investigate why this 
					_osc[voice].currentLFSR= simLSFR;	// this should be the correctly reduced bug time: newThreshold+(simLSFR-newThreshold)
				} else {
					// testcase: Eskimonika - without any bug handling the song sounds "ok" and respective bug handling rather
					//                        for scenario==2 seems to mess up the result.. (seems to be a good test for "false positive")
				}
			}		
		}
//	}	
	
//	_osc[voice].adsrBugFrameCount= rsidGetFrameCount();				
}

static void handleAdsrBug(uint8_t voice, uint8_t reg, uint8_t val) {
	// example LMan - Confusion 2015 Remix.sid: updates threshold then switches to lower threshold via GATE
	
	switch (reg) {		
	case 4: { // wave
		// scenario 1
		uint8_t oldGate= _sid.voices[voice].wave & 0x1;
		uint8_t newGate= val & 0x01;
		if (!oldGate && newGate) {
			simGateAdsrBug(voice, 0, _sid.voices[voice].ad >> 4);	// switch to 'attack'
		}
		else if (oldGate && !newGate) {
			simGateAdsrBug(voice, 1, _sid.voices[voice].sr & 0xf);	// switch to release
		}
		break;
	}
	case 5: { // new AD
		// scenario 1
		if (_osc[voice].envphase != Release) {
			simGateAdsrBug(voice, 2, val >> 4);
		}
		break;
	}
	case 6: { // new SR
		// scenario 1
		if (_osc[voice].envphase == Release) {
			simGateAdsrBug(voice, 3, val & 0xf);
		}
		break;
	}
	}
}

void sidPoke(uint8_t reg, uint8_t val)
{
    uint8_t voice=0;
	if (reg < 7) {}
    if ((reg >= 7) && (reg <=13)) {voice=1; reg-=7;}
    if ((reg >= 14) && (reg <=20)) {voice=2; reg-=14;}

    switch (reg) {		
        case 0x0: { // Set frequency: Low byte
			_sid.voices[voice].freq = (_sid.voices[voice].freq&0xff00) | val;
            break;
        }
        case 0x1: { // Set frequency: High byte
            _sid.voices[voice].freq = (_sid.voices[voice].freq&0xff) | (val<<8);
            break;
        }
        case 0x2: { // Set pulse width: Low byte
            _sid.voices[voice].pulse = (_sid.voices[voice].pulse&0x0f00) | val;
            break;
        }
        case 0x3: { // Set pulse width: High byte
            _sid.voices[voice].pulse = (_sid.voices[voice].pulse&0xff) | ((val & 0xf)<<8);
            break;
        }
        case 0x4: {
			handleAdsrBug(voice, reg, val);
			
			simStartOscillatorVoice3(voice, val);
			
			uint8_t oldGate= _sid.voices[voice].wave&0x1;
			uint8_t oldTest= _sid.voices[voice].wave&0x8;		// oscillator stop
			uint8_t newGate= val & 0x01;

			_sid.voices[voice].wave = val;
			
			if (!oldGate && newGate) {
				/* 
				If the envelope is then gated again (before the RELEASE cycle has reached 
				zero amplitude), another ATTACK cycle will begin, starting from whatever 
				amplitude had been reached.
				*/
				_osc[voice].envphase= Attack;				
				_osc[voice].zeroLock= 0;
			} else if (oldGate && !newGate) {
				/* 
				if the gate bit is reset before the envelope has finished the ATTACK cycle, 
				the RELEASE cycles will immediately begin, starting from whatever amplitude 
				had been reached
				// see http://www.sidmusic.org/sid/sidtech2.html
				*/
				_osc[voice].envphase= Release;
			}
            break;
        }
        case 0x5: {
			handleAdsrBug(voice, reg, val);
			_sid.voices[voice].ad = val;			
			break;
		}
        case 0x6: { 
			handleAdsrBug(voice, reg, val);			
			_sid.voices[voice].sr = val; 
			break;	
		}
        case 0x15: { _sid.ffreqlo = val; break; }
        case 0x16: { _sid.ffreqhi = val; break; }
        case 0x17: { _sid.resFtv = val; break; }
        case 0x18: { _sid.ftpVol = val; break;}
    }
    return;
}

static void resetEngine(uint32_t sampleRate, uint8_t isModel6581) {
	// init "sid" structures
	fillMem((uint8_t*)&_sid,0,sizeof(_sid));

    createCombinedWF(_sid.TriSaw_8580, 0.8, 2.4, 0.64); //precalculate combined waveform
    createCombinedWF(_sid.PulseSaw_8580, 1.4, 1.9, 0.68);
	createCombinedWF(_sid.PulseTriSaw_8580, 0.8, 2.5, 0.64);
	// far from "correct" but at least a bit better than Hermit's use of PulseSaw_8580 (see Last_Ninja)
    createCombinedWF(_sid.PulseTri_8580, 0.8, 1.5, 0.38);	// improved settings are welcome!

	_sid.sampleRate = sampleRate;
	
	_sid.cycleOverflow = 0;
	_sid.cyclesPerSample = ((double)envCyclesPerSec()) / sampleRate;
		
	_sid.isModel6581= isModel6581;	
	_sid.level_DC= isModel6581 ? 0x38 : 0x80;	// supposedly the DC level for respective chip model

	_sid.voiceEnableMask= 0x7;
	
	// init "filter" structures
	fillMem((uint8_t*)&_filter,0,sizeof(_filter));

    _filter.cutoff_ratio_8580 = ((double)-2.0) * 3.1415926535897932385 * (12500.0 / 256) / _sid.sampleRate,
    _filter.cutoff_ratio_6581 = ((double)-2.0) * 3.1415926535897932385 * (20000.0 / 256) / _sid.sampleRate;
//	_filter.prevbandpass = 0;	// redundant
//	_filter.prevlowpass = 0;	
	
	// init "oscillator" structures
	fillMem((uint8_t*)&_osc,0,sizeof(_osc));

	for (uint8_t i=0;i<3;i++) {
		// note: by default the rest of _sid, _osc & _filter 
		// above is set to 0
		_osc[i].envphase= Release;
		_osc[i].zeroLock= 1;
		_osc[i].noiseval = 0x7ffff8;
		
//		_osc[i].adsrBugFrameCount= 0;
	}
}

static void resetEnvelopeGenerator(uint32_t sampleRate) {
	struct EnvGenerator *env= &(_sid.env);

	// The ATTACK rate determines how rapidly the output of a voice rises from zero to peak amplitude when 
	// the envelope generator is gated (time/cycle in ms). The respective gradient is used whether the attack is 
	// actually started from 0 or from some higher level. The terms "attack time", "decay time" and 
	// "release time" are actually very misleading here, since all they translate into are actually 
	// some fixed gradients, and the actual decay or release may complete much sooner, e.g. depending
	// on the selected "sustain" level.
	// note: decay/release times are 3x longer (implemented via exponential_counter)
	const int32_t attackTimes[16]  =	{
		2, 8, 16, 24, 38, 56, 68, 80, 100, 240, 500, 800, 1000, 3000, 5000, 8000
	};
	
	/* 
	in regular SID, 15-bit LFSR counter counts cpu-clocks, our problem is the lack of 
	cycle by cycle SID emulation (we only have a SID snapshot every ~20ms to work with) 
	during rendering our computing granularity then is 'one sample' (not 'one cpu cycle'
	- but around 20).. instead of still trying to simulate a 15-bit cycle-counter we 
	may directly use a sample-counter (which also reduces rounding issues).
	*/
	uint16_t i;
	env->limitLFSR= floor(((float)0x8000)/_sid.cyclesPerSample);	// original counter was 15-bit
	for (i=0; i<16; i++) {
		// counter must reach respective threshold before envelope value is incremented/decremented								
		// note: attack times are in millis & there are 255 steps for envelope..
		env->counterPeriod[i]= (int32_t)floor(((double)envCyclesPerSec())/(255*1000) * attackTimes[i] / _sid.cyclesPerSample)+1;	// in samples
	}
	// lookup table for decay rates
	uint8_t from[] =  {93, 54, 26, 14,  6,  0};
	uint8_t val[] = { 1,  2,  4,  8, 16, 30};
	for (i= 0; i<256; i++) {
		uint8_t q= 1;
		for (uint8_t j= 0; j<6; j++) {
			if (i>from[j]) {
				q= val[j];
				break;
			}
		}
		env->exponentialDelays[i]= q;
	}	
}

void sidReset(uint32_t sampleRate, uint8_t isModel6581, uint8_t compatibility) {
	resetEngine(sampleRate, isModel6581);
	resetEnvelopeGenerator(sampleRate);

	digiReset(compatibility, isModel6581);
}

// -----------------------------  SID I/O -------------------------------------------

uint8_t sidReadMem(uint16_t addr) {
	switch (addr) {
	case 0xd41c:					
		/* 
		used by Alien.sid to set filter cutoff freq(hi): unfortunately the 
		filter impl seems to be rather shitty.. and if the actual envelopeOutput
		is used, then the filter will almost mute the signal (refers to original TinySid filter)
		*/
		return _osc[2].envelopeOutput*4/5+20;	// use hack to avoid the worst
	case 0xd41b:
		if (useOscPollingHack()) { return simReadD41B(); }					
		return memReadIO(addr);
	}
	return memReadIO(addr);
}

void sidWriteMem(uint16_t addr, uint8_t value) {
	if (!digiDetectSample(addr, value)) {
		sidPoke(addr&0x1f, value);							
		memWriteIO((addr&0xfc1f), value);		
	}
}

