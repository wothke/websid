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
 * <li>Hermit's jsSID.js provided a variant of "resid" filter implementation, an "anti aliasing" 
 *             for "pulse" and "saw" waveforms, and a rather clever approach to
 *             generate combined waveforms (see http://hermit.sidrip.com/jsSID.html)
 * </ul> 
 *
 * <p>Tiny'R'Sid add-ons (c) 2017 J.Wothke
 * <p>version 0.82
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

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

#define USE_FILTER
#define ANTIALIAS_WAVE

// integer based filter implementation originally used in the old "tinysid": supposedly this
// was a performance optimization meant to avoid "expensive" floating point calculations.
// It seems there is no point in using this optimization for the JavaScript version and as compared to 
// the non-optimized version it actually seems to be flawed as well:
//#define OLD_TINYSID_FILTER				

// FIXME: broken.. Clique_Baby.sid does not work in "cycle mode" .. why?
// switch between 'cycle' or 'sample' based envelope-generator counter 
// (performance wise it does not seem to make much difference)
#define USE_SAMPLE_ENV_COUNTER


// SID register definition
struct mosSid {
	uint8_t isModel6581;
	uint8_t level_DC;
	uint32_t  sampleRate;	// e.g. 44100
	uint16_t  multiplicator;

	uint8_t voiceEnableMask;	// allows to mute certain voices (3 low bits mask)

    struct voice {
        uint16_t freq;
        uint16_t pulse;
        uint8_t wave;
        uint8_t ad;
        uint8_t sr;
		
		// add-ons snatched from Hermit's implementation
		double prevwavdata;
		uint8_t prevwfout;
    } voices[3];
    uint8_t ffreqlo;
    uint8_t ffreqhi;
    uint8_t resFtv;
    uint8_t ftpVol;
	
	struct envGenerator {	// envelope generator stuff
		int32_t limitLFSR; // the original cycle counter would be 15-bit (here samples are counted & counter is rescaled accordingly)
		int32_t counterPeriod[16];
		uint8_t exponentialDelays[256];

		float cyclesPerSample;
		float cycleOverflow;
    } env;
	
	// Hermit's precalculated "combined waveforms" 
	double TriSaw_8580[4096];
	double PulseSaw_8580[4096];
	double PulseTriSaw_8580[4096];
};

static struct mosSid sid;

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

struct simosc3 {
	uint8_t waveform;
	
	// more recent hack for main loop polling
	uint32_t baseCycles;
	uint32_t counter;
	uint32_t multiplicator;	
};
static struct simosc3 osc3;

static void simStartOscillatorVoice3(uint8_t voice, uint8_t val) {
	if ((voice == 2) && useOscPollingHack()) {
		osc3.waveform= val & 0xf0;

		// hack: use only for main/pulse cases like Instantfunk.sid
		osc3.baseCycles= cpuTotalCycles();
		osc3.counter= 0; 
		
		// for some reason the playback is slightly slower than in ACID64
		osc3.multiplicator= sid.multiplicator * envNumberOfSamplesPerCall() / envCyclesPerScreen();
	}
}
static uint32_t simOsc3Counter() {
	// voice 3 oscillator counter based on elapsed time
	uint32_t diff= cpuTotalCycles() - osc3.baseCycles;
	osc3.baseCycles= cpuTotalCycles();

	uint32_t f= ((uint32_t)sid.voices[2].freq) * osc3.multiplicator * diff;		
	osc3.counter= (osc3.counter + f) & 0xfffffff;
	return osc3.counter;
}

static uint8_t simReadSawtoothD41B() {
	// simulate sawtooth voice 3 oscillator level based on elapsed time	
	// (handle busy polling for sid oscillator3 - e.g. Ring_Ring_Ring.sid)

	return (uint8_t) (simOsc3Counter() >> 20);
}

static uint8_t simReadPulsedD41B() {
	// simulate pulse voice 3 oscillator level based on elapsed time	
	uint32_t p= (((uint32_t)sid.voices[2].pulse) & 0xfff) << 16;
	return (simOsc3Counter() > p) ? 0 : 1;
}

static uint8_t simReadD41B() {
	if (osc3.waveform == 0x40) {
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
struct sidOscillator {
    uint32_t freq;
    uint32_t pulse;
    uint8_t wave;
    uint8_t filter;
    uint32_t attack;	// for 255 steps
    uint32_t decay;		// for 255 steps
    uint32_t sustain;
    uint32_t release;
    uint32_t counter;		// 28-bit

	// updated envelope generation based on reSID
	uint8_t envelopeOutput;
	int16_t currentLFSR;	// sim counter	(continuously counting / only reset by AD(S)R match)
	uint8_t zeroLock;  
	uint8_t exponentialCounter;    
	
    uint8_t envphase;
    uint32_t noisepos;
    uint32_t noiseval;
    uint8_t noiseout;
	
	// detection of ADSR-bug conditions
	uint32_t adsrBugTriggerTime;
	uint32_t adsrBugFrameCount;
};

// internal filter def
struct sidFilter {
    uint8_t  lowEna;
	uint8_t  bandEna;
    uint8_t  hiEna;
    uint8_t  v3ena;
    int32_t vol;
		
#ifdef OLD_TINYSID_FILTER
	// old tinysid filter implementation based on integer arithmetic
	int32_t  multiplicator;	// since multi-cycle steps are simulated

    int32_t freq;
    int32_t rez;
    int32_t h;
    int32_t b;
    int32_t l;
#else
	// "resid" based implementation derived from Hermit's version: see http://hermit.sidrip.com/jsSID.html
	double prevlowpass;
    double prevbandpass;
	double cutoff_ratio_8580;
    double cutoff_ratio_6581;
#endif
};

uint32_t sidGetSampleFreq() {
	return sid.sampleRate;
}
uint8_t sidGetWave(uint8_t voice) {
	return sid.voices[voice].wave;
}
uint8_t sidGetAD(uint8_t voice) {
	return sid.voices[voice].ad;	
}
uint8_t sidGetSR(uint8_t voice) {
	return sid.voices[voice].sr;	
}
uint16_t sidGetFreq(uint8_t voice) {
	return sid.voices[voice].freq;
}
uint16_t sidGetPulse(uint8_t voice) {
	return sid.voices[voice].pulse;
}

static struct sidOscillator osc[3];
static struct sidFilter filter;

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
		sid.voiceEnableMask &= ~(1 << voice);	// disable voice
	else 
		sid.voiceEnableMask |= (1 << voice);	// enable voice
}


// util related to envelope generator LFSR counter
static int32_t clocksToSamples(int32_t clocks) {
#ifdef USE_SAMPLE_ENV_COUNTER
	return round(((float)clocks)/sid.env.cyclesPerSample)+1;
#else
	return clocks;
#endif
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
	osc[voice].exponentialCounter+= 1;
	
	uint8_t result= (osc[voice].exponentialCounter >= sid.env.exponentialDelays[osc[voice].envelopeOutput]);
	if (result) {
		osc[voice].exponentialCounter= 0;	// reset to start next round
	}
	return result;
}

static void simOneEnvelopeCycle(uint8_t voice) {
	/* 
	now process the volume according to the phase and adsr values (explicit switching of ADSR 
	phase is handled in sidPoke() so there is no need to handle that here)

	advance envelope LFSR counter (originally this would be a 15-bit cycle counter.. but we 
	may be counting samples here)

	ADSR bug scenario: normally the maximum thresholds used for the original 15-bit counter 
	would have been around 0x7a13 (i.e. somewhat below the 7fff range that can be handled by 
	the counter). For certain bug scenarios it is possible that the threshold is missed and 
	the counter keeps counting until it again reaches the threshold after a wrap-around.. 
	(see sidPoke() for specific ADSR-bug handling)
	*/
	if (++osc[voice].currentLFSR == sid.env.limitLFSR) {
		osc[voice].currentLFSR= 0;
	}
	
	uint8_t previousEnvelopeOutput = osc[voice].envelopeOutput;
				
	switch (osc[voice].envphase) {
		case Attack: {                          // Phase 0 : Attack
			if (triggerLFSR_Threshold(osc[voice].attack, &osc[voice].currentLFSR)) {	
				// inc volume when threshold is reached						
				if (!osc[voice].zeroLock) {
					if (osc[voice].envelopeOutput < 0xff) {
						/* see Alien.sid: "full envelopeOutput level" GATE off/on sequences 
						   within same IRQ will cause undesireable overflow.. this might not 
						   be a problem in cycle accurate emulations.. but here it is (we 
						   only see a 20ms snapshot)
						*/
						osc[voice].envelopeOutput= (osc[voice].envelopeOutput + 1) & 0xff;	// increase volume
					}							
				
					osc[voice].exponentialCounter = 0;

					if (osc[voice].envelopeOutput == 0xff) {
						osc[voice].envphase = Decay;
					}							
				}
			}
			break;
		}
		case Decay: {                   	// Phase 1 : Decay      
			if (triggerLFSR_Threshold(osc[voice].decay, &osc[voice].currentLFSR) 
				&& handleExponentialDelay(voice)) { 	// dec volume when threshold is reached
				
				if (!osc[voice].zeroLock) {
					if (osc[voice].envelopeOutput != osc[voice].sustain) {
						osc[voice].envelopeOutput= (osc[voice].envelopeOutput - 1) & 0xff;	// decrease volume
					} else {
						osc[voice].envphase = Sustain;
					}
				}	
			}
			break;
		}
		case Sustain: {                        // Phase 2 : Sustain
			triggerLFSR_Threshold(osc[voice].decay, &osc[voice].currentLFSR);	// keeps using the decay threshold!
		
			if (osc[voice].envelopeOutput != osc[voice].sustain) {
				osc[voice].envphase = Decay;
			}
			break;
		}					
		case Release: {                          // Phase 3 : Release
			// this phase must be explicitly triggered by clearing the GATE bit..
			if (triggerLFSR_Threshold(osc[voice].release, &osc[voice].currentLFSR) 
				&& handleExponentialDelay(voice)) { 		// dec volume when threshold is reached

				if (!osc[voice].zeroLock) {				
					osc[voice].envelopeOutput= (osc[voice].envelopeOutput - 1) & 0xff;	// decrease volume
				}
			}						
			break;
		}
	}
	if ((osc[voice].envelopeOutput == 0) && (previousEnvelopeOutput > osc[voice].envelopeOutput)) {
		osc[voice].zeroLock = 1;	// new "attack" phase must be started to unlock
	}			
}	  

// Hermit's impl to calculate combined waveforms (check his jsSID-0.9.1-tech_comments in commented jsSID.js for background info): 
// I did not thoroughly check how well this really works (it works well enough for Kentilla and Clique_Baby apparently has to sound as shitty as it does)
static uint8_t combinedWF(uint8_t channel, double *wfarray, uint16_t index, uint8_t differ6581) { //on 6581 most combined waveforms are essentially halved 8580-like waves
	if (differ6581 && sid.isModel6581) index &= 0x7FF;
	double combiwf = (wfarray[index] + sid.voices[channel].prevwavdata) / 2;
	sid.voices[channel].prevwavdata = wfarray[index];
	
//	return combiwf; // original impl uses 16-bit 
	return ((uint32_t)round(combiwf)) >> 8;
}

static void createCombinedWF(double *wfarray, double bitmul, double bitstrength, double treshold) { //I found out how the combined waveform works (neighboring bits affect each other recursively)
	for (uint16_t i = 0; i < 4096; i++) {
		wfarray[i] = 0; //neighbour-bit strength and DAC MOSFET treshold is approximately set by ears'n'trials
		for (uint8_t j = 0; j < 12; j++) {
			double bitlevel = 0;
			for (uint8_t k = 0; k < 12; k++) {
				bitlevel += (bitmul / pow(bitstrength, abs(k - j))) * (((i >> k) & 1) - 0.5);
			}
			wfarray[i] += (bitlevel >= treshold) ? pow(2, j) : 0;
		}
		wfarray[i] *= 12;
	}
}

inline void sidFilterSamples (uint8_t *digiBuffer, uint32_t len, int8_t voice)
{
	// notice: depending on the used sample playback implementation, respectieve digi samples 
	// might normally be processed by the filter (e.g. for PWM - but not for D418).. to deal 
	// with this correctly the emulation would need to track how a sample was created (and by which 
	// voice) and the sample data would need to be merged into the regular SID output before 
	// the filter is applied... this emulator does NOT support this yet - and therefore there
	// is no point in trying anything fancy here.
}

#define isTestBit(voice) osc[voice].wave&TEST_BITMASK

inline uint8_t createTriangleOutput(uint8_t voice, uint32_t ringMSB) {
	uint8_t tripos = (uint8_t) (osc[voice].counter>>19);		// use top 9 bits of oscillator
	uint8_t triout= (osc[voice].counter>>27) ? tripos^0xff : tripos;
	if (ringMSB) { triout^= 0xff; }	// modulate triangle wave if ringmod bit set 
	return triout;
}			
inline uint8_t createSawOutput(uint8_t voice) {	// test with Alien or Kawasaki_Synthesizer_Demo
#ifndef ANTIALIAS_WAVE
	// old impl
	uint8_t sawout = (uint8_t) (osc[voice].counter >> 20);	// just use top 8-bits of our 28bit counter 
	return sawout;
#else
	// Hermit's "anti-aliasing" (note: this impl is based on 28-bit counter where Hermit uses 24-bit)
	uint32_t wfout = osc[voice].counter >> 12;	// top 16-bits
	double step = ((double)(osc[voice].freq>>4)) / 0x1200000;
	wfout += round(wfout * step);
	if (wfout > 0xFFFF) wfout = 0xFFFF - round (((double)(wfout - 0x10000)) / step);

	return (wfout >> 8) & 0xff;
#endif
}	
inline uint8_t createPulseOutput(uint8_t voice) {
#ifndef ANTIALIAS_WAVE
	// old impl
	uint8_t plsout = isTestBit(voice) ? sid.level_DC : (uint8_t) ((osc[voice].counter > osc[voice].pulse)-1) ^ 0xff;
	return plsout;
#else
	// Hermit's "anti-aliasing" (note: this impl is based on 28-bit counter where Hermit uses 24-bit)
	if (isTestBit(voice)) return 0xFF;	// pulse start position
	
	uint32_t pw = osc[voice].pulse >> 12;	// 16 bits pulse expected (but we have 28)
	uint32_t tmp = osc[voice].freq >> 13;	// our freq is for 28bit counter - not 24bit as in Hermit's impl
	if (0 < pw && pw < tmp) pw = tmp;
	tmp ^= 0xFFFF;
	if (pw > tmp) pw = tmp;
	tmp = osc[voice].counter >> 12;			// 28bits not 24
	double step = 256 / (osc[voice].freq >> 20); //simple pulse, most often used waveform, make it sound as clean as possible without oversampling

	int32_t wfout;
	if (tmp < pw) {
		wfout = round((0xFFFF - pw) * step);
		if (wfout > 0xFFFF) wfout = 0xFFFF;
		wfout = wfout - round((pw - tmp) * step);
		if (wfout < 0) wfout = 0;
	} //rising edge
	else {
		wfout = pw * step;
		if (wfout > 0xFFFF) wfout = 0xFFFF;
		wfout = round((0xFFFF - tmp) * step) - wfout;
		if (wfout >= 0) wfout = 0xFFFF;
		wfout &= 0xFFFF;
	} //falling edge	
	return (wfout >> 8) & 0xff;		// todo: might optimize Hermit's above impl to directly produce 8-bit
#endif
}	
inline uint8_t createNoiseOutput(uint8_t voice) {
	// generate noise waveform exactly as the SID does. 			
	if (osc[voice].noisepos!=(osc[voice].counter>>23))	
	{
		osc[voice].noisepos = osc[voice].counter >> 23;	
		osc[voice].noiseval = (osc[voice].noiseval << 1) |
				(getBit(osc[voice].noiseval,22) ^ getBit(osc[voice].noiseval,17));
				
		// impl consistent with: http://www.sidmusic.org/sid/sidtech5.html
		// doc here is probably wrong: http://www.oxyron.de/html/registers_sid.html
		osc[voice].noiseout = (getBit(osc[voice].noiseval,22) << 7) |
				(getBit(osc[voice].noiseval,20) << 6) |
				(getBit(osc[voice].noiseval,16) << 5) |
				(getBit(osc[voice].noiseval,13) << 4) |
				(getBit(osc[voice].noiseval,11) << 3) |
				(getBit(osc[voice].noiseval, 7) << 2) |
				(getBit(osc[voice].noiseval, 4) << 1) |
				(getBit(osc[voice].noiseval, 2) << 0);
	}
	return osc[voice].noiseout;
}

#ifdef OLD_TINYSID_FILTER	
/* Routines for quick & dirty float calculation */
static inline int32_t pfloatConvertFromInt(int32_t i) 		{ return (i<<16); }
static inline int32_t pfloatConvertFromFloat(float f) 		{ return (int32_t)(f*(1<<16)); }
static inline int32_t pfloatMultiply(int32_t a, int32_t b)	{ return (a>>8)*(b>>8); }
static inline int32_t pfloatConvertToInt(int32_t i) 		{ return (i>>16); }
#endif

/*
* While "sid" data structure is automatically kept in sync by the emulator's memory access 
* implementation, the "osc" and "filter" helper structures are NOT - i.e. they need to 
* be explicitly synced before calculating SID output.
*/
inline void syncRegisterCache() {
    /* 
	"step 1: convert the not easily processable sid registers into some
            more convenient and fast values (makes the thing much faster
            if you process more than 1 sample value at once)"
	*/
	struct envGenerator *env= &(sid.env);

    for (uint8_t voice=0;voice<3;voice++) {
        osc[voice].pulse   = (sid.voices[voice].pulse & 0xfff) << 16;
        osc[voice].filter  = getBit(sid.resFtv, voice);
		// threshold to be reached before incrementing volume
        osc[voice].attack  = env->counterPeriod[sid.voices[voice].ad >> 4];
        osc[voice].decay   = env->counterPeriod[sid.voices[voice].ad & 0xf];
		uint8_t sustain= sid.voices[voice].sr >> 4;
        osc[voice].sustain = sustain<<4 | sustain;
        osc[voice].release = env->counterPeriod[sid.voices[voice].sr & 0xf];
        osc[voice].wave    = sid.voices[voice].wave;

        osc[voice].freq    = ((uint32_t)sid.voices[voice].freq) * sid.multiplicator;
    }
#ifdef USE_FILTER
	filter.lowEna = getBit(sid.ftpVol,4);	// lowpass
	filter.bandEna = getBit(sid.ftpVol,5);	// bandpass
	filter.hiEna = getBit(sid.ftpVol,6);	// highpass
	filter.v3ena = !getBit(sid.ftpVol,7);	// chan3 off
	filter.vol   = (sid.ftpVol & 0xf);

	#ifdef OLD_TINYSID_FILTER			
		/* 
		"[this implementation] isnt correct at all - the problem is that the filter
		works only up to rmxfreq/4 - this is sufficient for 44KHz but isnt
		for 32KHz and lower - well, but sound quality is bad enough then to
		neglect the fact that the filter doesnt come that high ;)"
		*/
		filter.freq  = ((sid.ffreqhi << 3) + (sid.ffreqlo&0x7)) * filter.multiplicator;
		filter.freq <<= 1;

		if (filter.freq>pfloatConvertFromInt(1)) { 
			filter.freq=pfloatConvertFromInt(1);
		}

		/* We precalculate part of the quick float operation, saves time in loop later */
		filter.rez   = (pfloatConvertFromFloat(1.2f) - pfloatConvertFromFloat(0.04f)*(sid.resFtv >> 4)) >> 8;	
	#endif  
#endif  
}

inline void updateOscillator(uint8_t refosc, uint8_t voice) {
	uint8_t ctrl= osc[voice].wave;

	// reset counter / noise generator if TEST bit set (blocked at 0 as long as set)
	if (isTestBit(voice)) {
		// note: test bit has no influence on the envelope generator whatsoever
		osc[voice].counter  = 0;
		osc[voice].noisepos = 0;
		osc[voice].noiseval = 0xffffff;
	} else {
		// update wave counter
		osc[voice].counter = (osc[voice].counter + osc[voice].freq) & 0xFFFFFFF;				
	}
	if ((ctrl & SYNC_BITMASK) && (osc[refosc].counter < osc[refosc].freq)) {
		// sync oscillator to refosc if sync bit set 
		osc[voice].counter = osc[refosc].counter * osc[voice].freq / osc[refosc].freq;
	}	
}

/* 
* Render a buffer of n samples using the current SID register contents.
*
* KNOWN LIMITATIONS: This impl is based on a specific starting point "snapshot" of the SID registers
* and it is NOT aware of how this SID state was reached - nor any related timing information. At this
* point the CPU/CIA/VIC emulation has already been completed for the respective time interval.
* Conversely the SID's oscillators are only updated within the below function, i.e. the previously run 
* CPU/CIA/VIC emulation will NOT correctly see respective oscillator state (the above "simosc3" hack 
* was introduced to address resulting problems in selected scenarios, i.e. "main loop polling for 
* voice 3 oscillator"). 
*
* To properly avoid respective issues the complete emulation would need to be performed on a per 
* sample or better per cycle basis. But that would mean that the currently used "predictive" emulation 
* logic would need to be completely replaced..  and so far that does not seem to be worth the trouble.
*/
void sidSynthRender (int16_t *buffer, uint32_t len) {
	/*
	note: TEST (Bit 3): The TEST bit, when set to one, resets and locks oscillator 1 at zero 
	until the TEST bit is cleared. The noise waveform output of oscillator 1 is also 
	reset and the pulse waveform output is held at a DC level
	*/
	syncRegisterCache();
    
	// now render the buffer
	for (uint32_t bp=0; bp<len; bp++) {		
		int32_t outo= 0, outf= 0;
		
		/* 
		step 2 : generate the two output signals (for filtered and non-
		         filtered) from the osc/eg sections
		*/
		for (uint8_t voice=0; voice<3; voice++) {
			uint8_t ctrl= osc[voice].wave;
						
			uint8_t refosc = voice ? voice-1 : 2;  // reference oscillator for sync/ring (always the "previous" voice)
			updateOscillator(refosc, voice);
			
			uint32_t ringMSB= 0;
			if ((ctrl & RING_BITMASK) && (osc[refosc].counter < 0x8000000)) {
				ringMSB= 0x8000000;
			}
			
			// generate waveforms with really simple algorithms (note: correctly "Saw/Triangle should start from 0, 
			// Pulse should start from FF" )

			uint8_t outv=0xFF;
			uint8_t voiceMute= !((0x1 << voice) & sid.voiceEnableMask);
			
			if (!voiceMute) {
				int8_t combined= 0;

				// use special handling for certain combined waveforms
				uint8_t plsout;
				if ((ctrl & PULSE_BITMASK)) {
					plsout= createPulseOutput(voice);

					if ((ctrl & TRI_BITMASK) && ++combined)  {
						if (ctrl & SAW_BITMASK) {	// PULSE & TRIANGLE & SAW - XXX test case?
							uint32_t tmp = osc[voice].counter >> 16;	// top 12-bits
							outv &= plsout ? combinedWF(voice, sid.PulseTriSaw_8580, tmp, 1) : 0;
						} else { // PULSE & TRIANGLE - songs like Kentilla, Convincing, Clique_Baby, etc
							uint32_t tmp = osc[voice].counter ^ (ctrl & RING_BITMASK ? ringMSB : 0);
							outv &= plsout ? combinedWF(voice, sid.PulseSaw_8580, (tmp ^ (tmp & 0x8000000 ? 0xFFFFFFF : 0)) >> 15, 0) : 0;	// either on or off						
						}				
					} else if ((ctrl & SAW_BITMASK) && ++combined)  {	// PULSE & SAW - XXX test case?
						uint32_t tmp = osc[voice].counter >> 16;	// top 12-bits
						outv &= plsout ? combinedWF(voice, sid.PulseSaw_8580, tmp, 1) : 0;
					}
				} else if ((ctrl & TRI_BITMASK) && (ctrl & SAW_BITMASK) && ++combined) {		// TRIANGLE & SAW - XXX test case?
					uint32_t tmp = osc[voice].counter >> 16;	// top 12-bits
					outv &= combinedWF(voice, sid.TriSaw_8580, tmp, 1);
				} 
				if (!combined) {
					/* for the rest mix the oscillators with an AND operation as stated in
						the SID's reference manual - even if this is quite wrong. */
				
					if (ctrl & TRI_BITMASK)  outv &= createTriangleOutput(voice, ringMSB);					
					if (ctrl & SAW_BITMASK)  outv &= createSawOutput(voice);
					if (ctrl & PULSE_BITMASK) outv &= plsout;
					if (ctrl & NOISE_BITMASK)  outv &= createNoiseOutput(voice);
					
					if (ctrl & 0xf0) {
						sid.voices[voice].prevwfout= outv;
					} else {
						// no waveform set						
						outv= sid.voices[voice].prevwfout;		// old impl: outv &= sid.level_DC;			
					}	
				}
			} else {
				outv= sid.level_DC;
			}
			
#ifdef USE_SAMPLE_ENV_COUNTER
			// using samples
			simOneEnvelopeCycle(voice);
#else
			float c= sid.env.cyclesPerSample + sid.env.cycleOverflow;
			uint16_t cycles= (uint16_t)c;		
			sid.env.cycleOverflow= c-cycles;
			
			for (int32_t i= 0; i<cycles; i++) {
				simOneEnvelopeCycle(voice);
			}	  
#endif
			// now route the voice output to either the non-filtered or the
			// filtered channel and dont forget to blank out osc3 if desired	


// envelopeOutput and outv have 8-bit.. but after >>6 the result here is 10-bit.. WHY?
// finally outf and outo here end of with the sum of 3 voices..
#ifdef OLD_TINYSID_FILTER
	// old impl based on 8-bit wave output
	#define RESCALE >>6
#else
	// Hermit's impl based on 16-bit wave output
	#define RESCALE
#endif
			
#ifdef USE_FILTER
			if (((voice<2) || filter.v3ena) && !voiceMute) {							
				if (osc[voice].filter) {
					outf+=( ((int32_t)(outv-0x80)) * (int32_t)((osc[voice].envelopeOutput)) ) RESCALE;
				} else {
					outo+=( ((int32_t)(outv-0x80)) * (int32_t)((osc[voice].envelopeOutput)) ) RESCALE;
				}
			}
#else
			// Don't use filters, just mix all voices together
			if (!voiceMute) outf+= (int32_t)(((int16_t)(outv-0x80)) * (osc[voice].envelopeOutput)); 
#endif
		}

#ifdef USE_FILTER
	#ifndef OLD_TINYSID_FILTER	
	
		// variant of "resid" implementation used by Hermit:
        //"FILTER: two integrator loop bi-quadratic filter, workings learned from resid code, but I kindof simplified the equations
        //The phases of lowpass and highpass outputs are inverted compared to the input, but bandpass IS in phase with the input signal.
        //The 8580 cutoff frequency control-curve is ideal, while the 6581 has a threshold, and below it outputs a constant lowpass frequency."
        double cutoff = ((double)(sid.ffreqlo & 7)) / 8 +  sid.ffreqhi + 0.2;	// why the +0.2 ?
		double resonance;
		
        if (!sid.isModel6581) {
            cutoff = 1 - exp(cutoff * filter.cutoff_ratio_8580);
            resonance = pow(2, ((double)(4 - (sid.resFtv >> 4)) / 8));
        } else {
            if (cutoff < 24) cutoff = 0.035;
            else cutoff = (double)1 - 1.263 * exp(cutoff * filter.cutoff_ratio_6581);
            resonance = (sid.resFtv > 0x5F) ? (double)8 / (sid.resFtv >> 4) : 1.41;
        }
        double tmp = (double)(outf<<8) + filter.prevbandpass * resonance + filter.prevlowpass;	// outf: use 16-bit as in Hermit's player
				
        if (filter.hiEna) 
			outo -= ((int32_t)round(tmp)) >>8;
        tmp = filter.prevbandpass - tmp * cutoff;
        filter.prevbandpass = tmp;
        if (filter.bandEna) 
			outo -= ((int32_t)round(tmp)) >>8;
        tmp = filter.prevlowpass + tmp * cutoff;
        filter.prevlowpass = tmp;
        if (filter.lowEna) 
			outo += ((int32_t)round(tmp)) >>8;

        int32_t OUTPUT_SCALEDOWN = 3 * 16;	// need signed 16-bit here (unlike -1..1 range in resid)
        int32_t finalSample= outo* filter.vol / OUTPUT_SCALEDOWN ; // SID output
	#else
		// old tinysid implementation:
		/*
		step 3
		so, now theres finally time to apply the multi-mode resonant filter
		to the signal. The easiest thing is just modelling a real electronic
		filter circuit instead of fiddling around with complex IIRs or even
		FIRs ...
		it sounds as good as them or maybe better and needs only 3 MULs and
		4 ADDs for EVERYTHING. SIDPlay uses this kind of filter, too, but
		Mage messed the whole thing completely up - as the rest of the
		emulator.
		This filter sounds a lot like the 8580, as the low-quality, dirty
		sound of the 6581 is uuh too hard to achieve :) 
		*/
		filter.h = pfloatConvertFromInt(outf) - (filter.b>>8)*filter.rez - filter.l;
		filter.b += pfloatMultiply(filter.freq, filter.h);
		filter.l += pfloatMultiply(filter.freq, filter.b);

		if (filter.lowEna || filter.bandEna || filter.hiEna) {	
			// voice may be routed through filter without actually using any 
			// filters.. e.g. Dancing_in_the_Moonshine.sid
			outf = 0;
			
			if (filter.lowEna) outf+=pfloatConvertToInt(filter.l);
			if (filter.bandEna) outf+=pfloatConvertToInt(filter.b);
			if (filter.hiEna) outf+=pfloatConvertToInt(filter.h);
		}		

		int32_t finalSample = (filter.vol*(outo+outf));
	#endif
#else
		int32_t finalSample = outf>>2;
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
* it seems to be an adequate approximation. The infamous ADSR-bug seems to be good illustration
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

uint16_t getCurrentThreshold(uint8_t voice) {
	uint16_t threshold;
	switch (osc[voice].envphase) {
		case Attack: 
			{ threshold = sid.env.counterPeriod[sid.voices[voice].ad >> 4]; break; }
		case Decay: 
		case Sustain:	// keeps using the decay limit
			{ threshold = sid.env.counterPeriod[sid.voices[voice].ad & 0xf]; break; }
		case Release: 
			{ threshold = sid.env.counterPeriod[sid.voices[voice].sr & 0xf]; break; }
	}
	return threshold;
}

void simGateAdsrBug(uint8_t voice, uint16_t newRate) {
	if (osc[voice].adsrBugFrameCount == rsidGetFrameCount()) {
		uint16_t oldThreshold= getCurrentThreshold(voice);
		uint16_t newThreshold= sid.env.counterPeriod[newRate];
		
		if (oldThreshold > newThreshold ) {
			// only reduction may lead to overflow
			
			uint32_t elapsed = clocksToSamples(cpuCycles() - osc[voice].adsrBugTriggerTime);
			uint16_t lsfr = elapsed % oldThreshold;	// not correct (ignores lsfr start state)
			
			if (lsfr > newThreshold ) {	
				// ADSR BUG activated
				osc[voice].currentLFSR= newThreshold;	// force overflow
			}		
		}
	}	
	osc[voice].adsrBugTriggerTime= cpuCycles();
	osc[voice].adsrBugFrameCount= rsidGetFrameCount();				
}

void handleAdsrBug(uint8_t voice, uint8_t reg, uint8_t val) {
	// example LMan - Confusion 2015 Remix.sid: updates threshold then switches to lower threshold via GATE
	
	switch (reg) {		
	case 4: { // wave
		// scenario 1
		uint8_t oldGate= sid.voices[voice].wave & 0x1;
		uint8_t newGate= val & 0x01;
		if (!oldGate && newGate) {
			simGateAdsrBug(voice, sid.voices[voice].ad >> 4);	// switch to 'attack'
		}
		else if (oldGate && !newGate) {
			simGateAdsrBug(voice, sid.voices[voice].sr & 0xf);	// switch to release
		}
		break;
	}
	case 5: { // new AD
		// scenario 1
		if (osc[voice].envphase != Release) {
			simGateAdsrBug(voice, val >> 4);
		}
		break;
	}
	case 6: { // new SR
		// scenario 1
		if (osc[voice].envphase == Release) {
			simGateAdsrBug(voice, val & 0xf);
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
			sid.voices[voice].freq = (sid.voices[voice].freq&0xff00) | val;
            break;
        }
        case 0x1: { // Set frequency: High byte
            sid.voices[voice].freq = (sid.voices[voice].freq&0xff) | (val<<8);
            break;
        }
        case 0x2: { // Set pulse width: Low byte
            sid.voices[voice].pulse = (sid.voices[voice].pulse&0x0f00) | val;
            break;
        }
        case 0x3: { // Set pulse width: High byte
            sid.voices[voice].pulse = (sid.voices[voice].pulse&0xff) | ((val & 0xf)<<8);
            break;
        }
        case 0x4: {
			handleAdsrBug(voice, reg, val);
			
			simStartOscillatorVoice3(voice, val);
			
			uint8_t oldGate= sid.voices[voice].wave&0x1;
			uint8_t oldTest= sid.voices[voice].wave&0x8;		// oscillator stop
			uint8_t newGate= val & 0x01;

			sid.voices[voice].wave = val;
			
			if (!oldGate && newGate) {
				/* 
				If the envelope is then gated again (before the RELEASE cycle has reached 
				zero amplitude), another ATTACK cycle will begin, starting from whatever 
				amplitude had been reached.
				*/
				osc[voice].envphase= Attack;				
				osc[voice].zeroLock= 0;
			} else if (oldGate && !newGate) {
				/* 
				if the gate bit is reset before the envelope has finished the ATTACK cycle, 
				the RELEASE cycles will immediately begin, starting from whatever amplitude 
				had been reached
				// see http://www.sidmusic.org/sid/sidtech2.html
				*/
				osc[voice].envphase= Release;
			}
            break;
        }
        case 0x5: {
			handleAdsrBug(voice, reg, val);
			sid.voices[voice].ad = val;			
			break;
		}
        case 0x6: { 
			handleAdsrBug(voice, reg, val);			
			sid.voices[voice].sr = val; 
			break;	
		}
        case 0x15: { sid.ffreqlo = val; break; }
        case 0x16: { sid.ffreqhi = val; break; }
        case 0x17: { sid.resFtv = val; break; }
        case 0x18: { sid.ftpVol = val; break;}
    }
    return;
}

static void resetEngine(uint32_t sampleRate, uint8_t isModel6581) 
{
	// init "sid" structures
	fillMem((uint8_t*)&sid,0,sizeof(sid));

    createCombinedWF(sid.TriSaw_8580, 0.8, 2.4, 0.64); //precalculate combined waveform
    createCombinedWF(sid.PulseSaw_8580, 1.4, 1.9, 0.68);
    createCombinedWF(sid.PulseTriSaw_8580, 0.8, 2.5, 0.64);

	sid.sampleRate = sampleRate;
	sid.multiplicator = (envCyclesPerSec()<<4) / sampleRate; // accu uses 28 rather than 24 bits - therefore *16 (no idea where old tinysid's 15872000 came from)
		
	sid.isModel6581= isModel6581;	
	sid.level_DC= isModel6581 ? 0x38 : 0x80;	// supposedly the DC level for respective chip model

	sid.voiceEnableMask= 0x7;
	
	// init "filter" structures
	fillMem((uint8_t*)&filter,0,sizeof(filter));

#ifndef OLD_TINYSID_FILTER
    filter.cutoff_ratio_8580 = ((double)-2) * 3.14 * (12500 / 256) / sid.sampleRate,
    filter.cutoff_ratio_6581 = ((double)-2) * 3.14 * (20000 / 256) / sid.sampleRate;
//	filter.prevbandpass = 0;	// redundant
//	filter.prevlowpass = 0;	
#else
	filter.multiplicator = pfloatConvertFromFloat(21.5332031f)/sampleRate;
#endif
	
	
	// init "oscillator" structures
	fillMem((uint8_t*)&osc,0,sizeof(osc));

	for (uint8_t i=0;i<3;i++) {
		// note: by default the rest of sid, osc & filter 
		// above is set to 0
		osc[i].envphase= Release;
		osc[i].zeroLock= 1;
		osc[i].noiseval = 0xffffff;		
		
//		osc[i].adsrBugTriggerTime= 0;	// redundant
//		osc[i].adsrBugFrameCount= 0;
	}
}


static void resetEnvelopeGenerator(uint32_t sampleRate) {
	struct envGenerator *env= &(sid.env);

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
	
	uint32_t cyclesPerSec= envCyclesPerSec();	
	env->cycleOverflow= 0;
	env->cyclesPerSample= ((float)cyclesPerSec/sampleRate);
	
	/* 
	in regular SID, 15-bit LFSR counter counts cpu-clocks, our problem is the lack of 
	cycle by cycle SID emulation (we only have a SID snapshot every 20ms to work with) 
	during rendering our computing granularity then is 'one sample' (not 'one cpu cycle'
	- but around 20).. instead of still trying to simulate a 15-bit cycle-counter we 
	may directly use a sample-counter instead (which also reduces rounding issues).
	*/
	uint16_t i;
#ifdef USE_SAMPLE_ENV_COUNTER
	env->limitLFSR= round(((float)0x8000)/env->cyclesPerSample);	// original counter was 15-bit
	for (i=0; i<16; i++) {
		// counter must reach respective threshold before envelope value is incremented/decremented								
		env->counterPeriod[i]= (int32_t)floor(((double)cyclesPerSec)/(255*1000) * attackTimes[i] / env->cyclesPerSample)+1;	// in samples	
	}
#else
	env->limitLFSR= 0x8000;	// counter 15-bit
	for (i=0;i<16;i++) {
		// LFSR-counter (which is incremented each cycle) must reach respective counterPeriod before envelope
		// value is incremented (it takes 255 increments of the env-value, i.e. counterPeriod must be reached 
		// 255 times to go from 0 to 255), i.e. a complete attack is divided into 255 steps..		
		env->counterPeriod[i]= (int32_t)floor(((double)cyclesPerSec)/(255*1000) * attackTimes[i])+1;	// in clocks	
	}
#endif	
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

void sidReset(uint32_t sampleRate, uint8_t isModel6581)
{
	resetEngine(sampleRate, isModel6581);
	digiPsidSampleReset();	
	resetEnvelopeGenerator(sampleRate);
}

// -----------------------------  SID I/O -------------------------------------------

uint8_t sidReadMem(uint16_t addr) {
	switch (addr) {
	case 0xd41c:					
		/* 
		used by Alien.sid to set filter cutoff freq(hi): unfortunately the 
		filter impl seems to be rather shitty.. and if the actual envelopeOutput
		is used, then the filter will almost mute the signal 
		*/
		return osc[2].envelopeOutput*4/5+20;	// use hack to avoid the worst
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

