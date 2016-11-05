/*
 * This is a reworked version of the original sidengine.c from the "TinySid for Linux" distribution.
 *
 * <p>TinySid (c) 1999-2012 T. Hinrichs, R. Sinsch
 *
 * <p>This file now contains the logic used to emulate the "MOS Technology SID" (see 6581, 8580 or 
 * 6582), and only the added handling of 'digi samples' is kept separately in digi.c
 * 
 * <p>The code from the original SID impl was extensively updated: It was merged with the latest 
 * "Rockbox" version (This noticably fixed playback problems with "Yie Ar Kung Fu"..) Then 
 * additional fixes were contributed by Markus Gritsch. (Unfortunately a revision history of the 
 * old TinySid codebase does not seem to exist.. so we'll probably never know what was used for 
 * the TinySid Windows executable and why it is the only version that correctly plays 
 * Electric_Girls.sid)
 *
 * <p>My later additions:
 *   <ol>
 *    <li>fixed PSID digi playback volume (was originally too low)
 *    <li>Poor man's "combined pulse/triangle waveform" impl to allow playback of songs like 
 *         Kentilla.sid.
 *    <li>RSID digi playback support (D418 based as well as "pulse width modulation" 
 *         based): it is a "special feature" of this implementation that regular SID emulation 
 *         is performed somewhat independently from the handling of digi samples, i.e. playback
 *         of digi samples is tracked separately (for main, IRQ and NMI) and the respective digi 
 *         samples are then merged with the regular SID output as some kind of postprocessing step 
 *    <li> replaced original "envelope generator" impl with a more realistic one (incl. "ADSR-bug" 
 *          handling)
 *  </ol>
 *
 * <p>Tiny'R'Sid add-ons (c) 2016 J.Wothke
 * <p>version 0.81
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

 
// useful links:
// http://www.waitingforfriday.com/index.php/Commodore_SID_6581_Datasheet
// http://www.sidmusic.org/sid/sidtech2.html
 
 
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "sid.h"

#include "digi.h"
#include "memory.h"
#include "cpu.h"		// cpuGetProgramMode(), etc
#include "env.h"		// envNumberOfSamplesPerCall(), etc
#include "rsidengine.h" // rsidGetFrameCount() FIXME remove dependency 


//#define DEBUG 
static uint8_t traceOn= 0;	// only used in DEBUG mode

#define USE_FILTER

// switch between 'cycle' or 'sample' based envelope-generator counter 
// (performance wise it does not seem to make much difference)
#define USE_SAMPLE_ENV_COUNTER

static uint32_t  mixingFrequency;
static uint32_t  freqmul;


// SID register definition
struct s6581 {
    struct sidvoice {
        uint16_t freq;
        uint16_t pulse;
        uint8_t wave;
        uint8_t ad;
        uint8_t sr;
    } v[3];
    uint8_t ffreqlo;
    uint8_t ffreqhi;
    uint8_t resFtv;
    uint8_t ftpVol;
};

static struct s6581 sid;


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
	uint32_t freqmul;	
};
static struct simosc3 osc3;

static void simStartOscillatorVoice3(uint8_t voice, uint8_t val) {
	if ((voice == 2) && useOscPollingHack()) {
		osc3.waveform= val & 0xf0;

		// hack: use only for main/pulse cases like Instantfunk.sid
		osc3.baseCycles= cpuTotalCycles();
		osc3.counter= 0; 
		
		// for some reason the playback is slightly slower than in ACID64
		osc3.freqmul= freqmul*envNumberOfSamplesPerCall()/envCyclesPerScreen();
	}
}
static uint32_t simOsc3Counter() {
	// voice 3 oscillator counter based on elapsed time
	uint32_t diff= cpuTotalCycles() - osc3.baseCycles;
	osc3.baseCycles= cpuTotalCycles();

	uint32_t f= ((uint32_t)sid.v[2].freq)*osc3.freqmul*diff;		
	osc3.counter=(osc3.counter + f) & 0xfffffff;
	return osc3.counter;
}

static uint8_t simReadSawtoothD41B() {
	// simulate sawtooth voice 3 oscillator level based on elapsed time	
	// (handle busy polling for sid oscillator3 - e.g. Ring_Ring_Ring.sid)

	return (uint8_t) (simOsc3Counter() >> 20);
}

static uint8_t simReadPulsedD41B() {
	// simulate pulse voice 3 oscillator level based on elapsed time	
	uint32_t p= (((uint32_t)sid.v[2].pulse) & 0xfff) << 16;
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


/* Routines for quick & dirty float calculation */
static inline int32_t pfloatConvertFromInt(int32_t i) 		{ return (i<<16); }
static inline int32_t pfloatConvertFromFloat(float f) 		{ return (int32_t)(f*(1<<16)); }
static inline int32_t pfloatMultiply(int32_t a, int32_t b)	{ return (a>>8)*(b>>8); }
static inline int32_t pfloatConvertToInt(int32_t i) 		{ return (i>>16); }

// internal oscillator def
struct sidosc {
    uint32_t freq;
    uint32_t pulse;
    uint8_t wave;
    uint8_t filter;
    uint32_t attack;
    uint32_t decay;
    uint32_t sustain;
    uint32_t release;
    uint32_t counter;

	// updated envelope generation based on reSID
	uint8_t envelopeOutput;
	int16_t currentLFSR;	// sim counter	
	uint8_t zeroLock;  
	uint8_t exponentialCounter;
	
    uint8_t envphase;
    uint32_t noisepos;
    uint32_t noiseval;
    uint8_t noiseout;
};

// internal filter def
struct sidflt {
    int32_t freq;
    uint8_t  lowEna;
	uint8_t  bandEna;
    uint8_t  hiEna;
    uint8_t  v3ena;
    int32_t vol;
    int32_t rez;
    int32_t h;
    int32_t b;
    int32_t l;
};

uint32_t sidGetSampleFreq() {
	return mixingFrequency;
}

uint8_t sidGetWave(uint8_t voice) {
	return sid.v[voice].wave;
}
uint8_t sidGetAD(uint8_t voice) {
	return sid.v[voice].ad;	
}
uint8_t sidGetSR(uint8_t voice) {
	return sid.v[voice].sr;	
}

uint16_t sidGetFreq(uint8_t voice) {
	return sid.v[voice].freq;
}

uint16_t sidGetPulse(uint8_t voice) {
		return sid.v[voice].pulse;
}

// the original cycle counter would be 15-bit 
// (here samples are counted & counter is rescaled accordingly)
static int32_t limitLFSR= 0;
static int32_t envelopeCounterPeriod[16];
static int32_t envelopeCounterPeriodClck[16];


// note: decay/release times are 3x longer (implemented via exponentialCounter)
static const int32_t attackTimes[16]  =	{
	2, 8, 16, 24, 38, 56, 68, 80, 100, 240, 500, 800, 1000, 3000, 5000, 8000
};

static int32_t  filtmul;
static struct sidosc osc[3];
static struct sidflt filter;

/* Get the bit from an uint32_t at a specified position */
static inline uint8_t get_bit(uint32_t val, uint8_t b)
{
    return (uint8_t) ((val >> b) & 1);
}

// detection of ADSR-bug conditions
static uint32_t adsrBugTriggerTime= 0;
static uint32_t adsrBugFrameCount= 0;

/* 
poor man's lookup table for combined pulse/triangle waveform (this table does not 
lead to correct results but it is better that nothing for songs like Kentilla.sid)
feel free to come up with a better impl!
hack: this table was created by sampling kentilla output.. i.e. it already reflects the 
envelope used there and may actually be far from the correct waveform
*/
static int8_t pulseTriangleWavetable[] =
{
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x06, 0x00, 
	0x00, 0x06, 0x06, 0x06, 0x00, 0x00, 0x00, 0x06, 0x06, 0x06, 0x06, 0x00, 0x06, 0x06, 0x00, 0x10, 
	0x10, 0x00, 0x00, 0x06, 0x06, 0x06, 0x00, 0x06, 0x06, 0x06, 0x06, 0x00, 0x06, 0x06, 0x00, 0x20, 
	0x10, 0x00, 0x06, 0x06, 0x06, 0x06, 0x0b, 0x15, 0x0b, 0x0b, 0x0b, 0x15, 0x25, 0x2f, 0x2f, 0x69, 
	0x20, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x0b, 0x15, 0x15, 
	0x1b, 0x06, 0x0b, 0x10, 0x0b, 0x0b, 0x15, 0x25, 0x15, 0x0b, 0x0b, 0x63, 0x49, 0x69, 0x88, 0x3a, 
	0x3a, 0x06, 0x0b, 0x06, 0x10, 0x10, 0x15, 0x3f, 0x10, 0x25, 0x59, 0x59, 0x3f, 0x9d, 0xa7, 0x59, 
	0x00, 0x00, 0x5e, 0x59, 0x88, 0xb7, 0xb7, 0xac, 0x83, 0xac, 0xd1, 0xc6, 0xc1, 0xdb, 0xdb, 0xeb,
	// this half table is mirrored when used	
};

static uint8_t exponentialDelays[256];

static float cyclesPerSample= 0;
static float cycleOverflow= 0;

// supposedly DC level for MOS6581 (whereas it would be 0x80 for the "crappy new chip")
const uint8_t level_DC= 0x38;

static uint8_t voiceEnableMask= 0x7;	// allows to mute certain voices..

void sidSetMute(uint8_t voice, uint8_t value) {
	if (value)
		voiceEnableMask &= ~(1 << voice);	// disable voice
	else 
		voiceEnableMask |= (1 << voice);	// enable voice
}


// util related to envelope generator LFSR counter
static int32_t clocksToSamples(int32_t clocks) {
#ifdef USE_SAMPLE_ENV_COUNTER
	return round(((float)clocks)/cyclesPerSample)+1;
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
	
	uint8_t result= (osc[voice].exponentialCounter >= exponentialDelays[osc[voice].envelopeOutput]);
	if (result) {
		osc[voice].exponentialCounter= 0;	// reset to start next round
	}
	return result;
}

static void simOneEnvelopeCycle(uint8_t v) {
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
	if (++osc[v].currentLFSR == limitLFSR) {
		osc[v].currentLFSR= 0;
	}
	
	uint8_t previousEnvelopeOutput = osc[v].envelopeOutput;
				
	switch (osc[v].envphase) {
		case Attack: {                          // Phase 0 : Attack
			if (triggerLFSR_Threshold(osc[v].attack, &osc[v].currentLFSR)) {	
				// inc volume when threshold is reached						
				if (!osc[v].zeroLock) {
					if (osc[v].envelopeOutput < 0xff) {
						/* see Alien.sid: "full envelopeOutput level" GATE off/on sequences 
						   within same IRQ will cause undesireable overflow.. this might not 
						   be a problem in cycle accurate emulations.. but here it is (we 
						   only see a 20ms snapshot)
						*/
						osc[v].envelopeOutput= (osc[v].envelopeOutput + 1) & 0xff;	// increase volume
					}							
				
					osc[v].exponentialCounter = 0;

					if (osc[v].envelopeOutput == 0xff) {
						osc[v].envphase = Decay;
					}							
				}
			}
			break;
		}
		case Decay: {                   	// Phase 1 : Decay      
			if (triggerLFSR_Threshold(osc[v].decay, &osc[v].currentLFSR) 
				&& handleExponentialDelay(v)) { 	// dec volume when threshold is reached
				
				if (!osc[v].zeroLock) {
					if (osc[v].envelopeOutput != osc[v].sustain) {
						osc[v].envelopeOutput= (osc[v].envelopeOutput - 1) & 0xff;	// decrease volume
					} else {
						osc[v].envphase = Sustain;
					}
				}	
			}
			break;
		}
		case Sustain: {                        // Phase 2 : Sustain
			if (osc[v].envelopeOutput != osc[v].sustain) {
				osc[v].envphase = Decay;
			}
			break;
		}					
		case Release: {                          // Phase 3 : Release
			// this phase must be explicitly triggered by clearing the GATE bit..
			if (triggerLFSR_Threshold(osc[v].release, &osc[v].currentLFSR) 
				&& handleExponentialDelay(v)) { 		// dec volume when threshold is reached

				if (!osc[v].zeroLock) {				
					osc[v].envelopeOutput= (osc[v].envelopeOutput - 1) & 0xff;	// decrease volume
				}
			}						
			break;
		}
	}
	if ((osc[v].envelopeOutput == 0) && (previousEnvelopeOutput > osc[v].envelopeOutput)) {
		osc[v].zeroLock = 1;	// new "attack" phase must be started to unlock
	}			
}	  

// render a buffer of n samples using the current SID register contents
void sidSynthRender (int16_t *buffer, uint32_t len)
{
    uint32_t bp;
    /* 
	step 1: convert the not easily processable sid registers into some
            more convenient and fast values (makes the thing much faster
            if you process more than 1 sample value at once)
	*/
    uint8_t v;
    for (v=0;v<3;v++) {
        osc[v].pulse   = (sid.v[v].pulse & 0xfff) << 16;
        osc[v].filter  = get_bit(sid.resFtv,v);
		// threshhold to be reached before incrementing volume
        osc[v].attack  = envelopeCounterPeriod[sid.v[v].ad >> 4];
        osc[v].decay   = envelopeCounterPeriod[sid.v[v].ad & 0xf];
		uint8_t sustain= sid.v[v].sr >> 4;
        osc[v].sustain = sustain<<4 | sustain;
        osc[v].release = envelopeCounterPeriod[sid.v[v].sr & 0xf];
        osc[v].wave    = sid.v[v].wave;

        osc[v].freq    = ((uint32_t)sid.v[v].freq)*freqmul;
    }

#ifdef USE_FILTER
	filter.freq  = ((sid.ffreqhi << 3) + (sid.ffreqlo&0x7)) * filtmul;
	filter.freq <<= 1;

	if (filter.freq>pfloatConvertFromInt(1)) { 
		filter.freq=pfloatConvertFromInt(1);
	}
	/* 
	the above line isnt correct at all - the problem is that the filter
	works only up to rmxfreq/4 - this is sufficient for 44KHz but isnt
	for 32KHz and lower - well, but sound quality is bad enough then to
	neglect the fact that the filter doesnt come that high ;)
	*/
	filter.lowEna = get_bit(sid.ftpVol,4);	// lowpass
	filter.bandEna = get_bit(sid.ftpVol,5);	// bandpass
	filter.hiEna = get_bit(sid.ftpVol,6);	// highpass
	filter.v3ena = !get_bit(sid.ftpVol,7);	// chan3 off
	filter.vol   = (sid.ftpVol & 0xf);
	//  filter.rez   = 1.0-0.04*(float)(sid.resFtv >> 4);

	/* We precalculate part of the quick float operation, saves time in loop later */
	filter.rez   = (pfloatConvertFromFloat(1.2f) -
		pfloatConvertFromFloat(0.04f)*(sid.resFtv >> 4)) >> 8;
#endif  
  
	// now render the buffer
	for (bp=0;bp<len;bp++) {		
		int32_t outo=0;
		int32_t outf=0;
		
		/* 
		step 2 : generate the two output signals (for filtered and non-
		         filtered) from the osc/eg sections
		*/
		for (v=0;v<3;v++) {
			// update wave counter
			osc[v].counter = (osc[v].counter+osc[v].freq) & 0xFFFFFFF;
			// reset counter / noise generator if TEST bit set (blocked at 0 as long as set)
			if (osc[v].wave & 0x08) {
				// note: test bit has no influence on the envelope generator whatsoever
				osc[v].counter  = 0;
				osc[v].noisepos = 0;
				osc[v].noiseval = 0xffffff;
			}
			uint8_t refosc = v?v-1:2;  // reference oscillator for sync/ring
			// sync oscillator to refosc if sync bit set 
			if (osc[v].wave & 0x02)
				if (osc[refosc].counter < osc[refosc].freq)
					osc[v].counter = osc[refosc].counter * osc[v].freq / osc[refosc].freq;
			// generate waveforms with really simple algorithms
			uint8_t tripos = (uint8_t) (osc[v].counter>>19);
			uint8_t triout= tripos;
			if (osc[v].counter>>27) {
				triout^=0xff;
			}
			uint8_t sawout = (uint8_t) (osc[v].counter >> 20);

			uint8_t plsout = (uint8_t) ((osc[v].counter > osc[v].pulse)-1);			
			if (osc[v].wave&0x8) {
				/* 
				TEST (Bit 3): The TEST bit, when set to one, resets and locks oscillator 1 at zero 
				until the TEST bit is cleared. The noise waveform output of oscillator 1 is also 
				reset and the pulse waveform output is held at a DC level
				*/
				plsout= level_DC;
			}

			if ((osc[v].wave & 0x40) && (osc[v].wave & 0x10)) {
				/*
				note: correctly "Saw/Triangle should start from 0 and Pulse from FF"
			
				see $50 waveform impl below.. (because the impl is just a hack, this
				is an attempt to limit undesireable side effects and keep the original
				impl unchanged as far as possible..)
				*/
				plsout ^= 0xff;
			}
		  		  
			// generate noise waveform exactly as the SID does. 			
			if (osc[v].noisepos!=(osc[v].counter>>23))	
			{
				osc[v].noisepos = osc[v].counter >> 23;	
				osc[v].noiseval = (osc[v].noiseval << 1) |
						(get_bit(osc[v].noiseval,22) ^ get_bit(osc[v].noiseval,17));
						
				// impl consistent with: http://www.sidmusic.org/sid/sidtech5.html
				// doc here is probably wrong: http://www.oxyron.de/html/registers_sid.html
				osc[v].noiseout = (get_bit(osc[v].noiseval,22) << 7) |
						(get_bit(osc[v].noiseval,20) << 6) |
						(get_bit(osc[v].noiseval,16) << 5) |
						(get_bit(osc[v].noiseval,13) << 4) |
						(get_bit(osc[v].noiseval,11) << 3) |
						(get_bit(osc[v].noiseval, 7) << 2) |
						(get_bit(osc[v].noiseval, 4) << 1) |
						(get_bit(osc[v].noiseval, 2) << 0);
			}
			uint8_t nseout = osc[v].noiseout;

			// modulate triangle wave if ringmod bit set 
			if (osc[v].wave & 0x04)
				if (osc[refosc].counter < 0x8000000)
					triout^=0xff;

			/* 
			"now mix the oscillators with an AND operation as stated in
			the SID's reference manual - even if this is completely wrong.
			well, at least, the $30 and $70 waveform sounds correct and there's
			no real solution to do $50 and $60, so who cares."
			
			the above statement is nonsense: there are many songs that need $50!
			*/
			uint8_t outv=0xFF;

			uint8_t voiceMute= !((0x1 << v) & voiceEnableMask);
			
			if (!voiceMute) {
				if ((osc[v].wave & 0x40) && (osc[v].wave & 0x10))  {		
					// this is a poor man's impl for $50 waveform to improve playback of 
					// songs like Kentilla.sid, Convincing.sid, etc
					
					uint8_t idx= tripos > 0x7f ? 0xff-tripos : tripos;							
					outv &= pulseTriangleWavetable[idx];					
					outv &= plsout;	// either on or off
				} else {
					int32_t updated= 0;
				
					if ((osc[v].wave & 0x10) && ++updated)  outv &= triout;					
					if ((osc[v].wave & 0x20) && ++updated)  outv &= sawout;
					if ((osc[v].wave & 0x40) && ++updated) 	outv &= plsout;
					if ((osc[v].wave & 0x80) && ++updated)  outv &= nseout;
					if (!updated) 	outv &= level_DC;			
				}
			} else {
				outv=level_DC;
			}

#ifdef USE_SAMPLE_ENV_COUNTER
			// using samples
			simOneEnvelopeCycle(v);
#else
			// using cycles
			float c= cyclesPerSample+cycleOverflow;
			uint16_t cycles= (uint16_t)c;		
			cycleOverflow= c-cycles;
			
			for (int32_t i= 0; i<cycles; i++) {
				simOneEnvelopeCycle(v);
			}	  
#endif
			// now route the voice output to either the non-filtered or the
			// filtered channel and dont forget to blank out osc3 if desired	
			
#ifdef USE_FILTER
			if (((v<2) || filter.v3ena) && !voiceMute) {
				if (osc[v].filter) {
					outf+=( ((int32_t)(outv-0x80)) * (int32_t)((osc[v].envelopeOutput)) ) >>6;
				} else {
					outo+=( ((int32_t)(outv-0x80)) * (int32_t)((osc[v].envelopeOutput)) ) >>6;
				}
			}
#else
			// Don't use filters, just mix all voices together
			if (!voiceMute) outf+= (int32_t)(((int16_t)(outv-0x80)) * (osc[v].envelopeOutput)); 
#endif
		}

#ifdef USE_FILTER
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

#ifdef DEBUG
static char hex1 [16]= {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
static char *pokeInfo;

static void traceSidPoke(uint8_t reg, uint8_t val) {
	pokeInfo= malloc(sizeof(char)*13);

	pokeInfo[0]= hex1[(sFrameCount>>12)&0xf];
	pokeInfo[1]= hex1[(sFrameCount>>8)&0xf];
	pokeInfo[2]= hex1[(sFrameCount>>4)&0xf];
	pokeInfo[3]= hex1[(sFrameCount&0xf)];
	pokeInfo[4]= ' ';
	pokeInfo[5]= 'D';
	pokeInfo[6]= '4';
	pokeInfo[7]= hex1[(reg>>4)];
	pokeInfo[8]= hex1[(reg&0xf)];
	pokeInfo[9]= ' ';
	pokeInfo[10]= hex1[(val>>4)];
	pokeInfo[11]= hex1[(val&0xf)];
	
	fprintf(stderr, "%s\n", pokeInfo);
	free(pokeInfo);
}
#endif

void sidPoke(uint8_t reg, uint8_t val)
{
    uint8_t voice=0;
#ifdef DEBUG
//		if (traceOn) traceSidPoke(reg, val);	
#endif	
	if (reg < 7) {}
    if ((reg >= 7) && (reg <=13)) {voice=1; reg-=7;}
    if ((reg >= 14) && (reg <=20)) {voice=2; reg-=14;}

    switch (reg) {		
        case 0: { // Set frequency: Low byte
			sid.v[voice].freq = (sid.v[voice].freq&0xff00) | val;
            break;
        }
        case 1: { // Set frequency: High byte
            sid.v[voice].freq = (sid.v[voice].freq&0xff) | (val<<8);
            break;
        }
        case 2: { // Set pulse width: Low byte
            sid.v[voice].pulse = (sid.v[voice].pulse&0x0f00) | val;
            break;
        }
        case 3: { // Set pulse width: High byte
            sid.v[voice].pulse = (sid.v[voice].pulse&0xff) | ((val & 0xf)<<8);
            break;
        }
        case 4: {
			simStartOscillatorVoice3(voice, val);
			
			uint8_t oldGate= sid.v[voice].wave&0x1;
			uint8_t oldTest= sid.v[voice].wave&0x8;		// oscillator stop
			uint8_t newGate= val & 0x01;

			sid.v[voice].wave = val;
			
			// poor man's ADSR-bug detection: this is the kind of logic a player would 
			// most likely be using to deliberately trigger the the counter overflow..
			if (oldTest && (val&0x8) && !oldGate && newGate) {
				adsrBugTriggerTime= cpuCycles();	// FIXME replace using cpuTotalCycles()
				adsrBugFrameCount= rsidGetFrameCount();
			}
						
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
        case 5: { 
			sid.v[voice].ad = val;
			
			/* 
			ADSR-bug: if somebody goes through the above TEST/GATE drill and shortly thereafter 
			sets up an A/D that is bound to already have run over then we can be pretty sure 
			what he is after..			
			*/
			if (adsrBugFrameCount == rsidGetFrameCount()) {	// limitation: same frame only
				int32_t delay= envelopeCounterPeriodClck[val >> 4];
				if ((cpuCycles()-adsrBugTriggerTime) > delay ) {
					// force ARSR-bug by setting counter higher than the threshold
					osc[voice].currentLFSR= clocksToSamples(delay);	
				}
				adsrBugTriggerTime= 0;			
			}
			break;
		}
        case 6: { sid.v[voice].sr = val; break;	}
        case 21: { sid.ffreqlo = val; break; }
        case 22: { sid.ffreqhi = val; break; }
        case 23: { sid.resFtv = val; break; }
        case 24: { sid.ftpVol = val; break;}
    }
    return;
}





static void resetEngine(uint32_t mixfrq) 
{
	mixingFrequency = mixfrq;
	
//	freqmul = 15872000 / mixfrq;	// FIXME XXX what's the logic behind original TinySid value? 
	freqmul = envCyclesPerSec()*16 / mixfrq; // accu uses 28 rather than 24 bits (therefore *16)
		
	filtmul = pfloatConvertFromFloat(21.5332031f)/mixfrq;

	fillMem((uint8_t*)&sid,0,sizeof(sid));
	fillMem((uint8_t*)&osc,0,sizeof(osc));
	fillMem((uint8_t*)&filter,0,sizeof(filter));
	
	for (uint8_t i=0;i<3;i++) {
		// note: by default the rest of sid, osc & filter 
		// above is set to 0
		osc[i].envphase= Release;
		osc[i].zeroLock= 1;
		osc[i].noiseval = 0xffffff;		
	}
	
	voiceEnableMask= 0x7;
	
	adsrBugTriggerTime= 0;
	adsrBugFrameCount= 0;
	
	// hack
	fillMem((uint8_t*)&osc3,0,sizeof(osc3));

		
}
static void resetEnvelopeGenerator(uint32_t mixfrq) {
	// envelope-generator stuff
	uint32_t cyclesPerSec= envCyclesPerSec();
	
	cycleOverflow= 0;
	cyclesPerSample= ((float)cyclesPerSec/mixfrq);
	
	/* 
	in regular SID, 15-bit LFSR counter counts cpu-clocks, our problem is the lack of 
	cycle by cycle SID emulation (we only have a SID snapshot every 20ms to work with) 
	during rendering our computing granularity then is 'one sample' (not 'one cpu cycle'
	- but around 20).. instead of still trying to simulate a 15-bit cycle-counter we 
	may directly use a sample-counter instead (which also reduces rounding issues).
	*/
	uint16_t i;
#ifdef USE_SAMPLE_ENV_COUNTER
	limitLFSR= round(((float)0x8000)/cyclesPerSample);	// original counter was 15-bit
	for (i=0; i<16; i++) {
		// counter must reach respective threshold before envelope value is incremented/decremented
		envelopeCounterPeriod[i]= (int32_t)round((float)(
									attackTimes[i]*cyclesPerSec)/1000/256/cyclesPerSample)+1;	// in samples
		envelopeCounterPeriodClck[i]= (int32_t)round((float)(
									attackTimes[i]*cyclesPerSec)/1000/256)+1;				// in clocks
	}
#else
	limitLFSR= 0x8000;	// counter 15-bit
	for (i=0;i<16;i++) {
		// counter must reach respective threshold before envelope value is incremented/decremented
		envelopeCounterPeriod[i]= (int32_t)floor((float)(
									attackTimes[i]*cyclesPerSec)/1000/256)+1;	// in samples
		envelopeCounterPeriodClck[i]= (int32_t)floor((float)(
										attackTimes[i]*cyclesPerSec)/1000/256)+1;	// in clocks
	}
#endif	
	// lookup table for decay rates
	uint8_t from[] =  {93, 54, 26, 14,  6,  0};
	uint8_t val[] = { 1,  2,  4,  8, 16, 30};
	for (i= 0; i<256; i++) {
		uint8_t v= 1;
		for (uint8_t j= 0; j<6; j++) {
			if (i>from[j]) {
				v= val[j];
				break;
			}
		}
		exponentialDelays[i]= v;
	}	
}

void sidReset(uint32_t mixfrq)
{
	resetEngine(mixfrq);
	digiPsidSampleReset();	
	resetEnvelopeGenerator(mixfrq);
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

