/*
 * This file is largely the original file from the "TinySid for Linux" distribution.
 *
 * <p>TinySid (c) 1999-2012 T. Hinrichs, R. Sinsch
 *
 * <p>It was updated by merging in the latest "Rockbox" version (This noticably fixed playback problems with 
 * "Yie Ar Kung Fu"..) and by applying fixes contributed by Markus Gritsch. Unfortunately a revision history of the old
 * TinySid codebase does not seem to exist.. so we'll probably never know what was used for the TinySid Windows executable and
 * why it is the only version that correctly plays Electric_Girls.sid..)
 * <p>In this file I deliberately kept the "naming conventions" used in the original TinySID code - to ease future merging
 * of potential TinySid fixes (consequently there is a mismatch with the conventions that I am using in my own code..) 
 *
 * <p>My additions here are:
 *   <ol>
 *    <li>fixed PSID digi playback volume (was originally too low)
 *    <li>correct cycle-time calculation for ALL 6510 op codes (also illegal ones)
 *    <li>added impls for illegal 6510 op codes, fixed errors in V-flag calculation, added handling for 6510 addressing "bugs"
 *    <li>poor man's VIC and CIA handling
 *    <li>"cycle limit" feature used to interrupt emulation runs (e.g. main prog can now be suspended/continued)
 *    <li>Poor man's "combined pulse/triangle waveform" impl to allow playback of songs like Kentilla.sid.
 *    <li>added RSID digi playback support (D418 based as well as "pulse width modulation" based): it is a "special feature" of 
 *    this implementation that regular SID emulation is performed somewhat independently from the handling of digi samples, i.e. playback
 *    of digi samples is tracked separately (for main, IRQ and NMI) and the respective digi samples are then merged with the regular SID 
 *    output as some kind of postprocessing step 
 *    <li> replaced original "envelope generator" impl with a more realistic one (incl. "ADSR-bug" handling)
 *  </ol>
 *
 *	FIXME: refactor CPU and SID emulation into separate files..
 *
 * known limitation: basic-ROM specific handling not implemented...
 * 
 * <p>Notice: if you have questions regarding the details of the below SID emulation, then you should better get in touch with R.Sinsch :-)
 *
 * <p>Tiny'R'Sid add-ons (c) 2015 J.Wothke
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

 
 // useful links:
 // http://www.waitingforfriday.com/index.php/Commodore_SID_6581_Datasheet
 // http://www.sidmusic.org/sid/sidtech2.html
 // http://www.oxyron.de/html/opcodes02.html
 
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

//#define DEBUG 
#define USE_FILTER

// switch between 'cycle' or 'sample' based envelope-generator counter 
// (performance wise it does not seem to make much difference)
//#define USE_SAMPLE_ENV_COUNTER


#include "sidengine.h"

#include "defines.h"
#include "digi.h"
#include "nanovic.h"
#include "nanocia.h"
#include "rsidengine.h"
#include "sidplayer.h"

typedef enum {
    Attack=0,
    Decay=1,
    Sustain=2,
    Release=3,
} EnvelopePhase;


int sFrameCount= 0;

// hacks
unsigned char sFake_d012_count=0;
unsigned char sFake_d012_loop=0;

unsigned int sCiaNmiVectorHack= 0;

void setCiaNmiVectorHack(){
	sCiaNmiVectorHack= 1;
}

/* Routines for quick & dirty float calculation */
static inline int pfloat_ConvertFromInt(int i) 		{ return (i<<16); }
static inline int pfloat_ConvertFromFloat(float f) 	{ return (int)(f*(1<<16)); }
static inline int pfloat_Multiply(int a, int b) 	{ return (a>>8)*(b>>8); }
static inline int pfloat_ConvertToInt(int i) 		{ return (i>>16); }


// internal oscillator def
struct sidosc {
    unsigned long freq;
    unsigned long pulse;
    unsigned char wave;
    unsigned char filter;
    unsigned long attack;
    unsigned long decay;
    unsigned long sustain;
    unsigned long release;
    unsigned long counter;

	// updated envelope generation based on reSID
	unsigned char envelopeOutput;
	signed int currentLFSR;	// sim counter	
	unsigned char zero_lock;  
	unsigned char exponential_counter;
	
    unsigned char envphase;
    unsigned long noisepos;
    unsigned long noiseval;
    unsigned char noiseout;
};

// internal filter def
struct sidflt {
    int freq;
    unsigned char  l_ena;
	unsigned char  b_ena;
    unsigned char  h_ena;
    unsigned char  v3ena;
    int vol;
    int rez;
    int h;
    int b;
    int l;
};


int limit_LFSR= 0;	// the original cycle counter would be 15-bit (but we are counting samples & may rescale the counter accordingly)
int envelope_counter_period[16];
int envelope_counter_period_clck[16];


// note: decay/release times are 3x longer (implemented via exponential_counter)
static const int attackTimes[16]  =	{
	2, 8, 16, 24, 38, 56, 68, 80, 100, 240, 500, 800, 1000, 3000, 5000, 8000
};

static unsigned long  mixing_frequency;
static unsigned long  freqmul;
static int  filtmul;

unsigned long getSampleFrequency() {
	return mixing_frequency;
}

struct s6581 sid;
static struct sidosc osc[3];
static struct sidflt filter;

static unsigned int sMuteVoice[3];

void setMute(unsigned char voice) {
	sMuteVoice[voice] = 1;
}

/* Get the bit from an unsigned long at a specified position */
static inline unsigned char get_bit(unsigned long val, unsigned char b)
{
    return (unsigned char) ((val >> b) & 1);
}

/*
* @return 0 if RAM is visible; 1 if ROM is visible
*/ 
static char isKernalRomVisible() {
	return memory[0x0001] & 0x2;
}

/*
* @return 0 if RAM is visible; 1 if IO area is visible
*/ 
static char isIoAreaVisible() {
	unsigned char bits= memory[0x0001] & 0x7;	
	return ((bits & 0x4) != 0) && (bits != 0x4);
}

unsigned short pc;

unsigned long sCycles= 0;					// counter keeps track of burned cpu cycles

unsigned long sAdsrBugTriggerTime= 0;					// detection of ADSR-bug conditions
unsigned long sAdsrBugFrameCount= 0;

static unsigned char sDummyDC04;


// poor man's lookup table for combined pulse/triangle waveform (this table does not 
// lead to correct results but it is better that nothing for songs like Kentilla.sid)
// feel free to come up with a better impl!
// FIXME: this table was created by sampling kentilla output.. i.e. it already reflects the envelope 
// used there and may actually be far from the correct waveform
signed char pulseTriangleWavetable[] =
{
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x06, 0x00, 
	0x00, 0x06, 0x06, 0x06, 0x00, 0x00, 0x00, 0x06, 0x06, 0x06, 0x06, 0x00, 0x06, 0x06, 0x00, 0x10, 
	0x10, 0x00, 0x00, 0x06, 0x06, 0x06, 0x00, 0x06, 0x06, 0x06, 0x06, 0x00, 0x06, 0x06, 0x00, 0x20, 
	0x10, 0x00, 0x06, 0x06, 0x06, 0x06, 0x0b, 0x15, 0x0b, 0x0b, 0x0b, 0x15, 0x25, 0x2f, 0x2f, 0x69, 
	0x20, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x0b, 0x15, 0x15, 
	0x1b, 0x06, 0x0b, 0x10, 0x0b, 0x0b, 0x15, 0x25, 0x15, 0x0b, 0x0b, 0x63, 0x49, 0x69, 0x88, 0x3a, 
	0x3a, 0x06, 0x0b, 0x06, 0x10, 0x10, 0x15, 0x3f, 0x10, 0x25, 0x59, 0x59, 0x3f, 0x9d, 0xa7, 0x59, 
	0x00, 0x00, 0x5e, 0x59, 0x88, 0xb7, 0xb7, 0xac, 0x83, 0xac, 0xd1, 0xc6, 0xc1, 0xdb, 0xdb, 0xeb, 
	
/*	
	rather symetrical: let's just mirror the above part
	0xdb, 0xd6, 0xd1, 0xc6, 0xcb, 0xa2, 0x63, 0x5e, 0xb7, 0x92, 0x92, 0x59, 0x44, 0x44, 0xfb, 0x6e, 
	0x97, 0x6e, 0x6e, 0x63, 0x3f, 0x10, 0x15, 0x49, 0x49, 0xfb, 0x0b, 0xfb, 0x00, 0x00, 0xf6, 0x63, 
	0x83, 0x4f, 0x2a, 0x2a, 0x3f, 0x00, 0x20, 0x2f, 0xfb, 0xfb, 0x06, 0xfb, 0x00, 0xfb, 0x00, 0x2f, 
	0x0b, 0x06, 0xfb, 0x00, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x3a, 0x3a, 
	0x59, 0x25, 0x10, 0x15, 0xfb, 0xfb, 0x15, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x06, 
	0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x06, 
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
	0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x06, 0x06, 0x06, 0x00, 0x00, 0x00, 0x06, 0x06, 0x06, */
};

unsigned char exponential_delays[256];

float cyclesPerSample= 0;	// rename global stuff
float cycleOverflow= 0;

// supposedly DC level for MOS6581 (whereas it would be 0x80 for the "crappy new chip")
const unsigned level_DC= 0x38;

#ifdef DEBUG
const unsigned char voiceEnableMask= 0x7;	// for debugging: allows to mute certain voices..
#endif

// util related to envelope generator LFSR counter
int clocksToSamples(int clocks) {
#ifdef USE_SAMPLE_ENV_COUNTER
	return round(((float)clocks)/cyclesPerSample)+1;
#else
	return clocks;
#endif
}

/*
* check if LFSR threshold was reached 
*/
unsigned char triggerLFSR_Threshold(unsigned int threshold, signed int *end) {
	if (threshold == (*end)) {
		(*end)= 0; // reset counter
		return 1;
	}
	return 0;
}

unsigned char handleExponentialDelay(unsigned char voice) {
	osc[voice].exponential_counter+= 1;
	
	unsigned char result= (osc[voice].exponential_counter >= exponential_delays[osc[voice].envelopeOutput]);
	if (result) {
		osc[voice].exponential_counter= 0;	// reset to start next round
	}
	return result;
}

void simOneEnvelopeCycle(unsigned char v) {
	// now process the volume according to the phase and adsr values
	// (explicit switching of ADSR phase is handled in sidPoke() so there is no need to handle that here)

	// advance envelope LFSR counter (originally this would be a 15-bit cycle counter.. but we may be counting samples here)

	// ADSR bug scenario: normally the maximum thresholds used for the original 15-bit counter would have been around 
	// 0x7a13 (i.e. somewhat below the 7fff range that can be handled by the counter). For certain bug scenarios 
	// it is possible that the threshold is missed and the counter keeps counting until it again reaches the
	// threshold after a wrap-around.. (see sidPoke() for specific ADSR-bug handling)
		
	if (++osc[v].currentLFSR == limit_LFSR) {
		osc[v].currentLFSR= 0;
	}
	
	unsigned char previousEnvelopeOutput = osc[v].envelopeOutput;
				
	switch (osc[v].envphase) {
		case Attack: {                          // Phase 0 : Attack
			if (triggerLFSR_Threshold(osc[v].attack, &osc[v].currentLFSR)) {	// inc volume when threshold is reached						
				if (!osc[v].zero_lock) {
					if (osc[v].envelopeOutput < 0xff) {
						// see Alien.sid: "full envelopeOutput level" GATE off/on sequences within same 
						// IRQ will cause undesireable overflow.. this might not be a problem in cycle accurate
						// emulations.. but here it is (we only see a 20ms snapshot)

						osc[v].envelopeOutput= (osc[v].envelopeOutput + 1) & 0xff;	// increase volume
					}							
				
					osc[v].exponential_counter = 0;

					if (osc[v].envelopeOutput == 0xff) {
						osc[v].envphase = Decay;
					}							
				}
			}
			break;
		}
		case Decay: {                   	// Phase 1 : Decay      
			if (triggerLFSR_Threshold(osc[v].decay, &osc[v].currentLFSR) && handleExponentialDelay(v)) { 	// dec volume when threshold is reached
				if (!osc[v].zero_lock) {
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
			if (triggerLFSR_Threshold(osc[v].release, &osc[v].currentLFSR) && handleExponentialDelay(v)) { 		// dec volume when threshold is reached
				if (!osc[v].zero_lock) {				
					osc[v].envelopeOutput= (osc[v].envelopeOutput - 1) & 0xff;	// decrease volume
				}
			}						
			break;
		}
	}
	if ((osc[v].envelopeOutput == 0) && (previousEnvelopeOutput > osc[v].envelopeOutput)) {
		osc[v].zero_lock = 1;	// new "attack" phase must be started to unlock
	}			
}	  

// render a buffer of n samples with the actual register contents
void synth_render (short *buffer, unsigned long len)
{
    unsigned long bp;
    // step 1: convert the not easily processable sid registers into some
    //           more convenient and fast values (makes the thing much faster
    //          if you process more than 1 sample value at once)
    unsigned char v;
    for (v=0;v<3;v++) {
        osc[v].pulse   = (sid.v[v].pulse & 0xfff) << 16;
        osc[v].filter  = get_bit(sid.res_ftv,v);
        osc[v].attack  = envelope_counter_period[sid.v[v].ad >> 4];		// threshhold to be reached before incrementing volume
        osc[v].decay   = envelope_counter_period[sid.v[v].ad & 0xf];
		unsigned char sustain= sid.v[v].sr >> 4;
        osc[v].sustain = sustain<<4 | sustain;
        osc[v].release = envelope_counter_period[sid.v[v].sr & 0xf];
        osc[v].wave    = sid.v[v].wave;
        osc[v].freq    = ((unsigned long)sid.v[v].freq)*freqmul;
    }

#ifdef USE_FILTER
	filter.freq  = ((sid.ffreqhi << 3) + (sid.ffreqlo&0x7)) * filtmul;
	filter.freq <<= 1;

	if (filter.freq>pfloat_ConvertFromInt(1)) { 
		filter.freq=pfloat_ConvertFromInt(1);
	}
	// the above line isnt correct at all - the problem is that the filter
	// works only up to rmxfreq/4 - this is sufficient for 44KHz but isnt
	// for 32KHz and lower - well, but sound quality is bad enough then to
	// neglect the fact that the filter doesnt come that high ;)
	filter.l_ena = get_bit(sid.ftp_vol,4);	// lowpass
	filter.b_ena = get_bit(sid.ftp_vol,5);	// bandpass
	filter.h_ena = get_bit(sid.ftp_vol,6);	// highpass
	filter.v3ena = !get_bit(sid.ftp_vol,7);	// chan3 off
	filter.vol   = (sid.ftp_vol & 0xf);
	//  filter.rez   = 1.0-0.04*(float)(sid.res_ftv >> 4);

	/* We precalculate part of the quick float operation, saves time in loop later */
	filter.rez   = (pfloat_ConvertFromFloat(1.2f) -
		pfloat_ConvertFromFloat(0.04f)*(sid.res_ftv >> 4)) >> 8;
#endif  
  
	// now render the buffer
	for (bp=0;bp<len;bp++) {		
		int outo=0;
		int outf=0;
		
		// step 2 : generate the two output signals (for filtered and non-
		//          filtered) from the osc/eg sections
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
			unsigned char refosc = v?v-1:2;  // reference oscillator for sync/ring
			// sync oscillator to refosc if sync bit set 
			if (osc[v].wave & 0x02)
				if (osc[refosc].counter < osc[refosc].freq)
					osc[v].counter = osc[refosc].counter * osc[v].freq / osc[refosc].freq;
			// generate waveforms with really simple algorithms
			unsigned char tripos = (unsigned char) (osc[v].counter>>19);
			unsigned char triout= tripos;
			if (osc[v].counter>>27) {
				triout^=0xff;
			}
			unsigned char sawout = (unsigned char) (osc[v].counter >> 20);

			unsigned char plsout = (unsigned char) ((osc[v].counter > osc[v].pulse)-1);			
			if (osc[v].wave&0x8) {
				// TEST (Bit 3): The TEST bit, when set to one, resets and locks oscillator 1 at zero 
				// until the TEST bit is cleared. The noise waveform output of oscillator 1 is also 
				// reset and the pulse waveform output is held at a DC level
				plsout= level_DC;
			}

			if ((osc[v].wave & 0x40) && (osc[v].wave & 0x10)) {
				// note: correctly "Saw/Triangle should start from 0 and Pulse from FF"
			
				// see $50 waveform impl below.. (because the impl is just a hack, this
				// is an attempt to limit undesireable side effects and keep the original
				// impl unchanged as far as possible..)
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
			unsigned char nseout = osc[v].noiseout;

			// modulate triangle wave if ringmod bit set 
			if (osc[v].wave & 0x04)
				if (osc[refosc].counter < 0x8000000)
					triout^=0xff;

			// now mix the oscillators with an AND operation as stated in
			// the SID's reference manual - even if this is completely wrong.
			// well, at least, the $30 and $70 waveform sounds correct and there's
			// no real solution to do $50 and $60, so who cares.
			
			// => wothke: the above statement is nonsense: there are many songs that need $50!

			unsigned char outv=0xFF;
#ifdef DEBUG			
			if ((0x1 << v) & voiceEnableMask) {
#endif
				if ((osc[v].wave & 0x40) && (osc[v].wave & 0x10))  {		
					// this is a poor man's impl for $50 waveform to improve playback of 
					// songs like Kentilla.sid, Convincing.sid, etc
					
					unsigned char idx= tripos > 0x7f ? 0xff-tripos : tripos;							
					outv &= pulseTriangleWavetable[idx];					
					outv &= plsout;	// either on or off
				} else {
					int updated= 0;
				
					if ((osc[v].wave & 0x10) && ++updated)  outv &= triout;					
					if ((osc[v].wave & 0x20) && ++updated)  outv &= sawout;
					if ((osc[v].wave & 0x40) && ++updated) 	outv &= plsout;
					if ((osc[v].wave & 0x80) && ++updated)  outv &= nseout;
					if (!updated) 	outv &= level_DC;			
				}
#ifdef DEBUG			
			} else {
				outv=level_DC;
			}
#endif						
#ifdef USE_SAMPLE_ENV_COUNTER
			// using samples
			simOneEnvelopeCycle(v);
#else
			// using cycles
			float c= cyclesPerSample+cycleOverflow;
			unsigned int cycles= (unsigned int)c;		
			cycleOverflow= c-cycles;
			
			for (int i= 0; i<cycles; i++) {
				simOneEnvelopeCycle(v);
			}	  
#endif
			// now route the voice output to either the non-filtered or the
			// filtered channel and dont forget to blank out osc3 if desired	
			
#ifdef USE_FILTER
			if (((v<2) || filter.v3ena) && !sMuteVoice[v]) {
				if (osc[v].filter) {
					outf+=( ((int)(outv-0x80)) * (int)((osc[v].envelopeOutput)) ) >>6;
				} else {
					outo+=( ((int)(outv-0x80)) * (int)((osc[v].envelopeOutput)) ) >>6;
				}
			}
#else
			// Don't use filters, just mix all voices together
			if (!sMuteVoice[v]) outf+= (int)(((signed short)(outv-0x80)) * (osc[v].envelopeOutput)); 
#endif
		}

#ifdef USE_FILTER
		// step 3
		// so, now theres finally time to apply the multi-mode resonant filter
		// to the signal. The easiest thing is just modelling a real electronic
		// filter circuit instead of fiddling around with complex IIRs or even
		// FIRs ...
		// it sounds as good as them or maybe better and needs only 3 MULs and
		// 4 ADDs for EVERYTHING. SIDPlay uses this kind of filter, too, but
		// Mage messed the whole thing completely up - as the rest of the
		// emulator.
		// This filter sounds a lot like the 8580, as the low-quality, dirty
		// sound of the 6581 is uuh too hard to achieve :) 

		filter.h = pfloat_ConvertFromInt(outf) - (filter.b>>8)*filter.rez - filter.l;
		filter.b += pfloat_Multiply(filter.freq, filter.h);
		filter.l += pfloat_Multiply(filter.freq, filter.b);

		if (filter.l_ena || filter.b_ena || filter.h_ena) {	
			// voice may be routed through filter without actually using any 
			// filters.. e.g. Dancing_in_the_Moonshine.sid
			outf = 0;
			
			if (filter.l_ena) outf+=pfloat_ConvertToInt(filter.l);
			if (filter.b_ena) outf+=pfloat_ConvertToInt(filter.b);
			if (filter.h_ena) outf+=pfloat_ConvertToInt(filter.h);
		}		

		int final_sample = (filter.vol*(outo+outf));
#else
		int final_sample = outf>>2;
#endif

		final_sample= generatePsidDigi(final_sample);	// PSID stuff

		// Clipping
		const int clipValue = 32767;
		if ( final_sample < -clipValue ) {
			final_sample = -clipValue;
		} else if ( final_sample > clipValue ) {
			final_sample = clipValue;
		}

		short out= final_sample;
		*(buffer+bp)= out;
    }
}

#ifdef DEBUG
char hex1 [16]= {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
char *pokeInfo;

void traceSidPoke(int reg, unsigned char val) {
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
	
//	AS3_Trace(AS3_String(pokeInfo));		
	fprintf(stderr, "%s\n", pokeInfo);
	free(pokeInfo);
}
#endif

unsigned char sTraceon= 0;

unsigned long getFrameCount() {
	return sFrameCount;
}

void incFrameCount() {
	sFrameCount++;
}

unsigned long sLastFrameCount= 0;

//
// Poke a value into the sid register
//
void sidPoke(int reg, unsigned char val)
{
    int voice=0;
#ifdef DEBUG
//		if (sTraceon) traceSidPoke(reg, val);	
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
			unsigned char oldGate= sid.v[voice].wave&0x1;
			unsigned char oldTest= sid.v[voice].wave&0x8;		// oscillator stop
			unsigned char newGate= val & 0x01;

			sid.v[voice].wave = val;
			
			// poor man's ADSR-bug detection: this is the kind of logic a player would most
			// likely be using to deliberately trigger the the counter overflow..
			if (oldTest && (val&0x8) && !oldGate && newGate) {
				sAdsrBugTriggerTime= sCycles;
				sAdsrBugFrameCount= getFrameCount();
			}
						
			if (!oldGate && newGate) {
				// If the envelope is then gated again (before the RELEASE cycle has reached 
				// zero amplitude), another ATTACK cycle will begin, starting from whatever amplitude had been reached.
				osc[voice].envphase= Attack;				
				osc[voice].zero_lock= 0;
			} else if (oldGate && !newGate) {
				// if the gate bit is reset before the envelope has finished the ATTACK cycle, 
				// the RELEASE cycles will immediately begin, starting from whatever amplitude had been reached
				// see http://www.sidmusic.org/sid/sidtech2.html
				osc[voice].envphase= Release;
			}
            break;
        }
        case 5: { 
			sid.v[voice].ad = val;
			
			// ADSR-bug: if somebody goes through the above TEST/GATE drill and shortly thereafter 
			// sets up an A/D that is bound to already have run over then we can be pretty sure what he is after..			

			if (sAdsrBugFrameCount == getFrameCount()) {				// limitation: only works within the same frame
				int delay= envelope_counter_period_clck[val >> 4];
				if ((sCycles-sAdsrBugTriggerTime) > delay ) {
					osc[voice].currentLFSR= clocksToSamples(delay);	// force ARSR-bug by setting counter higher than the threshold
				}
				sAdsrBugTriggerTime= 0;			
			}
			break;
		}
        case 6: { sid.v[voice].sr = val; break;	}
        case 21: { sid.ffreqlo = val; break; }
        case 22: { sid.ffreqhi = val; break; }
        case 23: { sid.res_ftv = val; break; }
        case 24: { sid.ftp_vol = val; break;}
    }
    return;
}

unsigned char getmem(unsigned short addr)
{
	if (addr < 0xd000) {
		return  memory[addr];
	} else if ((addr >= 0xd000) && (addr < 0xe000)) {	// handle I/O area 		
		if (isIoAreaVisible()) {		
			switch (addr) {
				// SID
				case 0xd41c:					
					// used by Alien.sid to set filter cutoff freq(hi): unfortunately the 
					// filter impl seems to be rather shitty.. and if the actual envelopeOutput
					// is used, then the filter will almost mute the signal 
					return osc[2].envelopeOutput*4/5+20;	// use hack to avoid the worst
					
				// CIA	
				case 0xdc01:
					return 0xff;	// some songs use this as an exit criteria.. see Master_Blaster_intro.sid
					
				case 0xdc04:
					if (isPsid()) {
						// hack for Delta_Mix-E-Load_loader.sid which uses counter to control the progress of its melody:
						// PSID is invoked via CIA1 timer.. however regular timing handling (e.g. based on used cycles)
						// does not seem to work - but this hack does..
						sDummyDC04+=3;
						return sDummyDC04;
					} 
					if (sCiaNmiVectorHack) {
						// wonderland_xii*.sid hack: NMI routines at $8xx, $9xx, ... 
						// set the low-byte to the next routine to be called in 0xdc03 and it is 
						// the hi-byte here that would be changed by the timer to point it 
						// to the correct $8,$9,etc offset.. we just use one hardcoded offset here..
						return 0x08;							
					}
					// songs like LMan - Vortex.sid actually place a JMP at this location.. so
					// the above hack MUST NOT be always enabled
					 return io_area[addr-0xd000];
				case 0xdc06:
					// hack originally used for Storebror.sid.. but apparently other songs also 
					// "benefit" from it (e.g. Uwe Anfang's stuff)...  always use 0x08xx NMI vector 
					// (our timing is too imprecise to reliably calc the required values between 0x08 and 
					// 0x0e.. (other songs - e.g. Hunters_Moon.sid - are using 0x02-0x09) but since jitter is the
					// least of our problems, there is no need for the respective timing logic anyway)
					return 0x08;	

				// fixme: getmem is also used for write.. clearing the status here then might be a problem:
				case 0xdc0d:
					return getInterruptStatus(&(cia[0]));	
				case 0xdd0d:		
					return getInterruptStatus(&(cia[1]));
				
				case 0xdc08: 
					// TOD tenth of second
					return (getTimeOfDayMillis()%1000)/100;
				case 0xdc09: 
					// TOD second
					return ((unsigned int)(getTimeOfDayMillis()/1000))%60;
					
				// VIC
				case 0xd011:
					return getCurrentD011();
				case 0xd012:					
					return getCurrentD012();
				case 0xd019:
					return getD019();
							
				default:
					if ((addr&0xfc00)==0xd400) {			
						return io_area[(addr&0xfc1f) - 0xd000];
					}
					return io_area[addr-0xd000];
			}
		} else {
			// normal RAM access
			return  memory[addr];
		}
	} else {	// handle kernal ROM
		if (isKernalRomVisible()) {
			return kernal_rom[addr - 0xe000];
		} else {
			// normal RAM access
			return  memory[addr];
		}
	}
}

// ----------------------------------------------------------------- Register
unsigned char a,x,y,s,p;	// p= status register

static void setmem(unsigned short addr, unsigned char value)
{
	if ((addr >= 0xd000) && (addr < 0xe000)) {	// handle I/O area 
		if (isIoAreaVisible()) {
			
			// CIA timers
			if ((addr >= 0xdc00) && (addr < 0xde00)) {
				addr&=0xFF0F;	// handle the 16 mirrored CIA registers just in case
				
				switch (addr) {
					case 0xdc0d:
						setInterruptMask(&(cia[0]), value);
						break;
					case 0xdd0d:
						setInterruptMask(&(cia[1]), value);
						break;
					case 0xdc04:
					case 0xdc05:
					case 0xdc06:
					case 0xdc07:	
						setTimer(&(cia[0]), addr-ADDR_CIA1, value);
						break;						
					case 0xdc08: 
						updateTimeOfDay10thOfSec(value);
						break;
					case 0xdc09: 
						updateTimeOfDaySec(value);
						break;						
					case 0xdd04:
					case 0xdd05:
					case 0xdd06:
					case 0xdd07:
						setTimer(&(cia[1]), addr-ADDR_CIA2, value);
						break;
					default:
						io_area[addr-0xd000]=value;
						break;
				}
			} 
			
			// SID stuff
			else if ((addr&0xfc00)==0xd400) {
				handleSidWrite(addr, value);
			} 
			  
			// remaining IO area (VIC, etc)
			else {
				switch (addr) {
				case 0xd019:
					setD019(value);
				default:
					io_area[addr - 0xd000]= value;
				}
			}
		} else {
			// normal RAM access
			memory[addr]=value;
		}
		
	} else {
		// normal RAM or
		// kernal ROM (even if the ROM is visible, writes always go to the RAM)
		memory[addr]=value;
	}
}

// c64 instruction modes
#define imp 0
#define imm 1
#define abs 2
#define abx 3
#define aby 4
#define zpg 6
#define zpx 7
#define zpy 8
#define ind 9
#define idx 10
#define idy 11
#define acc 12
#define rel 13

// enum of all c64 operations
enum {
	adc, alr, anc, and, ane, arr, asl, bcc, bcs, beq, bit, bmi, bne, bpl, brk, bvc, 
    bvs, clc, cld, cli, clv, cmp, cpx, cpy, dcp, dec, dex, dey, eor, inc, inx, iny, 
	isb, jam, jmp, jsr, lae, lax, lda, ldx, ldy, lsr, lxa, nop, ora, pha, php, pla, 
	plp, rla, rol, ror, rra, rti, rts, sax, sbc, sbx, sec, sed, sei, sha, shs, shx, 
	shy, slo, sre, sta, stx, sty, tax, tay, tsx, txa, txs, tya,
	l_a, c_a// additional pseudo ops used for D012 polling hack (replaces 2 jam ops..)
};
	
static unsigned int isClass2(int cmd) {
	switch (cmd) {
		case adc:
		case and:
		case cmp:
		case eor:
		case lae:
		case lax:
		case lda:
		case ldx:
		case ldy:
		case nop:
		case ora:
		case sbc:
		case l_a:	// additional ops introduced for D012-polling hacks 
		case c_a:
			return 1;
		default:
			return 0;
	}
}
	
static const int opcodes[256]  = {
	brk,ora,l_a,slo,nop,ora,asl,slo,php,ora,asl,anc,nop,ora,asl,slo,
	bpl,ora,c_a,slo,nop,ora,asl,slo,clc,ora,nop,slo,nop,ora,asl,slo,
	jsr,and,jam,rla,bit,and,rol,rla,plp,and,rol,anc,bit,and,rol,rla,
	bmi,and,jam,rla,nop,and,rol,rla,sec,and,nop,rla,nop,and,rol,rla,
	rti,eor,jam,sre,nop,eor,lsr,sre,pha,eor,lsr,alr,jmp,eor,lsr,sre,
	bvc,eor,jam,sre,nop,eor,lsr,sre,cli,eor,nop,sre,nop,eor,lsr,sre,
	rts,adc,jam,rra,nop,adc,ror,rra,pla,adc,ror,arr,jmp,adc,ror,rra,
	bvs,adc,jam,rra,nop,adc,ror,rra,sei,adc,nop,rra,nop,adc,ror,rra,
	nop,sta,nop,sax,sty,sta,stx,sax,dey,nop,txa,ane,sty,sta,stx,sax,
	bcc,sta,jam,sha,sty,sta,stx,sax,tya,sta,txs,shs,shy,sta,shx,sha,
	ldy,lda,ldx,lax,ldy,lda,ldx,lax,tay,lda,tax,lxa,ldy,lda,ldx,lax,
	bcs,lda,jam,lax,ldy,lda,ldx,lax,clv,lda,tsx,lae,ldy,lda,ldx,lax,
	cpy,cmp,nop,dcp,cpy,cmp,dec,dcp,iny,cmp,dex,sbx,cpy,cmp,dec,dcp,
	bne,cmp,jam,dcp,nop,cmp,dec,dcp,cld,cmp,nop,dcp,nop,cmp,dec,dcp,
	cpx,sbc,nop,isb,cpx,sbc,inc,isb,inx,sbc,nop,sbc,cpx,sbc,inc,isb,
	beq,sbc,jam,isb,nop,sbc,inc,isb,sed,sbc,nop,isb,nop,sbc,inc,isb
};

static const int modes[256]  = {
	imp,idx,abs,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,abs,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	abs,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,abx,abx,abx,abx,abx,
	imp,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imp,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,ind,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,abx,zpx,zpx,zpy,zpy,imp,aby,imp,aby,abx,abx,aby,aby,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpy,zpy,imp,aby,imp,aby,abx,abx,aby,aby,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx
};

// cycles per operation (adjustments apply)
static const int opBaseCycles[256] = {
	7,6,4,8,3,3,5,5,3,2,2,2,4,4,6,6,
	2,5,4,8,4,4,6,6,2,4,2,7,4,4,7,7,
	6,6,0,8,3,3,5,5,4,2,2,2,4,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7,
	6,6,0,8,3,3,5,5,3,2,2,2,3,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7,
	6,6,0,8,3,3,5,5,4,2,2,2,5,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7,
	2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4,
	2,6,0,5,4,4,4,4,2,5,2,5,5,5,5,5,
	2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4,
	2,5,0,5,4,4,4,4,2,4,2,4,4,4,4,4,
	2,6,2,8,3,3,5,5,2,2,2,2,4,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7,
	2,6,2,8,3,3,5,5,2,2,2,2,4,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7
};

// ----------------------------------------------- globale Faulheitsvariablen
static unsigned char bval;
static unsigned short wval;

static unsigned long sLastPolledOsc;
static void simOsc3Polling(unsigned short ad) {
	// handle busy polling for sid oscillator3 (e.g. Ring_Ring_Ring.sid)
	if ((ad == 0xd41b) && (memory[pc] == 0xd0) && (memory[pc+1] == 0xfb) /*BEQ above read*/) {					
		unsigned int t=(16777216/sid.v[2].freq)>>8; // cycles per 1 osc step up (if waveform is "sawtooth")
		
		unsigned long usedCycles= sCycles;
		if (sLastPolledOsc < usedCycles) {
			usedCycles-= sLastPolledOsc;					
		}				
		if (usedCycles<t) {
			sCycles+= (t-usedCycles);	// sim busywait	(just give them evenly spaced signals)
		}
		sLastPolledOsc= sCycles;

		io_area[0x041b]+= 1;	// this hack should at least avoid endless loops
	}				
}

static unsigned char getaddr(unsigned char opc, int mode)
{
	// reads all the bytes belonging to the operation and advances the pc accordingly
    unsigned short ad,ad2;  
    switch(mode)
    {
        case imp:
            return 0;
        case imm:
            return getmem(pc++);
        case abs:
            ad=getmem(pc++);
            ad|=getmem(pc++)<<8;

			simTimerPolling(ad, &sCycles, pc);
			simOsc3Polling(ad);
			
            return getmem(ad);
        case abx:
        case aby:			
            ad=getmem(pc++);
            ad|=getmem(pc++)<<8;
            ad2=ad +(mode==abx?x:y);

			if (isClass2(opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00)))	// page boundary crossed
				sCycles++;
				
            return getmem(ad2);
        case zpg:
			ad=getmem(pc++);
            return getmem(ad);
        case zpx:
            ad=getmem(pc++);
            ad+=x;
            return getmem(ad&0xff);
        case zpy:
            ad=getmem(pc++);
            ad+=y;
            return getmem(ad&0xff);
        case idx:
			// indexed indirect, e.g. LDA ($10,X)
            ad=getmem(pc++);
            ad+=x;
            ad2=getmem(ad&0xff);
            ad++;
            ad2|=getmem(ad&0xff)<<8;
            return getmem(ad2);
        case idy:
			// indirect indexed, e.g. LDA ($20),Y
            ad=getmem(pc++);
            ad2=getmem(ad);
            ad2|=getmem((ad+1)&0xff)<<8;
            ad=ad2+y;
			
			if (isClass2(opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00)))	// page boundary crossed
				sCycles++;
            return getmem(ad);
        case acc:
            return a;
    }  
    return 0;
}

static void setaddr(unsigned char opc, int mode, unsigned char val)
{
	// note: orig impl only covered the modes used by "regular" ops but
	// for the support of illegal ops there are some more..
    unsigned short ad,ad2;
    switch(mode)
    {
        case abs:
            ad=getmem(pc-2);
            ad|=getmem(pc-1)<<8;
            setmem(ad,val);
            return;
        case abx:
        case aby:
            ad=getmem(pc-2);
            ad|=getmem(pc-1)<<8;
	        ad2=ad +(mode==abx?x:y);
            setmem(ad2,val);
            return;
        case idx:
			// indexed indirect, e.g. LDA ($10,X)
            ad=getmem(pc++);
            ad+=x;
            ad2=getmem(ad&0xff);
            ad++;
            ad2|=getmem(ad&0xff)<<8;
			setmem(ad2,val);
            return;
        case idy:
			// indirect indexed, e.g. LDA ($20),Y
            ad=getmem(pc++);
            ad2=getmem(ad);
            ad2|=getmem((ad+1)&0xff)<<8;
            ad=ad2+y;
			
			if (isClass2(opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00)))	// page boundary crossed
				sCycles++;
			setmem(ad,val);
            return;
        case zpg:
            ad=getmem(pc-1);
            setmem(ad,val);
            return;
        case zpx:
        case zpy:
            ad=getmem(pc-1);
	        ad+=(mode==zpx?x:y);
            setmem(ad&0xff,val);
            return;
        case acc:
            a=val;
            return;
    }
}

static void putaddr(unsigned char opc, int mode, unsigned char val)
{
    unsigned short ad,ad2;
    switch(mode)
    {
        case abs:
            ad=getmem(pc++);
            ad|=getmem(pc++)<<8;
            setmem(ad,val);
            return;
        case abx:
        case aby:
            ad=getmem(pc++);
            ad|=getmem(pc++)<<8;				
            ad2=ad +(mode==abx?x:y);

			if (isClass2(opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00)))	// page boundary crossed
				sCycles++;
				
            setmem(ad2,val);
            return;
        case zpg:
            ad=getmem(pc++);
            setmem(ad,val);
            return;
        case zpx:
            ad=getmem(pc++);
            ad+=x;
            setmem(ad&0xff,val);
            return;
        case zpy:
            ad=getmem(pc++);
            ad+=y;
            setmem(ad&0xff,val);
            return;
        case idx:
            ad=getmem(pc++);
            ad+=x;
            ad2=getmem(ad&0xff);
            ad++;
            ad2|=getmem(ad&0xff)<<8;
            setmem(ad2,val);
            return;
        case idy:
			// no cycle adjustment needed here.. all relevant cases are handled in "getaddr"
            ad=getmem(pc++);
            ad2=getmem(ad);
            ad2|=getmem((ad+1)&0xff)<<8;
            ad=ad2+y;
            setmem(ad,val);
            return;
        case acc:
            a=val;
            return;
    }
}

static void setflags(int flag, int cond)
{
    if (cond) p|=flag;
    else p&=~flag;
}

unsigned char isIrqBlocked()
{
	unsigned char irqSet= (p & FLAG_I) != 0;	
	return irqSet;
}

void push(unsigned char val)
{
    setmem(0x100+s,val);	
	s= (s-1)&0xff;			// real stack just wraps around...
}

static unsigned char pop(void)
{
	s= (s+1)&0xff;			// real stack just wraps around...	
    return getmem(0x100+s);	// pos is now the new first free element..
}

static void branch(unsigned char opc, int flag)
{
    signed char dist;
    dist=(signed char)getaddr(opc, imm);
    wval=pc+dist;
    if (flag) { 		
    	sCycles+=((pc&0x100)!=(wval&0x100))?2:1; // + 1 if branch occurs to same page/ + 2 if branch occurs to different page		
		pc=wval; 
	}
}

void cpuReset(void)
{
    a=x=y=0;
    p=0;
    s=255; 
    pc= 0;
}

#ifdef DEBUG
unsigned short lastTraced;
// poor man's util to check what's going on..
void trace(unsigned short addr, char *text) {
/*
	if (pc == addr) {
		if (lastTraced != addr) {
			lastTraced= addr;
			AS3_Trace(AS3_String(text));
			
			if (addr == 0x1E0C) {
			AS3_Trace(AS3_String("mem:"));
			AS3_Trace(AS3_Number(memory[0x1]));
			}
			
		}	
	}
	*/
}
#endif

// KNOWN LIMITATION: flag handling in BCD mode is not implemented (see http://www.oxyron.de/html/opcodes02.html)
void cpuParse(void)
{
	simRasterline();

    unsigned char opc=getmem(pc++);
	
    int cmd=opcodes[opc];
    int mode=modes[opc];
	
	sCycles += opBaseCycles[opc];	// see adjustments in "branch", "putaddr" and "getaddr"
    
	int c;  
    switch (cmd)
    {
        case adc: {
			unsigned char in1= a;
			unsigned char in2= getaddr(opc, mode);
			
			// note: The carry flag is used as the carry-in (bit 0) for the operation, and the 
			// resulting carry-out (bit 8) value is stored in the carry flag.
            wval=(unsigned short)in1+in2+((p&FLAG_C)?1:0);	// "carry-in"
            setflags(FLAG_C, wval&0x100);
            a=(unsigned char)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
			
			// calc overflow flag (http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html)
			// also see http://www.6502.org/tutorials/vflag.html
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ a)&0x80);
			}
            break;
        case anc:	// used by Axelf.sid (Crowther)
            bval=getaddr(opc, mode);
			a= a&bval;
			
			// http://codebase64.org/doku.php?id=base:some_words_about_the_anc_opcode
            setflags(FLAG_C, a&0x80);
			
			// supposedly also sets these (http://www.oxyron.de/html/opcodes02.html)
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break;
        case and:
            bval=getaddr(opc, mode);
            a&=bval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break;
        case asl:
            wval=getaddr(opc, mode);
            wval<<=1;
            setaddr(opc, mode,(unsigned char)wval);
            setflags(FLAG_Z,!(wval&0xff));
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,wval&0x100);
            break;
        case bcc:
            branch(opc, !(p&FLAG_C));
            break;
        case bcs:
            branch(opc, p&FLAG_C);
            break;
        case bne:
            branch(opc, !(p&FLAG_Z));
            break;
        case beq:
            branch(opc, p&FLAG_Z);
            break;
        case bpl:
            branch(opc, !(p&FLAG_N));
            break;
        case bmi:
            branch(opc, p&FLAG_N);
            break;
        case bvc:
            branch(opc, !(p&FLAG_V));
            break;
        case bvs:
            branch(opc, p&FLAG_V);
            break;
        case bit:
            bval=getaddr(opc, mode);
            setflags(FLAG_Z,!(a&bval));
            setflags(FLAG_N,bval&0x80);
            setflags(FLAG_V,bval&0x40);	// bit 6
            break;
        case brk:					            
			pc= 0; // code probably called non existent ROM routine.. 
			
			/* proper impl would look like this:

			// pc has already been incremented by 1 (see above) 
			// (return address to be stored on the stack is original pc+2 )
			push((pc+1)>>8);
            push((pc+1));
            push(p);
			
			if (getProgramMode() == NMI_OFFSET_MASK) {
				pc=getmem(0xfffa);
				pc|=getmem(0xfffb)<<8;
 			} else {
				pc=getmem(0xfffe);
				pc|=getmem(0xffff)<<8;
 			}
            setflags(FLAG_I,1);
            setflags(FLAG_B,1);
			*/
            break;
        case clc:
            setflags(FLAG_C,0);
            break;
        case cld:
            setflags(FLAG_D,0);
            break;
        case cli:
            setflags(FLAG_I,0);
            break;
        case clv:
            setflags(FLAG_V,0);
            break;
        case cmp:
            bval=getaddr(opc, mode);
            wval=(unsigned short)a-bval;
            setflags(FLAG_Z,!wval);		// a == bval
            setflags(FLAG_N,wval&0x80);	// a < bval
            setflags(FLAG_C,a>=bval);
            break;
        case cpx:
            bval=getaddr(opc, mode);
            wval=(unsigned short)x-bval;
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);      
            setflags(FLAG_C,x>=bval);
            break;
        case cpy:
            bval=getaddr(opc,mode);
            wval=(unsigned short)y-bval;
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);      
            setflags(FLAG_C,y>=bval);
            break;
        case dcp:		// used by: Clique_Baby.sid
            bval=getaddr(opc, mode);
			// dec
            bval--;
            setaddr(opc,mode,bval);
			// cmp
            wval=(unsigned short)a-bval;
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,a>=bval);
            break;
        case dec:
            bval=getaddr(opc, mode);
            bval--;
            setaddr(opc,mode,bval);
            setflags(FLAG_Z,!bval);
            setflags(FLAG_N,bval&0x80);
            break;
        case dex:
            x--;
            setflags(FLAG_Z,!x);
            setflags(FLAG_N,x&0x80);
            break;
        case dey:
            y--;
            setflags(FLAG_Z,!y);
            setflags(FLAG_N,y&0x80);
            break;
        case eor:
            bval=getaddr(opc, mode);
            a^=bval;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case inc:
            bval=getaddr(opc, mode);
            bval++;
            setaddr(opc,mode,bval);
            setflags(FLAG_Z,!bval);
            setflags(FLAG_N,bval&0x80);
            break;
        case inx:
            x++;
            setflags(FLAG_Z,!x);
            setflags(FLAG_N,x&0x80);
            break;
        case iny:
            y++;
            setflags(FLAG_Z,!y);
            setflags(FLAG_N,y&0x80);
            break;
        case isb: {
			// inc
            bval=getaddr(opc, mode);
            bval++;
            setaddr(opc,mode,bval);
            setflags(FLAG_Z,!bval);
            setflags(FLAG_N,bval&0x80);

			// + sbc			
			unsigned char in1= a;
			unsigned char in2= bval;
			
            wval=(unsigned short)in1+in2+((p&FLAG_C)?1:0);
            setflags(FLAG_C, wval&0x100);
            a=(unsigned char)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
			
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ a)&0x80);
            }
			break;
		case jam:	// this op would have crashed the C64
		    pc=0;           // Just quit the emulation
            break;
        case jmp:
            wval=getmem(pc++);
            wval|=getmem(pc++)<<8;
            switch (mode) {
                case abs:
					if ((wval==pc-3) && (getProgramMode() == MAIN_OFFSET_MASK)) {
						pc= 0;	// main loop would steal cycles from NMI/IRQ which it normally would not..
					} else {
						pc=wval;
					}
                    break;
                case ind:
					// 6502 bug: JMP ($12FF) will fetch the low-byte from $12FF and the high-byte from $1200
                    pc=getmem(wval);
                    pc|=getmem((wval==0xff) ? 0 : wval+1)<<8;
                    break;
            }
            break;
        case jsr:
			// pc has already been incremented by 1 (see above) 
			// (return address to be stored on the stack is original pc+2 )
            push((pc+1)>>8);
            push((pc+1));
            wval=getmem(pc++);
            wval|=getmem(pc++)<<8;
            pc=wval;
            break;
		case lax:
			// e.g. Vicious_SID_2-15638Hz.sid
            a=getaddr(opc, mode);
			x= a;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case lda:
            a=getaddr(opc, mode);
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case ldx:
            x=getaddr(opc, mode);
            setflags(FLAG_Z,!x);
            setflags(FLAG_N,x&0x80);
            break;
        case ldy:
            y=getaddr(opc, mode);
            setflags(FLAG_Z,!y);
            setflags(FLAG_N,y&0x80);
            break;
        case lsr:      
            bval=getaddr(opc, mode); 
			wval=(unsigned char)bval;
            wval>>=1;
            setaddr(opc,mode,(unsigned char)wval);
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,bval&1);
            break;
        case nop:
			getaddr(opc, mode);	 // make sure the PC is advanced correctly
            break;
        case ora:
            bval=getaddr(opc, mode);
            a|=bval;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case pha:
            push(a);
            break;
        case php:
            push(p);
            break;
        case pla:
            a=pop();
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case plp:
            p=pop();
            break;
        case rla:				// see Spasmolytic_part_6.sid
			// rol
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&0x80);
            bval<<=1;
            bval|=c;
            setaddr(opc,mode,bval);

			// + and
            a&=bval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break;
        case rol:
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&0x80);
            bval<<=1;
            bval|=c;
            setaddr(opc,mode,bval);
            setflags(FLAG_N,bval&0x80);
            setflags(FLAG_Z,!bval);
            break;
        case ror:
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&1);
            bval>>=1;
            bval|= 0x80*c;
            setaddr(opc,mode,bval);
            setflags(FLAG_N,bval&0x80);
            setflags(FLAG_Z,!bval);
            break;
        case rra:
			// ror
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&1);
            bval>>=1;
            bval|=0x80*c;
            setaddr(opc,mode,bval);

			// + adc
			unsigned char in1= a;
			unsigned char in2= bval;
			
            wval=(unsigned short)in1+in2+((p&FLAG_C)?1:0);
            setflags(FLAG_C, wval&0x100);
            a=(unsigned char)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
			
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ a)&0x80);
            break;
        case rti:
			// timing hack: some optimized progs use JMP to an RTI that is placed such that
			// the nearby interrupt status register is implicitly also read - thereby automatically 
			// acknowledging the interrupt without having to explicitly read the register.
			switch(pc) {
				case 0xdc0d:
					getInterruptStatus(&(cia[0]));
					break;
				case 0xdd0d:		
					getInterruptStatus(&(cia[1]));
					break;
				default:
					break;
			}

			p=pop();                        
            wval=pop();
            wval|=pop()<<8;
            pc=wval;	// not like 'rts'! correct address is expected here!
			
			// todo: if interrupts where to be handled correctly then we'd need to clear interrupt flag here
			// (and set it when NMI is invoked...)
            break;
        case rts:
            wval=pop();
            wval|=pop()<<8;
            pc=wval+1;
            break;
        case sbc:    {
            bval=getaddr(opc, mode)^0xff;
			
			unsigned char in1= a;
			unsigned char in2= bval;
						
            wval=(unsigned short)in1+in2+((p&FLAG_C)?1:0);
            setflags(FLAG_C, wval&0x100);
            a=(unsigned char)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
			
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ a)&0x80);
			}
            break;
        case sax:				// e.g. Vicious_SID_2-15638Hz.sid
			getaddr(opc, mode);	 // make sure the PC is advanced correctly
            bval=a&x;
			setaddr(opc,mode,bval);
            break;
		case sbx:	// used in Artefacts.sid	
			bval=getaddr(opc, mode);
			x=(x&a)-bval;
			break;
        case sec:
            setflags(FLAG_C,1);
            break;
        case sed:
            setflags(FLAG_D,1);
            break;
        case sei:
            setflags(FLAG_I,1);
            break;
        case slo:			// see Spasmolytic_part_6.sid
			// asl
            wval=getaddr(opc, mode);
            wval<<=1;
            setaddr(opc,mode,(unsigned char)wval);
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,wval&0x100);
			// + ora
            bval=wval & 0xff;
            a|=bval;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);			
            break;
        case sre:      		// see Spasmolytic_part_6.sid
			// lsr
            bval=getaddr(opc, mode); 
			wval=(unsigned char)bval;
            wval>>=1;
            setaddr(opc,mode,(unsigned char)wval);
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,bval&1);
			// + eor
            bval=wval & 0xff;
            a^=bval;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case sta:
            putaddr(opc,mode,a);
            break;
        case stx:
            putaddr(opc,mode,x);
            break;
        case sty:
            putaddr(opc,mode,y);
            break;
        case tax:
            x=a;
            setflags(FLAG_Z, !x);
            setflags(FLAG_N, x&0x80);
            break;
        case tay:
            y=a;
            setflags(FLAG_Z, !y);
            setflags(FLAG_N, y&0x80);
            break;
        case tsx:
			x=s;
            setflags(FLAG_Z, !x);
            setflags(FLAG_N, x&0x80);
            break;
        case txa:
            a=x;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break;
        case txs:
            s=x;
            break;
        case tya:
            a=y;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break; 
		// ----- D012 hacks ->	
        case c_a:
			// this op (0x12) - which is normally illegal (and would freeze the machine)
			// can be used to patch programs that use CMP $D012 for comparisons (see 'l_a' for explanation).
			// the op: "0x12 0x11 0x99" allows to supply 2 infos: $11 the countdown value and $99 which is unused
			// each use of the op drives the countdown (hack only implemented for abs mode, i.e. 3 byte op)

            wval=getmem(pc++);	// countdown
            getmem(pc++);  // unused			
			bval= 0;
			if (!sFake_d012_count) {
				sFake_d012_count= wval & 0xff;
			} 
			if (--sFake_d012_count == 0) {
				bval= a;
			}	
            wval=(unsigned short)a-bval;
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,a>=bval);
            break;			
        case l_a:
			// this op (0x02) - which is normally illegal (and would freeze the machine)
			// can be used to patch programs that poll LDA $D012 for comparisons (see NMI sample player in 
			// Wonderland_XII-Digi_part_1.sid). Since VIC is not properly simulated during NMI handling, 
			// respective conditions will never work.. for the above case some fixed size loop is probably 
			// a better -but far from correct- solution.

			// the op: "0x02 0x11 0x22" allows to supply 2 infos: $11 the countdown value and $22 the fake result 
			// that is returned at the end of the countdown.. each use of the op drives the countdown
			
			// hack only implemented for abs mode, i.e. 3 byte op. also see "c_a"

            bval=getmem(pc++);	// countdown
            wval=getmem(pc++);  // success result
			
			a= 0;
			if (!sFake_d012_count && !sFake_d012_loop) {
				sFake_d012_count= bval;
				sFake_d012_loop= 40;	// slow down more
			}
			if (--sFake_d012_count == 0) {
					sFake_d012_count= bval;
				if (--sFake_d012_loop == 0) {
					a= wval;
					sFake_d012_loop= 40;	// slow down more
				}
			}			
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
			
		default:			
//AS3_Trace(AS3_String("------------ use of illegal opcode ---------------"));
//AS3_Trace(AS3_Number(opc));
//fprintf(stderr, "op code not implemented: %d at %d\n", opc, pc);
			getaddr(opc, mode);	 // at least make sure the PC is advanced correctly (potentially used in timing)
    }
}

static void resetEngine(unsigned long mixfrq) 
{
	mixing_frequency = mixfrq;
	
	freqmul = 15872000 / mixfrq;
	filtmul = pfloat_ConvertFromFloat(21.5332031f)/mixfrq;

	memSet((unsigned char*)&sid,0,sizeof(sid));
	memSet((unsigned char*)osc,0,sizeof(osc));
	memSet((unsigned char*)&filter,0,sizeof(filter));
	
	int i;
	for (i=0;i<3;i++) {
		// note: by default the rest of sid, osc & filter 
		// above is set to 0
		osc[i].envphase= Release;
		osc[i].zero_lock= 1;
		osc[i].noiseval = 0xffffff;		

		sMuteVoice[i]= 0;
	}
	
	pc= 0;
	a= x= y= s= p= 0;
	bval= 0;
	wval= 0;
	
	// status
	sFrameCount= 0;	
	sCycles= 0;
	
	sAdsrBugTriggerTime= 0;
	sAdsrBugFrameCount= 0;

	// reset hacks
	sCiaNmiVectorHack= 0;
	sDummyDC04= 0;
	sFake_d012_count= 0;
	sFake_d012_loop= 0;
		
	sLastPolledOsc= 0;
}

//   initialize SID and frequency dependant values
void synth_init(unsigned long mixfrq)
{
	resetEngine(mixfrq);
	
	resetPsidDigi();
	
	// envelope-generator stuff
	unsigned long cyclesPerSec= getCyclesPerSec();
	
	cycleOverflow= 0;
	cyclesPerSample= ((float)cyclesPerSec/mixfrq);
	
	// in regular SID, 15-bit LFSR counter counts cpu-clocks, our problem is the lack of cycle by cycle 
	// SID emulation (we only have a SID snapshot every 20ms to work with) during rendering our computing 
	// granularity then is 'one sample' (not 'one cpu cycle' - but around 20).. instead of still trying to simulate a
	// 15-bit cycle-counter we may directly use a sample-counter instead (which also reduces rounding issues).
	
	int i;
#ifdef USE_SAMPLE_ENV_COUNTER
	limit_LFSR= round(((float)0x8000)/cyclesPerSample);	// original counter was 15-bit
	for (i=0; i<16; i++) {
		// counter must reach respective threshold before envelope value is incremented/decremented
		envelope_counter_period[i]= (int)round((float)(attackTimes[i]*cyclesPerSec)/1000/256/cyclesPerSample)+1;	// in samples
		envelope_counter_period_clck[i]= (int)round((float)(attackTimes[i]*cyclesPerSec)/1000/256)+1;				// in clocks
	}
#else
	limit_LFSR= 0x8000;	// counter 15-bit
	for (i=0;i<16;i++) {
		// counter must reach respective threshold before envelope value is incremented/decremented
		envelope_counter_period[i]=      (int)floor((float)(attackTimes[i]*cyclesPerSec)/1000/256)+1;	// in samples
		envelope_counter_period_clck[i]= (int)floor((float)(attackTimes[i]*cyclesPerSec)/1000/256)+1;	// in clocks
	}
#endif	
	// lookup table for decay rates
	unsigned char from[] =  {93, 54, 26, 14,  6,  0};
	unsigned char val[] = { 1,  2,  4,  8, 16, 30};
	for (i= 0; i<256; i++) {
		unsigned char v= 1;
		for (unsigned char j= 0; j<6; j++) {
			if (i>from[j]) {
				v= val[j];
				break;
			}
		}
		exponential_delays[i]= v;
	}
}