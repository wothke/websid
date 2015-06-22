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
 *    <li>poor man's VIC and CIA handling
 *    <li>"cycle limit" feature used to interrupt emulation runs (e.g. main prog can now be suspended/continued)
 *    <li>Poor man's "combined pulse/triangle waveform" impl to allow playback of songs like Kentilla.sid.
 *    <li>added RSID digi playback support (D418 based as well as "pulse width modulation" based): it is a "special feature" of 
 *    this implementation that regular SID emulation is performed somewhat independently from the handling of digi samples, i.e. playback
 *    of digi samples is tracked separately (for main, IRQ and NMI) and the respective digi samples are then merged with the regular SID 
 *    output as some kind of postprocessing step 
 *  </ol>
 *
 * <p>Notice: if you have questions regarding the details of the below SID emulation, then you should better get in touch with R.Sinsch :-)
 *
 * <p>Tiny'R'Sid add-ons (c) 2013 J.Wothke
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

#include <string.h>

//#define DEBUG 
#include "defines.h"

#include "nanovic.h"
#include "nanocia.h"
#include "sidengine.h"
#include "rsidengine.h"
#include "sidplayer.h"



unsigned int sProgramMode= MAIN_OFFSET_MASK;

#define USE_FILTER

/* Routines for quick & dirty float calculation */

void memSet(unsigned char *mem, char val, int len) {
	// for some reason 'memset' does not seem to work in Alchemy...
	int i;
	for (i= 0; i<len; i++) {
		mem[i]= val;
	}
}

static inline int pfloat_ConvertFromInt(int i)
{
    return (i<<16);
}
static inline int pfloat_ConvertFromFloat(float f)
{
    return (int)(f*(1<<16));
}
static inline int pfloat_Multiply(int a, int b)
{
    return (a>>8)*(b>>8);
}
static inline int pfloat_ConvertToInt(int i)
{
    return (i>>16);
}

// SID register definition
struct s6581 {
    struct sidvoice {
        unsigned short freq;
        unsigned short pulse;
        unsigned char wave;
        unsigned char ad;
        unsigned char sr;
    } v[3];
    unsigned char ffreqlo;
    unsigned char ffreqhi;
    unsigned char res_ftv;
    unsigned char ftp_vol;
};

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
    signed long   envval;
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


// ---------------------------------------------------------- constants
static const float attackTimes[16]  =
{
  0.0022528606, 0.0080099577, 0.0157696042, 0.0237795619, 0.0372963655, 
  0.0550684591,0.0668330845, 0.0783473987, 0.0981219818, 0.244554021,
  0.489108042, 0.782472742, 0.977715461, 2.93364701, 4.88907793, 7.82272493
};
static const float decayReleaseTimes[16]  =
{
    0.00891777693, 0.024594051, 0.0484185907, 0.0730116639, 0.114512475,
    0.169078356, 0.205199432, 0.240551975, 0.301266125, 0.750858245,
    1.50171551, 2.40243682, 3.00189298, 9.00721405, 15.010998, 24.0182111
};

// ------------------------ pseudo-constants (depending on mixing freq)
static int  mixing_frequency;
static unsigned long  freqmul;
static int  filtmul;
static unsigned long  attacks [16];
static unsigned long  releases[16];

// ------------------------------------------------------------ globals
static struct s6581 sid;
static struct sidosc osc[3];
static struct sidflt filter;

/* Get the bit from an unsigned long at a specified position */
static inline unsigned char get_bit(unsigned long val, unsigned char b)
{
    return (unsigned char) ((val >> b) & 1);
}

// ------------------------------------------------------------- synthesis


// known limitation: basic-ROM specific handling not implemented...

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

static unsigned char dummyCount;

unsigned long sTodInMillies= 0;

/* ----------------------------------------- Variables for sample stuff */
static int sample_active;
static int sample_position, sample_start, sample_end, sample_repeat_start;
static int fracPos = 0;  /* Fractal position of sample */
static int sample_period;
static int sample_repeats;
static int sample_order;
static int sample_nibble;

static inline int GenerateDigi(int sIn)
{
    static int sample = 0;

    if (!sample_active) return(sIn);

    if ((sample_position < sample_end) && (sample_position >= sample_start))
    {
		//Interpolation routine
		//float a = (float)fracPos/(float)mixing_frequency;
		//float b = 1-a;
		//sIn += a*sample + b*last_sample;

        sIn += sample;

        fracPos += 985248/sample_period;		// CIA Timer clock rate 0.985248MHz (PAL)
        
        if (fracPos > mixing_frequency) 
        {
            fracPos%=mixing_frequency;

			// Naechstes Samples holen
            if (sample_order == 0) {
                sample_nibble++;                        // Naechstes Sample-Nibble
                if (sample_nibble==2) {
                    sample_nibble = 0;
                    sample_position++;
                }
            }
            else {
                sample_nibble--;
                if (sample_nibble < 0) {
                    sample_nibble=1;
                    sample_position++;
                }
            }       
            if (sample_repeats)
            {
                if  (sample_position > sample_end)
                {
                    sample_repeats--;
                    sample_position = sample_repeat_start;
                }                       
                else sample_active = 0;
            }
            
            sample = memory[sample_position&0xffff];
            if (sample_nibble==1)   // Hi-Nibble holen?     
                sample = (sample & 0xf0)>>4;
            else sample = sample & 0x0f;
			
			sample = (sample << 11) - 0x4000; // transform unsigned 4 bit range into signed 16 bit (–32,768 to 32,767) range			
        }
    }
    return (sIn);
}

// poor man's lookup table for combined pulse/triangle waveform (this table does not 
// lead to correct results but it is better that nothing for songs like Kentilla.sid)
// feel free to come up with a better impl!
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

//   initialize SID and frequency dependant values

void synth_init(unsigned long mixfrq)
{
  int i;
  mixing_frequency = mixfrq;
  fracPos = 0;
  freqmul = 15872000 / mixfrq;
  filtmul = pfloat_ConvertFromFloat(21.5332031f)/mixfrq;
  for (i=0;i<16;i++) {
    attacks [i]=(int) (0x1000000 / (attackTimes[i]*mixfrq));
 //   releases[i]=(int) (0x1000000 / (decayReleaseTimes[i]*mixfrq));	// Rockbox version
    releases[i]=(int) (0x1000000 / (attackTimes[i]*mixfrq)); // MGr: the decay/release times are three times as long as the corresponding attack times.  We use the attack value here and get it stretched by three times with our exponential envelope curve fix below.
  }
  memSet((unsigned char*)&sid,0,sizeof(sid));
  memSet((unsigned char*)osc,0,sizeof(osc));
  memSet((unsigned char*)&filter,0,sizeof(filter));
  osc[0].noiseval = 0xffffff;
  osc[1].noiseval = 0xffffff;
  osc[2].noiseval = 0xffffff;  
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
        osc[v].attack  = attacks[sid.v[v].ad >> 4];
        osc[v].decay   = releases[sid.v[v].ad & 0xf];
        osc[v].sustain = sid.v[v].sr & 0xf0;
        osc[v].release = releases[sid.v[v].sr & 0xf];
        osc[v].wave    = sid.v[v].wave;
        osc[v].freq    = ((unsigned long)sid.v[v].freq)*freqmul;
    }

#ifdef USE_FILTER
  //filter.freq  = (16 * sid.ffreqhi + (sid.ffreqlo&0x7)) * filtmul;

  filter.freq  = ((sid.ffreqhi << 3) + (sid.ffreqlo&0x7)) * filtmul;
  filter.freq <<= 1; // MGr

 if (filter.freq>pfloat_ConvertFromInt(1))
     filter.freq=pfloat_ConvertFromInt(1);
  // the above line isnt correct at all - the problem is that the filter
  // works only up to rmxfreq/4 - this is sufficient for 44KHz but isnt
  // for 32KHz and lower - well, but sound quality is bad enough then to
  // neglect the fact that the filter doesnt come that high ;)
  filter.l_ena = get_bit(sid.ftp_vol,4);
  filter.b_ena = get_bit(sid.ftp_vol,5);
  filter.h_ena = get_bit(sid.ftp_vol,6);
  filter.v3ena = !get_bit(sid.ftp_vol,7);
  filter.vol   = (sid.ftp_vol & 0xf);
//  filter.rez   = 1.0-0.04*(float)(sid.res_ftv >> 4);
  filter.rez   = pfloat_ConvertFromFloat(1.2f) - 
          pfloat_ConvertFromFloat(0.04f)*(sid.res_ftv >> 4);
  /* We precalculate part of the quick float operation, saves time in loop later */
  filter.rez>>=8;
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
            // reset counter / noise generator if reset get_bit set
            if (osc[v].wave & 0x08) {
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
            unsigned char triout = (unsigned char) (osc[v].counter>>19);
						
            if (osc[v].counter>>27) {
                triout^=0xff;
			}
            unsigned char sawout = (unsigned char) (osc[v].counter >> 20);
            unsigned char plsout = (unsigned char) ((osc[v].counter > osc[v].pulse)-1);

			if ((osc[v].wave & 0x40) && (osc[v].wave & 0x10)) {
				// see $50 waveform impl below.. (because the impl is pretty bad, this
				// is an attempt to limit undesireable side effects and keep the original
				// impl unchanged as far as possible..)
				plsout = (unsigned char) ((osc[v].counter >= osc[v].pulse)? 0xff : 0x0);
			}
	  
            // generate noise waveform exactly as the SID does. 
			
            if (osc[v].noisepos!=(osc[v].counter>>23))	
            {
                osc[v].noisepos = osc[v].counter >> 23;	
                osc[v].noiseval = (osc[v].noiseval << 1) |
                        (get_bit(osc[v].noiseval,22) ^ get_bit(osc[v].noiseval,17));
						
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
                    triout ^= 0xff;
			
            // now mix the oscillators with an AND operation as stated in
            // the SID's reference manual - even if this is completely wrong.
            // well, at least, the $30 and $70 waveform sounds correct and there's
            // no real solution to do $50 and $60, so who cares.
			
			// => wothke: the above statement is nonsense: there are many songs that need $50!

            unsigned char outv=0xFF;
			
            if ((osc[v].wave & 0x40) && (osc[v].wave & 0x10))  {		
				// this is a poor man's impl for $50 waveform to improve playback of 
				// songs like Kentilla.sid, Convincing.sid, etc
								
				outv &= (pulseTriangleWavetable[(triout>>1)] & plsout);			
			} else {
				if (osc[v].wave & 0x10) outv &= triout;
				if (osc[v].wave & 0x20) outv &= sawout;
				if (osc[v].wave & 0x40) outv &= plsout;
				if (osc[v].wave & 0x80) outv &= nseout;
			}
			
            

      // now process the envelopes. the first thing about this is testing
      // the gate bit and put the EG into attack or release phase if desired
// -----> Rockbox: non existent	  
      if (!(osc[v].wave & 0x01)) osc[v].envphase=3;
      else if (osc[v].envphase==3) osc[v].envphase=0;
// <-----	  
	  
            // so now process the volume according to the phase and adsr values
            switch (osc[v].envphase) {
                case 0 : {                          // Phase 0 : Attack
                    osc[v].envval+=osc[v].attack;
                    if (osc[v].envval >= 0xFFFFFF)
                    {
                        osc[v].envval   = 0xFFFFFF;
                        osc[v].envphase = 1;
                    }
                    break;
                }
                case 1 : {                          // Phase 1 : Decay
				// osc[v].envval-=osc[v].decay;   
					
				// ------> Rockbox: non existent
                   if ( (signed int)osc[v].envval > 0x888888 ) { // MGr: Approximate exponential curve by picewise linear functions.
                       osc[v].envval-=osc[v].decay;              //      1/2*1 + 1/4*2 + 1/8*4 + 1/16*8 + 1/16*16 = 3
                   } else if ( (signed int)osc[v].envval > 0x444444 ) {
                       osc[v].envval-=osc[v].decay>>1;
                   } else if ( (signed int)osc[v].envval > 0x222222 ) {
                       osc[v].envval-=osc[v].decay>>2;
                   } else if ( (signed int)osc[v].envval > 0x111111 ) {
                       osc[v].envval-=osc[v].decay>>3;
                   } else {
                       osc[v].envval-=osc[v].decay>>4;
                   }
				 // <-------
                    if ((signed int) osc[v].envval <= (signed int) (osc[v].sustain<<16))
                    {
                        osc[v].envval   = osc[v].sustain<<16;
                        osc[v].envphase = 2;
                    }
                    break;
                }
                case 2 : {                          // Phase 2 : Sustain
                    if ((signed int) osc[v].envval != (signed int) (osc[v].sustain<<16))
                    {
                        osc[v].envphase = 1;
                    }
                    // :) yes, thats exactly how the SID works. and maybe
                    // a music routine out there supports this, so better
                    // let it in, thanks :)
                    break;
                }
                case 3 : {                          // Phase 3 : Release
					// osc[v].envval-=osc[v].release;
                    //if (osc[v].envval < 0x40000) osc[v].envval= 0x40000;
					
		// --------> Rockbox: non existent		
                   if ( (signed int)osc[v].envval > 0x888888 ) {
                       osc[v].envval-=osc[v].release;
                   } else if ( (signed int)osc[v].envval > 0x444444 ) {
                       osc[v].envval-=osc[v].release>>1;
                   } else if ( (signed int)osc[v].envval > 0x222222 ) {
                       osc[v].envval-=osc[v].release>>2;
                   } else if ( (signed int)osc[v].envval > 0x111111 ) {
                       osc[v].envval-=osc[v].release>>3;
                   } else {
                       osc[v].envval-=osc[v].release>>4;
                   }
                   if (osc[v].envval < 0x1) osc[v].envval= 0x1; // MGr: The origonal value of 40000 may be ok for 6581 but my 8580 is a lot more ideal.  (Tested with Savage.sid)
                     // the volume offset is because the SID does not
                     // completely silence the voices when it should. most
                     // emulators do so though and thats the main reason
                     // why the sound of emulators is too, err... emulated :)
		// <------			 
                   break;
                }
            }
            // now route the voice output to either the non-filtered or the
            // filtered channel and dont forget to blank out osc3 if desired
#ifdef USE_FILTER
            if ((v<2) || filter.v3ena) {
                if (osc[v].filter) {
                    outf+=(((int)(outv-0x80))*osc[v].envval)>>22;
                } else {
                    outo+=(((int)(outv-0x80))*osc[v].envval)>>22;
				}
            }
#endif
#ifndef USE_FILTER
            // Don't use filters, just mix all voices together
            outf+=((signed short)(outv-0x80)) * (osc[v].envval>>8);	// Rockbox: >>4
#endif
        }
        // step 3
        // so, now theres finally time to apply the multi-mode resonant filter
        // to the signal. The easiest thing ist just modelling a real electronic
        // filter circuit instead of fiddling around with complex IIRs or even
        // FIRs ...
        // it sounds as good as them or maybe better and needs only 3 MULs and
        // 4 ADDs for EVERYTHING. SIDPlay uses this kind of filter, too, but
        // Mage messed the whole thing completely up - as the rest of the
        // emulator.
        // This filter sounds a lot like the 8580, as the low-quality, dirty
        // sound of the 6581 is uuh too hard to achieve :) 


#ifdef USE_FILTER

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

#endif
#ifndef USE_FILTER

		int final_sample = outf>>10;
#endif

		final_sample= GenerateDigi(final_sample);		// Rockbox <<13

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
int screen= 0;
char hex1 [16]= {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
char *pokeInfo;

void traceSidPoke(int reg, unsigned char val) {
	pokeInfo= malloc(sizeof(char)*13);

	pokeInfo[0]= hex1[(screen>>12)&0xf];
	pokeInfo[1]= hex1[(screen>>8)&0xf];
	pokeInfo[2]= hex1[(screen>>4)&0xf];
	pokeInfo[3]= hex1[(screen&0xf)];
	pokeInfo[4]= ' ';
	pokeInfo[5]= 'D';
	pokeInfo[6]= '4';
	pokeInfo[7]= hex1[(reg>>4)];
	pokeInfo[8]= hex1[(reg&0xf)];
	pokeInfo[9]= ' ';
	pokeInfo[10]= hex1[(val>>4)];
	pokeInfo[11]= hex1[(val&0xf)];
	
	AS3_Trace(AS3_String(pokeInfo));		

	free(pokeInfo);
}
#endif

//
// C64 Mem Routines
//

// used to handle the "pulse width modulation" based digi impl used by Swallow
static unsigned int sSwallowTypeDigi[3];

//
// Poke a value into the sid register
//
void sidPoke(int reg, unsigned char val)
{
    int voice=0;
	if (reg < 7) {
#ifdef DEBUG
//		traceSidPoke(reg, val);	
#endif	
	}
	
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
			if ((sid.v[voice].wave & 0x8) && !(val & 0x8) && (val & 0x40) 
				&& (((sid.v[voice].ad == 0) && (sid.v[voice].sr == 0xf0)) && (((sid.v[voice].pulse&0xff00) == 0x0800) 
					||((sid.v[voice].pulse == 0x0555) && (sid.v[voice].freq == 0xfe04))		// e.g. Spasmolytic_part_2.sid
					||((sid.v[voice].pulse == 0x08fe) && (sid.v[voice].freq == 0xffff))
					))) {
					
				// the tricky part here is that the above test does not trigger for songs which act similarily 
				// but which are not using "pulse width modulation" to play digis (e.g. Combat_School.sid, etc)
				
				sSwallowTypeDigi[voice]= 1;
				
				sid.v[voice].wave= 0;	// kill the wheezing base signal (in old&new players)
				sid.v[voice].freq= 0;	// kill the wheezing base signal (in new players)
				
			} else {	
				sid.v[voice].wave = val;
			}
			
            // Directly look at GATE-Bit!
            // a change may happen twice or more often during one cpujsr
            // Put the Envelope Generator into attack or release phase if desired 
            //
            if ((val & 0x01) == 0) osc[voice].envphase=3;
            else if (osc[voice].envphase==3) osc[voice].envphase=0;
            break;
        }

        case 5: { sid.v[voice].ad = val; break;}
        case 6: { 
			if (sSwallowTypeDigi[voice]) {				
				sid.v[voice].sr= 0;		// kill the wheezing base signal
			} else {
				sid.v[voice].sr = val; 
			}
			break;
			}

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
			if ((addr >= 0xdc00) && (addr < 0xde00)) {
				addr&=0xFF0F;	// handle the 16 mirrored CIA registers just in case
			}
			switch (addr) {
				// SID
				case 0xd41c:					
					// used by Alien.sid
					return (osc[2].envval >> 16) + 0x80;	// Voice #3 ADSR output
					
				// CIA	
				case 0xdc01:
					return 0xff;	// some songs use this as an exit criteria.. see Master_Blaster_intro.sid
				case 0xdc04:
					if (sIsPSID) {
						// hack for Delta_Mix-E-Load_loader.sid which uses counter to control the progress of its melody:
						// PSID is invoked via CIA1 timer.. however regular timing handling (e.g. based on used cycles)
						// does not seem to work - but this hack does..
						dummyCount+=3;
						return dummyCount;
					}
					return io_area[addr-0xd000];
				case 0xdc06:
					// hack originally used for Storebror.sid.. but apparently other songs also 
					// "benefit" from it (e.g. Uwe Anfang's stuff)...  always use 0x08xx NMI vector 
					// (our timing is too imprecise to reliably calc the required values between 0x08 and 
					// 0x0e.. (other songs - e.g. Hunters_Moon.sid - are using 0x02-0x09) but since jitter is the
					// least of our problems, there is no need for the respective timing logic anyways)
					return 0x08;							
				case 0xdc0d:
					return getInterruptStatus(&(cia[0]));
				case 0xdd0d:		
					return getInterruptStatus(&(cia[1]));
				
				case 0xdc08: 
					// TOD tenth of second
					return (sTodInMillies%1000)/100;
				case 0xdc09: 
					// TOD second
					return ((unsigned int)(sTodInMillies/1000))%60;
					
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

static int internal_period, internal_order, internal_start, internal_end,
internal_add, internal_repeat_times, internal_repeat_start;

// ----------------------------------------------------------------- Register

unsigned char a,x,y,s,p;	// p= status register



/*
* work buffers used to record digi samples produced by NMI/IRQ/main
*/ 
unsigned int sDigiCount= 0;

unsigned long sDigiTime[DIGI_BUF_SIZE];		// time in cycles counted from the beginning of the current screen
unsigned char sDigiVolume[DIGI_BUF_SIZE];	// 8-bit sample

/*
* samples which already belong to the next screen, e.g. produced by some long running IRQ which
* crossed over to the next screen..
*/
unsigned int sOverflowDigiCount= 0;
unsigned long sOverflowDigiTime[DIGI_BUF_SIZE];
unsigned char sOverflowDigiVolume[DIGI_BUF_SIZE];

static void recordSample(unsigned char sample) {
	if (sCycles <= sTotalCyclesPerScreen) {
		sDigiTime[sDigiCount%(DIGI_BUF_SIZE-1)]= sCycles;
		sDigiVolume[sDigiCount%(DIGI_BUF_SIZE-1)]= sample;	// always use 8-bit to ease handling									

		sDigiCount+=1;	// buffer is meant to collect no more than the samples from one screen refresh!
	}
	else {
		// some players (e.g. Digital_Music.sid) start long running IRQ routines at the end of one screen 
		// producing most of their output on the next screen... so we have to deal with this scenario..
		
		sOverflowDigiTime[sDigiCount%(DIGI_BUF_SIZE-1)]= sCycles-sTotalCyclesPerScreen;
		sOverflowDigiVolume[sDigiCount%(DIGI_BUF_SIZE-1)]= sample;	// always use 8-bit to ease handling									

		sOverflowDigiCount+=1;	// buffer is meant to collect no more than the samples from one screen refresh!		
	}
}

static void handleSwallowDigi(unsigned short addr, unsigned char value) {
	// depending in the specific player routine, the sample info is available in different 
	// registers(d402/d403 and d409/a) ..  for retrieval d403 seems to work for most player impls
	if (sSwallowTypeDigi[0] && (addr == 0xd403)) {
		recordSample((value<<4) & 0xff);		// Swallow's PWM uses 4-bit samples
	}	
}

static unsigned char handleNewGenDigi(unsigned short addr, unsigned short ctrlReg, unsigned short freqCtrlReg, unsigned char value) {
	if ((addr == ctrlReg) && (value&0x8)) {	// 8-bit digi sound created using "test bit" feature..
		io_area[ctrlReg-0xd000]= value;		
	} 
	// the test bit approach to play 8-bit samples comes in different flavors..
	else  {
		// actually the test-bit needs longer be active when the sample is played (see Vicious_SID_2-15638Hz.sid)
		// rather we'd need to check the time since the test bit was last set.. (for now we dont because only the
		// one song mentioned seems to be affected..) 
		if ((addr == freqCtrlReg) && (io_area[ctrlReg-0xd000]&0x8)) {	// "Frequency Control - High-Byte" 	
			recordSample(value);		// e.g. THCM stuff	
			return 1;
		} else if ((addr == (freqCtrlReg+1)) && (io_area[ctrlReg-0xd000]&0x8)) {	// "Pulse Waveform Width - Low-Byte"
			// FIXME: untested .. might not be as simple as this.. examples?
			recordSample(value);	
			return 1;			
		} else if ((addr == ctrlReg) && (sProgramMode == NMI_OFFSET_MASK)
						&& !(io_area[ctrlReg-0xd000]&0x8)	// must not be confused with "regular" PWM cases.
			){	
						
			// Ice_Guys.sid uses an interesting kind of digi approach (samples seem to be written into d412!?
			// this impl will play digis that are barely recognizable.. but
			// nowhere the quality you get when playing the song with Acid64, etc
			recordSample(((value + 0x80)&0xf0));
			return 1;
		}
	}
	return 0;
}

void handlePsidDigs(unsigned short addr, unsigned char value) {			
	// Neue SID-Register
	if ((addr > 0xd418) && (addr < 0xd500))
	{		
		// Start-Hi
		if (addr == 0xd41f) internal_start = (internal_start&0x00ff) | (value<<8);
	  // Start-Lo
		if (addr == 0xd41e) internal_start = (internal_start&0xff00) | (value);
	  // Repeat-Hi
		if (addr == 0xd47f) internal_repeat_start = (internal_repeat_start&0x00ff) | (value<<8);
	  // Repeat-Lo
		if (addr == 0xd47e) internal_repeat_start = (internal_repeat_start&0xff00) | (value);

	  // End-Hi
		if (addr == 0xd43e) {
			internal_end = (internal_end&0x00ff) | (value<<8);
		}
	  // End-Lo
		if (addr == 0xd43d) {
			internal_end = (internal_end&0xff00) | (value);
		}
	  // Loop-Size
		if (addr == 0xd43f) internal_repeat_times = value;
	  // Period-Hi
		if (addr == 0xd45e) internal_period = (internal_period&0x00ff) | (value<<8);
	  // Period-Lo
		if (addr == 0xd45d) {
			internal_period = (internal_period&0xff00) | (value);
		}
	  // Sample Order
		if (addr == 0xd47d) internal_order = value;
	  // Sample Add
		if (addr == 0xd45f) internal_add = value;
	  // Start-Sampling
		if (addr == 0xd41d)
		{
			sample_repeats = internal_repeat_times;
			sample_position = internal_start;
			sample_start = internal_start;
			sample_end = internal_end;
			sample_repeat_start = internal_repeat_start;
			sample_period = internal_period;
			sample_order = internal_order;
			switch (value)
			{
				case 0xfd: sample_active = 0; break;
				case 0xfe:
				case 0xff: sample_active = 1; break;

				default: return;
			}
		}
	}	
}

static void moreDigi(unsigned short addr, unsigned char value)
{
	unsigned char found= 0;
	found|= handleNewGenDigi(addr, 0xd412, 0xd40f, value);	// voice 3
	found|= handleNewGenDigi(addr, 0xd40b, 0xd408, value);	// voice 2
	found|= handleNewGenDigi(addr, 0xd404, 0xd401, value);	// voice 1

	if (!found) handleSwallowDigi(addr, value);	// actually used in main by Sverige.sid
}

unsigned char sMainLoopOnlyMode= 0;
unsigned char sSynthDisabled= 0;

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
						
					// poor man's TOD sim (only secs & 10th of sec), see Kawasaki_Synthesizer_Demo.sid
					case 0xdc08: 
						// TOD tenth of second
						sTodInMillies= ((unsigned long)(sTodInMillies/1000))*1000 + value*100;
						break;
					case 0xdc09: 
						// TOD second
						sTodInMillies= value*1000 + (sTodInMillies%1000);	// ignore minutes, etc
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
				if (sProgramMode == NMI_OFFSET_MASK) {

					// traditional digis
					if (addr == 0xd418) {
						// note: some tunes also set filters while they play digis, e.g. Digi-Piece_for_Telecomsoft.sid
						recordSample(value << 4);

						sidPoke(addr&0x1f, value);	// GianaSisters seems to rely on setting made from NMI			
					} 
					moreDigi(addr, value);					
				} else {
					// normal handling
					if (!sIsPSID && (addr == 0xd418)) {
						recordSample(value << 4);		// mb a digi from some main loop
						
						// info: Fanta_in_Space.sid, digital_music.sid need regular volume 
						// settings produced here from Main..
					}					
					sidPoke(addr&0x1f, value);					

					if ((sProgramMode == MAIN_OFFSET_MASK) && sMainLoopOnlyMode) {
						// this disturbes most "normal" songs - only use it 
						// in the few cases were it is safe (see Wonderland_XI-End.sid, Vicious_SID_2-15638Hz.sid)
						moreDigi(addr, value);
					}
					
					io_area[(addr&0xfc1f) - 0xd000]= value;
					
					if (!isC64compatible())
						handlePsidDigs(addr, value);
				}
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
	shy, slo, sre, sta, stx, sty, tax, tay, tsx, txa, txs, tya
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
			return 1;
		default:
			return 0;
	}
}
	
static const int opcodes[256]  = {
	brk,ora,jam,slo,nop,ora,asl,slo,php,ora,asl,anc,nop,ora,asl,slo,
	bpl,ora,jam,slo,nop,ora,asl,slo,clc,ora,nop,slo,nop,ora,asl,slo,
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
	imp,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
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
	7,6,0,8,3,3,5,5,3,2,2,2,4,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7,
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


// ----------------------------------------------------------- DER HARTE KERN

static unsigned long sLastPolled[4];
static void simTimerPolling(unsigned short ad) {
	// handle busy polling for CIA underflow (e.g. Kai Walter stuff) 
	// the timing of the below hack is bound to be pretty inaccurate but better than nothing.. 
	if (((ad == 0xdc0d) || (ad == 0xdd0d)) && (memory[pc] == 0xf0) && (memory[pc+1] == 0xfb) /*BEQ above read*/) {
		unsigned char i= (ad == 0xdc0d) ? 0 : 1;
		
		struct timer *t= &(cia[i]);
		char timerId= (isTimer_Started(t, TIMER_A) ? TIMER_A : 
			(isTimer_Started(t, TIMER_B) ? TIMER_B : -1));

		if (timerId >= 0) {
			unsigned long latch= getTimerLatch(t, timerId);	
			unsigned long usedCycles= sCycles;
			if (sLastPolled[(i<<1) + timerId] < usedCycles) {
				usedCycles-= sLastPolled[(i<<1) + timerId];					
			}				
			if (usedCycles<latch) {
				sCycles+= (latch-usedCycles);	// sim busywait
			}				
			sLastPolled[(i<<1) + timerId]= sCycles;
			
			signalTimerInterrupt(t);
		}	
	}				
}

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

			simTimerPolling(ad);
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
            ad=getmem(pc++);
            ad+=x;
            ad2=getmem(ad&0xff);
            ad++;
            ad2|=getmem(ad&0xff)<<8;
            return getmem(ad2);
        case idy:
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

static void setaddr(int mode, unsigned char val)
{
    unsigned short ad,ad2;
    switch(mode)
    {
        case abs:
            ad=getmem(pc-2);
            ad|=getmem(pc-1)<<8;
            setmem(ad,val);
            return;
        case abx:
            ad=getmem(pc-2);
            ad|=getmem(pc-1)<<8;
            ad2=ad+x;                
            setmem(ad2,val);
            return;
        case zpg:
            ad=getmem(pc-1);
            setmem(ad,val);
            return;
        case zpx:
            ad=getmem(pc-1);
            ad+=x;
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
}
#endif


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
        case adc:
            wval=(unsigned short)a+getaddr(opc, mode)+((p&FLAG_C)?1:0);
            setflags(FLAG_C, wval&0x100);
            a=(unsigned char)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            setflags(FLAG_V, (!!(p&FLAG_C)) ^ (!!(p&FLAG_N)));
            break;
        case anc:	// used by Axelf.sid (Crowther)
            bval=getaddr(opc, mode);
			a= a&bval;
            setflags(FLAG_C, a&0x80);
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
            setaddr(mode,(unsigned char)wval);
            setflags(FLAG_Z,!wval);
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
            setflags(FLAG_V,bval&0x40);
            break;
	
        case brk:					            
			pc= 0; // code probably called non existent ROM routine.. 
			
			/* proper impl would look like this:

			// pc has already been incremented by 1 (see above) 
			// (return address to be stored on the stack is original pc+2 )
			push((pc+1)>>8);
            push((pc+1));
            push(p);
			
			if (sProgramMode== NMI_OFFSET_MASK) {
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
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
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
        case dec:
            bval=getaddr(opc, mode);
            bval--;
            setaddr(mode,bval);
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
            setaddr(mode,bval);
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
		case jam:	// this op would have crashed the C64
		    pc=0;           // Just quit the emulation
            break;
        case jmp:
            wval=getmem(pc++);
            wval|=getmem(pc++)<<8;
            switch (mode) {
                case abs:
					if ((wval==pc-3) && (sProgramMode== MAIN_OFFSET_MASK)) {
						pc= 0;	// main loop would steal cycles from NMI/IRQ which it normally would not..
					} else {
						pc=wval;
					}
                    break;
                case ind:
                    pc=getmem(wval);
                    pc|=getmem(wval+1)<<8;
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
			sSynthDisabled= 1; // hack: to suppress SID rendering for the above song..
			
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
            setaddr(mode,(unsigned char)wval);
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
        case rol:
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&0x80);
            bval<<=1;
            bval|=c;
            setaddr(mode,bval);
            setflags(FLAG_N,bval&0x80);
            setflags(FLAG_Z,!bval);
            break;
        case ror:
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&1);
            bval>>=1;
            bval|=128*c;
            setaddr(mode,bval);
            setflags(FLAG_N,bval&0x80);
            setflags(FLAG_Z,!bval);
            break;
        case rti:
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
        case sbc:      
            bval=getaddr(opc, mode)^0xff;
            wval=(unsigned short)a+bval+((p&FLAG_C)?1:0);
            setflags(FLAG_C, wval&0x100);
            a=(unsigned char)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a>127);
            setflags(FLAG_V, (!!(p&FLAG_C)) ^ (!!(p&FLAG_N)));
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
		/*
        case sax:
			// e.g. Vicious_SID_2-15638Hz.sid (uses this to clear test-bit for its digi-samples -  
			// which causes the flawed emulator logic to no longer recognize the digi samples.. we'd need to 
			// improve the digi recognition logic befor we enable the "sax" op)		
			putaddr(opc,mode,a&x);
		  break;
		 */
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
			
		default:			
//AS3_Trace(AS3_String("------------ use of illegal opcode ---------------"));
//AS3_Trace(AS3_Number(opc));
			getaddr(opc, mode);	 // at least make sure the PC is advanced correctly (potentially used in timing)
    }
}

const static unsigned char sFF48IrqHandler[19] ={0x48,0x8A,0x48,0x98,0x48,0xBA,0xBD,0x04,0x01,0x29,0x10,0xEA,0xEA,0xEA,0xEA,0xEA,0x6C,0x14,0x03};
const static unsigned char sEA7EIrqHandler[9] ={0xAD,0x0D,0xDC,0x68,0xA8,0x68,0xAA,0x68,0x40};
const static unsigned char sFE43NmiHandler[4] ={0x78,0x6c,0x18,0x03};

void initC64Rom() {
	// we dont have the complete rom but in order to ensure consistent stack handling (regardless of
	// which vector the sid-program is using) we provide dummy versions of the respective standard 
	// IRQ/NMI routines..
	
    memSet(&kernal_rom[0], 0, KERNAL_SIZE);	

    memcpy(&kernal_rom[0x1f48], sFF48IrqHandler, 19);	// $ff48 irq routine
    memSet(&kernal_rom[0x0a31], 0xea, 0x4d);			// $ea31 fill some NOPs	
    memcpy(&kernal_rom[0x0a7e], sEA7EIrqHandler, 9);	// $ea31 return sequence
    memcpy(&kernal_rom[0x1e43], sFE43NmiHandler, 4);	// $fe43 nmi handler
	
	kernal_rom[0x1ffe]= 0x48;
	kernal_rom[0x1fff]= 0xff;
		
	kernal_rom[0x1ffa]= 0x43;	// standard NMI vectors (this will point into the void at: 0318/19)
	kernal_rom[0x1ffb]= 0xfe;	

	// basic rom init routines (e.g. used by Soundking_V1.sid)
	kernal_rom[0x1D50]= 0x60;	
	kernal_rom[0x1D15]= 0x60;	
	kernal_rom[0x1F5E]= 0x60;	
		
	// kernal vector: initalise screen and keyboard (e.g. used by Voodoo_People_part_1.sid)
	kernal_rom[0x1F81]= 0x60;	
	
    memSet(&io_area[0], 0, IO_AREA_SIZE);
	io_area[0x0418]= 0xf;					// turn on full volume
	sidPoke(0x18, 0xf);  
}

void initC64Memory() {
	initC64Rom();
	
    memSet(memory, 0, sizeof(memory));

	memory[0x0314]= 0x31;		// standard IRQ vector
	memory[0x0315]= 0xea;

	// Master_Blaster_intro.sid actually checks this:
	memory[0x00cb]= 0x40;		// no key pressed 
	io_area[0x0c01]= 0xff;	 	// Port B, keyboard matrix rows and joystick #1
	
	// for our PSID friends who don't know how to properly use memory banks lets mirror the kernal ROM into RAM
	if (sIsPSID) {
		memcpy(&memory[0xe000], &kernal_rom[0], 0x2000);
	}
}

unsigned short LoadSIDFromMemory(void *pSidData, unsigned short *load_addr, unsigned short *load_end_addr, unsigned short *init_addr, unsigned short *play_addr, unsigned char *subsongs, unsigned char *startsong, unsigned long *speed, unsigned short size)
{
	initC64Memory();
	
    unsigned char *pData;
    unsigned char data_file_offset;	
	
    pData = (unsigned char*)pSidData;
    data_file_offset = pData[7];

    *load_addr = pData[8]<<8;
    *load_addr|= pData[9];

    *init_addr = pData[10]<<8;
    *init_addr|= pData[11];

    *play_addr = pData[12]<<8;
    *play_addr|= pData[13];

    *subsongs = pData[0xf]-1;
    *startsong = pData[0x11]-1;

	if (*load_addr == 0) {
		// original C64 binary file format
		
		*load_addr = pData[data_file_offset];
		*load_addr|= pData[data_file_offset+1]<<8;
		
		data_file_offset +=2;
	}
	if (*init_addr == 0) {
		*init_addr= *load_addr;	// 0 implies that init routine is at load_addr
	}	
	
    *speed = pData[0x12]<<24;
    *speed|= pData[0x13]<<16;
    *speed|= pData[0x14]<<8;
    *speed|= pData[0x15];
    
	*load_end_addr= *load_addr+size-data_file_offset;
    memcpy(&memory[*load_addr], &pData[data_file_offset], size-data_file_offset);
    
    return *load_addr;
}

void reInitEngine() {
	// reset all the tinysid stuff.. just in case
	mixing_frequency= filtmul= 0;
	freqmul= 0;
	memSet( (unsigned char*)&attacks, 0, sizeof(attacks) ); 
    memSet( (unsigned char*)&releases, 0, sizeof(releases) ); 
	memSet((unsigned char*)&sid,0,sizeof(sid));
	memSet((unsigned char*)osc,0,sizeof(osc));
	memSet((unsigned char*)&filter,0,sizeof(filter));

	pc= 0;
	a= x= y= s= p= 0;
	
	sample_active= sample_position= sample_start= sample_end= sample_repeat_start= fracPos= 
		sample_period= sample_repeats= sample_order= sample_nibble= 0;
	internal_period= internal_order= internal_start= internal_end=
		internal_add= internal_repeat_times= internal_repeat_start= 0;
		
	sProgramMode= MAIN_OFFSET_MASK;
	memSet( (unsigned char*)&sSwallowTypeDigi, 0, sizeof(sSwallowTypeDigi) ); 
	dummyCount= 0;
	sTodInMillies= 0;
	sDigiCount= 0;
    memSet( (unsigned char*)&sDigiTime, 0, sizeof(sDigiTime) ); 
    memSet( (unsigned char*)&sDigiVolume, 0, sizeof(sDigiVolume) ); 
		
	sOverflowDigiCount= 0;
    memSet( (unsigned char*)&sOverflowDigiTime, 0, sizeof(sOverflowDigiTime) ); 
    memSet( (unsigned char*)&sOverflowDigiVolume, 0, sizeof(sOverflowDigiVolume) ); 
	sMainLoopOnlyMode= 0;
	sSynthDisabled= 0;
	
	bval= 0;
	wval= 0;
	memSet( (unsigned char*)&sLastPolled, 0, sizeof(sLastPolled) ); 
	sLastPolledOsc= 0;
}
