/*
 * This file provides the code used for the detection and the handling of digi-samples.
 *
 * Due to the "predictive approach" of the emulator (i.e. fact that the CPU/VIC/SID is not emulated 
 * on a cycle by cycle basis) special logic is needed to detect the scenarios where a music program's 
 * interaction with the SID might result in "direct digi-sample playback).
 *
 * <p>version 0.7
 * <p>Copyright (C) 2013 Juergen Wothke
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

#include "digi.h"
 
#include <stdio.h>
#include <string.h>

#include "sidengine.h"	// for sCycles
#include "rsidengine.h"	// for io_area
#include "nanovic.h"	// for getRasterlineTimer
#include "sidplayer.h"	// for getCyclesPerScreen();

unsigned long INVALID_TIME= (0x1 << 27);	// large enough so that any regular timestamp will get preference
unsigned int IDX_NOT_FOUND= 0x1fff;			// we'll never have that many samples for one screen..

static unsigned char sCurrentDigi=  0x80;		// last digi sample / default: neutral value 


 /*
* work buffers used to record digi samples produced by NMI/IRQ/main
*/ 
unsigned int sDigiCount= 0;


unsigned long sDigiTime[DIGI_BUF_SIZE];		// time in cycles counted from the beginning of the current screen
unsigned char sDigiVolume[DIGI_BUF_SIZE];	// 8-bit sample

static unsigned long sSortedDigiTime[DIGI_BUF_SIZE];
static unsigned char sSortedDigiVolume[DIGI_BUF_SIZE];

static unsigned char sIsC64compatible= 1;

/*
* samples which already belong to the next screen, e.g. produced by some long running IRQ which
* crossed over to the next screen..
*/
unsigned int sOverflowDigiCount= 0;
unsigned long sOverflowDigiTime[DIGI_BUF_SIZE];
unsigned char sOverflowDigiVolume[DIGI_BUF_SIZE];

const unsigned char sTestBitDetectTimeout = 12; 	// in cycles

/* legacy PSID sample playback */
static int sample_active;
static int sample_position, sample_start, sample_end, sample_repeat_start;
static int fracPos = 0;  /* Fractal position of sample */
static int sample_period;
static int sample_repeats;
static int sample_order;
static int sample_nibble;

static int internal_period, internal_order, internal_start, internal_end,
internal_add, internal_repeat_times, internal_repeat_start;

static void recordSample(unsigned char sample) {
	if (sCycles <= getCyclesPerScreen()) {
		sDigiTime[sDigiCount%(DIGI_BUF_SIZE-1)]= sCycles;
		sDigiVolume[sDigiCount%(DIGI_BUF_SIZE-1)]= sample;	// always use 8-bit to ease handling									

		sDigiCount+=1;	// buffer is meant to collect no more than the samples from one screen refresh!
	}
	else {
		// some players (e.g. Digital_Music.sid) start long running IRQ routines at the end of one screen 
		// producing most of their output on the next screen... so we have to deal with this scenario..
		
		sOverflowDigiTime[sDigiCount%(DIGI_BUF_SIZE-1)]= sCycles-getCyclesPerScreen();
		sOverflowDigiVolume[sDigiCount%(DIGI_BUF_SIZE-1)]= sample;	// always use 8-bit to ease handling									

		sOverflowDigiCount+=1;	// buffer is meant to collect no more than the samples from one screen refresh!		
	}
}

// -------------------------------------------------------------------------------------------
// detection of test-bit/frequency modulation digi-sample technique (e.g. used in 
// Vicious_SID_2-15638Hz.sid, Storebror.sid, etc)
// -------------------------------------------------------------------------------------------

// KNOWN LIMITATION: the beauty of this approach is that the regular SID filters 
// are still applied to the digi-sample signal.. due to the fact that the current filter 
// impl is lacking (e.g. it does not work for sampling rates below 44kHz) there was no point 
// in adding it here.. with a better filter impl the sample recorded below would first need 
// to be passed through the SID filter..

typedef enum {
    FreqIdle=0,
    FreqPrep=1,
    FreqSet=2,
	
    FreqVariant1=3,
    FreqVariant2=4,

} FreqDetectState;


// relevant timing state is tracked for each of the 3 channels
static FreqDetectState sFreqDetectState[3];
static unsigned long sFreqDetectTimestamp[3];
static unsigned char sFreqDetectDelayedSample[3];

inline unsigned char isWithinFreqDetectTimeout(unsigned char voice) {
	return (sCycles-sFreqDetectTimestamp[voice]) < sTestBitDetectTimeout;
}

static unsigned char recordFreqSample(unsigned char voice, unsigned char sample) {
	recordSample(sample);

	// reset those SID regs before envelope generator does any damage
	sidPoke(voice*7 + 4, 0);	// GATE
	sidPoke(voice*7 + 1, 0);	// freq HI
	
	sFreqDetectState[voice]= FreqIdle;
	sFreqDetectTimestamp[voice]= 0;
	return 1;
}

static unsigned char handleFreqModulationDigi(unsigned char voice, unsigned char reg, unsigned char value) {
	// test-bit approach: the following settings are performed on the waveform/freq register in short order:
	// 1) Triangle+GATE, 2) TEST+GATE 3) GATE only 4) then the desired output sample is played by setting 
	// the "frequency hi-byte" (the whole sequence usually takes about 20-30 cycles.. - exaxt limits
	// still to be verified) .. possible variations: GATE is not set in step 2 and/or steps 3 and 4 are 
	// switched (see LMan - Vortex.sid)
	
	if (reg == 4) {	// waveform
		value &= 0x19;	// mask all excess bits..
		switch (value) {
			case 0x11:	// triangle/GATE
				// reset statemachine 
				sFreqDetectState[voice] = FreqPrep;				// this may be the start of a digi playback
				sFreqDetectTimestamp[voice]= sCycles;
				break;
			case 0x8:	// TEST
			case 0x9:	// TEST/GATE
				if ((sFreqDetectState[voice] == FreqPrep) && isWithinFreqDetectTimeout(voice)) {
					sFreqDetectState[voice] = FreqSet;			// we are getting closer
					sFreqDetectTimestamp[voice]= sCycles;
				} else {
					sFreqDetectState[voice] = FreqIdle;	// just to reduce future comparisons			
				}
				break;
			case 0x1:	// GATE
				if ((sFreqDetectState[voice] == FreqSet) && isWithinFreqDetectTimeout(voice)) {
					// variant 1: sample set after GATE
					sFreqDetectState[voice] = FreqVariant1;		// bring on that sample!
					sFreqDetectTimestamp[voice]= sCycles;
				} else if ((sFreqDetectState[voice] == FreqVariant2) && isWithinFreqDetectTimeout(voice)) {
					// variant 2: sample set before GATE
					return recordFreqSample(voice, sFreqDetectDelayedSample[voice]);				
				} else {
					sFreqDetectState[voice] = FreqIdle;	// just to reduce future comparisons			
				}
				break;
		}
	} else if (reg == 1) {	// step: set sample 
		if ((sFreqDetectState[voice] == FreqSet) && isWithinFreqDetectTimeout(voice)) {
			// variant 2: sample before GATE
			sFreqDetectDelayedSample[voice]= value;
			
			sFreqDetectState[voice] = FreqVariant2;		// now we only need confirmation
			sFreqDetectTimestamp[voice]= sCycles;
		} else if ((sFreqDetectState[voice] == FreqVariant1) && isWithinFreqDetectTimeout(voice)) {
			// variant 1: sample set after GATE
			return recordFreqSample(voice, value);
		}
	}	
	return 0;	// other "detectors" may still take their turn
}

// -------------------------------------------------------------------------------------------
// detection of test-bit/pulse width modulation digi-sample technique (e.g. used in 
// Wonderland_XII-Digi_part_1.sid, GhostOrGoblin.sid, etc)
// -------------------------------------------------------------------------------------------

typedef enum {
    PulseIdle=0,
	// variant 2
    PulsePrep=1,
    PulseConfirm=2,
	
	// variant 2
	PulsePrep2= 3,
	PulseConfirm2= 4
} PulseDetectState;


// relevant timing state is tracked for each of the 3 channels
static PulseDetectState sPulseDetectState[3];
static unsigned long sPulseDetectTimestamp[3];
static unsigned char sPulseDetectDelayedSample[3];
static unsigned char sPulseDetectMode[3];	// 2= Pulse width LO/ 3= Pulse width HI

inline unsigned char isWithinPulseDetectTimeout(unsigned char voice) {
	return (sCycles-sPulseDetectTimestamp[voice]) < sTestBitDetectTimeout;
}

static unsigned char recordPulseSample(unsigned char voice, unsigned char sample) {
	recordSample(sample);

	// reset those SID regs before envelope generator does any damage
	sidPoke(voice*7 + 4, 0);	// GATE
	sidPoke(voice*7 + 2, 0);	// pulse width
	
	sPulseDetectState[voice]= PulseIdle;
	sPulseDetectTimestamp[voice]= 0;
	return 1;
}

static unsigned char handlePulseModulationDigi(unsigned char voice, unsigned char reg, unsigned char value) {
	// approach: the following settings are performed on the waveform/pulsewidth register in short order:
	// 1) desired output sample is played by setting the "pulse width" then	2) PULSE+TEST+GATE, 3) PULSE+GATE 
	// (the whole sequence usually takes about 20-30 cycles.. - exaxt limits still to be verified) variant:
	// pulse-width is set between 2 and 3.
	
	if (reg == 4) {	// waveform
		value &= 0x49;	// mask all excess bits..
		switch (value) {
			case 0x49:	// PULSE/TEST/GATE
				// test bit is set
				if ((sPulseDetectState[voice] == PulsePrep) && isWithinPulseDetectTimeout(voice)) {
					sPulseDetectState[voice] = PulseConfirm;			// we are getting closer
					sPulseDetectTimestamp[voice]= sCycles;
				} else {
					// start of variant 2
					sPulseDetectState[voice] = PulsePrep2;
					sPulseDetectTimestamp[voice]= sCycles;
				}
				break;
			case 0x41:	// PULSE/GATE
				if (((sPulseDetectState[voice] == PulseConfirm) || (sPulseDetectState[voice] == PulseConfirm2)) 
						&& isWithinPulseDetectTimeout(voice)) {
					unsigned char sample= (sPulseDetectMode[voice] == 2) ? 
									sPulseDetectDelayedSample[voice] : (sPulseDetectDelayedSample[voice] << 4) & 0xff;

					setMute(voice);	// avoid wheezing base signals
					
					sPulseDetectState[voice] = PulseIdle;	// just to reduce future comparisons

					return recordFreqSample(voice, sample);								
				} else {
					sPulseDetectState[voice] = PulseIdle;	// just to reduce future comparisons			
				}
				break;
		}
	} else if ((reg == 2) || (reg == 3)) {	// PULSE width 
		unsigned char followState;
		if ((sPulseDetectState[voice] == PulsePrep2) && isWithinPulseDetectTimeout(voice)) {
			followState= PulseConfirm2;	// variant 2
		} else {
			followState= PulsePrep;		// variant 1
		}
		// reset statemachine 
		sPulseDetectState[voice] = followState;				// this may be the start of a digi playback
		sPulseDetectTimestamp[voice]= sCycles;
		sPulseDetectDelayedSample[voice]= value;
		sPulseDetectMode[voice]= reg;
	}
	return 0;	// other "detectors" may still take their turn
}

// -------------------------------------------------------------------------------------------
// Detection of the peculiar digi approach used in Ice_Guys.sid: 2-bit samples are written  
// by selecting a corresponding waveform in d412.
// -------------------------------------------------------------------------------------------

static unsigned char handleIceGuysDigi(unsigned char voice, unsigned char reg, unsigned char value) {
	// would be nice to find a robost check for non Ice_Guys.sid scenarios (false positives) 
	// unfortunately I have not spottet it yet... (maybe a file specific hack would be in order to avoid any sideeffects)
	
	unsigned ctrlReg= 0x0400 + voice*7 + 4;	// waveform control reg

	if ((reg == 4) && (getProgramMode() == NMI_OFFSET_MASK)
			&& !(sid.v[voice].wave&0x8) && (sid.v[voice].ad == 0x0f)){	
		
		switch(value) {	// no idea about the correct output levels generated by this approach..
			case 0x71:		// idx 0
				recordSample(0);
				break;
			case 0x21:		// idx 1
				recordSample(0x44);
				break;
			case 0x11:		// idx 2
				recordSample(0x88);
				break;
			case 0x41:		// idx 3
				recordSample(0xcc);
				break;
			default:
				return 0;	// false positive, e.g. 	Wonderland_XII-Digi_part_1.sid	
		}
		return 1;
	}
	return 0;
}

// -------------------------------------------------------------------------------------------
// Mahoney's D418 "8-bit" digi sample technique..
// -------------------------------------------------------------------------------------------

// based on Mahoney's amplitude_table_8580.txt
const unsigned char sMahoneySample[256]= {164, 170, 176, 182, 188, 194, 199, 205, 212, 218, 224, 230, 236, 242, 248, 254, 164, 159, 153, 148, 142, 137, 132, 127, 120, 115, 110, 105, 99, 94, 89, 84, 164, 170, 176, 181, 187, 193, 199, 205, 212, 217, 223, 229, 235, 241, 246, 252, 164, 159, 153, 148, 142, 137, 132, 127, 120, 115, 110, 105, 100, 94, 90, 85, 164, 170, 176, 182, 188, 194, 200, 206, 213, 219, 225, 231, 237, 243, 249, 255, 164, 159, 154, 149, 143, 138, 133, 128, 122, 117, 112, 107, 102, 97, 92, 87, 164, 170, 176, 182, 188, 194, 199, 205, 212, 218, 224, 230, 236, 242, 248, 253, 164, 159, 154, 149, 143, 138, 133, 128, 122, 117, 112, 107, 102, 97, 92, 87, 164, 164, 164, 164, 164, 164, 164, 164, 163, 163, 163, 163, 163, 163, 163, 163, 164, 153, 142, 130, 119, 108, 97, 86, 73, 62, 52, 41, 30, 20, 10, 0, 164, 164, 164, 164, 164, 164, 163, 163, 163, 163, 163, 163, 163, 163, 162, 162, 164, 153, 142, 131, 119, 108, 97, 87, 73, 63, 52, 42, 31, 21, 11, 1, 164, 164, 164, 164, 164, 164, 164, 165, 165, 165, 165, 165, 165, 165, 165, 165, 164, 153, 142, 131, 120, 109, 98, 88, 75, 64, 54, 44, 33, 23, 13, 3, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 153, 142, 131, 120, 109, 99, 88, 75, 65, 55, 44, 34, 24, 14, 4} ;

inline static unsigned char isMahoneyDigi() {
	// Mahoney's "8-bit" D418 sample-technique requires a specific SID setup
	if (((getmem(0xd406) == 0xff) && (getmem(0xd40d) == 0xff) && (getmem(0xd414) == 0xff)) && // correct SR
		((getmem(0xd404) == 0x49) && (getmem(0xd40b) == 0x49) && (getmem(0xd412) == 0x49)) && // correct waveform
		((getmem(0xd415) == 0xff) && (getmem(0xd416) == 0xff) ) && // correct filter cutoff
		(getmem(0xd417) == 0x3)) {	// voice 1&2 through filter
		
		return 1;
	} else {
		return 0;
	}
}

// -------------------------------------------------------------------------------------------
// Swallow 'pulse width modulation': the players handled here use some PWM approach but without 
// the more recent test-bit technique.. each player depends on specific frequency 
// settings and differs in how sample values are transformed and then written as 
// differently interpreted hi/lo pulse-width settings.. examples can be found 
// from musicians like Swallow, Danko or Cyberbrain
// -------------------------------------------------------------------------------------------

static unsigned int sSwallowPWM[3];

static unsigned char setSwallowMode(unsigned char voice, unsigned char m) {
	sSwallowPWM[voice]= m;
	setMute(voice);	// avoid wheezing base signals
	
	return 1;
}

static unsigned char handleSwallowDigi(unsigned char voice, unsigned char reg, unsigned short addr, unsigned char value) {
	if (reg == 4) {
		if ((sid.v[voice].wave & 0x8) && !(value & 0x8) && (value & 0x40) && ((sid.v[voice].ad == 0) && (sid.v[voice].sr == 0xf0))) {
			// the tricky part here is that the tests here do not trigger for songs which act similarily 
			// but which are not using "pulse width modulation" to play digis (e.g. Combat_School.sid, etc)
			
			if ((sid.v[voice].pulse == 0x0555) && (sid.v[voice].freq == 0xfe04)) {
				// e.g. Spasmolytic_part_2.sid
				return setSwallowMode(voice, 1);
			}  else if ((sid.v[voice].pulse == 0x08fe) && (sid.v[voice].freq == 0xffff)) {
				// e.g. Sverige.sid, Holy_Maling.sid, Voodoo_People_part_*.sid
				return setSwallowMode(voice, 2);	
			} else if ( (sid.v[voice].pulse&0xff00) == 0x0800 ) {
				// e.g. Bla_Bla.sid, Bouncy_Balls_RCA_Intro.sid, Spasmolytic_part_6.sid				
				// Bouncy_Balls_RCA_Intro.sid, Ragga_Run.sid, Wonderland_X_part_1.sid
				return setSwallowMode(voice, 3);
			} 
		}
	} else if (sSwallowPWM[0] && (reg == 3)) {
		// depending in the specific player routine, the sample info is available in different 
		// registers(d402/d403 and d409/a) ..  for retrieval d403 seems to work for most player impls
		switch(sSwallowPWM[voice]) {
			case 1:
				recordSample((value<<4) & 0xff);
				break;
			case 2: 
				recordSample((value<<4) | (value>>4) );				
				break;
			case 3: 
				recordSample((value<<4) & 0xff);
				break;
		}
		return 1;
	}
	return 0;
}


// ------------------------------ legacy PSID digi stuff ----------------------------------

void resetPsidDigi() {
	fracPos = 0;
}

static void handlePsidDigi(unsigned short addr, unsigned char value) {			
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

int generatePsidDigi(int sIn)
{
    static int sample = 0;

    if (!sample_active) return(sIn);

    if ((sample_position < sample_end) && (sample_position >= sample_start))
    {
		//Interpolation routine
		//float a = (float)fracPos/(float)getSampleFrequency();
		//float b = 1-a;
		//sIn += a*sample + b*last_sample;

        sIn += sample;

        fracPos += 985248/sample_period;		// CIA Timer clock rate 0.985248MHz (PAL)
        
        if (fracPos > getSampleFrequency()) 
        {
            fracPos%=getSampleFrequency();

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

unsigned int getDigiCount() {
	return sDigiCount;
}

void clearDigiBuffer() {
	sDigiCount= 0;
}

void markSampleOrigin(unsigned long mask, unsigned long offset, unsigned long originalDigiCount) {
	/*
	* Due to the current implementation which only considers timer start times but not delays through 
	* interrupts - the enties in the digi-sample recording may be out of sequence (e.g. if IRQ starts first 
	* it will first write all its values and the NMI will only then add its data - eventhough the NMI's entry 
	* logically may belong between some IRQ's entries. The same applies to the main prog which adds all its 
	* entries at the very end - after the interrupt routines have already added all their enties)
	*
	* For the rendering the respective sample recording list must first be sorted (by timestamp). To avoid a 
	* full fledged sort of the complete list, the timestamps generated by the different producer streams (NMI/IRQ/main 
	* prog) are flagged using a producer specific bit. This then is used as a shortcut when sorting..
	*
	* This method turns the original relative timestamps into absolute ones by adding a respective offset. It also 
	* sets the producer specific bit. 
	*
	* Mystery time: Instead setting the start time to 0 and then adding the below stuff to the recordings afterwards, the
	* original idea was to directly set the flag and offset in the start time for the simulation. This should have led to 
	* the same result.. alone it did not :( mb it's a C skills problem or just some Alchemy bug.. for now the hack works.
	*/
	
	unsigned int len= sDigiCount - originalDigiCount;
	
	int i;
	if (len > 0) {
		for (i= 0; i<len; i++) {
			sDigiTime[originalDigiCount+i] = (offset + sDigiTime[originalDigiCount+i]) | mask;
		}
	}
}

void resetDigi(unsigned char compatibility) {
	sIsC64compatible= compatibility;

	memSet( (unsigned char*)&sSwallowPWM, 0, sizeof(sSwallowPWM) ); 
	
	sDigiCount= 0;

    memSet( (unsigned char*)&sDigiTime, 0, sizeof(sDigiTime) ); 
    memSet( (unsigned char*)&sDigiVolume, 0, sizeof(sDigiVolume) ); 

	sOverflowDigiCount= 0;
    memSet( (unsigned char*)&sOverflowDigiTime, 0, sizeof(sOverflowDigiTime) ); 
    memSet( (unsigned char*)&sOverflowDigiVolume, 0, sizeof(sOverflowDigiVolume) ); 
	
	// PSID digi stuff
	sample_active= sample_position= sample_start= sample_end= sample_repeat_start= fracPos= 
		sample_period= sample_repeats= sample_order= sample_nibble= 0;
	internal_period= internal_order= internal_start= internal_end=
		internal_add= internal_repeat_times= internal_repeat_start= 0;

	sOverflowDigiCount=0;	
	sDigiCount=0;
	
	//	digi sample detection 
	for (int i= 0; i<3; i++) {
		sFreqDetectState[i]= FreqIdle;
		sFreqDetectTimestamp[i]= 0;
		sFreqDetectDelayedSample[i]= 0;
		
		sPulseDetectState[i]= PulseIdle;
		sPulseDetectMode[i]= 0;
		sPulseDetectTimestamp[i]= 0;
		sPulseDetectDelayedSample[i]= 0;
		
		sSwallowPWM[i]= 0;
	}
}

const int sDigiSampleDetectLimit= 10;	// samples per frame.. actually should be well more than this..

void handleSidWrite(unsigned short addr, unsigned char value) {
	unsigned char reg= addr&0x1f;
	unsigned char voice= 0;
    if ((reg >= 7) && (reg <=13)) {voice=1; reg-=7;}
    if ((reg >= 14) && (reg <=20)) {voice=2; reg-=14;}

	if (handleFreqModulationDigi(voice, reg, value)) return;
	if (handlePulseModulationDigi(voice, reg, value)) return;
	if (handleIceGuysDigi(voice, reg, value)) return;			// brittle detector
	if (handleSwallowDigi(voice, reg, addr, value)) return;

	if (getProgramMode() == NMI_OFFSET_MASK) {
		// Some players (e.g. Storebror.sid, Blood_Money_Ingame.sid, Boing_Boom_Tschak.sid,  etc) 
		// use NMI not only for sample playback but also to make other SID-settings.. which may 
		// lead to a base hum-noise.. (the ealier digi-approaches do not share this issue since they 
		// change only settings within the digi-voice - and that voice is then muted by default..)
		
		// hack: to avoid these issues SID settings from within an NMI are limited to the volume register
		if (addr == 0xd418) {	// traditional digis
			// note: some tunes also set filters while they play digis, e.g. Digi-Piece_for_Telecomsoft.sid
			recordSample(isMahoneyDigi() ? sMahoneySample[value] : value << 4);	// this may lead to false positives..

			sidPoke(addr&0x1f, value);	// GianaSisters seems to rely on setting made from NMI
			// hack: don't write io_area so that NMI digis don't disturb loudness of Ferrari_Formula_One.sid
		}
	} else {
		// normal handling
		if (!isPsid() && (addr == 0xd418)) {
			recordSample(isMahoneyDigi() ? sMahoneySample[value] : value << 4);	// this may lead to false positives..
		}					
		// info: Fanta_in_Space.sid, digital_music.sid need regular volume settings produced here from Main..
		sidPoke(addr&0x1f, value);							
		io_area[(addr&0xfc1f) - 0xd000]= value;
		
		if (!sIsC64compatible)
			handlePsidDigi(addr, value);
	}
}

int getDigiOverflowCount() {
	return sOverflowDigiCount;
} 
 
 void moveDigiBuffer2NextFrame() {
	if (sOverflowDigiCount > 0) {
		unsigned int len= sizeof(long)*sOverflowDigiCount;
		memcpy(sDigiTime, sOverflowDigiTime, len);		
		memcpy(sDigiVolume, sOverflowDigiVolume, len);		
		
		// clear just in case..
		memSet(((unsigned char*)sDigiTime)+len, 0, DIGI_BUF_SIZE-len);
		memSet(((unsigned char*)sDigiVolume)+len, 0, DIGI_BUF_SIZE-len);
	} else {
		memSet((unsigned char*)sDigiTime, 0, DIGI_BUF_SIZE);
		memSet((unsigned char*)sDigiVolume, 0, DIGI_BUF_SIZE);
	}
	sDigiCount= sOverflowDigiCount;
	sOverflowDigiCount= 0;	
}

static int isVal(unsigned long mask, unsigned long val) {
	return val&mask;		// is this a value of the selected "mask" category?
}

static unsigned long getValue(unsigned long mask, unsigned int fromIdx) {
	if (fromIdx == IDX_NOT_FOUND) {
		return INVALID_TIME;			// only relevant while it is used for comparisons..
	}
	return (sDigiTime[fromIdx])&(~mask);	// remove marker flag
}

// must only be used for valid "fromIdx"
static void copy(unsigned long mask, unsigned int toIdx, unsigned int fromIdx) {
	sSortedDigiTime[toIdx]= getValue(mask, fromIdx);
	sSortedDigiVolume[toIdx]= sDigiVolume[fromIdx];
}

static unsigned int next(unsigned long mask, unsigned int toIdx, unsigned int fromIdx) {
	copy(mask, toIdx, fromIdx);

	unsigned int i; 
	for (i= fromIdx+1; i<sDigiCount; i++) {
		if(isVal(mask, sDigiTime[i])) {
			return i;	// advance index to the next value of this category
		}
	}
	return IDX_NOT_FOUND;
}

static void sortDigiSamples() {
	if (sDigiCount >0) {
		// IRQ and NMI routines are only more or less aligned and may actually be executed in an overlapping
		// manner. Samples may therefore be stored out of sequence and we need to sort them here first before we render
				
		// the three sample providers:
		unsigned int irqIdx= IDX_NOT_FOUND;
		unsigned int nmiIdx= IDX_NOT_FOUND;
		unsigned int mainIdx= IDX_NOT_FOUND;

		unsigned int i;
		unsigned int noOfIrqSamples= 0;
		unsigned int noOfMainSamples= 0;
		
		// find respective start index for data generated by IRQ/NMI/main
		for (i= 0; i<sDigiCount; i++) {
			unsigned long val= sDigiTime[i];
			if (val & MAIN_OFFSET_MASK) {
				noOfMainSamples+= 1;

				if (mainIdx == IDX_NOT_FOUND) mainIdx= i;
			} else if (val & IRQ_OFFSET_MASK) {
				noOfIrqSamples+= 1;
				
				if (irqIdx == IDX_NOT_FOUND) irqIdx= i;
			} else if (val & NMI_OFFSET_MASK){
				
				if (nmiIdx == IDX_NOT_FOUND) nmiIdx= i;
			} else {
				// error..
			}
		}
		
		char ignoreIrqValue= (noOfIrqSamples == 1);	// not meant as digi (Digi-Piece_for_Telecomsoft.sid uses IRQ for digi..)
		
		if (nmiIdx != IDX_NOT_FOUND) {
			ignoreIrqValue= 1;			// see Coma_Light_13_tune_4.sid (players will typically not mix the two..)
		}

		char ignoreMainValue= (noOfMainSamples < 10);	// not meant as digi		

		// create new list with strictly ascending timestamps
		unsigned int toIdx;
		signed char offset= 0;
		for (toIdx= 0; toIdx<sDigiCount; toIdx++) {
			if (getValue(MAIN_OFFSET_MASK, mainIdx) < getValue(IRQ_OFFSET_MASK, irqIdx)) {
				if (getValue(MAIN_OFFSET_MASK, mainIdx) < getValue(NMI_OFFSET_MASK, nmiIdx)) {		// "main" is next
					mainIdx= next(MAIN_OFFSET_MASK, toIdx+offset, mainIdx);
					
					if (ignoreMainValue) {
						offset--;
					}
				} else {									// "nmi" is next 
					nmiIdx= next(NMI_OFFSET_MASK, toIdx+offset, nmiIdx);				
				}				
			} else if (getValue(IRQ_OFFSET_MASK, irqIdx) < getValue(MAIN_OFFSET_MASK, mainIdx)) {
				if (getValue(IRQ_OFFSET_MASK, irqIdx) < getValue(NMI_OFFSET_MASK, nmiIdx)) {		// "irq" is next
					irqIdx= next(IRQ_OFFSET_MASK, toIdx+offset, irqIdx);
					
					if (ignoreIrqValue) {
						offset--;
					}
				} else {									// "nmi" is next 
					nmiIdx= next(NMI_OFFSET_MASK, toIdx+offset, nmiIdx);	
				}								
			} else { 
				nmiIdx= next(NMI_OFFSET_MASK, toIdx+offset, nmiIdx);	// only "nmi" left		
			}			
		}
		sDigiCount+= offset;		
	}	
}

static void fillDigi(unsigned char * digiBuffer, int startIdx, int endIdx, unsigned char digi) {
	if (endIdx>=startIdx) {
		memSet( &digiBuffer[startIdx], digi, (endIdx-startIdx)+1 );
	}
}

int renderDigiSamples(unsigned char * digiBuffer, unsigned long cyclesPerScreen, unsigned int samplesPerCall) {	
	/*
	* if there are too few signals, then it's probably just the player setting filters or
	* resetting the volume with no intention to play a digi-sample, e.g. Transformers.sid (Russel Lieblich)
	*/
	if (sDigiCount > sDigiSampleDetectLimit) {
		sortDigiSamples();

		// render digi samples
		unsigned int fromIdx=0;	
		int j;
		for (j= 0; j<sDigiCount; j++) {
			float scale= (float) (samplesPerCall-1) / cyclesPerScreen;
			unsigned int toIdx= scale*((sSortedDigiTime[j] > cyclesPerScreen) ? cyclesPerScreen : sSortedDigiTime[j]);

			fillDigi(digiBuffer, fromIdx, toIdx, sCurrentDigi);				

			fromIdx= toIdx;
			sCurrentDigi= sSortedDigiVolume[j];
		}
		fillDigi(digiBuffer, fromIdx, (samplesPerCall-1), sCurrentDigi);

		if (getRasterlineTimer() == 0xf8) {
			// hack fixes volume issue in Ferrari_Formula_One.sid
			// (todo: the rasterline check is a rather brittle impl... a more 
			// robost/foolproof impl fix needs to be found here)
			// restore last value that was set by main or IRQ (see handleSidWrite()):
			sidPoke(0xd418 & 0x1f, io_area[0x0418]);
		}
		return 1;
	}
	return 0;
}

/*
* @param digi   is an unsigned 8-bit sample (i.e. origial $d418 4-bit samples have already been shifted
*/
static inline short genDigi(short in, unsigned char digi) { 
    // transform unsigned 8 bit range into signed 16 bit (–32,768 to 32,767) range	(
	// shift only 7 instead of 8 because digis are otherwise too loud)	
	signed long value = in + (((digi & 0xff) << 7) - 0x4000); 
	
	const int clipValue = 32767;
	if ( value < -clipValue ) {
		value = -clipValue;
	} else if ( value > clipValue ) {
		value = clipValue;
	}
	short out= value;
    return out;
}

void mergeDigi(int hasDigi, short *sound_buffer, unsigned char *digi_buffer, unsigned long len) {
	if (hasDigi) {
		int i;
		for (i= 0; i<len; i++) {
			sound_buffer[i]= genDigi(sound_buffer[i], digi_buffer[i]);
		}
	} 
}
