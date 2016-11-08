/*
 * This file provides the code used for the detection and the handling of digi-samples.
 *
 * Due to the "predictive approach" of the emulator (i.e. fact that the CPU/VIC/SID is not emulated 
 * on a cycle by cycle basis) special logic is needed to detect the scenarios where a music program's 
 * interaction with the SID might result in "direct digi-sample playback).
 *
 * <p>Copyright (C) 2013 Juergen Wothke
 * <p>version 0.81
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

#include "digi.h"
 
#include <stdio.h>
#include <string.h>

#include "env.h"		// for envCyclesPerScreen();
#include "cpu.h"		// for cpuCycles()
#include "memory.h"

#include "sid.h"
#include "vic.h"		// for vicGetRasterline()

#define DIGI_BUF_SIZE 1000

static uint32_t INVALID_TIME= (0x1 << 27);	// large enough so that any regular timestamp will get preference
static uint16_t IDX_NOT_FOUND= 0x1fff;			// we'll never have that many samples for one screen..

static uint8_t currentDigi=  0x80;		// last digi sample / default: neutral value 

/*
* work buffers used to record digi samples produced by NMI/IRQ/main
*/ 
static uint16_t digiCount= 0;
static uint32_t digiTime[DIGI_BUF_SIZE];	// time in cycles counted from the beginning of the current screen
static uint8_t digiVolume[DIGI_BUF_SIZE];	// 8-bit sample

static uint32_t sortedDigiTime[DIGI_BUF_SIZE];
static uint8_t sortedDigiVolume[DIGI_BUF_SIZE];

static uint8_t isC64compatible= 1;

/*
* samples which already belong to the next screen, e.g. produced by some long running IRQ which
* crossed over to the next screen..
*/
static uint16_t overflowDigiCount= 0;
static uint32_t overflowDigiTime[DIGI_BUF_SIZE];
static uint8_t overflowDigiVolume[DIGI_BUF_SIZE];

// detection of test-bit based samples
const uint8_t TB_TIMEOUT = 12;	 	// minimum that still works for "Vortex" is 10
const uint8_t TB_PULSE_TIMEOUT = 7; // already reduced this one to avoid false positive in "Yie ar kung fu"

/* legacy PSID sample playback */
static int32_t sampleActive;
static int32_t samplePosition, sampleStart, sampleEnd, sampleRepeatStart;
static int32_t fracPos = 0;  /* Fractal position of sample */
static int32_t samplePeriod;
static int32_t sampleRepeats;
static int32_t sampleOrder;
static int32_t sampleNibble;

static int32_t internalPeriod, internalOrder, internalStart, internalEnd,
internalAdd, internalRepeatTimes, internalRepeatStart;

static void recordSample(uint8_t sample) {
	if (cpuCycles() <= envCyclesPerScreen()) {
		digiTime[digiCount%(DIGI_BUF_SIZE-1)]= cpuCycles();
		digiVolume[digiCount%(DIGI_BUF_SIZE-1)]= sample;	// always use 8-bit to ease handling									

		digiCount+=1;	// buffer is meant to collect no more than the samples from one screen refresh!
	}
	else {
		// some players (e.g. Digital_Music.sid) start long running IRQ routines at the end of one screen 
		// producing most of their output on the next screen... so we have to deal with this scenario..
		
		overflowDigiTime[digiCount%(DIGI_BUF_SIZE-1)]= cpuCycles()-envCyclesPerScreen();
		overflowDigiVolume[digiCount%(DIGI_BUF_SIZE-1)]= sample;	// always use 8-bit to ease handling									

		overflowDigiCount+=1;	// buffer is meant to collect no more than the samples from one screen refresh!		
	}
}

// -------------------------------------------------------------------------------------------
// detection of test-bit/frequency modulation digi-sample technique (e.g. used in 
// Vicious_SID_2-15638Hz.sid, Storebror.sid, etc)
// -------------------------------------------------------------------------------------------

/* 
KNOWN LIMITATION: the beauty of this approach is that the regular SID filters 
are still applied to the digi-sample signal.. due to the fact that the current filter 
impl is lacking (e.g. it does not work for sampling rates below 44kHz) there was no point 
in adding it here.. with a better filter impl the sample recorded below would first need 
to be passed through the SID filter..
*/
typedef enum {
    FreqIdle=0,
    FreqPrep=1,
    FreqSet=2,
	
    FreqVariant1=3,
    FreqVariant2=4,

} FreqDetectState;


// relevant timing state is tracked for each of the 3 channels
static FreqDetectState freqDetectState[3];
static uint32_t freqDetectTimestamp[3];
static uint8_t freqDetectDelayedSample[3];

inline uint8_t isWithinFreqDetectTimeout(uint8_t voice) {
	return (cpuCycles()-freqDetectTimestamp[voice]) < TB_TIMEOUT;
}

static uint8_t recordFreqSample(uint8_t voice, uint8_t sample) {
	recordSample(sample);

	// reset those SID regs before envelope generator does any damage
	sidPoke(voice*7 + 4, 0);	// GATE
	sidPoke(voice*7 + 1, 0);	// freq HI
	
	freqDetectState[voice]= FreqIdle;
	freqDetectTimestamp[voice]= 0;
	return 1;
}

static uint8_t handleFreqModulationDigi(uint8_t voice, uint8_t reg, uint8_t value) {
	/* 
	test-bit approach: the following settings are performed on the 
	waveform/freq register in short order: 1) Triangle+GATE, 2) TEST+GATE 3) 
	GATE only 4) then the desired output sample is played by setting 
	the "frequency hi-byte" (the whole sequence usually takes about 20-30 
	cycles.. - exaxt limits still to be verified) .. possible variations: GATE 
	is not set in step 2 and/or steps 3 and 4 are switched (see LMan - Vortex.sid)
	*/
	if (reg == 4) {	// waveform
		value &= 0x19;	// mask all excess bits..
		switch (value) {
			case 0x11:	// triangle/GATE
				// reset statemachine 
				freqDetectState[voice] = FreqPrep;				// this may be the start of a digi playback
				freqDetectTimestamp[voice]= cpuCycles();
				break;
			case 0x8:	// TEST
			case 0x9:	// TEST/GATE
				if ((freqDetectState[voice] == FreqPrep) && isWithinFreqDetectTimeout(voice)) {
					freqDetectState[voice] = FreqSet;			// we are getting closer
					freqDetectTimestamp[voice]= cpuCycles();
				} else {
					freqDetectState[voice] = FreqIdle;	// just to reduce future comparisons			
				}
				break;
			case 0x1:	// GATE
				if ((freqDetectState[voice] == FreqSet) && isWithinFreqDetectTimeout(voice)) {
					// variant 1: sample set after GATE
					freqDetectState[voice] = FreqVariant1;		// bring on that sample!
					freqDetectTimestamp[voice]= cpuCycles();
				} else if ((freqDetectState[voice] == FreqVariant2) && isWithinFreqDetectTimeout(voice)) {
					// variant 2: sample set before GATE
					return recordFreqSample(voice, freqDetectDelayedSample[voice]);				
				} else {
					freqDetectState[voice] = FreqIdle;	// just to reduce future comparisons			
				}
				break;
		}
	} else if (reg == 1) {	// step: set sample 
		if ((freqDetectState[voice] == FreqSet) && isWithinFreqDetectTimeout(voice)) {
			// variant 2: sample before GATE
			freqDetectDelayedSample[voice]= value;
			
			freqDetectState[voice] = FreqVariant2;		// now we only need confirmation
			freqDetectTimestamp[voice]= cpuCycles();
		} else if ((freqDetectState[voice] == FreqVariant1) && isWithinFreqDetectTimeout(voice)) {
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
static PulseDetectState pulseDetectState[3];
static uint32_t pulseDetectTimestamp[3];
static uint8_t pulseDetectDelayedSample[3];
static uint8_t pulseDetectMode[3];	// 2= Pulse width LO/ 3= Pulse width HI

inline uint8_t isWithinPulseDetectTimeout(uint8_t voice) {
	return (cpuCycles()-pulseDetectTimestamp[voice]) < TB_PULSE_TIMEOUT;
}

static uint8_t recordPulseSample(uint8_t voice, uint8_t sample) {
	recordSample(sample);

	// reset those SID regs before envelope generator does any damage
	sidPoke(voice*7 + 4, 0);	// GATE
	sidPoke(voice*7 + 2, 0);	// pulse width
	
	pulseDetectState[voice]= PulseIdle;
	pulseDetectTimestamp[voice]= 0;
	return 1;
}

static uint8_t handlePulseModulationDigi(uint8_t voice, uint8_t reg, uint8_t value) {
	/* 
	approach: the following settings are performed on the waveform/pulsewidth 
	register in short order:
	1) desired output sample is played by setting the "pulse width" then	
	2) PULSE+TEST+GATE, 3) PULSE+GATE (the whole sequence usually takes about 
	20-30 cycles.. - exaxt limits still to be verified) variant: pulse-width 
	is set between 2 and 3.

	e.g. used in "Wonderland XII - Digi (part4)"
	
	problem: Galway songs like Yie_Ar_Kung_Fu (see voice 2) are actually using 
	the same sequence within regular song (reduce detection timeout to 7 cycles 
	to avoid false positive).	
	*/
	if (reg == 4) {	// waveform
		value &= 0x49;	// mask all excess bits..
		switch (value) {
			case 0x49:	// PULSE/TEST/GATE
				// test bit is set
				if ((pulseDetectState[voice] == PulsePrep) && isWithinPulseDetectTimeout(voice)) {
					pulseDetectState[voice] = PulseConfirm;			// we are getting closer
					pulseDetectTimestamp[voice]= cpuCycles();
				} else {
					// start of variant 2
					pulseDetectState[voice] = PulsePrep2;
					pulseDetectTimestamp[voice]= cpuCycles();
				}
				break;
			case 0x41:	// PULSE/GATE
				if (((pulseDetectState[voice] == PulseConfirm) || (pulseDetectState[voice] == PulseConfirm2)) 
						&& isWithinPulseDetectTimeout(voice)) {
					uint8_t sample= (pulseDetectMode[voice] == 2) ? 
									pulseDetectDelayedSample[voice] : (pulseDetectDelayedSample[voice] << 4) & 0xff;

					sidSetMute(voice, 1);	// avoid wheezing base signals
					
					pulseDetectState[voice] = PulseIdle;	// just to reduce future comparisons
					return recordFreqSample(voice, sample);								
				} else {
					pulseDetectState[voice] = PulseIdle;	// just to reduce future comparisons			
				}
				break;
		}
	} else if ((reg == 2) || (reg == 3)) {	// PULSE width 
		uint8_t followState;
		if ((pulseDetectState[voice] == PulsePrep2) && isWithinPulseDetectTimeout(voice)) {
			followState= PulseConfirm2;	// variant 2
		} else {
			followState= PulsePrep;		// variant 1
		}
		// reset statemachine 
		pulseDetectState[voice] = followState;		// this may be the start of a digi playback
		pulseDetectTimestamp[voice]= cpuCycles();
		pulseDetectDelayedSample[voice]= value;
		pulseDetectMode[voice]= reg;
	}
	return 0;	// other "detectors" may still take their turn
}

// -------------------------------------------------------------------------------------------
// Detection of the peculiar digi approach used in Ice_Guys.sid: 2-bit samples are written  
// by selecting a corresponding waveform in d412.
// -------------------------------------------------------------------------------------------

static uint8_t handleIceGuysDigi(uint8_t voice, uint8_t reg, uint8_t value) {
	/* 
	would be nice to find a robost check for non Ice_Guys.sid 
	scenarios (false positives) unfortunately I have not spottet it yet... 
	(maybe a file specific hack would be in order to avoid any sideeffects)
	*/
	if ((reg == 4) && (cpuGetProgramMode() == NMI_OFFSET_MASK)
			&& !(sidGetWave(voice)&0x8) && (sidGetAD(voice) == 0x0f)){	
		
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
static const uint8_t mahoneySample[256]= {
	164, 170, 176, 182, 188, 194, 199, 205, 212, 218, 224, 230, 236, 242, 248, 254, 
	164, 159, 153, 148, 142, 137, 132, 127, 120, 115, 110, 105, 99, 94, 89, 84, 
	164, 170, 176, 181, 187, 193, 199, 205, 212, 217, 223, 229, 235, 241, 246, 252, 
	164, 159, 153, 148, 142, 137, 132, 127, 120, 115, 110, 105, 100, 94, 90, 85, 
	164, 170, 176, 182, 188, 194, 200, 206, 213, 219, 225, 231, 237, 243, 249, 255, 
	164, 159, 154, 149, 143, 138, 133, 128, 122, 117, 112, 107, 102, 97, 92, 87, 
	164, 170, 176, 182, 188, 194, 199, 205, 212, 218, 224, 230, 236, 242, 248, 253, 
	164, 159, 154, 149, 143, 138, 133, 128, 122, 117, 112, 107, 102, 97, 92, 87, 
	164, 164, 164, 164, 164, 164, 164, 164, 163, 163, 163, 163, 163, 163, 163, 163, 
	164, 153, 142, 130, 119, 108, 97, 86, 73, 62, 52, 41, 30, 20, 10, 0, 
	164, 164, 164, 164, 164, 164, 163, 163, 163, 163, 163, 163, 163, 163, 162, 162, 
	164, 153, 142, 131, 119, 108, 97, 87, 73, 63, 52, 42, 31, 21, 11, 1, 
	164, 164, 164, 164, 164, 164, 164, 165, 165, 165, 165, 165, 165, 165, 165, 165, 
	164, 153, 142, 131, 120, 109, 98, 88, 75, 64, 54, 44, 33, 23, 13, 3, 
	164, 164, 164, 164, 164, 164, 164, 164,164, 164, 164, 164, 164, 164, 164, 164, 
	164, 153, 142, 131, 120, 109, 99, 88, 75, 65, 55, 44, 34, 24, 14, 4
} ;

inline static uint8_t isMahoneyDigi() {
	// Mahoney's "8-bit" D418 sample-technique requires a specific SID setup
	if (((memGet(0xd406) == 0xff) && (memGet(0xd40d) == 0xff) && (memGet(0xd414) == 0xff)) && // correct SR
		((memGet(0xd404) == 0x49) && (memGet(0xd40b) == 0x49) && (memGet(0xd412) == 0x49)) && // correct waveform
		((memGet(0xd415) == 0xff) && (memGet(0xd416) == 0xff) ) && // correct filter cutoff
		(memGet(0xd417) == 0x3)) {	// voice 1&2 through filter
		
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

static uint16_t swallowPWM[3];

static uint8_t setSwallowMode(uint8_t voice, uint8_t m) {
	swallowPWM[voice]= m;
	sidSetMute(voice, 1);	// avoid wheezing base signals
	
	return 1;
}

static uint8_t handleSwallowDigi(uint8_t voice, uint8_t reg, uint16_t addr, uint8_t value) {
	if (reg == 4) {
		if ((sidGetWave(voice) & 0x8) && !(value & 0x8) && (value & 0x40) 
				&& ((sidGetAD(voice) == 0) && (sidGetSR(voice) == 0xf0))) {
			/* 
			the tricky part here is that the tests here do not trigger for 
			songs which act similarily but which are not using "pulse width 
			modulation" to play digis (e.g. Combat_School.sid, etc)
			*/
			if ((sidGetPulse(voice) == 0x0555) && (sidGetFreq(voice) == 0xfe04)) {
				// e.g. Spasmolytic_part_2.sid
				return setSwallowMode(voice, 1);
			}  else if ((sidGetPulse(voice) == 0x08fe) && (sidGetFreq(voice) == 0xffff)) {
				// e.g. Sverige.sid, Holy_Maling.sid, Voodoo_People_part_*.sid
				return setSwallowMode(voice, 2);	
			} else if ( (sidGetPulse(voice)&0xff00) == 0x0800 ) {
				// e.g. Bla_Bla.sid, Bouncy_Balls_RCA_Intro.sid, Spasmolytic_part_6.sid				
				// Bouncy_Balls_RCA_Intro.sid, Ragga_Run.sid, Wonderland_X_part_1.sid
				return setSwallowMode(voice, 3);
			} 
		}
	} else if (swallowPWM[0] && (reg == 3)) {
		/* 
		depending in the specific player routine, the sample info is available 
		in different registers(d402/d403 and d409/a) ..  for retrieval d403 
		seems to work for most player impls
		*/
		switch(swallowPWM[voice]) {
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

void digiPsidSampleReset() {
	fracPos = 0;
}

static void handlePsidDigi(uint16_t addr, uint8_t value) {			
	// Neue SID-Register
	if ((addr > 0xd418) && (addr < 0xd500))
	{		
		// Start-Hi
		if (addr == 0xd41f) internalStart = (internalStart&0x00ff) | (value<<8);
	  // Start-Lo
		if (addr == 0xd41e) internalStart = (internalStart&0xff00) | (value);
	  // Repeat-Hi
		if (addr == 0xd47f) internalRepeatStart = (internalRepeatStart&0x00ff) | (value<<8);
	  // Repeat-Lo
		if (addr == 0xd47e) internalRepeatStart = (internalRepeatStart&0xff00) | (value);

	  // End-Hi
		if (addr == 0xd43e) {
			internalEnd = (internalEnd&0x00ff) | (value<<8);
		}
	  // End-Lo
		if (addr == 0xd43d) {
			internalEnd = (internalEnd&0xff00) | (value);
		}
	  // Loop-Size
		if (addr == 0xd43f) internalRepeatTimes = value;
	  // Period-Hi
		if (addr == 0xd45e) internalPeriod = (internalPeriod&0x00ff) | (value<<8);
	  // Period-Lo
		if (addr == 0xd45d) {
			internalPeriod = (internalPeriod&0xff00) | (value);
		}
	  // Sample Order
		if (addr == 0xd47d) internalOrder = value;
	  // Sample Add
		if (addr == 0xd45f) internalAdd = value;
	  // Start-Sampling
		if (addr == 0xd41d)
		{
			sampleRepeats = internalRepeatTimes;
			samplePosition = internalStart;
			sampleStart = internalStart;
			sampleEnd = internalEnd;
			sampleRepeatStart = internalRepeatStart;
			samplePeriod = internalPeriod;
			sampleOrder = internalOrder;
			switch (value)
			{
				case 0xfd: sampleActive = 0; break;
				case 0xfe:
				case 0xff: sampleActive = 1; break;

				default: return;
			}
		}
	}	
}

int32_t digiGenPsidSample(int32_t sIn)
{
    static int32_t sample = 0;

    if (!sampleActive) return(sIn);

    if ((samplePosition < sampleEnd) && (samplePosition >= sampleStart))
    {
		//Interpolation routine
		//float a = (float)fracPos/(float)sidGetSampleFreq();
		//float b = 1-a;
		//sIn += a*sample + b*last_sample;

        sIn += sample;

        fracPos += 985248/samplePeriod;		// CIA Timer clock rate 0.985248MHz (PAL)
        
        if (fracPos > sidGetSampleFreq()) 
        {
            fracPos%=sidGetSampleFreq();

			// Naechstes Samples holen
            if (sampleOrder == 0) {
                sampleNibble++;                        // Naechstes Sample-Nibble
                if (sampleNibble==2) {
                    sampleNibble = 0;
                    samplePosition++;
                }
            }
            else {
                sampleNibble--;
                if (sampleNibble < 0) {
                    sampleNibble=1;
                    samplePosition++;
                }
            }       
            if (sampleRepeats)
            {
                if  (samplePosition > sampleEnd)
                {
                    sampleRepeats--;
                    samplePosition = sampleRepeatStart;
                }                       
                else sampleActive = 0;
            }
            
            sample = memReadRAM(samplePosition&0xffff);
            if (sampleNibble==1)   // Hi-Nibble holen?     
                sample = (sample & 0xf0)>>4;
            else 
				sample = sample & 0x0f;
			
			// transform unsigned 4 bit range into signed 16 bit (–32,768 to 32,767) range			
			sample = (sample << 11) - 0x4000; 
        }
    }
    return (sIn);
}

uint16_t digiGetCount() {
	return digiCount;
}

void digiClearBuffer() {
	digiCount= 0;
}

void digiTagOrigin(uint32_t mask, uint32_t offset, uint32_t originalDigiCount) {
	/*
	Due to the current implementation which only considers timer start times but 
	not delays through interrupts - the enties in the digi-sample recording may be 
	out of sequence (e.g. if IRQ starts first it will first write all its values 
	and the NMI will only then add its data - eventhough the NMI's entry logically 
	may belong between some IRQ's entries. The same applies to the main prog which 
	adds all its entries at the very end - after the interrupt routines have 
	already added all their enties)
	
	For the rendering the respective sample recording list must first be sorted 
	(by timestamp). To avoid a full fledged sort of the complete list, the timestamps 
	generated by the different producer streams (NMI/IRQ/main prog) are flagged 
	using a producer specific bit. This then is used as a shortcut when sorting..
	
	This method turns the original relative timestamps into absolute ones by adding 
	a respective offset. It also sets the producer specific bit. 
	
	Mystery time: Instead setting the start time to 0 and then adding the below 
	stuff to the recordings afterwards, the original idea was to directly set the 
	flag and offset in the start time for the simulation. This should have led to 
	the same result.. alone it did not :( mb it's a C skills problem or just some 
	Alchemy bug.. for now the hack works.
	*/
	
	uint16_t len= digiCount - originalDigiCount;
	
	if (len > 0) {
		for (uint16_t i= 0; i<len; i++) {
			digiTime[originalDigiCount+i] = (offset + digiTime[originalDigiCount+i]) | mask;
		}
	}
}

void digiReset(uint8_t compatibility) {
	isC64compatible= compatibility;

	fillMem( (uint8_t*)&swallowPWM, 0, sizeof(swallowPWM) ); 	
    fillMem( (uint8_t*)&digiTime, 0, sizeof(digiTime) ); 
    fillMem( (uint8_t*)&digiVolume, 0, sizeof(digiVolume) ); 

	overflowDigiCount= 0;
    fillMem( (uint8_t*)&overflowDigiTime, 0, sizeof(overflowDigiTime) ); 
    fillMem( (uint8_t*)&overflowDigiVolume, 0, sizeof(overflowDigiVolume) ); 
	
	// PSID digi stuff
	sampleActive= samplePosition= sampleStart= sampleEnd= sampleRepeatStart= fracPos= 
		samplePeriod= sampleRepeats= sampleOrder= sampleNibble= 0;
	internalPeriod= internalOrder= internalStart= internalEnd=
		internalAdd= internalRepeatTimes= internalRepeatStart= 0;

	overflowDigiCount=0;	
	digiCount=0;
	
	//	digi sample detection 
	for (uint8_t i= 0; i<3; i++) {
		freqDetectState[i]= FreqIdle;
		freqDetectTimestamp[i]= 0;
		freqDetectDelayedSample[i]= 0;
		
		pulseDetectState[i]= PulseIdle;
		pulseDetectMode[i]= 0;
		pulseDetectTimestamp[i]= 0;
		pulseDetectDelayedSample[i]= 0;
		
		swallowPWM[i]= 0;
	}
}

// samples per frame.. actually should be well more than this..
const uint8_t digiSampleDetectLimit= 10;	

uint8_t digiDetectSample(uint16_t addr, uint8_t value) {
	uint8_t reg= addr&0x1f;
	uint8_t voice= 0;
    if ((reg >= 7) && (reg <=13)) {voice=1; reg-=7;}
    if ((reg >= 14) && (reg <=20)) {voice=2; reg-=14;}

	if (handleFreqModulationDigi(voice, reg, value)) return 1;
	if (handlePulseModulationDigi(voice, reg, value)) return 1;
	if (handleIceGuysDigi(voice, reg, value)) return 1;			// brittle detector
	if (handleSwallowDigi(voice, reg, addr, value)) return 1;

	if (cpuGetProgramMode() == NMI_OFFSET_MASK) {
		/*
		Some players (e.g. Storebror.sid, Blood_Money_Ingame.sid, Boing_Boom_Tschak.sid,
		etc) use NMI not only for sample playback but also to make other SID-settings.. 
		which may lead to a base hum-noise.. (the ealier digi-approaches do not share 
		this issue since they change only settings within the digi-voice - and that 
		voice is then muted by default..)
		
		hack: to avoid these issues SID settings from within an NMI are limited to 
		the volume register
		*/
		if (addr == 0xd418) {	// traditional digis
			// note: some tunes also set filters while they play digis, 
			// e.g. Digi-Piece_for_Telecomsoft.sid
			// (this may lead to false positives..)
			recordSample(isMahoneyDigi() ? mahoneySample[value] : value << 4);

			// GianaSisters seems to rely on setting made from NMI
			sidPoke(addr&0x1f, value);	
			
			// hack: don't write io_area so that NMI digis don't 
			// disturb loudness of Ferrari_Formula_One.sid
		}
		return 1;
	} else {
		// normal handling
		if (!envIsPSID() && (addr == 0xd418)) {
			recordSample(isMahoneyDigi() ? mahoneySample[value] : value << 4);	// this may lead to false positives..
		}					
		if (!isC64compatible) {
			handlePsidDigi(addr, value);
		}
		// info: Fanta_in_Space.sid, digital_music.sid need regular volume settings produced here from Main..
		return 0;	
	}
}

uint16_t digiGetOverflowCount() {
	return overflowDigiCount;
} 
 
 void digiMoveBuffer2NextFrame() {
	if (overflowDigiCount > 0) {
		uint16_t len= sizeof(long)*overflowDigiCount;
		memcpy(digiTime, overflowDigiTime, len);		
		memcpy(digiVolume, overflowDigiVolume, len);		
		
		// clear just in case..
		fillMem(((uint8_t*)digiTime)+len, 0, DIGI_BUF_SIZE-len);
		fillMem(((uint8_t*)digiVolume)+len, 0, DIGI_BUF_SIZE-len);
	} else {
		fillMem((uint8_t*)digiTime, 0, DIGI_BUF_SIZE);
		fillMem((uint8_t*)digiVolume, 0, DIGI_BUF_SIZE);
	}
	digiCount= overflowDigiCount;
	overflowDigiCount= 0;	
}

static uint32_t isVal(uint32_t mask, uint32_t val) {
	return val&mask;		// is this a value of the selected "mask" category?
}

static uint32_t getValue(uint32_t mask, uint16_t fromIdx) {
	if (fromIdx == IDX_NOT_FOUND) {
		return INVALID_TIME;			// only relevant while it is used for comparisons..
	}
	return (digiTime[fromIdx])&(~mask);	// remove marker flag
}

// must only be used for valid "fromIdx"
static void copy(uint32_t mask, uint16_t toIdx, uint16_t fromIdx) {
	sortedDigiTime[toIdx]= getValue(mask, fromIdx);
	sortedDigiVolume[toIdx]= digiVolume[fromIdx];
}

static uint16_t next(uint32_t mask, uint16_t toIdx, uint16_t fromIdx) {
	copy(mask, toIdx, fromIdx);

	uint16_t i; 
	for (i= fromIdx+1; i<digiCount; i++) {
		if(isVal(mask, digiTime[i])) {
			return i;	// advance index to the next value of this category
		}
	}
	return IDX_NOT_FOUND;
}

static void sortDigiSamples() {
	if (digiCount >0) {
		// IRQ and NMI routines are only more or less aligned and may actually be executed in an overlapping
		// manner. Samples may therefore be stored out of sequence and we need to sort them here first before we render
				
		// the three sample providers:
		uint16_t irqIdx= IDX_NOT_FOUND;
		uint16_t nmiIdx= IDX_NOT_FOUND;
		uint16_t mainIdx= IDX_NOT_FOUND;

		uint16_t i;
		uint16_t noOfIrqSamples= 0;
		uint16_t noOfMainSamples= 0;
		
		// find respective start index for data generated by IRQ/NMI/main
		for (i= 0; i<digiCount; i++) {
			uint32_t val= digiTime[i];
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
		uint16_t toIdx;
		int8_t offset= 0;
		for (toIdx= 0; toIdx<digiCount; toIdx++) {
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
		digiCount+= offset;		
	}	
}

static void fillDigi(uint8_t * digiBuffer, uint16_t startIdx, uint16_t endIdx, uint8_t digi) {
	if (endIdx>=startIdx) {
		fillMem( &digiBuffer[startIdx], digi, (endIdx-startIdx)+1 );
	}
}

uint8_t digiRenderSamples(uint8_t * digiBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall) {	
	/*
	* if there are too few signals, then it's probably just the player setting filters or
	* resetting the volume with no intention to play a digi-sample, e.g. Transformers.sid (Russel Lieblich)
	*/
	if (digiCount > digiSampleDetectLimit) {
		sortDigiSamples();

		// render digi samples
		uint16_t fromIdx=0;	
		uint16_t j;
		for (j= 0; j<digiCount; j++) {
			float scale= (float) (samplesPerCall-1) / cyclesPerScreen;
			uint16_t toIdx= scale*((sortedDigiTime[j] > cyclesPerScreen) ? cyclesPerScreen : sortedDigiTime[j]);

			fillDigi(digiBuffer, fromIdx, toIdx, currentDigi);				

			fromIdx= toIdx;
			currentDigi= sortedDigiVolume[j];
		}
		fillDigi(digiBuffer, fromIdx, (samplesPerCall-1), currentDigi);

		if (vicGetRasterline() == 0xf8) {
			// hack fixes volume issue in Ferrari_Formula_One.sid
			// (todo: the rasterline check is a rather brittle impl... a more 
			// robost/foolproof impl fix needs to be found here)
			// restore last value that was set by main or IRQ (see handleSidWrite()):
			sidPoke(0xd418 & 0x1f, memReadIO(0xd418));
		}
		return 1;
	}
	return 0;
}

/*
* @param digi   is an unsigned 8-bit sample (i.e. origial $d418 4-bit samples have already been shifted
*/
static inline int16_t genDigi(int16_t in, uint8_t digi) { 
    // transform unsigned 8 bit range into signed 16 bit (–32,768 to 32,767) range	(
	// shift only 7 instead of 8 because digis are otherwise too loud)	
	int32_t value = in + (((digi & 0xff) << 7) - 0x4000); 
	
	const int32_t clipValue = 32767;
	if ( value < -clipValue ) {
		value = -clipValue;
	} else if ( value > clipValue ) {
		value = clipValue;
	}
	int16_t out= value;
    return out;
}

void digiMergeSampleData(int8_t hasDigi, int16_t *soundBuffer, uint8_t *digiBuffer, uint32_t len) {
	if (hasDigi) {
		uint32_t i;
		for (i= 0; i<len; i++) {
			soundBuffer[i]= genDigi(soundBuffer[i], digiBuffer[i]);
		}
	} 
}
