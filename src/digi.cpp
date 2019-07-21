/*
* This file handles the detection of digi-samples.
*
* Ideally - i.e. if the SID emulation covered ALL the undocumented HW feature perfectly -
* then this logic here would be obsolete: The emulation then would know what extra 
* voltage peaks may be temporarilly induced by the use of certain chip features and it 
* would include those parameters for its output calculation.
*
* But currently this isn't modeled in the SID emulation and this here is a "poor man's" 
* prothesis to deal with certain known scenarios. (So that they can be merged on top of
* the regular SID output.)
*
* Some of the techniques (e.g. frequency modulation based) will cause the imperfect
* SID emulation to generate wheezing noises (that may or may not be a problem on the
* real hardware). In case use of a respective technique is detected, the regular SID
* output is disabled to suppress that effect.
*
* Note: In the old predictive Tiny'R'Sid implementation this logic had been performed 
* out of sync with the SID emulation but this is no longer the case with the current
* cycle-by-cycle emulation. (There still might be leftovers from that old version that 
* have not been completely cleaned up yet.)
*
* Known issue: NMI players that reset D418 in their IRQ may lead to periodic clicks
* (test-case: Graphixmania_2_part_6.sid). This is probably due to the currently used 
* ~22 cycles wide sampling interval which might cause an IRQ setting to be used for a 
* complete sample even if it is actually reset from the NMI much more quickly.
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#include "digi.h"
 
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <math.h>

extern "C" {
#include "cpu.h"		// cpuCycles()
#include "memory.h"
#include "env.h"		// envClockRate()
#include "vic.h"		// vicFPS()
}
#include "sid.h"

#define MASK_DIGI_UNUSED 0x80


/* legacy PSID sample playback (there are no multi-SID PSIDs) */
static int32_t _sampleActive;
static int32_t _samplePosition, _sampleStart, _sampleEnd, _sampleRepeatStart;
static int32_t _fracPos = 0;  /* Fractal position of sample */
static int32_t _samplePeriod;
static int32_t _sampleRepeats;
static int32_t _sampleOrder;
static int32_t _sampleNibble;

static int32_t _internalPeriod, _internalOrder, _internalStart, _internalEnd,
_internalAdd, _internalRepeatTimes, _internalRepeatStart;


uint8_t DigiDetector::isFiltered() {
	return (_usedDigiType == DigiPWMTest) || (_usedDigiType == DigiFM) || (_usedDigiType == DigiPWM);
}

DigiDetector::DigiDetector(SID *sid) {	
	_sid= sid;
	_baseAddr= 0;	// not available at this point
	_digiSource= MASK_DIGI_UNUSED;
	_isC64compatible= 1;
}


DigiType DigiDetector::getType() {
	return _usedDigiType;
}

const char* _typeDesc[7] = {
	"NONE",
	"D418",
	"8-BIT",
	"FM",
	"PWM",
	"PWM T",
	"WFM",
};

const char * DigiDetector::getTypeDesc() {
	return _typeDesc[_usedDigiType];
}

uint16_t DigiDetector::getRate() {
	return round(vicFPS() *_digiCount);
}

// detection of test-bit based samples

// note: Swallow's latest FM player (see Comaland_tune_3) uses 21 cycles..
const uint8_t TB_TIMEOUT = 22; // was 12 earlier;	 	// minimum that still works for "Vortex" is 10
const uint8_t TB_PULSE_TIMEOUT = 7; // already reduced this one to avoid false positive in "Yie_ar_kung_fu.sid"


uint8_t DigiDetector::getSample() {
	return _currentDigiSample;
}

uint8_t DigiDetector::getSource() {
	return _currentDigiSrc; 
}


void DigiDetector::recordSample(uint8_t sample, uint8_t voice) {
	
	// SID emu will just use the last set value
	_currentDigiSample= sample;
	_currentDigiSrc= voice;
	
	_digiCount++;
}

uint8_t _use_non_nmi_D418= 1;

uint8_t DigiDetector::assertSameSource(uint8_t voice_plus) {
	// a song may perform different SID interactions that may or may not be meant
	// to produce digi-sample output. the same program may do both, e.g. perform regular volume 
	// setting from MAIN or IRQ and also output samples from NMI. Other programs actually 
	// output sample data from their IRQ or MAIN. And some programs even do a mixed approach 
	// using both NMI and IRQ to output samples (see some of THCM's stuff). Some songs 
	// (e.g. Vicious_SID_2-15638Hz.sid) alternatingly use D418 and PWM (on voice1 & voice2) 
	// from their MAIN to create sample output.
	
	// The goal here is to filter out/ignore false positives - which may
	// cause audible clicks.
	
	// assumption: if some "voice specific" approach is used any D418 write will NOT
	// be interpreted as "sample output".. 

	if (_digiSource != voice_plus) {
		if (_digiSource&MASK_DIGI_UNUSED) {
			_digiSource= voice_plus;			// correct it later if necessary
		} else {
			if (voice_plus == 0) {			// d418 write while there is already other data.. just ignore					
				return 0;
			} else if (_digiSource == 0) {	
				// previously recorded D418 stuff is not really sample output
				// problem: is has already been rendered as a "sample".. i.e. that cannot be undone
				_digiCount= 0;
				_digiSource= voice_plus;		// assumtion: only one digi voice..			
			} else {
				// accept voice switches - test-case: Vicious_SID_2-15638Hz.sid
			}
		}
	} else {
		// hack: many NMI digi players still seem to make one volume reset from their IRQ.. when 
		// interpreted as a digi-sample this causes annoying clicks.. however there are also
		// songs that play from IRQ and from NMI: test-case: Vicious_SID_2-Blood_Money.sid
		if (voice_plus == 0) {
			if (!cpuIsInNMI()) {
				_non_nmi_count_D418++;

				if (!_use_non_nmi_D418) {
					return 0;
				}
			}
		}
		// same source is OK
	}
	return 1;
}

// -------------------------------------------------------------------------------------------
// detection of test-bit/frequency modulation digi-sample technique (e.g. used in 
// Vicious_SID_2-15638Hz.sid, Storebror.sid, Vaakataso.sid, etc)

// the beauty of this approach is that the regular SID filters 
// are still applied to the digi-sample signal.. 
// -------------------------------------------------------------------------------------------

uint8_t DigiDetector::isWithinFreqDetectTimeout(uint8_t voice) {
	return (cpuCycles()- _freqDetectTimestamp[voice]) < TB_TIMEOUT;
}

uint8_t DigiDetector::recordFreqSample(uint8_t voice, uint8_t sample) {
	if(assertSameSource(voice+1)) recordSample(sample, voice+1);

	// reset those SID regs before envelope generator does any damage
	_sid->poke(voice*7 + 4, 0);	// GATE
	_sid->poke(voice*7 + 1, 0);	// freq HI
	
	_freqDetectState[voice]= FreqIdle;
	_freqDetectTimestamp[voice]= 0;

	// test-case: Storebror.sid (there the same voice is later used for regular output)
	_fm_count++;
	_sid->setMute(voice, 1);
	
	_usedDigiType= DigiFM;
	return 1;
}

uint8_t DigiDetector::handleFreqModulationDigi(uint8_t voice, uint8_t reg, uint8_t value) {
	/* 
	test-bit approach: the following settings are performed on the 
	waveform/freq register in short order: 1) Triangle+GATE, 2) TEST+GATE 3) 
	GATE only 4) then the desired output sample is played by setting 
	the "frequency hi-byte" (the whole sequence usually takes about 20-30 
	cycles.. - exaxt limits still to be verified) .. possible variations: GATE 
	is not set in step 2 and/or steps 3 and 4 are switched (see LMan - Vortex.sid)
	
	An unusual (currently unsupported) variation can be seen in Super_Carling_the_Spider_credits 
	where 2 alternating NMIs are using 2 different voices for this..
	*/
	if (reg == 4) {	// waveform
		value &= 0x19;	// mask all excess bits..
		switch (value) {
			case 0x11:	// triangle/GATE
				// reset statemachine 
				_freqDetectState[voice] = FreqPrep;				// this may be the start of a digi playback
				_freqDetectTimestamp[voice]= cpuCycles();
			break;
			case 0x8:	// TEST
			case 0x9:	// TEST/GATE
				if ((_freqDetectState[voice] == FreqPrep) && isWithinFreqDetectTimeout(voice)) {
					_freqDetectState[voice] = FreqSet;			// we are getting closer
					_freqDetectTimestamp[voice]= cpuCycles();
				} else {
					_freqDetectState[voice] = FreqIdle;	// just to reduce future comparisons			
				}
				break;
			case 0x1:	// GATE
				if ((_freqDetectState[voice] == FreqSet) && isWithinFreqDetectTimeout(voice)) {
					// variant 1: sample set after GATE
					_freqDetectState[voice] = FreqVariant1;		// bring on that sample!
					_freqDetectTimestamp[voice]= cpuCycles();
				} else if ((_freqDetectState[voice] == FreqVariant2) && isWithinFreqDetectTimeout(voice)) {
					// variant 2: sample set before GATE
					return recordFreqSample(voice, _freqDetectDelayedSample[voice]);				
				} else {
					_freqDetectState[voice] = FreqIdle;	// just to reduce future comparisons			
				}
				break;
		}
	} else if (reg == 1) {	// step: set sample 
		if ((_freqDetectState[voice] == FreqSet) && isWithinFreqDetectTimeout(voice)) {
			// variant 2: sample before GATE
			_freqDetectDelayedSample[voice]= value;
			
			_freqDetectState[voice] = FreqVariant2;		// now we only need confirmation
			_freqDetectTimestamp[voice]= cpuCycles();
		} else if ((_freqDetectState[voice] == FreqVariant1) && isWithinFreqDetectTimeout(voice)) {
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

uint8_t DigiDetector::isWithinPulseDetectTimeout(uint8_t voice) {
	return (cpuCycles()- _pulseDetectTimestamp[voice]) < TB_PULSE_TIMEOUT;
}

uint8_t DigiDetector::recordPulseSample(uint8_t voice, uint8_t sample) {
	if(assertSameSource(voice+1))	recordSample(sample, voice+1);

	// reset those SID regs before envelope generator does any damage
	_sid->poke(voice*7 + 4, 0);	// GATE
	_sid->poke(voice*7 + 2, 0);	// pulse width
	
	_pulseDetectState[voice]= PulseIdle;
	_pulseDetectTimestamp[voice]= 0;
	
	_usedDigiType= DigiPWMTest;
	return 1;
}

uint8_t DigiDetector::handlePulseModulationDigi(uint8_t voice, uint8_t reg, uint8_t value) {
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
				if ((_pulseDetectState[voice] == PulsePrep) && isWithinPulseDetectTimeout(voice)) {
					_pulseDetectState[voice] = PulseConfirm;			// we are getting closer
					_pulseDetectTimestamp[voice]= cpuCycles();
				} else {
					// start of variant 2
					_pulseDetectState[voice] = PulsePrep2;
					_pulseDetectTimestamp[voice]= cpuCycles();
				}
				break;
			case 0x41:	// PULSE/GATE
				if (((_pulseDetectState[voice] == PulseConfirm) || (_pulseDetectState[voice] == PulseConfirm2)) 
						&& isWithinPulseDetectTimeout(voice)) {
					uint8_t sample= (_pulseDetectMode[voice] == 2) ? 
									_pulseDetectDelayedSample[voice] : (_pulseDetectDelayedSample[voice] << 4) & 0xff;

					_sid->setMute(voice, 1);	// avoid wheezing base signals
					
					_pulseDetectState[voice] = PulseIdle;	// just to reduce future comparisons
					return recordPulseSample(voice, sample);								
				} else {
					_pulseDetectState[voice] = PulseIdle;	// just to reduce future comparisons			
				}
				break;
		}
	} else if ((reg == 2) || (reg == 3)) {	// PULSE width 
		PulseDetectState followState;
		if ((_pulseDetectState[voice] == PulsePrep2) && isWithinPulseDetectTimeout(voice)) {
			followState= PulseConfirm2;	// variant 2
		} else {
			followState= PulsePrep;		// variant 1
		}
		// reset statemachine 
		_pulseDetectState[voice] = followState;		// this may be the start of a digi playback
		_pulseDetectTimestamp[voice]= cpuCycles();
		_pulseDetectDelayedSample[voice]= value;
		_pulseDetectMode[voice]= reg;
	}
	return 0;	// other "detectors" may still take their turn
}

// -------------------------------------------------------------------------------------------
// Mahoney's D418 "8-bit" digi sample technique..
// -------------------------------------------------------------------------------------------

// based on Mahoney's amplitude_table_8580.txt (not using SID model specific sample logic so this should be good enough)
static const uint8_t _mahoneySample[256]= {
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

uint8_t DigiDetector::isMahoneyDigi() {
	// Mahoney's "8-bit" D418 sample-technique requires a specific SID setup
	
	// The idea of the approach is that the combined effect of the output of all three voices 
	// (2 filtered, 1 unfiltered) will create the output sample that is written into the d418 
	// register (this is actually audible without using a separate digi-track - though the
	// quality seems to be lacking with the current -filter?- emulation)
	
	// Either the voice-output OR the separately recorded input sample but be used in the
	// output signal - but not both! (using the later here)
	
	//  We_Are_Demo_tune_2.sid seems to be using the same approach only the SR uses 0xfb instead of 0xff	

	if ((memGet(_baseAddr+0x17) == 0x3) && 														// voice 1&2 through filter
		(memGet(_baseAddr+0x15) == 0xff) && (memGet(_baseAddr+0x16) == 0xff) &&							// correct filter cutoff
		(memGet(_baseAddr+0x06) == memGet(_baseAddr+0x0d)) && (memGet(_baseAddr+0x06) == memGet(_baseAddr+0x14)) &&		// all same SR
		(memGet(_baseAddr+0x04) == 0x49) && (memGet(_baseAddr+0x0b) == 0x49) && (memGet(_baseAddr+0x12) == 0x49)  // correct waveform: pulse + test + gate
		) {	

		if (memGet(_baseAddr+0x06) >= 0xfb) {	// correct SR .. might shorten the tests some..
			_sid->setMute(0, 1);
			_sid->setMute(1, 1);
			_sid->setMute(2, 1);
			
			_usedDigiType = DigiMahoneyD418;
		}			
		
		return 1;
	}
	return 0;
}

// -------------------------------------------------------------------------------------------
// Swallow 'pulse width modulation': the players handled here use some PWM approach but without 
// the more recent test-bit technique.. each player depends on specific frequency 
// settings and differs in how sample values are transformed and then written as 
// differently interpreted hi/lo pulse-width settings.. examples can be found 
// from musicians like Swallow, Danko or Cyberbrain
// -------------------------------------------------------------------------------------------

uint8_t DigiDetector::setSwallowMode(uint8_t voice, uint8_t m) {
	_swallowPWM[voice]= m;
	_sid->setMute(voice, 1);	// avoid wheezing base signals
	
	return 1;
}

uint8_t DigiDetector::handleSwallowDigi(uint8_t voice, uint8_t reg, uint16_t addr, uint8_t value) {
	if (reg == 4) {
		if ((_sid->getWave(voice) & 0x8) && !(value & 0x8) && (value & 0x40) 
				&& ((_sid->getAD(voice) == 0) && (_sid->getSR(voice) == 0xf0))) {
			/* 
			the tricky part here is that the tests here do not trigger for 
			songs which act similarily but which are not using "pulse width 
			modulation" to play digis (e.g. Combat_School.sid, etc)
			*/
			if ((_sid->getPulse(voice) == 0x0555) && (_sid->getFreq(voice) == 0xfe04)) {
				// e.g. Spasmolytic_part_2.sid
				return setSwallowMode(voice, 1);
			}  else if ((_sid->getPulse(voice) == 0x08fe) && (_sid->getFreq(voice) == 0xffff)) {
				// e.g. Sverige.sid, Holy_Maling.sid, Voodoo_People_part_*.sid
				return setSwallowMode(voice, 2);	
			} else if ( (_sid->getPulse(voice)&0xff00) == 0x0800 ) {
				// e.g. Bla_Bla.sid, Bouncy_Balls_RCA_Intro.sid, Spasmolytic_part_6.sid				
				// Bouncy_Balls_RCA_Intro.sid, Ragga_Run.sid, Wonderland_X_part_1.sid
				return setSwallowMode(voice, 3);
			} 
		}
	} else if (_swallowPWM[0] && (reg == 3)) {
		/* 
		depending in the specific player routine, the sample info is available 
		in different registers(d402/d403 and d409/a) ..  for retrieval d403 
		seems to work for most player impls
		*/
		switch(_swallowPWM[voice]) {
			case 1:
				recordSample((value<<4) & 0xff, voice+1);
				break;
			case 2: 
				recordSample((value<<4) | (value>>4), voice+1);				
				break;
			case 3: 
				recordSample((value<<4) & 0xff, voice+1);
				break;
		}
		
		_usedDigiType= DigiPWM;
		
		return 1;
	}
	return 0;
}

// ------------------------------ legacy PSID digi stuff ----------------------------------

static void handlePsidDigi(uint16_t addr, uint8_t value) {			
	// Neue SID-Register
	if ((addr > 0xd418) && (addr < 0xd500))
	{		
		// Start-Hi
		if (addr == 0xd41f) _internalStart = (_internalStart&0x00ff) | (value<<8);
	  // Start-Lo
		if (addr == 0xd41e) _internalStart = (_internalStart&0xff00) | (value);
	  // Repeat-Hi
		if (addr == 0xd47f) _internalRepeatStart = (_internalRepeatStart&0x00ff) | (value<<8);
	  // Repeat-Lo
		if (addr == 0xd47e) _internalRepeatStart = (_internalRepeatStart&0xff00) | (value);

	  // End-Hi
		if (addr == 0xd43e) {
			_internalEnd = (_internalEnd&0x00ff) | (value<<8);
		}
	  // End-Lo
		if (addr == 0xd43d) {
			_internalEnd = (_internalEnd&0xff00) | (value);
		}
	  // Loop-Size
		if (addr == 0xd43f) _internalRepeatTimes = value;
	  // Period-Hi
		if (addr == 0xd45e) _internalPeriod = (_internalPeriod&0x00ff) | (value<<8);
	  // Period-Lo
		if (addr == 0xd45d) {
			_internalPeriod = (_internalPeriod&0xff00) | (value);
		}
	  // Sample Order
		if (addr == 0xd47d) _internalOrder = value;
	  // Sample Add
		if (addr == 0xd45f) _internalAdd = value;
	  // Start-Sampling
		if (addr == 0xd41d)
		{
			_sampleRepeats = _internalRepeatTimes;
			_samplePosition = _internalStart;
			_sampleStart = _internalStart;
			_sampleEnd = _internalEnd;
			_sampleRepeatStart = _internalRepeatStart;
			_samplePeriod = _internalPeriod;
			_sampleOrder = _internalOrder;
			switch (value)
			{
				case 0xfd: _sampleActive = 0; break;
				case 0xfe:
				case 0xff: _sampleActive = 1; break;

				default: return;
			}
		}
	}	
}

int32_t DigiDetector::genPsidSample(int32_t sample_in)
{
    static int32_t sample = 0;

    if (!_sampleActive) return(sample_in);

    if ((_samplePosition < _sampleEnd) && (_samplePosition >= _sampleStart))
    {
		//Interpolation routine
		//float a = (float)_fracPos/(float)sidGetSampleFreq();
		//float b = 1-a;
		//sample_in += a*sample + b*last_sample;

        sample_in += sample;

        _fracPos += envClockRate()/_samplePeriod;		
        
        if (_fracPos > SID::getSampleFreq()) 
        {
            _fracPos%= SID::getSampleFreq();

			// Naechstes Samples holen
            if (_sampleOrder == 0) {
                _sampleNibble++;                        // Naechstes Sample-Nibble
                if (_sampleNibble==2) {
                    _sampleNibble = 0;
                    _samplePosition++;
                }
            }
            else {
                _sampleNibble--;
                if (_sampleNibble < 0) {
                    _sampleNibble=1;
                    _samplePosition++;
                }
            }       
            if (_sampleRepeats)
            {
                if  (_samplePosition > _sampleEnd)
                {
                    _sampleRepeats--;
                    _samplePosition = _sampleRepeatStart;
                }                       
                else _sampleActive = 0;
            }
            
            sample = memReadRAM(_samplePosition&0xffff);
            if (_sampleNibble==1)   // Hi-Nibble holen?     
                sample = (sample & 0xf0)>>4;
            else 
				sample = sample & 0x0f;
			
			// transform unsigned 4 bit range into signed 16 bit (–32,768 to 32,767) range			
			sample = (sample << 11) - 0x3fc0; 
        }
    }	
    return sample_in;
}

void DigiDetector::resetCount() {
		
	if (_usedDigiType == DigiFM) {
		if (!_fm_count) {
			// test-case: Storebror.sid => switches back to other digi technique
			_sid->setMute(0, 0);
			_sid->setMute(1, 0);
			_sid->setMute(2, 0);
			_digiSource= 0;				// restart detection
			_usedDigiType= DigiNone;
		} else {
			_fm_count= 0;
		}
	} else if (_usedDigiType == DigiD418) {
		// test-case: Arkanoid.sid - doesn't use NMI at all
		if (_digiCount == 0) {
			_usedDigiType= DigiNone;
			_use_non_nmi_D418= 1;
		} else {
			// unfortunately there is a one frame delay here..
			_use_non_nmi_D418= _non_nmi_count_D418 > 10;
			_non_nmi_count_D418= 0;
		}
	}
	_digiCount= 0;
}

void DigiDetector::resetModel(uint8_t is_6581) {
	// currently unused.. might add handling for chip specific bahavior at some later stage
}

void DigiDetector::reset(uint8_t compatibility, uint8_t is_6581) {
	
	_baseAddr= _sid->getBaseAddr();
	
	_isC64compatible= compatibility;

	resetModel(is_6581);

	memset( (uint8_t*)&_swallowPWM, 0, sizeof(_swallowPWM) ); 	

	_digiCount= 0;
	_digiSource= MASK_DIGI_UNUSED;
		
	// PSID digi stuff
	_sampleActive= _samplePosition= _sampleStart= _sampleEnd= _sampleRepeatStart= _fracPos= 
		_samplePeriod= _sampleRepeats= _sampleOrder= _sampleNibble= 0;
	_internalPeriod= _internalOrder= _internalStart= _internalEnd=
		_internalAdd= _internalRepeatTimes= _internalRepeatStart= 0;

	//	digi sample detection 
	for (uint8_t i= 0; i<3; i++) {
		_freqDetectState[i]= FreqIdle;
		_freqDetectTimestamp[i]= 0;
		_freqDetectDelayedSample[i]= 0;
		
		_pulseDetectState[i]= PulseIdle;
		_pulseDetectMode[i]= 0;
		_pulseDetectTimestamp[i]= 0;
		_pulseDetectDelayedSample[i]= 0;
		
		_swallowPWM[i]= 0;
	}

	_currentDigiSample= 0x80;	// center
	_currentDigiSrc= 0;
	
	_usedDigiType= DigiNone;

	_non_nmi_count_D418= 0;
	_use_non_nmi_D418= 1;
}

uint8_t DigiDetector::getD418Sample( uint8_t value) {
	/*
	The D418 "volume register" technique was probably the oldest of the attempts to play 4-bit digi 
	samples on the C64.

	The output is based on 2 effects - whose force depends on the specific SID chip model
	being used:

	1) Setting the volume creates a short voltage surge (proportional to the used volume) which 
	in itself can be used to create digi output. However that effect is stonger on original 6581 chips 
	but almost inexistent on later 8580 models. (The reason why certain digis don't work on later C64s.)

	2) Since it is the "volume register" that is used, the setting impacts whatever signal is 
	being output by the three SID voices, i.e. the respective voice output is distorted 
	accordingly. (Songs like "Arkanoid" use all three voices for melody and parts of the played 
	digi-samples are audible based on the those distorted signals alone. On older 6581 SIDs 
	an additional "click boost" would have been created in addition to this effect - whereas 
	8580 would only have played the voice based part.).
	*/	
	
	_usedDigiType = DigiD418;
	
	return (value&0xf) * 0x11;
//	return (((value&0xf) << 3)) +64;	// better reduce the volume? 
}

uint8_t DigiDetector::detectSample(uint16_t addr, uint8_t value) {
	if ((SID::getNumberUsedChips() == 1) && _isC64compatible) addr&= ~(0x3e0); // mask out alternative addresses of d400 SID (see 5-Channel_Digi-Tune).. use in PSID would crash playback of recorded samples
	
	if (envIsPSID() && _isC64compatible) return 0;	// for songs like MicroProse_Soccer_V1.sid tracks >5 (PSID digis must still be handled.. like Demi-Demo_4_PSID.sid)
	
	uint8_t reg= addr&0x1f;
	uint8_t voice= 0;
    if ((reg >= 7) && (reg <=13)) {voice=1; reg-=7;}
    if ((reg >= 14) && (reg <=20)) {voice=2; reg-=14;}
	
	if (handleFreqModulationDigi(voice, reg, value)) return 1;
	if (handlePulseModulationDigi(voice, reg, value)) return 1;
	if (handleSwallowDigi(voice, reg, addr, value)) return 1;

	// normal handling
	if (envIsRSID() && (addr == (_baseAddr+0x18))) {
		if(assertSameSource(0)) recordSample(isMahoneyDigi() ? _mahoneySample[value] : getD418Sample(value), 0);	// this may lead to false positives..
	}					
	if (!_isC64compatible) {
		handlePsidDigi(addr, value);
	}
	return 0;
}
