/*
* This file contains everything to do with the emulation of the SID chip's
* envelope generator.
*
* Glossary: When talking about "frames" in the comments in this file, it means
* display frames (i.e. 50 or 60Hz screen refresh interval) as a measurement unit
* for elapsed time. (Tracker-musicians may also use "frame" for the "rows" in
* their tracker - but a row will only correspond to a display-frame in 1x speed
* songs whereas higher speed songs will play multiple rows per frame.)
*
* In the predictive implementation of the old emulator version, dealing with
* the ADSR-delay bug was an error prone nightmare. With the transition to a
* cycle-by-cycle emulation luckily all those problems vanished.
*
* Credits:
*  - Various docs found on the net provide the base for this implementation,
*    this includes the findings of the reSid team.
*
* 
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Terms of Use: This software is licensed under a CC BY-NC-SA
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#include "envelope.h"

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "sid.h"


typedef enum {
    Attack=0,
    Decay=1,
    Sustain=2,
    Release=3,
} EnvelopePhase;


// keep state in a separate struct to avoid exposure in the header file..
struct EnvelopeState {
 		// raw register content
	uint8_t ad;
	uint8_t sr;

	uint8_t envphase;

    uint16_t attack;	// for 255 steps
    uint16_t decay;		// for 255 steps
    uint16_t sustain;
    uint16_t release;

	uint8_t envelope_output;
	
	uint16_t current_LFSR;	// sim counter	(continuously counting / only reset by AD(S)R match)
	uint8_t zero_lock;  
	uint8_t exponential_counter;    
};

// this should rather be static - but "friend" wouldn't work then
struct EnvelopeState* getState(Envelope *e) {	
	return (struct EnvelopeState*)e->_state;
}

// what garbage language needs this... (to avoid "unresolved symbols")
uint16_t Envelope::__limit_LFSR;
uint16_t Envelope::__counter_period[16];
uint8_t Envelope::__exponential_delays[256];

Envelope::Envelope(SID *sid, uint8_t voice) {
	_sid= sid;
	_voice= voice;
	_state= (void*) malloc(sizeof(struct EnvelopeState));
}

uint8_t Envelope::getAD() {
	return getState(this)->ad;
}
uint8_t Envelope::getSR() {
	return getState(this)->sr;
}

void Envelope::poke(uint8_t reg, uint8_t val) {
	// thanks to the cycle-by-cycle emulation the below interactions are perfectly
	// in sync with SID (and a post-mortem workarounds are no longer required)

	switch (reg) {
        case 0x4: {
			struct EnvelopeState *state= getState(this);

			uint8_t old_gate= _sid->getWave(_voice)&0x1;
			uint8_t new_gate= val & 0x01;
						
			if (!old_gate && new_gate) {
				/* 
				If the envelope is then gated again (before the RELEASE cycle has
				reached zero amplitude), another ATTACK cycle will begin, starting
				from whatever amplitude had been reached.
				*/
				state->envphase= Attack;				
				state->zero_lock= 0;
				
			} else if (old_gate && !new_gate) {
				/* 
				if the gate bit is reset before the envelope has finished the
				ATTACK cycle, the RELEASE cycles will immediately begin, starting
				from whatever amplitude had been reached
				see http://www.sidmusic.org/sid/sidtech2.html
				*/
				state->envphase= Release;
			} else {
				/* 
				repeating the existing GATE status does NOT change a
				running attack/decay/sustain-phase or release-phase
				*/
			}
			break;
		}
        case 0x5: {		// set AD		
			struct EnvelopeState *state= getState(this);
			state->ad = val;
			
			// convenience: threshold to be reached before incrementing volume
			state->attack  = __counter_period[state->ad >> 4];
			state->decay   = __counter_period[state->ad & 0xf];
			break;
		}
        case 0x6: {		// set SR
			struct EnvelopeState *state= getState(this);
			state->sr = val;

			// convenience
			uint8_t sustain= state->sr >> 4;
			state->sustain = (sustain << 4) | sustain;
			state->release = __counter_period[state->sr & 0xf];
			break;	
		}
	};
}

uint8_t Envelope::getOutput() {
	struct EnvelopeState *state= getState(this);
	return state->envelope_output;
}

void Envelope::reset() {
	struct EnvelopeState *state= getState(this);
	memset((uint8_t*)state, 0, sizeof(EnvelopeState));
	
	state->envphase= Release;
	state->zero_lock= 1;	
}

void Envelope::resetConfiguration(uint32_t sample_rate) {
	
	// The ATTACK rate determines how rapidly the output of a voice
	// rises from zero to peak amplitude when the envelope generator
	// is gated (time/cycle in ms). The respective gradient is used
	// whether the attack is actually started from 0 or from some
	// higher level. The terms "attack time", "decay time" and "release
	// time" are actually very misleading here, since all they translate
	// into are actually some fixed gradients, and the actual decay
	// or release may complete much sooner, e.g. depending on the
	// selected "sustain" level. (note: decay/release times are 3x
	// slower - implemented via exponential_counter)

	const uint16_t attack_times[16]  =	{
		2, 8, 16, 24, 38, 56, 68, 80, 100, 240, 500, 800, 1000, 3000, 5000, 8000
	};
	
	// in regular SID, 15-bit LFSR is clocked each cycle and it is a 
	// shift-register and not a counter.. see http://blog.kevtris.org/?p=13 
	// for more correct modelling. (the current impl seems to be equivalent)

	__limit_LFSR= 0x8000;	// original counter was 15-bit
	
	uint16_t i;
	for (i=0; i<16; i++) {
		// counter must reach respective threshold before envelope value
		// is incremented/decremented
		// note: attack times are in millis & there are 255 steps for envelope..
		
		// would be more logical if actual system clock was used here.. but
		// probably CBM did not care to create separate NTSC/PAL SID versions
		// and the respective limits are just hard coded.. see Egypt.sid
		__counter_period[i]= floor(((double)1000000/1000)/255 * attack_times[i]) + 1;
	}
	
	// lookup table for decay rates
	uint8_t from[] =  {93, 54, 26, 14,  6,  0};
	uint8_t val[] =   { 1,  2,  4,  8, 16, 30};
	for (i= 0; i<256; i++) {
		uint8_t q= 1;
		for (uint8_t j= 0; j<6; j++) {
			if (i>from[j]) {
				q= val[j];
				break;
			}
		}
		__exponential_delays[i]= q;
	}
}
uint8_t Envelope::triggerLFSR_Threshold(uint16_t threshold, uint16_t *end) {
	// check if LFSR threshold was reached
	if (threshold == (*end)) {
		(*end)= 0; // reset counter
		return 1;
	}
	return 0;
}
uint8_t Envelope::handleExponentialDelay() {
	struct EnvelopeState *state= getState(this);
	
	state->exponential_counter+= 1;
	
	uint8_t result= (state->exponential_counter >= __exponential_delays[state->envelope_output]);
	if (result) {
		state->exponential_counter= 0;	// reset to start next round
	}
	return result;
}

void Envelope::clockEnvelope() {
	struct EnvelopeState *state= getState(this);
			
	if (++state->current_LFSR >= __limit_LFSR) {
		state->current_LFSR= 0;
	}
	
	uint8_t previous_envelope_output = state->envelope_output;
	
	switch (state->envphase) {
		case Attack: {                          // Phase 0 : Attack
			if (triggerLFSR_Threshold(state->attack, &state->current_LFSR) && !state->zero_lock) {	
				// inc volume when threshold is reached						
				if (state->envelope_output < 0xff) {	// see Alien.sid, Friday_the_13th, Dawn.sid
				
					// release->attack combo can flip 0xff to 0x0 (e.g. Acke.sid - voice3)
					state->envelope_output= (state->envelope_output + 1) & 0xff;	// increase volume					
				}							
			
				state->exponential_counter = 0;

				if (state->envelope_output == 0xff) {
					state->envphase = Decay;
				}							
			}
			break;
		}
		case Decay: {                   	// Phase 1 : Decay      
			if (triggerLFSR_Threshold(state->decay, &state->current_LFSR) 
					&& handleExponentialDelay() && !state->zero_lock) { 	// dec volume when threshold is reached
				
					if (state->envelope_output != state->sustain) {
						state->envelope_output= (state->envelope_output - 1) & 0xff;	// decrease volume
						
					} else {
						state->envphase = Sustain;
					}
				}	
			break;
		}
		case Sustain: {                        // Phase 2 : Sustain
			triggerLFSR_Threshold(state->decay, &state->current_LFSR);	// keeps using the decay threshold!
		
			// when S is set higher during the "sustain" phase, then this 
			// will NOT cause the level to go UP! 
			// (see http://sid.kubarth.com/articles/interview_bob_yannes.html)
 
			if (state->envelope_output > state->sustain) {
				state->envphase = Decay;
			}
			break;
		}					
		case Release: {                          // Phase 3 : Release
			// this phase must be explicitly triggered by clearing the GATE bit..
			if (triggerLFSR_Threshold(state->release, &state->current_LFSR) 
				&& handleExponentialDelay() && !state->zero_lock) { 		// dec volume when threshold is reached

				// FIXME: is potential "flip" used by any songs?
				state->envelope_output= (state->envelope_output - 1) & 0xff;	// decrease volume
			}
			break;
		}
	}
	if ((state->envelope_output == 0) && (previous_envelope_output > state->envelope_output)) {
		state->zero_lock = 1;	// new "attack" phase must be started to unlock
	}
}


/*
Notes regarding ADSR-bug:

The basic setup is this: There is a 15-bit LFSR that "increments" with each
cycle and eventually overflows. The user specified "attack", "decay" and
"release" settings translate into respective reference thresholds. The threshold
of the active phase (e.g. attack) is compared against the current LFSR and
in case there is an *exact* match, then the envelope counter is updated (i.e.
increased or decreased once) and the LFSR is reset to 0 (other than the
automatic overflow, this is the *only* way the LFSR can ever be reset to 0).
 
One important aspect is that "counting and comparing" *never* stops - i.e. even
when some goal has been achieved (e.g. "sustain" level for "decay" phase or 0
for "release" phase). There is no such thing as an idle mode and either the
"attack", "decay" or "release" counter is always active (e.g. the "release"
threshold stays active until the phase is manually switched to a new "attack"
by setting the GATE bit).

The ADSR-bug may be encountered whenever the currently active threshold is
manually changed to a *lower* value (e.g. by setting the AD or SR registers for
an active phase or by changing the GATE and thereby replacing the currently used
threshold): Whenever the new threshold is already lower than the current LFSR
content then the bug occurs: In order to reach the threshold the LFSR has to
first overflow and go "full cicle". In the worst case that amounts to 32k clock
ticks, i.e. 32ms by which the selected phase is delayed.


test-cases
----------
Confusion_2015_Remix.sid: updates threshold then switches to lower threshold via GATE
Trick'n'Treat.sid: horrible "shhhht-shhht" sounds when wrong/hacked handling..
K9_V_Orange_Main_Sequence.sid: creates "blips" when wrong/hacked handling
$11_Heaven.sid: creates audible clicks whem wrong/hacked handling
Eskimonika.sid: good test for false potitives
Monofail.sid
Blade_Runner_Main_Titles_2SID.sid
Move_Me_Like_A_Movie.sid: creates "false positives" that lead to muted voices
Macrocosm.sid  missing "base instrument" in voice 3 (20secs ff)


Note: the maximum ADSR-bug-condition duration is about 32k cycles/1.7 frames
long, i.e. there are players that explicitily trigger the bug 2 frames in
advance so they can then safely (without bug) start some new note 2 frames later.
*/

