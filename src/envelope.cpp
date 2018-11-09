/*
 * This file contains everything to do with the emulation of the SID chip's 
 * envelope generator.
 *
 * There is nothing left in here from the original "TinySid for Linux" implementation
 * (which did not have any support for the ADSR-delay bug, etc)
 * 
 * This implementation isn't as "clean" as it would be in some "cycle-by-cycle"
 * emulation, but again some hacks are needed to keep things "in-sync" which are
 * not properly in-sync on the emulation level.
 *
 * <p>Credits:
 * <ul>
 * <li>Various docs found on the net provide the base for this implementation, this
 *     includes the findings of the reSid team.
 * </ul>
 * <p>Tiny'R'Sid (c) 2011-2018 J.Wothke
 * <p>version 0.9
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */
#include "envelope.h"

 #include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
extern "C" {
#include "env.h"
#include "cpu.h"
}
#include "sid2.h"


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

    uint32_t attack;	// for 255 steps
    uint32_t decay;		// for 255 steps
    uint32_t sustain;
    uint32_t release;

	uint8_t envelopeOutput;
	
	uint16_t currentLFSR;	// sim counter	(continuously counting / only reset by AD(S)R match)
	uint8_t zeroLock;  
	uint8_t exponentialCounter;    

	// ADSR bug detection 
	uint32_t lastCycles;
	uint16_t simLFSR;		// base for CPU-side simulation
	uint16_t origLFSR;
	double overflow;
	uint8_t rUpped;
};

struct EnvelopeState* getState(Envelope *e) {	// this should rather be static - but "friend" wouldn't work then
	return (struct EnvelopeState*)e->_state;
}

// what garbage language needs this... (to avoid "unresolved symbols")
double Envelope::sCyclesPerSample;
uint32_t Envelope::sLimitLFSR;
uint32_t Envelope::sCounterPeriod[16];
uint8_t Envelope::sExponentialDelays[256];

Envelope::Envelope(SID *sid, uint8_t voice) {
	_sid= sid;
	_voice= voice;
	_state= (void*) malloc(sizeof(struct EnvelopeState));
}

void Envelope::syncState() {		// FIXME move this directly into poke()
	struct EnvelopeState *state= getState(this);

	// threshold to be reached before incrementing volume
	state->attack  = sCounterPeriod[state->ad >> 4];
	state->decay   = sCounterPeriod[state->ad & 0xf];
	uint8_t sustain= state->sr >> 4;
 	state->sustain = (sustain << 4) | sustain;
	state->release = sCounterPeriod[state->sr & 0xf];
}

uint8_t Envelope::getAD() {
	return getState(this)->ad;
}
uint8_t Envelope::getSR() {
	return getState(this)->sr;
}

void Envelope::poke(uint8_t reg, uint8_t val) {
	// problem: correctly the below updates should occur cpuCycles() into the next SID-
	// rendering but with the current impl the SID-rendering will use the updated 
	// state *immediately*..
	
	switch (reg) {
        case 0x4: {
			struct EnvelopeState *state= getState(this);

			uint8_t oldGate= _sid->getWave(_voice)&0x1;
			uint8_t newGate= val & 0x01;
			
			if (!oldGate && newGate) {
				simGateAdsrBug(0, state->ad >> 4);	// switch to 'attack'
				/* 
				If the envelope is then gated again (before the RELEASE cycle has reached 
				zero amplitude), another ATTACK cycle will begin, starting from whatever 
				amplitude had been reached.
				*/
				state->envphase= Attack;				
				state->zeroLock= 0;
			} else if (oldGate && !newGate) {
				simGateAdsrBug(1, state->sr & 0xf);	// switch to 'release'
				/* 
				if the gate bit is reset before the envelope has finished the ATTACK cycle, 
				the RELEASE cycles will immediately begin, starting from whatever amplitude 
				had been reached
				// see http://www.sidmusic.org/sid/sidtech2.html
				*/
				state->envphase= Release;
			}
			break;
		}
        case 0x5: {		// set AD
			struct EnvelopeState *state= getState(this);

			if (state->envphase != Release) {	// reminder: S keeps using the D threshold
				if (state->envphase == Attack) {
					simGateAdsrBug(2, val >> 4);
				} else {
					simGateAdsrBug(3, val & 0xf);
				}
			}
			state->ad = val;			
			break;
		}
        case 0x6: {		// set SR
			struct EnvelopeState *state= getState(this);

			if (state->envphase == Release) {
				simGateAdsrBug(4, val & 0xf);
			}			
			state->sr = val; 
			break;	
		}
	};
}

uint8_t Envelope::getOutput() {
	struct EnvelopeState *state= getState(this);
	return state->envelopeOutput;
}

void Envelope::reset() {
	struct EnvelopeState *state= getState(this);
	memset((uint8_t*)state, 0, sizeof(EnvelopeState));
	
	state->envphase= Release;
	state->zeroLock= 1;
}

void Envelope::resetConfiguration(uint32_t sampleRate) {
	sCyclesPerSample = ((double)envClockRate()) / sampleRate;
	
	// The ATTACK rate determines how rapidly the output of a voice rises from zero to peak amplitude when 
	// the envelope generator is gated (time/cycle in ms). The respective gradient is used whether the attack is 
	// actually started from 0 or from some higher level. The terms "attack time", "decay time" and 
	// "release time" are actually very misleading here, since all they translate into are actually 
	// some fixed gradients, and the actual decay or release may complete much sooner, e.g. depending
	// on the selected "sustain" level.
	// note: decay/release times are 3x longer (implemented via exponentialCounter)
	const int32_t attackTimes[16]  =	{
		2, 8, 16, 24, 38, 56, 68, 80, 100, 240, 500, 800, 1000, 3000, 5000, 8000
	};
	
	/* 
	in regular SID, 15-bit LFSR counter counts cpu-clocks, problem here is the lack of 
	cycle by cycle SID emulation (only have a SID snapshot every ~20ms to work with) 
	during rendering the computing granularity then is 'one sample' (not 'one cpu cycle'
	- but around 20).. instead of still trying to simulate a 15-bit cycle-counter a 
	sample-counter is used directly
	*/
	sLimitLFSR= round(((double)0x7fff)/sCyclesPerSample) + 1;	// original counter was 15-bit
	
	uint16_t i;
	for (i=0; i<16; i++) {
		// counter must reach respective threshold before envelope value is incremented/decremented								
		// note: attack times are in millis & there are 255 steps for envelope..
		
		sCounterPeriod[i]= floor(((double)envClockRate())/(255*1000) * attackTimes[i] / sCyclesPerSample)+1;	// in samples
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
		sExponentialDelays[i]= q;
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
	
	state->exponentialCounter+= 1;
	
	uint8_t result= (state->exponentialCounter >= sExponentialDelays[state->envelopeOutput]);
	if (result) {
		state->exponentialCounter= 0;	// reset to start next round
	}
	return result;
}
void Envelope::updateEnvelope() {
	struct EnvelopeState *state= getState(this);
	
	/*
	todo: step width here is 22 cycles - instead of 1; double check for overflow/rounding related issues  
	
	Updates the envelope related status by a "one sample" wide step (i.e. ~22 cycles - i.e. 
	about 0.0226ms). The shortest ADSR interval is 2ms which would then correspond to a counter of
	88.2.
	
	process the volume according to the phase and adsr values (explicit switching of ADSR 
	phase is handled in sidPoke() so there is no need to handle that here)

	advance envelope LFSR counter (originally this would be a 15-bit cycle counter.. but we 
	are counting samples here)

	ADSR bug scenario: normally the maximum thresholds used for the original 15-bit counter 
	would have been around 0x7a13 (i.e. somewhat below the 7fff range that can be handled by 
	the counter). For certain bug scenarios it is possible that the threshold is missed and 
	the counter keeps counting until it again reaches the threshold after a wrap-around.. 
	(see sidPoke() for specific ADSR-bug handling)
	*/
	if (++state->currentLFSR >= sLimitLFSR) {
		state->currentLFSR= 0;
	}
	
	uint8_t previousEnvelopeOutput = state->envelopeOutput;
				
	switch (state->envphase) {
		case Attack: {                          // Phase 0 : Attack
			if (triggerLFSR_Threshold(state->attack, &state->currentLFSR) && !state->zeroLock) {	
				// inc volume when threshold is reached						
					if (state->envelopeOutput < 0xff) {
					
					// FIXME check for undesireble side-effects of this hack..
					
						/* see Alien.sid: "full envelopeOutput level" GATE off/on sequences 
						   within same IRQ will cause undesireable overflow.. this might not 
						   be a problem in cycle accurate emulations.. but here it is (we 
						   only see a 20ms snapshot)
						*/
						state->envelopeOutput= (state->envelopeOutput + 1) & 0xff;	// increase volume
					}							
				
					state->exponentialCounter = 0;

					if (state->envelopeOutput == 0xff) {
						state->envphase = Decay;
					}							
				}
			break;
		}
		case Decay: {                   	// Phase 1 : Decay      
			if (triggerLFSR_Threshold(state->decay, &state->currentLFSR) 
					&& handleExponentialDelay() && !state->zeroLock) { 	// dec volume when threshold is reached
				
					if (state->envelopeOutput != state->sustain) {
						state->envelopeOutput= (state->envelopeOutput - 1) & 0xff;	// decrease volume
					} else {
						state->envphase = Sustain;
					}
				}	
			break;
		}
		case Sustain: {                        // Phase 2 : Sustain
			triggerLFSR_Threshold(state->decay, &state->currentLFSR);	// keeps using the decay threshold!
		
			// when S is set higher during the "sustain" phase, then this will NOT cause the level to go UP! 
			// (see http://sid.kubarth.com/articles/interview_bob_yannes.html)
 
	//		if (state->envelopeOutput != state->sustain) {	// old impl... lets see if this breaks something..
			if (state->envelopeOutput > state->sustain) {
				state->envphase = Decay;
			}
			break;
		}					
		case Release: {                          // Phase 3 : Release
			// this phase must be explicitly triggered by clearing the GATE bit..
			if (triggerLFSR_Threshold(state->release, &state->currentLFSR) 
				&& handleExponentialDelay() && !state->zeroLock) { 		// dec volume when threshold is reached

					state->envelopeOutput= (state->envelopeOutput - 1) & 0xff;	// decrease volume
				}
			break;
		}
	}
	if ((state->envelopeOutput == 0) && (previousEnvelopeOutput > state->envelopeOutput)) {
		state->zeroLock = 1;	// new "attack" phase must be started to unlock
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
* The below add-on hack in simGateAdsrBug() tries to mitigate that blind-spot - at least for some of the
* most relevant scenarios.
*/
uint16_t Envelope::getCurrentThreshold() {
	uint16_t threshold;
	switch (getState(this)->envphase) {
		case Attack: 
			{ threshold = sCounterPeriod[getState(this)->ad >> 4]; break; }
		case Decay: 
		case Sustain:	// keeps using the decay limit
			{ threshold = sCounterPeriod[getState(this)->ad & 0xf]; break; }
		case Release: 
			{ threshold = sCounterPeriod[getState(this)->sr & 0xf]; break; }
	}
	return threshold;
}
// util for envelope generator LFSR counter
int32_t Envelope::clocksToSamples(int32_t clocks) {
	struct EnvelopeState *state= getState(this);
	return round(((float)clocks)/sCyclesPerSample);
}

void Envelope::snapshotLFSR() {
	struct EnvelopeState *state= getState(this);
	state->lastCycles= 0; 
	state->simLFSR= state->origLFSR= state->currentLFSR;
	state->overflow= 0;
	state->rUpped = 0;
}

/*
	Note: the maximum ADSR-bug-condition duration is about 32k cycles/1.7 frames long, i.e. there are 
	players that explicitily trigger the bug 2 frames in advance so they can then safely (without bug) 
	start some new note 2 frames later. Therefore ADSR-bug handling must deal with multi-frame scenarios!

	Problem/limitation of the current emu impl is that the internal SID state ISN'T updated in sync with
	the CPU emulation, i.e. first the CPU is emulated for some interval (e.g. an IRQ) and then afterwards
	the SID emulation is run for that same interval: i.e. the CPU emulation DOESN'T see the current/up-to-date
	state of the SID - and vice-versa the SID emulation does not see CPU induced changes at the correct time.
	
	In the ADSR-bug context this means:

	Problem 1: simLFSR (initially) only reflects the state at the end of the last SID output renderung
	but to correctly detect the bug the up-to-date counter is needed here. Workaround: supposing this here 
	happends from an IRQ (the 95% case) then the previous SID rendering covered the time just up to the IRQ 
	call and cpuCycles() measured the cycles that have since passend within the IRQ.
	(note: cpuCycles() refers to the local context, i.e. it is reset for each new IRQ, etc.) 

	Problem 2: (correctly) the ADSR bug will occur "elapsed" (see var below) samples into the next SID output 
	rendering, i.e. NOT right from the start. Before that the old threshold would still be used. The overflow 
	at that point would mean that a total of "sLimitLFSR-(currentLFSR-newThreshold)" increment steps would 
	*then* be needed to reach the newThreshold, i.e. the "elapsed" time here would be relevant as a "correct 
	timing" offset.
	
	
	Rearding below method:
	
	The next SID emulation run will later start with whatever threshold is set here *last*, i.e. 
	any intermediate settings that might normally still be used for a little while are completely skipped. 
	The assumption is that most bug-cases the relavively short duration within the player-routine
	(e.g. 2000 cycles) should not matter too much.
	
	Theoretically a player might make multiple settings, where some intermediate setting first  	
	results in a bug-condition (i.e. lower threshold set) but where a later made higher-threshold setting
	again removes that bug-condition.
	
	The below method tries to incrementally predict the state of the LFSR at the time some setting
	is actually made and to force the SID-emu LFSR register into bug-mode if necessary.
*/
/*
void dbgPrint(uint8_t dbgIdx, uint16_t newRate) {
	const char* t="x";
	switch (dbgIdx) {
	case 0:
		t= "GATE ON";
		break;
	case 1:
		t= "GATE OFF";
		break;
	case 2: 
		t= "A";
		break;
	case 3:
		t= "D";
		break;
	case 4:
		t= "R";
		break;
	}	
	fprintf(stderr, "  %s %d %lu\n", t, newRate, cpuCycles());
}
*/
void Envelope::simGateAdsrBug(uint8_t dbgIdx, uint16_t newRate) {
//	dbgPrint(dbgIdx, newRate);

	struct EnvelopeState *state= getState(this);

	if (dbgIdx == 4) {					// hack (see below)
		uint8_t p= state->sr & 0xf;
		if ((p == 0) && (newRate > p)) state->rUpped = 1;
	}
		
	// these songs depend on the "un-hacked" logic below:
	//            Confusion_2015_Remix.sid: updates threshold then switches to lower threshold via GATE
	//            Trick'n'Treat.sid: horrible "shhhht-shhht" sounds when wrong/hacked handling..
	//            K9_V_Orange_Main_Sequence.sid: creates "blips" when wrong/hacked handling
	//            $11_Heaven.sid: creates audible clicks whem wrong/hacked handling
	//            Eskimonika.sid: good test for false potitives
	//            Monofail.sid
	//            Blade_Runner_Main_Titles_2SID.sid
	// for some reason this one requires hack - which conflicts with the above:
	//            Move_Me_Like_A_Movie.sid: creates "false positives" that lead to muted voices
	
		
	uint16_t oldThreshold= getCurrentThreshold();
	uint16_t newThreshold= sCounterPeriod[newRate];		// XXX problem this is rounded to 22cycle-steps?
	
	// probably a useless overkill to track the overflow: 
	double e= ((double)cpuCycles() - state->lastCycles)/sCyclesPerSample + state->overflow;
	uint32_t elapsed = floor(e);
	state->overflow= e-elapsed;
	
	// simulate what happend since this method was last called...
	uint16_t simLSFR = state->simLFSR;		// last state used by SID emu (that was cpuCycles() ago)
	if (simLSFR < oldThreshold) {
		// might have reached the threshold during elapsed time
		simLSFR= (simLSFR + elapsed) % oldThreshold;
	} else {
		// already in overflow.. so let it do the full circle
		simLSFR= (simLSFR + elapsed) % sLimitLFSR;
	}			

	if (oldThreshold > newThreshold ) {	// only a reduction may lead to an overflow
		// note: there seems to be some precision issue.. when using newThreshold+2 then the long mute sections in 
		// Move_Me_Like_A_Movie.sid no longer occur - however there then are beeps in K9_V_Orange_Main_Sequence.sid
		// but newThreshold+1 fixes Gloomy.sid but causes blips in HiFiSky.sid
	
	
		// FIXME PROBLEM: below logic leads to false positive in Macrocosm.sid and is responsible for the missing 
		// "base instrument" in voice 3 (20secs ff) .. for some reason the simLSFR seems to be so far off
		// that even a newThreshold+3 still does NOT lead to good results:
		
		if (simLSFR >= newThreshold) {
			// by setting currentLFSR to something equal or higher than newThreshold it is forced into "overflow territory"):
			
			// try to trigger bug for correct "overall duration" (see problem 2): when set to newThreshold then 
			// the maximum of sLimitLFSR steps would be needed to get out of the bug, any higher value will still 
			// tigger the bug but reduce the steps needed to get out of it..

			if (dbgIdx == 0) {	// hack 
				// note: the same R=0 -> R=x -> A(GATE)=0 sequence is used by many songs - which "all" seem
				// to work fine with the regular impl (many actually seem to depend on it) - but for some reason
				// it doesn't work in Move_Me_Like_A_Movie.sid (the currently used test is fragile.. and conflicts 
				// with other yet to be discovered examples are to be expected).. 
				// todo: find root cause - maybe some lack of precision issue (see ~22 cycle step width)?	
				uint16_t diff= simLSFR - newThreshold;
				
				// supposing this sequence is meant to ensure direct start of note (8 is hand-tuned.. lower causes 
				// bleeps in K9_V_Orange_Main_Sequence)
				if (state->rUpped && (newRate == 0) && ((state->sr & 0xf) >= 8) && (diff < 3)) {	// Trick'n'Treat: (diff < 3) 
					// the hack is still wrongly used in Blade_Runner_Main_Titles_2SID but the effect seems neglegible
					
					state->currentLFSR= 0;	// Move_Me_Like_A_Movie.sid specific hack
				} else {
					state->currentLFSR= simLSFR;
				}
				state->rUpped= 0;
			} else {
				state->currentLFSR= simLSFR;	// this should be the correctly reduced bug time: newThreshold+(simLSFR-newThreshold)
			}
		}		
	} else {
		// some earlier setting might already have wrongly setup the bug-mode
		if (simLSFR < newThreshold ) {	// false alarm.. restore
			// flawed impl: the newThreshold might still be lower than what the SID emu
			// had last been using. Just continuing with the origLFSR may (wrongly) result 
			// in bug-mode - depending on what the timing of the update actually was.
			state->currentLFSR= state->origLFSR;	
		}
	}
	
	// update base (in case there are more updates later)
	state->lastCycles= cpuCycles();	
	state->simLFSR = simLSFR;
}