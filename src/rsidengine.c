/*
 * This is the engine driving the emulation.
 * 
 * <p>Tiny'R'Sid (c) 2015 Jürgen Wothke
 * <p>version 0.81
 * 
 * <p>This is NOT a full fledged/cycle correct C64 emulator. The current implementation 
 * has grown on a "need to have" basis: e.g. adding CIA based timing, adding NMI handling, 
 * adding main prog handling, etc.
 * 
 * <p>Consequently the emulator design is not as clean as it might be: the interleaving 
 * of certain processing steps is not completely accurate (e.g. processing of main prog 
 * between interrupts), timers are not simulated on a cycle by cycle basis. Instead this
 * emulator tries to predict the behavior of a "well behaved" program.
 *
 * <p>But since most songs actually work and I was actually most interrested to run 
 * the "good old" stuff (Galway, Hubbard, etc) I am not concerned enough about these 
 * limitations...
 *
 * <p>Known Limitations: Songs that use busy-waiting schemes (e.g. on $d41b, CIA underflows 
 * or specific counter values, D012 raster positions) most likely will not work properly. 
 * (e.g. Monster_Museum.sid, Thud_Ridge.sid, Wings_of_Fury.sid) 
 * The same holds for songs that simultaneously use VIC and CIA to trigger their IRQs, or
 * songs that use endless loops in their IRQ or NMI handlers, etc (e.g. Mahoney's or Swallow's
 * most recent sample playback tricks will usually not work). Also the emu does not cope with 
 * C64 disk drives and songs that "stream" data off the disk will also not work (e.g 
 * Wonderland XIII - but that shouldn't come as a surprise in the .sid file context). Dual-SID,
 * trippel-SID, quadruppel-SID, etc configurations have not been implemented (see .sid files
 * with version > 2). 
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 */

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "rsidengine.h"

#include "env.h"
#include "memory.h"
#include "cpu.h"
#include "vic.h"
#include "cia.h"
#include "sid.h"
#include "digi.h"
#include "hacks.h"

#ifdef DEBUG
#include <emscripten.h>
#endif

enum TimerType { RASTER_TIMER = 0, CIA1_TIMER = 1, NO_IRQ = 2};

// next interrupt not on this screen; value bigger 
// than any 16-bit counter for easy comparison
const uint32_t NO_INT= 0x1ffffff;			// aka 33554431

// if 'init' takes longer than 2 secs then something is wrong (mb in endless loop)
#define CYCLELIMIT 2000000

static uint8_t _mainLoopOnlyMode= 0;
static uint8_t _mainProgStatus= 0; 		// 0: completed 1: interrupted by cycleLimit

static uint32_t _irqTimeout= 0;		// will be reset anyway

/*
* snapshot of c64 memory right after loading.. 
* it is restored before playing a new track..
*/
static uint8_t _memorySnapshot[MEMORY_SIZE];

static void setIrqLimit(uint8_t numberOfFrames) {
	_irqTimeout= envCyclesPerScreen()*numberOfFrames;		// in cpu cycles
} 

static uint16_t getNmiVector() {
	// 0318/19 vectors will always be called via the fffa/b-> fe43 indirection
	uint16_t nmi_addr= (memGet(0xfffa)|(memGet(0xfffb)<<8));
	return nmi_addr;
}

static uint8_t isPsidDummyIrqVector() {
	// PSID use of 0314/0315 vector which is technically not setup correctly (e.g. Ode_to_Galway_Remix.sid):
	// PSIDs may actually set the 0314/15 vector via INIT, turn off the kernal ROM and 
	// not provide a useful fffe/f vector.. and we need to handle that crap..
	
	if ((envSidPlayAddr() != 0) || (((memGet(0xfffe)|(memGet(0xffff)<<8)) == 0) && envIsPSID() &&
			((memGet(0x0314)|(memGet(0x0315)<<8)) != 0))) {
		
		return 1;
	}
	return 0;
}

static uint16_t getIrqVector() {
	if (envSidPlayAddr() != 0) {
		// no point going through the standard interrupt routines with this PSID crap.. we
		// cannot tell if it behaves like a 0314/15 or like a fffe/f routine anyway... (and the stack will likely be messed up)
		return envSidPlayAddr();	
	} 

	uint16_t irqAddr= (memGet(0xfffe)|(memGet(0xffff)<<8));	

	if ((irqAddr == 0) && envIsPSID()) {	// see isPsidDummyIrqVector()
		irqAddr= (memGet(0x0314)|(memGet(0x0315)<<8));		
	}
	return irqAddr;
}

static void initCycleCount(uint32_t intMask, uint32_t basePos) {
	// interrupt sequence takes 7 additional cycles (FIXME why not used for IRQ?)
	uint32_t relOffset= (intMask == NMI_OFFSET_MASK) ? 7 : 0;

	// reminder: cycle time is here initialized relative to some basePos (i.e. sample recordings
	// made on this base must be transformed to "frame-time" via digiTagOrigin!)
	// FIXME: cleanup impl to always directly use "frame-time" - removing this unnecessary 
	//        source of confusion!
	cpuResetCycles(relOffset);

	digiBaseOffset(intMask == IRQ_OFFSET_MASK ? basePos : 0);	// HACK e.g. Axel_F.sid
	
	if (intMask != NMI_OFFSET_MASK) {	// testcase for MAIN usage Crush.sid
		// XXX disabling this for NMI may have introduced side-effects..
		
		// try to use approximate start pos, e.g. for MAIN sections.. might be usefull for songs like Boot_Zak_v2
		vicStartRasterSim(basePos + relOffset);		// FIXME: this seems to be rather flawed..
	}
}

static uint8_t _volUpdates; 		// number of times the "volume" has been updated in this frame (from anywhere BUT the NMI)

static void setupVolumeHack() {
	_volUpdates= 0; // detect updates from main/IRQ.. block NMI writes to volume
}

static uint32_t processInterrupt(uint32_t intMask, uint16_t npc, uint32_t startTime, int32_t cycleLimit) {
	// known limitation: if the code uses ROM routines calculation is flawed (see mock-up ROM routines).
	uint16_t originalDigiCount= digiGetCount();
	uint16_t originalDigiOverflowCount= digiGetOverflowCount();
	
	sidResetVolumeChangeCount();
	
	if (isPsidDummyIrqVector()) {
		// no point in trying to keep the stack consistent for a PSID
		cpuRegReset();
	}

	cpuResetToIrq(npc);
	cpuSetProgramMode(intMask);
	
	initCycleCount(intMask, startTime);
	
	if (intMask == IRQ_OFFSET_MASK)	{
		vicSyncRasterIRQ();
		
		// FIXME the cpu interrupt flag should also be set before starting the interrupt handler!
		// but since no one seems to have missed it yet..
	}
	
    while (cpuPcIsValid() && ((cycleLimit <0) || (cpuCycles() <cycleLimit))) {
		vicSimRasterline();
        cpuParse();
	}
	cpuSetProgramMode(MAIN_OFFSET_MASK);
	
	digiTagOrigin(intMask, startTime, originalDigiCount, originalDigiOverflowCount);		
	
	if (intMask == IRQ_OFFSET_MASK) _volUpdates+= sidGetNumberOfVolumeChanges();	// number of updates to the volume	
	
	return cpuCycles();
}

static int8_t isCiaActive(uint8_t ciaIdx) {
	return ((cpuIrqFlag() == 0) && ciaIsActive(ciaIdx)) || envIsTimerDrivenPSID();
}

static enum TimerType getIrqTimerMode(uint8_t ciaIdx) {	
	if (envIsPSID() == 1) {		
		return vicIsIrqActive() ? RASTER_TIMER : CIA1_TIMER;
	} else {	
		// todo: the below impl neglects that both raster and cia1 interrupts may be active at the same
		// time. the strategy to give priority to raster (and then ignore the cia1) works for some of
		// the affected songs (see Face_It.sid) but it is not correct

		if (vicIsIrqActive()) {
			return RASTER_TIMER;
		} else if (isCiaActive(ciaIdx)) {
			return CIA1_TIMER;
		} else {
			return NO_IRQ;
		}
	}
}

/*
* Moves to the next interrupt event (if any) which will occur within the specific
* window of time.
* 
* <p>The function emulates VIC & CIA interrupts. Simulates a fast forward to the next 
* interrupt or the end of the timeLimit.
*
* @return NO_INT: timeLimit was reached / [positive number] wait time in cycles until the interrupt occurs
*/
static uint32_t forwardToNextInterrupt(enum TimerType timerType, uint8_t ciaIdx, uint32_t timeLimit) {
	// timing based on CIA1 timer (e.g. Wizball) or timing based on raster timer (e.g. Arkanoid subsongs 3-8)

	if (timerType == NO_IRQ) {
		return NO_INT;
	} else if (timerType == RASTER_TIMER) {
		return vicForwardToNextRaster();
	} else {
		return ciaForwardToNextInterrupt(ciaIdx, timeLimit);
	}
}

static void renderSynth(int16_t *synthBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall, uint32_t fromTime, uint32_t toTime, int16_t **synthTraceBufs){
	if (fromTime < cyclesPerScreen) {
		if (toTime > cyclesPerScreen) toTime= cyclesPerScreen;
		
		float scale= (float)samplesPerCall/cyclesPerScreen;
		uint16_t len= (toTime - fromTime)*scale;
		if(len) {
			uint16_t startIdx= fromTime*scale;
			sidSynthRender(&synthBuffer[startIdx], len+1, synthTraceBufs);
		}
	}
}
// precondition: currentIrqTimer and currentNmiTimer MUST NOT be both equal NO_INT!
static uint8_t isIrqNext(uint32_t currentIrqTimer, uint32_t currentNmiTimer, uint32_t tIrq, uint32_t tNmi) {
	uint8_t irqIsNext;
	if(currentNmiTimer == NO_INT) {
		irqIsNext= 1;	// NMI no longer relevant
	} else {
		if (currentIrqTimer == NO_INT) {
			irqIsNext= 0;
		} else{
			if (tNmi < tIrq) {
				irqIsNext= 0;
			} else {
				irqIsNext= 1;
			}
		}			
	}
	return irqIsNext;
}

/*
 * handle a potentially endless running main program (e.g. started from "init_addr").
 * @param npc 			0 means that interrupted main program is continued (whereever it was)
 * @param cycleLimit	limit the number of available CPU cycles before the processing stops
 * @return 				0= run to completion; 1= interrupted by cyclelimit or due to number of produced digi samples
 */
static uint8_t callMain(uint16_t npc, uint8_t na, uint32_t startTime, int32_t cycleLimit) {

	/*
		the way how the sequence of main and INT calls is scheduled is obviously incorrect, and
		the logic below tries to at least workaround (which does not always work) some the flaws.
		the problems arise when:
				
		scenario 1: main program is interrupted by some INT and when it later resumes it expects to
                    continue with the register content it had at the time of the interrupt (why
					is special handling needed here: any well behaved INT would perform that cleanup
					anyway.. but various songs do seem to benefit from a respective hack.. see 
					comment in cpuRegSave)	

		scenario 2: supposing a chain of INTs does not expect any interference from MAIN and registers
					are just passed on from one INT call to the next (see Jevers_Bannys_and_the_Master_Mixers.sid):
					a main call should then under no circumstances mess with the registers... (caution: doing
					too much here will harm songs like: Storebror.sid, Wonderland XII part1.sid, Comalight 13 tune4.sid)
	*/
	
	initCycleCount(0, startTime); // use new timestamps for potential digi-samples & cycleLimit check

	if (npc == 0) cpuRegSave(1, 1);

	if (npc == 0) {
		cpuRegRestore(0,0);
	} else {
		cpuReset(npc, na);	// sid init call: with 'start addr' and 'seleced track' in acc
	}
    while (cpuPcIsValid()) {
		// if a main progs is already producing samples then it is done with the "init" phase and
		// we interrupt it when we have enough samples for one screen (see Suicide_Express.sid)

		if (((cycleLimit >0) && (cpuCycles() >=cycleLimit)) || (digiGetOverflowCount() >0)) {	
			cpuRegSave(0,0);
			
			if (npc == 0) cpuRegRestore(1, 1);
			return 1;
		}		
		vicSimRasterline();
		cpuParse();
	}
	
	if (npc == 0) cpuRegRestore(1, 1);
	return 0;
}

static void processMain(uint32_t cyclesPerScreen, uint32_t startTime, int32_t timeout) {
	// the current impl may cause the interrupt routines to burn more cycles than would be normally possible (e.g. if
	// they use busy-wait for some condition which does not materialize in our emulator env. Consequently our 
	// "timeout" may be completely off the target...

	if ((_mainProgStatus >0) && (timeout >0)) {		// might be the rest of some "init" logic... or some loop playing samples		
		sidResetVolumeChangeCount();					
	
		uint16_t originalDigiCount= digiGetCount();
		uint16_t originalDigiOverflowCount= digiGetOverflowCount();
		_mainProgStatus= callMain(0, 0, startTime, timeout);	// continue where interrupted
		digiTagOrigin(MAIN_OFFSET_MASK, startTime, originalDigiCount, originalDigiOverflowCount);

		_volUpdates+= sidGetNumberOfVolumeChanges();	// number of updates to the volume
	}
}

#ifdef DBG_TRACE_ADSR
uint16_t _frameCount;
#endif

static void runScreenSimulation(int16_t *synthBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall, int16_t **synthTraceBufs) {
	setupVolumeHack();

	// cpu cycles used during processing
	int32_t irqCycles= 0, nmiCycles= 0, mainProgCycles= 0;	// only mainProgCycles can become negative..

	// reminder: timings of IRQ and NMI are calculated as if these where independent (which is
	// of course flawed). A proper premptive scheduling is not simulated.

	int32_t availableIrqCycles= cyclesPerScreen;	// cpu cycles still available for processing (limitation: only the waiting time is currently considered..)
	int32_t availableNmiCycles= cyclesPerScreen;

	uint32_t synthPos= 0;	// start time for the samples synthesized from SID settings (measured in cycles)						

	/*
	* process NMI and IRQ interrupts in their order of appearance - and as long as they fit into this screen
	*/	
	uint32_t currentIrqTimer= forwardToNextInterrupt(getIrqTimerMode(CIA1), CIA1, availableIrqCycles);
	
	// FIXME: the assumption that only RSID uses NMI meanwhile proved to be wrong (see MicroProse_Soccer_V1.sid 
	// tracks >4) .. also an IRQ run may also update the NMI schedule! flawed: ciaForwardToNextInterrupt 
	// calculation does NOT consider the time when the timer is actually started but only the time that some 
	// interrupt returns (which will lead to distortions)
	
	uint32_t currentNmiTimer= envIsRSID() ? ciaForwardToNextInterrupt(CIA2, availableNmiCycles) : NO_INT;	
	
	// KNOWN LIMITATION: ideally all 4 timers should be kept in sync - not just A/B within the same unit! 
	// however progs that actually rely on multiple timers (like Vicious_SID_2-Carmina_Burana.sid) probably 
	// need timer info that is continuously updated - so that effort would only make sense in a full fledged 
	// cycle-by-cycle emulator.
	
	uint8_t hack= vicIsIrqActive() && isCiaActive(CIA1);
	
	uint32_t tNmi= currentNmiTimer;		// NMI simulation progress in cycles
	uint32_t tIrq= currentIrqTimer;		// IRQ simulation progress in cycles
	uint32_t tDone= 0;
	
	uint16_t mainProgStep= 100;	// periodically give the main prog a chance to run.. 
	uint16_t mainProgStart= mainProgStep;

	if (0 || (envIsRSID() && (currentIrqTimer == NO_INT) && (currentNmiTimer == NO_INT))) {
		_mainLoopOnlyMode= 1;
		mainProgStart= 0; 	// this might be the better choice in any case.. but to avoid regression testing lets use it here for now..
	} else {
		_mainLoopOnlyMode= 0;

		// Wonderland_XII-Digi_part_4 has high interrupt rate..
		#define ABORT_FUSE 400		// better safe than sorry.. no need to crash the player with an endless loop

		uint16_t fuse;
		for (fuse= 0; (fuse<ABORT_FUSE) && ((currentIrqTimer != NO_INT) || (currentNmiTimer != NO_INT)) ; fuse++) {
			// periodically give unused cycles to main program (needed, e.g. by THCM's stuff)	
			
			if (tDone > mainProgStart) {
				// todo: the handing of main prog cycles is rather flawed (timing for main code that would normally run before 
				// 'mainProgStep'). In an alternative "improved" impl I ran "main" directly before "processInterrupt" calls below - letting 
				// "main" use all cycles unused up to that moment. also I had added interruption for "LDA#00; BEQ .." based endless main-loop 
				// logic (which would require IRQ logic to break out) to avoid unnecessary cycle usage.. while those changes work fine for 
				// THCM's recent stuff they broke stuff like "Arkanoid", etc (so I rolled them back for now..) 
				// -> now that the D012 handling has been improved I might give it another try..
				
				int32_t availableMainCycles= tDone - (nmiCycles + irqCycles + mainProgCycles);
				
				if (availableMainCycles > 0) {
					processMain(cyclesPerScreen, mainProgStart, availableMainCycles);

					mainProgCycles+= availableMainCycles;				
				}			
				mainProgStart= tDone + mainProgStep;
			}
			
			// FIXME: better allow NMI to interrupt IRQ, i.e. set respective cycle-limit
			// accordingly - and then resume the IRQ later. see Ferrari_Formula_One.sid
			
			if (!isIrqNext(currentIrqTimer, currentNmiTimer, tIrq, tNmi)) {		// handle next NMI
				tDone= tNmi;
				// problem: songs like Arcade_Classics re-set the d418 volume nowhere else but in their
				// NMI digi-player: when only used for the short intervals between NMIs the respective changing
				// settings do not seem to pose any problems for the rendering of the regular SID output 
				// (or are even intentionally creating some kind of tremolo) but when SID output rendering 
				// is performed for longer intervals (e.g. like is done here) then "wrong" settings are also 
				// used for these longer intervals which may cause audible clicking/pauses in the output.

				// if the handled NMIs were preemtively scheduled/properly timed, then renderSynth() could 
				// be performed also for these shorter intervals.. but unfortunately they are not and respective 
				// rendering cannot be done here, i.e. the IRQ based rendering then has no clue regarding the 
				// validity/duration of respectice NMI-volume infos..
				
				nmiCycles+= processInterrupt(NMI_OFFSET_MASK, getNmiVector(), tNmi, cyclesPerScreen);
							
				availableNmiCycles-= currentNmiTimer;		

				if (availableNmiCycles <= 0) {
					availableNmiCycles= 0;		// see unsigned use below
				}
				currentNmiTimer= ciaForwardToNextInterrupt(CIA2, availableNmiCycles);
				
				tNmi+= currentNmiTimer;		// will be garbage on last run..		
			} else {															// handle next IRQ
				tDone= tIrq;
				renderSynth(synthBuffer, cyclesPerScreen, samplesPerCall, synthPos, tIrq, synthTraceBufs);
				synthPos= tIrq;
				
				if (hack) {
					// in case CIA1 has also been configured to trigger IRQs, our current "raster IRQ or timer IRQ" impl 
					// is obviously flawed.. this hack will fix simple scenarios like Cycles.sid
					ciaSignalUnderflow(CIA1, TIMER_A); 
				}
				
				// FIXME: multi-frame IRQ handling would be necessary to deal with songs like: Musik_Run_Stop.sid	
				uint32_t usedCycles= processInterrupt(IRQ_OFFSET_MASK, getIrqVector(), tIrq, _irqTimeout);
				
				// flaw: IRQ might have switched off NMI, e.g. Vicious_SID_2-Blood_Money.sid
				if (usedCycles >=_irqTimeout) { // IRQ gets aborted due to a timeout
					if (cpuIrqFlag()) {
						// this IRQ handler does not want to be interrupted.. (either the code got stuck due to some impossible
						// condition - e.g. waiting for a timer which we don't update - or it is an endless IRQ routine that needs to 
						// continue..)						
					} else {
						// this IRQ handler was intentionally waiting to get interrupted - and it would normally not have burned
						// the same amount of cycles (we have no chance to get the timing right here..)
						usedCycles= 1;
					}					
				}
				
				irqCycles+= usedCycles;
				availableIrqCycles-= currentIrqTimer;
			
				if (availableIrqCycles <= 0) {
					availableIrqCycles= 0;		// see unsigned use below
				}
				currentIrqTimer= forwardToNextInterrupt(getIrqTimerMode(CIA1), CIA1, availableIrqCycles);
							
				tIrq+= currentIrqTimer;		// will be garbage on last run..	
			}
		}

#ifdef DEBUG		
		if (fuse >= ABORT_FUSE) {
			EM_ASM_({ console.log('fuse blown: ' + $0); }, fuse);
			EM_ASM_({ console.log('last IRQ timer: ' + $0); }, currentIrqTimer);
			EM_ASM_({ console.log('last NMI timer: ' + $0); }, currentNmiTimer);

			ciaPrintDebug();
		}
#endif
		renderSynth(synthBuffer, cyclesPerScreen, samplesPerCall, synthPos, cyclesPerScreen, synthTraceBufs);	// fill remainder of buffer	
	}
	
	
	mainProgCycles= cyclesPerScreen - (nmiCycles + irqCycles + mainProgCycles);

	if (mainProgCycles >0) {
		processMain(cyclesPerScreen, mainProgStart, mainProgCycles);
		
		if (_mainLoopOnlyMode && envIsRSID()) {
			// e.g. Dane's "Crush.sid"
			renderSynth(synthBuffer, cyclesPerScreen, samplesPerCall, synthPos, cyclesPerScreen, synthTraceBufs);
		}
	}
	
	// hack (e.g. Arcade_Classics): when IRQ updates the volume (but not using it to play
	// samples then volume changes originating from NMI should be ignored (with the current design of
	// rendering SID output for longer time windows these volume settings otherwise mess up the output)
 	sidDisableVolumeChangeNMI(_volUpdates && (_volUpdates < 3));
}

/*
* @return 		1: if digi data available   0: if not
*/
uint8_t rsidProcessOneScreen(int16_t *synthBuffer, uint8_t *digiBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall, int16_t **synthTraceBufs) {	
#ifdef DBG_TRACE_ADSR
	fprintf(stderr, "frame %d\n", _frameCount++);
#endif
	digiFetchOverflowData();
		
	initCycleCount(0, 0);

	ciaUpdateTOD(envCurrentSongSpeed());
	
	if (!envSidPlayAddr()) {
		envSetPsidMode(0);		// FIXME confusing mode change should be avoided (if at all then rather in rsidPlayTrack() than here..)
	}

	if (envIsPSID()) memResetPsidBanks(1, getIrqVector());	// must be reset before every "play" - see crap like 8-Bit_Keys_Theme.sid
	
	
	// note: The D019 Raster IRQ status bit is set WHENEVER the configured (D011/D012) 
	// raster line is reached (also while RASTER IRQs are disabled!).. this emulator does 
	// NOT correctly track the raster position while RASTER IRQ is disabled but some songs (PSIDs and RSIDs)
	// will still check the flag to make sure that it is a new frame.. (see Come_What_May.sid, Viking.sid).
	// for the benefit of those songs the flag is set for each frame (the timing will be wrong but 
	// it should be good enough for those songs..):
	if ((envIsFilePSID() && !envIsTimerDrivenPSID()) ||
			!vicIsIrqActive()) {
		vicSimIRQ(); 	// see Come_What_May.sid
	}
	
	uint8_t retVal;
	
	// caution: envIsRSID() at this point also includes certain PSIDs that 
	// have been deemed fit for use of RSID emulation.. 
	if (envIsTimerDrivenPSID() || vicIsIrqActive() || envIsRSID()) {
		// info failed idea: the cycleLimit used below has the effect that
		// a screen uses AT LEAST that many cycles (e.g. Axel_F.sid) and it 
		// might be "more realistic" if a respective overflow would be 
		// substracted on the following screen. however a test with a 
		// respective feature showed that some songs (e.g. Cycles.sid, 
		// Wonderland XII part1.sid) fail miserably when it is used..
				
		runScreenSimulation(synthBuffer, cyclesPerScreen, samplesPerCall, synthTraceBufs);
		
//		retVal= digiRenderSamples(digiBuffer, cyclesPerScreen, samplesPerCall);	// FIXME this change might break stuff
		retVal= digiRenderSamples(digiBuffer, round(((double)envClockRate())/envFPS()), samplesPerCall);
	} else {
		// legacy PSID mode: one "IRQ" call per screen refresh (a PSID may actually setup CIA 1 
		// timers but without wanting to use them - e.g. ZigZag.sid track2). note: most files from the
		// HVSC actually fall into this mode...
		
		// might not be a good idea to give those garbage PSIDs unlimited cycles (see main-loop players
		// disguised as PSID - which would otherwise hang the player/browser)! though some songs do seem to use 
		// more that one frame, e.g. A-Maze-Ing.sid
		int32_t cycleLimit= cyclesPerScreen*3;		// was originally -1
		uint32_t c= processInterrupt(IRQ_OFFSET_MASK, getIrqVector(), 0, cycleLimit);
		
		sidSynthRender(synthBuffer, samplesPerCall, synthTraceBufs);	
		digiClearBuffer();		
		
		retVal= 0;
	}

//	cpuReSyncTotalCycles(envClockRate()/envFPS());	
	
	sidSnapshotAdsrState();	// ADSR-delay bug handling
	return retVal;
}

void rsidReset(uint32_t sampleRate, uint8_t compatibility)
{		
	cpuInit();	
	cpuSetProgramMode(MAIN_OFFSET_MASK);
	_mainLoopOnlyMode= 0;
	
	// hack: some IRQ players don't finish within one screen, e.g. A-Maze-Ing.sid and Axel_F.sid 
	// (Sean Connolly) even seems to play digis during multi-screen IRQs (so give them more time)
	uint8_t overflowFrames= 4;
	
	setIrqLimit(overflowFrames);
	
    memResetIO();	// reset IO area

	ciaReset(envCyclesPerScreen(), envIsRSID(), NO_INT);
		
	vicReset(envIsRSID(), NO_INT);
	
	sidReset(sampleRate, envSIDAddresses(), envSID6581s(), compatibility, overflowFrames, 1);
	
#ifdef DBG_TRACE_ADSR
	_frameCount= 0;
#endif	
}

void rsidLoadSongBinary(uint8_t *src, uint16_t destAddr, uint32_t len) {
	memCopyToRAM(src, destAddr, len);

	// backup initial state for use in 'track change'	
	memCopyFromRAM(_memorySnapshot, 0, MEMORY_SIZE);	
}

void rsidPlayTrack(uint32_t sampleRate, uint8_t compatibility, uint16_t *pInitAddr, uint16_t loadEndAddr, 
					uint16_t playAddr, uint8_t actualSubsong) {
	
	rsidReset(sampleRate, compatibility);
	
	// restore original mem image.. previous "initAddr" run may have corrupted the state
	memCopyToRAM(_memorySnapshot, 0, MEMORY_SIZE);

	hackIfNeeded(pInitAddr);	
	
	memSetDefaultBanks(envIsRSID(), (*pInitAddr), loadEndAddr);	// PSID crap
	
	// if initAddr call does not complete then it is likely in an endless loop / maybe digi player
	// FIXME use CYCLELIMIT only for PSID: unfortunately RSIDs like Wonderland_XII-Digi_part_1.sid still 
	// need some kind of startup phase
	_mainProgStatus= callMain((*pInitAddr), actualSubsong, 0, !envIsRSID()? CYCLELIMIT : 200000);		

	ciaResetPsid60Hz();
	
	/*
	FIXME: for the below to work it would seem to be the "right thing" to fetch whatever 
	the PSID INIT setup.. but since this same call is repeated in each PLAY, the below memResetPsidBanks
	call actually seems to be redundant here..
	
	if (!playAddr) {
		playAddr= getIrqVector();	// should have been initialized by now..
	}
	*/
	memResetPsidBanks(envIsPSID(), playAddr);	// PSID again
}

static uint16_t parseSYS(uint16_t start, uint16_t end) {
	// parse a simple "123 SYS3000" BASIC command
	uint16_t result= 0;
	uint8_t c= 0;
	for (uint16_t i= start; i<=end; i++) {
		c= memReadRAM(i);
		if (!c) break;

		if ((c >= 0x30) && (c <= 0x39) ) { 
			result = result*10 + (c-0x30); 
		} else if (result > 0) {
			break;
		}
	}
	return result;
}	

void rsidStartFromBasic(uint16_t *initAddr) {
	// don't have C64 BASIC ROM if BASIC program is just used to 
	// jump to some address (SYS xxxxx) then try to do that for 
	// simple one line programs..

	if ((*initAddr) == 0x0801) {
		uint16_t nextLine= memReadRAM(*initAddr) | ((memReadRAM((*initAddr)+1)) << 8);
		if (!memReadRAM(nextLine)) {
			// one line program	(bad luck if additional REM etc lines are used..)
			if (memReadRAM(((*initAddr) + 4)) == 0x9e) {	// command is SYS
				 (*initAddr) = parseSYS((*initAddr) + 5, nextLine);
			}			
		}
	}
}
