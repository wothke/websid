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

#include "rsidengine.h"

#include "env.h"
#include "memory.h"
#include "cpu.h"
#include "vic.h"
#include "cia.h"
#include "sid.h"
#include "digi.h"
#include "hacks.h"

enum TimerType { RASTER_TIMER = 0, CIA1_TIMER = 1, NO_IRQ = 2};

// next interrupt not on this screen; value bigger 
// than any 16-bit counter for easy comparison
const uint32_t NO_INT= 0x1ffffff;		

// if 'init' takes longer than 2 secs then something is wrong (mb in endless loop)
#define CYCLELIMIT 2000000

static uint8_t _mainLoopOnlyMode= 0;
static uint8_t _mainProgStatus= 0; 		// 0: completed 1: interrupted by cycleLimit

static uint32_t _irqTimeout= 18000;		// will be reset anyway

/*
* snapshot of c64 memory right after loading.. 
* it is restored before playing a new track..
*/
static uint8_t _memorySnapshot[MEMORY_SIZE];


static void setIrqTimeout(uint32_t t) {
	_irqTimeout= t;		// in cpu cycles
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

static void initCycleCount(uint32_t relOffset, uint32_t basePos) {
	cpuResetCycles(relOffset);
	vicStartRasterSim(basePos+ relOffset);
}

static uint32_t processInterrupt(uint32_t intMask, uint16_t npc, uint32_t startTime, int32_t cycleLimit) {
	// known limitation: if the code uses ROM routines (e.g. register restore/return sequence) then we will 
	// be missing those cpu cycles in our calculation..
	uint32_t originalDigiCount= digiGetCount();
	
	if (isPsidDummyIrqVector()) {
		// no point in trying to keep the stack consistent for a PSID
		cpuRegReset();
	}

	cpuResetToIrq(npc);
	cpuSetProgramMode(intMask);
	
	initCycleCount( (intMask == NMI_OFFSET_MASK) ? 7 : 0, startTime);

    while (cpuPcIsValid() && ((cycleLimit <0) || (cpuCycles() <cycleLimit))) {
		vicSimRasterline();
        cpuParse();
	}
		
	cpuSetProgramMode(MAIN_OFFSET_MASK);
	
	digiTagOrigin(intMask, startTime, originalDigiCount);		

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
		uint32_t originalDigiCount= digiGetCount();
		_mainProgStatus= callMain(0, 0, startTime, timeout);	// continue where interrupted
		digiTagOrigin(MAIN_OFFSET_MASK, startTime, originalDigiCount);
	}
}

static void runScreenSimulation(int16_t *synthBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall, int16_t **synthTraceBufs) {	
	// cpu cycles used during processing
	int32_t irqCycles= 0, nmiCycles= 0, mainProgCycles= 0;			

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
	
	uint32_t tNmi= currentNmiTimer;
	uint32_t tIrq= currentIrqTimer;
	uint32_t tDone= 0;
	
	uint16_t mainProgStep= 100;	// periodically give the main prog a chance to run.. 
	uint16_t mainProgStart= mainProgStep;

	if (0 || (envIsRSID() && (currentIrqTimer == NO_INT) && (currentNmiTimer == NO_INT))) {
		_mainLoopOnlyMode= 1;
		mainProgStart= 0; 	// this might be the better choice in any case.. but to avoid regression testing lets use it here for now..
	} else {
		_mainLoopOnlyMode= 0;
		for (;(currentIrqTimer != NO_INT) || (currentNmiTimer != NO_INT) ;) {
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
			
			// FIXME: better allow NMI to interrupt IRQ, i.e. set respectice cycle-limit
			// accordingly - and then resume the IRQ later. see Ferrari_Formula_One.sid
			
			if (!isIrqNext(currentIrqTimer, currentNmiTimer, tIrq, tNmi)) {		// handle next NMI
				tDone= tNmi;
				nmiCycles+= processInterrupt(NMI_OFFSET_MASK, getNmiVector(), tNmi, cyclesPerScreen);
							
				availableNmiCycles-= currentNmiTimer;		
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
			
				currentIrqTimer= forwardToNextInterrupt(getIrqTimerMode(CIA1), CIA1, availableIrqCycles);
								
				tIrq+= currentIrqTimer;		// will be garbage on last run..	
			}
		}
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
}

/*
* @return 		1: if digi data available   0: if not
*/
uint8_t rsidProcessOneScreen(int16_t *synthBuffer, uint8_t *digiBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall, int16_t **synthTraceBufs) {
	digiMoveBuffer2NextFrame();
		
	initCycleCount(0, 0);

	ciaUpdateTOD(envCurrentSongSpeed());
	
	if (!envSidPlayAddr()) {
		envSetPsidMode(0);	
	}

	if (envIsPSID()) memResetPsidBanks(1, getIrqVector());	// must be reset before every "play" - see crap like 8-Bit_Keys_Theme.sid
	
	if (envIsTimerDrivenPSID() || vicIsIrqActive() || envIsRSID()) {

		// info failed idea: the cycleLimit used below has the effect that
		// a screen uses AT LEAST that many cycles (e.g. Axel_F.sid) and it 
		// might be "more realistic" if a respective overflow would be 
		// substracted on the following screen. however a test with a 
		// respective feature showed that some songs (e.g. Cycles.sid, 
		// Wonderland XII part1.sid) fail miserably when it is used..

		runScreenSimulation(synthBuffer, cyclesPerScreen, samplesPerCall, synthTraceBufs);
		return digiRenderSamples(digiBuffer, cyclesPerScreen, samplesPerCall);
	} else {
		// legacy PSID mode: one "IRQ" call per screen refresh (a PSID may actually setup CIA 1 
		// timers but without wanting to use them - e.g. ZigZag.sid track2)
		processInterrupt(IRQ_OFFSET_MASK, getIrqVector(), 0, -1);
		sidSynthRender(synthBuffer, samplesPerCall, synthTraceBufs);	
		digiClearBuffer();		
		return 0;
	}
}

void rsidReset(uint32_t sampleRate, uint8_t compatibility)
{		
	cpuInit();	
	cpuSetProgramMode(MAIN_OFFSET_MASK);
	_mainLoopOnlyMode= 0;
	
	// hack: some IRQ players don't finish within one screen, e.g. A-Maze-Ing.sid and Axel_F.sid 
	// (Sean Connolly) even seems to play digis during multi-screen IRQs (so give them more time)			
	setIrqTimeout(envCyclesPerScreen()*4);
	
    memResetIO();	// reset IO area

	ciaReset(envCyclesPerScreen(), envIsRSID(), NO_INT);
		
	vicReset(envIsRSID(), NO_INT);
	
	sidReset(sampleRate, envSIDAddresses(), envSID6581s(), compatibility, 1);
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
