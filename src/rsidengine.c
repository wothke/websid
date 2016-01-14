/*
 * This file contains additional emulation logic used to play RSID files.
 * 
 * <p>version 0.80
 * <p>Tiny'R'Sid (c) 2015 Jürgen Wothke
 * 
 * <p>This is not a full fledged/cycle correct C64 emulator. The current implementation 
 * has grown on a "need to have" basis: e.g. adding CIA based timing, adding NMI handling, 
 * adding main prog handling, etc.
 * 
 * <p>Consequently the emulator design is not as clean as it might be: the interleaving 
 * of certain processing steps is not completely accurate (e.g. processing of main prog between interrupts),
 * timers are not simulated on a cycle by cycle basis.
 *
 * <p>But since most songs actually work and I was actually most interrested to run the "good old" 
 * stuff (Galway, Hubbard, etc) I am not concerned enough about these limitations...
 *
 * <p>Known Limitations: Songs that use busy-waiting schemes (e.g. on $d41b, CIA underflows or specific counter values,
 * D012 raster positions) most likely will not work properly. (e.g. Monster_Museum.sid, Thud_Ridge.sid, Wings_of_Fury.sid) 
 * The same holds for songs that simultaneously use VIC and CIA to trigger their IRQs.
 *
 *
 * Terms of Use: This software is licensed under a CC BY-NC-SA 
 * (http://creativecommons.org/licenses/by-nc-sa/4.0/).
 *
 * Change log:
 *
 * version 0.8     replaced original envelope generator with new impl that also handles ADSR-bug, fixed bugs in 
 *                 6502 emu (additional ILLIGAL ops, wrong V-flag calculation), refactored digi-sample handling, 
 *                 various cleanups, removed Storebror.sid hack (no longer needed)
 *
 * version 0.77: - refactored & improved nanocia.c to fix issues with some of the songs of Kris Hatlelid; added hack  
 *                 for mixed raster/timer IRQ scenarios; added Lame_Tune.sid hack; DC0D polling hack to make Kai Walter 
 *                 stuff play; added D41B polling hack (e.g. Ring_Ring_Ring.sid), added support for main-loop sid play (e.g. Dane's 'Crush.sid');
 *                 added handling for 60hz songs; removed d012 polling specific hacks and improved d011/d012 handling (see 
 *                 Swallow's stuff); fixed original TinySid filter bug (see Dancing_in_the_Moonshine.sid)
 *
 * version 0.76: - added poor man's combined pulse/triangle waveform impl: for songs like Kentilla.sid; improved IRQ digi 
 *                 filter to fix noise in Coma_Light_13_tune_4.sid; added handling for "pulse width modulation" 
 *                 digis as used by Swallow, Cyberbrain, Danko_Tomas, etc
 *
 * version 0.75: - added poor man's badline cycle handling in sidplayer.c - as a workaround for noise artefacts in THCM's new stuff
 *               - added "new digi" handling for "main" (see Wonderland_XI-End.sid) - also see SEI handling
 *
 * version 0.74: - fixed sizzling noise (e.g. Christoph Bergmann's tunes) due to bug in digi-scaling (see "genDigi()")
 *				   added TOD sim, fixed 'memset' bug, added minimal support for rasterline polling, added 
 *                 NTSC playback speed adjustment, added impl for "Voice #3 ADSR output"
 *
 * version 0.73: - fixed digi sizzling noise caused by main prog, added 'sbx' op for Artefacts.sid
 *
 * version 0.72: - fixed "IO area" handling bug which had corrupted RAM (see Digital_Music.sid problems)
 *				 - added handling for "test bit" driven digis (see Storebror.sid)
 *				 - added hack to give Storebror.sif a "valid" $DC06 counter for use in NMI vector
 *				 - added "enough digi samples in main prog" interrupt condition (see Suicide_Express.sid)
 *				 - fixed wrong stack handling
 *				 - added fake timer to make Delta_Mix-E-Load_loader.sid PSID happy 
 *
 * version 0.71: - added missing RSID $01 handling (fixed IK+)
 *               - added "read" d012 emulation (fix endless loop in Storebror.sid)
 *			 	 - added handling for illegal op SLO
 *				 - improved stack/register handling
 *				 - fixed IRQ init for PSID
 *				 - added filtering for "fake" digi signals
 *				 - added $01 handling for faulty PSIDs
 *				 - reset SID synth before each new song.. see GreenBeret.sid problem
 *
 * version 0.7:  - added poor man's VIC and CIA emulation
 *               - added interleaving of NMI and IRQ calls
 *
 * changes 0.6:  - added full 6510 instruction set timing support
 *               - added NMI and main program based digi-player support
 *
 * changes 0.5:  - initial version with PSID-only support
 *               - merged latest TinySid fixes from RockBox version
 *               - added loadAddress handling for 'non original C64 file format handling'
 *               - fixed handling of 'sPlaySpeed' flags
 *               - patched original 'synth_render' PSID digi-playback for louder digi playback			
 */

#include <string.h>
#include <stdio.h>

#include "defines.h"
#include "digi.h"
#include "nanovic.h"
#include "nanocia.h"
#include "sidengine.h"
#include "rsidengine.h"
#include "sidplayer.h"



enum timertype { RASTER_TIMER = 0, CIA1_TIMER = 1, NO_IRQ = 2};


unsigned char memory[MEMORY_SIZE];
unsigned char kernal_rom[KERNAL_SIZE];	// mapped to $e000-$ffff
unsigned char io_area[IO_AREA_SIZE];	// mapped to $d000-$dfff


static unsigned int sProgramMode= MAIN_OFFSET_MASK;
static unsigned char sMainLoopOnlyMode= 0;

// poor man's TOD sim (only secs & 10th of sec), see Kawasaki_Synthesizer_Demo.sid
unsigned long sTodInMillies= 0;

unsigned char sMainProgStatus= 0; 				// 0: completed 1: interrupted by cycleLimit

unsigned char sIsPSID;

unsigned long sIrqTimeout= 18000;	// will be reset anyway

// snapshot of current registers and stack (so we can ignore IRQ/NMI effects)
static unsigned char sSnapshotAcc, sSnapshotX, sSnapshotY, sSnapshotPFlags, sSnapshotStackpointer;
static unsigned short sSnapshotPC;
static unsigned char sSnapshotStack[0xff];

void setIrqTimeout(unsigned long t) {
	sIrqTimeout= t;		// in cpu cycles
} 


void setProgramStatus(unsigned char s) {
	sMainProgStatus= s;
} 

unsigned long getTimeOfDayMillis() {
	return sTodInMillies;
}

void updateTimeOfDay10thOfSec(unsigned char value) {
	sTodInMillies= ((unsigned long)(sTodInMillies/1000))*1000 + value*100;
}
void updateTimeOfDaySec(unsigned char value) {
	sTodInMillies= value*1000 + (sTodInMillies%1000);	// ignore minutes, etc
}

void memSet(unsigned char *mem, char val, int len) {
	// for some reason 'memset' does not seem to work in Alchemy...
	int i;
	for (i= 0; i<len; i++) {
		mem[i]= val;
	}
}

unsigned char isMainLoopMode() {
	return sMainLoopOnlyMode;
}

unsigned int getProgramMode() {
	return sProgramMode;
}

static void setIO(unsigned int addr, unsigned char value) {
	io_area[addr - 0xd000]= value;
}

static void resetIO(unsigned long cyclesPerScreen, unsigned char isRsid) {
    memSet(&io_area[0], 0x0, IO_AREA_SIZE);

	// Master_Blaster_intro.sid actually checks this:
	io_area[0x0c01]= 0xff;	 	// Port B, keyboard matrix rows and joystick #1

	// CIA 1 defaults	(by default C64 is configured with CIA1 timer / not raster irq)
	setIO(0xdc0d, 0x81);	// interrupt control	(interrupt through timer A)
	setIO(0xdc0e, 0x01); 	// control timer A: start - must already be started (e.g. Phobia, GianaSisters, etc expect it)
	setIO(0xdc0f, 0x08); 	// control timer B (start/stop) means auto-restart
	setIO(0xdc04, cyclesPerScreen&0xff); 	// timer A (1x pro screen refresh)
	setIO(0xdc05, cyclesPerScreen>>8);
	
	if (isRsid) {	
		// by default C64 is configured with CIA1 timer / not raster irq
		setIO(0xd01a, 0x00); 	// raster irq not active
		setIO(0xd011, 0x1B);
		setIO(0xd012, 0x00); 	// raster at line x

		// CIA 2 defaults
		setIO(0xdd0e, 0x08); 	// control timer 2A (start/stop)
		setIO(0xdd0f, 0x08); 	// control timer 2B (start/stop)		
	}

	resetCiaTimer();
	resetVic();
	
	io_area[0x0418]= 0xf;					// turn on full volume	
	sidPoke(0x18, 0xf);  
}

void resetRSID() {
	sProgramMode= MAIN_OFFSET_MASK;
	sMainLoopOnlyMode= 0;
	sTodInMillies= 0;
	
	// some IRQ players don't finish within one screen, e.g. A-Maze-Ing.sid and Axel_F.sid 
	// (Sean Connolly) even seems to play digis during multi-screen IRQs (so give them more time)			
	setIrqTimeout(getCyclesPerScreen()*4);
	
	// reset IO area
    resetIO(getCyclesPerScreen(), isRsid());	
}

unsigned char isRsid() {
	return (sIsPSID == 0);
}

void setPsidMode(unsigned char m) {
	sIsPSID= m;
}

unsigned char isPsid() {
	return sIsPSID;
}

unsigned int getNmiVector() {
	// 0318/19 vectors will always be called via the fffa/b-> fe43 indirection
	unsigned int nmi_addr= (getmem(0xfffa)|(getmem(0xfffb)<<8));
	return nmi_addr;
}

unsigned char isPsidDummyIrqVector() {
	// PSID use of 0314/0315 vector which is technically not setup correctly (e.g. Ode_to_Galway_Remix.sid):
	// PSIDs may actually set the 0314/15 vector via INIT, turn off the kernal ROM and 
	// not provide a useful fffe/f vector.. and we need to handle that crap..
	
	if ((getSidPlayAddr() != 0) || (((getmem(0xfffe)|(getmem(0xffff)<<8)) == 0) && sIsPSID &&
			((getmem(0x0314)|(getmem(0x0315)<<8)) != 0))) {
		
		return 1;
	}
	return 0;
}

unsigned int getIrqVector() {
	if (getSidPlayAddr() != 0) {
		// no point going through the standard interrupt routines with this PSID crap.. we
		// cannot tell if it behaves like a 0314/15 or like a fffe/f routine anyway... (and the stack will likely be messed up)
		return getSidPlayAddr();	
	} 

	unsigned int irq_addr= (getmem(0xfffe)|(getmem(0xffff)<<8));	

	if ((irq_addr == 0) && sIsPSID) {	// see isPsidDummyIrqVector()
		irq_addr= (getmem(0x0314)|(getmem(0x0315)<<8));		
	}
	return irq_addr;

}

void initCycleCount(unsigned long relOffset, unsigned long basePos) {
	sCycles= relOffset;
	initRasterlineSim(basePos+ relOffset);
}

unsigned long processInterrupt(unsigned long intMask, unsigned short npc, unsigned long startTime, signed long cycleLimit) {
	// known limitation: if the code uses ROM routines (e.g. register restore/return sequence) then we will 
	// be missing those cpu cycles in our calculation..
	
	unsigned long originalDigiCount= getDigiCount();
	
	if (isPsidDummyIrqVector()) {
		// no point in trying to keep the stack consistent for a PSID
		cpuReset();
	}

	// provide dummy return address - which we use to return from the emulation:
	// in case of RTI (e.g. progs implementing $fffe/f vector directly) this will be used "as is".
	// if some program was to return with "RTS" (e.g. legacy PSID) the address would be returned as $0001.
	
	push(0);	// addr high
	push(0);	// addr low
	push(p);	// processor status (processor would do this in case of interrupt...)	

	
	// only set pc and keep current stackpointer and registers in case player directly passes 
	// them between calls (see Look_sharp.sid)
	pc= npc;
		
	sProgramMode= intMask;
	
	initCycleCount( (intMask == NMI_OFFSET_MASK) ? 7 : 0, startTime);

    while (pc > 1 && ((cycleLimit <0) || (sCycles <cycleLimit)))
        cpuParse();
		
	sProgramMode= MAIN_OFFSET_MASK;
	
	markSampleOrigin(intMask, startTime, originalDigiCount);		

	return sCycles;
}

static void updateTOD() {
	sTodInMillies+= (getCurrentSongSpeed() ? 17 : 20);	
}

int isTimerDrivenPsid() {
	return ((sIsPSID == 1) && (getCurrentSongSpeed() == 1));
}

int isRasterDrivenPsid() {
	return ((sIsPSID == 1) && (getCurrentSongSpeed() == 0));
}

int isCiaActive(struct timer *t) {
	return (!isIrqBlocked() && isTimerActive(t)) || isTimerDrivenPsid();
}

enum timertype getIrqTimerMode(struct timer *t) {	
	if (sIsPSID == 1) {
		return isRasterIrqActive() ? RASTER_TIMER : CIA1_TIMER;
	} else {	
		// todo: the below impl neglects that both raster and cia1 interrupts may be active at the same
		// time. the strategy to give priority to raster (and then ignore the cia1) works for some of
		// the affected songs (see Face_It.sid) but it is not correct

		if (isRasterIrqActive()) {
			return RASTER_TIMER;
		} else if (isCiaActive(t)) {
			return CIA1_TIMER;
		} else {
			return NO_IRQ;
		}
	}
}

#ifdef DEBUG
char hex [16]= {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
char *debugBuf;
void traceBuffer(char *label, unsigned char *buf, int bufLen) {
	AS3_Trace(AS3_String(label));		

	int len= 50; 		// chars per line
	int o, offset=0; 
	for (o= 0; o<bufLen/len; o++, offset+=len)	{		
		debugBuf= malloc(sizeof(char)*(len*3)+1); 
		
		int t;
		for (t= 0; t<len; t++) {
			debugBuf[t*3]= hex[(buf[t+offset]>>4)];
			debugBuf[(t*3)+1]= hex[(buf[t+offset]&0xf)];
			debugBuf[(t*3)+2]= ' ';
		}
		debugBuf[t*3]= 0;
		AS3_Trace(AS3_String(debugBuf));	
		free(debugBuf);
	}
	if (bufLen%len) {
		debugBuf= malloc(sizeof(char)*(len*3)+1); 
		
		int t;
		for (t= 0; t<bufLen%len; t++) {
			debugBuf[t*3]= hex[(buf[t+offset]>>4)];
			debugBuf[(t*3)+1]= hex[(buf[t+offset]&0xf)];
			debugBuf[(t*3)+2]= ' ';
		}
		debugBuf[t*3]= 0;
		AS3_Trace(AS3_String(debugBuf));
		free(debugBuf);
	}
}
#endif

/*
* Moves to the next interrupt event (if any) which will occur within the specific
* window of time.
* 
* <p>The function emulates VIC & CIA interrupts. Simulates a fast forward to the next 
* interrupt or the end of the timeLimit.
*
* @return NO_INT: timeLimit was reached / [positive number] wait time in cycles until the interrupt occurs
*/
unsigned long forwardToNextInterrupt(enum timertype timerType, struct timer *t, unsigned long timeLimit) {
	// timing based on CIA1 timer (e.g. Wizball) or timing based on raster timer (e.g. Arkanoid subsongs 3-8)

	if (timerType == NO_IRQ) {
		return NO_INT;
	} else if (timerType == RASTER_TIMER) {
		return forwardToNextRasterTimer();
	} else {
		return forwardToNextCiaInterrupt(t, timeLimit);
	}
}

void renderSynth(short *synthBuffer, unsigned long cyclesPerScreen, unsigned int samplesPerCall, unsigned long fromTime, unsigned long toTime){
	if (fromTime < cyclesPerScreen) {
		if (toTime > cyclesPerScreen) toTime= cyclesPerScreen;
		
		float scale= (float)samplesPerCall/cyclesPerScreen;
		unsigned int len= (toTime - fromTime)*scale;
		if(len) {
			unsigned int startIdx= fromTime*scale;
			synth_render(&synthBuffer[startIdx], len+1);
		}
	}
}

int isIrqNext(unsigned long currentIrqTimer, unsigned long currentNmiTimer, unsigned long tIrq, unsigned long tNmi) {
	int irqIsNext;
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

void saveRegisters() {
    sSnapshotAcc = a;
    sSnapshotX = x;
    sSnapshotY = y;
    sSnapshotPFlags = p;
    sSnapshotPC = pc;
	
	// songs like: ParanoiaComplex, Another_Feekzoid_Digi_Tune or Demi-Demo_4 currently need this..
	// todo: check what goes wrong and fix the cause... 
    sSnapshotStackpointer = s;
	memcpy(sSnapshotStack, &memory[0x0100], 0xff);
}

void restoreRegisters() {
    a = sSnapshotAcc;
    x = sSnapshotX;
    y = sSnapshotY;
    p = sSnapshotPFlags;
    pc = sSnapshotPC;
    s = sSnapshotStackpointer;	
	memcpy(&memory[0x0100], sSnapshotStack, 0xff);		
}

/*
 * handle a potentially endless running main program (e.g. started from "init_addr").
 * @param npc 			0 means that interrupted main program is continued (whereever it was)
 * @param cycleLimit	limit the number of available CPU cycles before the processing stops
 * @return 				0= run to completion; 1= interrupted by cyclelimit or due to number of produced digi samples
 */
unsigned char callMain(unsigned short npc, unsigned char na, unsigned long startTime, signed long cycleLimit) {	
	initCycleCount(0, startTime); // use new timestamps for potential digi-samples & cycleLimit check

	if (npc == 0) {
		restoreRegisters();
	} else {
		cpuReset();
		a=na;
		pc=npc;	
		
		push(0);
		push(0);
	}
    while (pc > 1) {
		// if a main progs is already producing samples then it is done with the "init" phase and
		// we interrupt it when we have enough samples for one screen (see Suicide_Express.sid)
		if (((cycleLimit >0) && (sCycles >=cycleLimit)) || (getDigiOverflowCount() >0)) {	
			saveRegisters();			
			return 1;
		}		
        cpuParse();
	}
	return 0;
}

void processMain(unsigned long cyclesPerScreen, unsigned long startTime, signed long timeout) {
	// the current impl may cause the interrupt routines to burn more cycles than would be normally possible (e.g. if
	// they use busy-wait for some condition which does not materialize in our emulator env. Consequently our 
	// "timeout" may be completely off the target...

	if ((sMainProgStatus >0) && (timeout >0)) {		// might be the rest of some "init" logic... or some loop playing samples		
		unsigned long originalDigiCount= getDigiCount();
		sMainProgStatus= callMain(0, 0, startTime, timeout);	// continue where interrupted	
		markSampleOrigin(MAIN_OFFSET_MASK, startTime, originalDigiCount);
	}
}

static void runScreenSimulation(short *synthBuffer, unsigned long cyclesPerScreen, unsigned int samplesPerCall) {	
	struct timer *cia1= &(cia[0]);		// the different CIA timers
	struct timer *cia2= &(cia[1]);

	// cpu cycles used during processing
	signed long irqCycles= 0, nmiCycles= 0, mainProgCycles= 0;			

	signed long availableIrqCycles= cyclesPerScreen;	// cpu cycles still available for processing (limitation: only the waiting time is currently considered..)
	signed long availableNmiCycles= cyclesPerScreen;

	unsigned long synthPos= 0;	// start time for the samples synthesized from SID settings (measured in cycles)						

	/*
	* process NMI and IRQ interrupts in their order of appearance - and as long as they fit into this screen
	*/	
	unsigned long currentIrqTimer= forwardToNextInterrupt(getIrqTimerMode(cia1), cia1, availableIrqCycles);
	unsigned long currentNmiTimer= isRsid() ? forwardToNextCiaInterrupt(cia2, availableNmiCycles) : NO_INT;

	// KNOWN LIMITATION: ideally all 4 timers should be kept in sync - not just A/B within the same unit! 
	// however progs that actually rely on multiple timers (like Vicious_SID_2-Carmina_Burana.sid) probably 
	// need timer info that is continuously updated - so that effort would only make sense in a full fledged 
	// cycle-by-cycle emulator.
	
	unsigned char hack= isRasterIrqActive() && isCiaActive(cia1);
	
	unsigned long tNmi= currentNmiTimer;
	unsigned long tIrq= currentIrqTimer;
	unsigned long tDone= 0;
	
	unsigned int mainProgStep= 100;	// periodically give the main prog a chance to run.. 
	unsigned int mainProgStart= mainProgStep;

	if (isRsid() && (currentIrqTimer == NO_INT) && (currentNmiTimer == NO_INT)) {
		sMainLoopOnlyMode= 1;
		mainProgStart= 0; 	// this might be the better choice in any case.. but to avoid regression testing lets use it here for now..
	} else {
		sMainLoopOnlyMode= 0;
		for (;(currentIrqTimer != NO_INT) || (currentNmiTimer != NO_INT) ;) {				
			
			// periodically give unused cycles to main program (needed, e.g. by THCM's stuff)	
			
			if (tDone > mainProgStart) {
				// todo: the handing of main prog cycles is rather flawed (timing for main code that would normally run before 
				// 'mainProgStep'). In an alternative "improved" impl I ran "main" directly before "processInterrupt" calls below - letting 
				// "main" use all cycles unused up to that moment. also I had added interruption for "LDA#00; BEQ .." based endless main-loop 
				// logic (which would require IRQ logic to break out) to avoid unnecessary cycle usage.. while those changes work fine for 
				// THCM's recent stuff they broke stuff like "Arkanoid", etc (so I rolled them back for now..) 
				// -> now that the D012 handling has been improved I might give it another try..
				
				signed long availableMainCycles= tDone - (nmiCycles + irqCycles + mainProgCycles);
				
				if (availableMainCycles > 0) {
					processMain(cyclesPerScreen, mainProgStart, availableMainCycles);
					mainProgCycles+= availableMainCycles;				
				}			
				mainProgStart= tDone + mainProgStep;	
			}
			
			// FIXME: we should better allow NMI to interrupt IRQ, i.e. set respectice cycle-limit
			// accordingly - and then resume the IRQ later. see Ferrari_Formula_One.sid
			
			if (!isIrqNext(currentIrqTimer, currentNmiTimer, tIrq, tNmi)) {		// handle next NMI
				tDone= tNmi;
				nmiCycles+= processInterrupt(NMI_OFFSET_MASK, getNmiVector(), tNmi, cyclesPerScreen);
							
				availableNmiCycles-= currentNmiTimer;		
				currentNmiTimer= forwardToNextCiaInterrupt(cia2, availableNmiCycles);
				
				tNmi+= currentNmiTimer;		// will be garbage on last run..		
			} else {															// handle next IRQ
				tDone= tIrq;								
				renderSynth(synthBuffer, cyclesPerScreen, samplesPerCall, synthPos, tIrq);
				synthPos= tIrq;
				
				if (hack) {
					// in case CIA1 has also been configured to trigger IRQs, our current "raster IRQ or timer IRQ" impl 
					// is obviously flawed.. this hack will fix simple scenarios like Cycles.sid
					signalTimerUnderflow(cia1, TIMER_A); 
				}
				
				// FIXME: multi-frame IRQ handling would be necessary to deal with songs like: Musik_Run_Stop.sid				
				unsigned long usedCycles= processInterrupt(IRQ_OFFSET_MASK, getIrqVector(), tIrq, sIrqTimeout);				
				// flaw: IRQ might have switched off NMI, e.g. Vicious_SID_2-Blood_Money.sid
				
				if (usedCycles >=sIrqTimeout) { // IRQ gets aborted due to a timeout
					if (p & FLAG_I) {
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
			
				currentIrqTimer= forwardToNextInterrupt(getIrqTimerMode(cia1), cia1, availableIrqCycles);
								
				tIrq+= currentIrqTimer;		// will be garbage on last run..	
			}
		}
		renderSynth(synthBuffer, cyclesPerScreen, samplesPerCall, synthPos, cyclesPerScreen);	// fill remainder of buffer	
	}
	
	mainProgCycles= cyclesPerScreen - (nmiCycles + irqCycles + mainProgCycles);
	
	if (mainProgCycles >0) {		
		processMain(cyclesPerScreen, mainProgStart, mainProgCycles);
		
		if (sMainLoopOnlyMode && isRsid()) {
			// e.g. Dane's "Crush.sid"
			renderSynth(synthBuffer, cyclesPerScreen, samplesPerCall, synthPos, cyclesPerScreen);
		}
	}
}

/*
* @return 		1: if digi data available   0: if not
*/
int processOneScreen(short *synthBuffer, unsigned char *digiBuffer, unsigned long cyclesPerScreen, unsigned int samplesPerCall) {
	
	moveDigiBuffer2NextFrame();
	initCycleCount(0, 0);

	updateTOD();
	
	incFrameCount();
	if (!getSidPlayAddr()) {
		sIsPSID = 0;	
	}

	if (isTimerDrivenPsid() || isRasterIrqActive() || isRsid()) {
		runScreenSimulation(synthBuffer, cyclesPerScreen, samplesPerCall);
		return renderDigiSamples(digiBuffer, cyclesPerScreen, samplesPerCall);
	} else {
		// legacy PSID mode: one "IRQ" call per screen refresh (a PSID may actually setup CIA 1 
		// timers but without wanting to use them - e.g. ZigZag.sid track2)
		processInterrupt(IRQ_OFFSET_MASK, getIrqVector(), 0, -1);
		synth_render(synthBuffer, samplesPerCall);	
		clearDigiBuffer();		
		return 0;
	}
}
