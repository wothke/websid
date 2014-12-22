/*
 * This file contains additional emulation logic used to play RSID files.
 * 
 * <p>version 0.77
 * <p>Tiny'R'Sid (c) 2013 Jürgen Wothke
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
 * stuff (Galway, Hubbard, etc) I am not very concerned enough about these limitations...
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

#include "defines.h"
#include "nanovic.h"
#include "nanocia.h"
#include "sidengine.h"
#include "rsidengine.h"
#include "sidplayer.h"



enum timertype { RASTER_TIMER = 0, CIA1_TIMER = 1, NO_IRQ = 2};

unsigned long INVALID_TIME= (0x1 << 27);	// large enough so that any regular timestamp will get preference
unsigned int IDX_NOT_FOUND= 0x1fff;			// we'll never have that many samples for one screen..

static unsigned char sCurrentDigi=  0x80;		// last digi sample / default: neutral value 

static unsigned long sSortedDigiTime[DIGI_BUF_SIZE];
static unsigned char sSortedDigiVolume[DIGI_BUF_SIZE];


unsigned char memory[MEMORY_SIZE];
unsigned char kernal_rom[KERNAL_SIZE];	// mapped to $e000-$ffff
unsigned char io_area[IO_AREA_SIZE];	// mapped to $d000-$dfff



int isRsid() {
	return (sIsPSID == 0);
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
	
	if ((sPlayAddr != 0) || (((getmem(0xfffe)|(getmem(0xffff)<<8)) == 0) && sIsPSID &&
			((getmem(0x0314)|(getmem(0x0315)<<8)) != 0))) {
		
		return 1;
	}
	return 0;
}

unsigned int getIrqVector() {
	if (sPlayAddr != 0) {
		// no point going through the standard interrupt routines with this PSID crap.. we
		// cannot tell if it behaves like a 0314/15 or like a fffe/f routine anyway... (and the stack will likely be messed up)
		return sPlayAddr;	
	} 

	unsigned int irq_addr= (getmem(0xfffe)|(getmem(0xffff)<<8));	

	if ((irq_addr == 0) && sIsPSID) {	// see isPsidDummyIrqVector()
		irq_addr= (getmem(0x0314)|(getmem(0x0315)<<8));		
	}
	return irq_addr;

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

static unsigned char sCurrentVolume= 0xf;

void initCycleCount(unsigned long relOffset, unsigned long basePos) {
	sCycles= relOffset;	
	initRasterlineSim(basePos+ relOffset);
}

unsigned long processInterrupt(unsigned long intMask, unsigned short npc, unsigned long startTime, signed long cycleLimit) {
	// known limitation: if the code uses ROM routines (e.g. register restore/return sequence) then we will 
	// be missing those cpu cycles in our calculation..
	
	unsigned long originalDigiCount= sDigiCount;
	
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

	if (intMask == IRQ_OFFSET_MASK) {
		sCurrentVolume= getmem(0xd418);
	}
	
	markSampleOrigin(intMask, startTime, originalDigiCount);		

	return sCycles;
}

int isCiaActive(struct timer *t) {
	return (!isIrqBlocked() && ((isTimer_Started(t, TIMER_A) && isTimer_Armed(t, TIMER_A)) || (isTimer_Started(t, TIMER_B) && isTimer_Armed(t, TIMER_B)))) ||
			isTimerDrivenPsid();
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

int isVal(unsigned long mask, unsigned long val) {
	return val&mask;		// is this a value of the selected "mask" category?
}

unsigned long getValue(unsigned long mask, unsigned int fromIdx) {
	if (fromIdx == IDX_NOT_FOUND) {
		return INVALID_TIME;			// only relevant while it is used for comparisons..
	}
	return (sDigiTime[fromIdx])&(~mask);	// remove marker flag
}

// must only be used for valid "fromIdx"
void copy(unsigned long mask, unsigned int toIdx, unsigned int fromIdx) {
	sSortedDigiTime[toIdx]= getValue(mask, fromIdx);
	sSortedDigiVolume[toIdx]= sDigiVolume[fromIdx];
}

unsigned int next(unsigned long mask, unsigned int toIdx, unsigned int fromIdx) {
	copy(mask, toIdx, fromIdx);

	unsigned int i; 
	for (i= fromIdx+1; i<sDigiCount; i++) {
		if(isVal(mask, sDigiTime[i])) {
			return i;	// advance index to the next value of this category
		}
	}
	return IDX_NOT_FOUND;
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


void sortDigiSamples() {
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

void fillDigi(int startIdx, int endIdx, unsigned char digi) {
	if (endIdx>=startIdx) {
		memSet( &sDigiBuffer[startIdx], digi, (endIdx-startIdx)+1 );
	}
}

int renderDigiSamples(unsigned long cyclesPerScreen, unsigned int samplesPerCall) {

	/*
	* if there are too few signals, then it's probably just the player setting filters or
	* resetting the volume with no intention to play a digi-sample, e.g. Transformers.sid (Russel Lieblich)
	*/

	
	if (sDigiCount > 5) {
		sortDigiSamples();

		// render digi samples
		unsigned int fromIdx=0;	
		int j;
		for (j= 0; j<sDigiCount; j++) {
			float scale= (float) (samplesPerCall-1) / cyclesPerScreen;
			unsigned int toIdx= scale*((sSortedDigiTime[j] > cyclesPerScreen) ? cyclesPerScreen : sSortedDigiTime[j]);

			fillDigi(fromIdx, toIdx, sCurrentDigi);				

			fromIdx= toIdx;
			sCurrentDigi= sSortedDigiVolume[j];
		}
		fillDigi(fromIdx, (samplesPerCall-1), sCurrentDigi);

		if (getRasterlineTimer() == 0xf8) {
			// hack fixes volume issue in Ferrari_Formula_One.sid
			// todo: the rasterline check is a rather brittle impl... a more 
			// robost/foolproof impl fix needs to be found here
			
			sidPoke(0xd418 & 0x1f, sCurrentVolume);	
		}

		return 1;
	}
	return 0;
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

void renderSynth(unsigned long cyclesPerScreen, unsigned int samplesPerCall, unsigned long fromTime, unsigned long toTime){
	if (fromTime < cyclesPerScreen) {
		if (toTime > cyclesPerScreen) toTime= cyclesPerScreen;
		
		float scale= (float)samplesPerCall/cyclesPerScreen;
		unsigned int len= (toTime - fromTime)*scale;
		unsigned int startIdx= fromTime*scale;

		synth_render(&sSynthBuffer[startIdx], len+1);
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


// snapshot of current registers and stack (so we can ignore IRQ/NMI effects)
static unsigned char sSnapshotAcc, sSnapshotX, sSnapshotY, sSnapshotPFlags, sSnapshotStackpointer;
static unsigned short sSnapshotPC;
static unsigned char sSnapshotStack[0xff];

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
		if (((cycleLimit >0) && (sCycles >=cycleLimit)) || (sOverflowDigiCount>0)) {	
			saveRegisters();			
			return 1;
		}		
        cpuParse();
	}
	return 0;
}

void processMain(unsigned long cyclesPerScreen, unsigned long startTime, signed long timeout) {
	// the current impl may cause the interrupt routines to burn more cycles than would be normally possibe (e.g. if
	// they use busy-wait for some condition which does not materialize in our emulator env. Consequently our 
	// "timeout" may be completely off the target...

	if ((sMainProgStatus >0) && (timeout >0)) {		// might be the rest of some "init" logic... or some loop playing samples		
		unsigned long originalDigiCount= sDigiCount;
		sMainProgStatus= callMain(0, 0, startTime, timeout);	// continue where interrupted	
		markSampleOrigin(MAIN_OFFSET_MASK, startTime, originalDigiCount);
	}
}

static void runScreenSimulation(unsigned long cyclesPerScreen, unsigned int samplesPerCall) {	
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
			if (!isIrqNext(currentIrqTimer, currentNmiTimer, tIrq, tNmi)) {		// handle next NMI
				tDone= tNmi;
				nmiCycles+= processInterrupt(NMI_OFFSET_MASK, getNmiVector(), tNmi, cyclesPerScreen);
							
				availableNmiCycles-= currentNmiTimer;		
				currentNmiTimer= forwardToNextCiaInterrupt(cia2, availableNmiCycles);
				
				tNmi+= currentNmiTimer;		// will be garbage on last run..				
			} else {															// handle next IRQ
				tDone= tIrq;								
				renderSynth(cyclesPerScreen, samplesPerCall, synthPos, tIrq);			
				synthPos= tIrq;
				
				if (hack) {
					// in case CIA1 has also been configured to trigger IRQs, our current "raster IRQ or timer IRQ" impl 
					// is obviously flawed.. this hack will fix simple scenarios like Cycles.sid
					signalTimerUnderflow(cia1, TIMER_A); 
				}
				
				unsigned long usedCycles= processInterrupt(IRQ_OFFSET_MASK, getIrqVector(), tIrq, sIrqTimeout);				
				// flaw: IRQ might have switched off NMI, e.g. Vicious_SID_2-Blood_Money.sid
				
				if (usedCycles >=sIrqTimeout) { // IRQ gets aborted due to a timeout
					if (p & FLAG_I) {
						// this IRQ handler does not want to be interrupted.. (either the code got stuck due to some impossible
						// condition - e.g. waiting for a timer which we dont update - or it is an endless IRQ routine that needs to 
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
		renderSynth(cyclesPerScreen, samplesPerCall, synthPos, cyclesPerScreen);	// fill remainder of buffer	
	}
		
	mainProgCycles= cyclesPerScreen - (nmiCycles + irqCycles + mainProgCycles);
	
	if (mainProgCycles >0) {		
		processMain(cyclesPerScreen, mainProgStart, mainProgCycles);
		
		if (sMainLoopOnlyMode && isRsid() && !sSynthDisabled) {
			// e.g. Dane's "Crush.sid"
			renderSynth(cyclesPerScreen, samplesPerCall, synthPos, cyclesPerScreen);
		}
	}
}

void initDigiBuffer() {	
	if (sOverflowDigiCount > 0) {
		memcpy(sDigiTime, sOverflowDigiTime, sizeof(long)*sOverflowDigiCount);		
		memcpy(sDigiVolume, sOverflowDigiVolume, sizeof(long)*sOverflowDigiCount);		
	}
	sDigiCount= sOverflowDigiCount;
	sOverflowDigiCount= 0;	
}

static void updateTOD() {
	sTodInMillies+= (getCurrentSongSpeed() ? 17 : 20);	
}

/*
* @return 		1: if digi data available   0: if not
*/
int processOneScreen(unsigned long cyclesPerScreen, unsigned int samplesPerCall) {
	
	initDigiBuffer();
	initCycleCount(0, 0);

	updateTOD();
	
#ifdef DEBUG				
screen+=1;
#endif
	if (!sPlayAddr) {
		sIsPSID = 0;	
	}

	if ((isTimerDrivenPsid() || isRasterIrqActive() || isRsid())) {
		runScreenSimulation(cyclesPerScreen, samplesPerCall);
		
		return renderDigiSamples(cyclesPerScreen, samplesPerCall);
		
	} else {		
		// legacy PSID mode: one "IRQ" call per screen refresh (a PSID may actually setup CIA 1 
		// timers but without wanting to use them - e.g. ZigZag.sid track2)

		processInterrupt(IRQ_OFFSET_MASK, getIrqVector(), 0, -1);
		synth_render(sSynthBuffer, samplesPerCall);	
		sDigiCount= 0;		
		return 0;
	}
}
