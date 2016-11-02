/*
 * Call it cheating... 
 *
 * <p>Tiny'R'Sid (c) 2013 J.Wothke
 * <p>version 0.77
 */

#include <string.h> 

#include "defines.h"
#include "sidengine.h"
#include "rsidengine.h"
#include "sidplayer.h"
 
/*
* Lame_Tune.sid would normally run endless digi-player loop from IRQ.
* as long as emu does not allow to resume suspended IRQ handlers on the next screen,
* the easiest workaround is to directly run the digi-player loop as Main..
*/
static void patchLameTuneIfNeeded(uint16_t *initAddr) {		
	uint8_t lamePattern[]= {0xa2, 0x00,0xbd, 0x00, 0x0c};
	if (((*initAddr) == 0x1000) && !memcmp(&(memory[0x1000]), lamePattern, 5)) {
		(*initAddr)= 0x112a;
	}
}

/*
* Mahoney's Storebror.sid is a good example of a song that is actually a complete demo program with graphix and all.
* Different sections of the tune are syncronized to the timing of the different demo parts - which unfortunately 
* use about every timing trick in the book and then some. Unfortunately the emu lacks the precision needed to 
* accurately emulate the song.. without the below hack, some of the song phases are unaligned, e.g. the 
* "singing" clashes with the bells, etc .. the hack just disabled the "difficult" part so that the 
* remaining (i.e. non-singing) parts work nicely..
*/
/*
static void patchStorebrorIfNeeded(uint16_t *initAddr) {
	if (((*initAddr) == 0xc800) && (memory[0xc82a]==0x58)) {
		// seems like we have a problem playing demophase $02 ("lyrics").. as a hack we 
		// just skip it: replace $xx08 NMI vector + 2x inc phase id
		memory[0x0e94]=0x00;	// replace $xx08 NMI vector (used for demophase $02 ("lyrics"))								
		// sneak in an extra INC $09 by calling the below $c700 subroutine
		memory[0x0e95]=0x20;	// jsr $c700
		memory[0x0e96]=0x00;
		memory[0x0e97]=0xc7;

		memory[0xc700]=0x8d;	// sta $dc05
		memory[0xc701]=0x05;
		memory[0xc702]=0xdc;
		memory[0xc703]=0xe6; 	// inc $09
		memory[0xc704]=0x09;
		memory[0xc705]=0x60;	// rts	
	}
}
*/

/*
* Knatter's song: Microsleep_tune_10.sid is based on an IRQ which should be interrupted by the next IRQ...
* Because the current emu impl does not allow for interrupts to be interrupted, the below cheat patches the 
* song to directly invoke the code of the 2nd IRQ  as a subroutine from the 1st IRQ.. 
* This makes the song play - eventhough somewhat distorted.
*/
static void patchKnatterIfNeeded(uint16_t *initAddr) {
	if (((*initAddr) == 0xc000) && (memory[0xC056]==0x71)) {
		memory[0xa7c4]=0x60;	// replace IRQ2 RTI with RTS	
		memory[0xa7ce]=0x60;		
		memory[0xa7d6]=0x60;		
		memory[0xa547]=0x20;	// use JSR into IRQ2 from IRQ1
		memory[0xa548]=0x89;	// IRQ2 entry with skipped register saving
		memory[0xa549]=0xe1;	
	}
}

/*
* Thats_All_Folks.sid: here again we have a problem with nested IRQs, in addition the main prog uses
* busy-d012-wait for timing.. (there still remains a speed problem at the very end of the melody but 
* the rest plays with this hack)
*/
static void patchThatsAllFolksIfNeeded(uint16_t *initAddr) {
	if (((*initAddr) == 0x4800) && (memory[0x4806]==0x48)) {	
		memory[0x461C]=0x4c;	// always use main loop at $4685
		memory[0x461D]=0x85;
		memory[0x461E]=0x46;		
	}
}

/*
* Uwe's original demo version switches between NMI and IRQ bases digi playback in the middle of the screen.
* Unfortunately the emulator's timing glitches make it play more samples than are actually provided.
*/
static void patchBloodMoneyIfNeeded(uint16_t *initAddr) {
	uint8_t bloodPattern[]= {0xa4,0x2e,0x4c,0x0c,0xdc};
	if (((*initAddr) == 0x080d) && !memcmp(&(memory[0x094B]), bloodPattern, 5)) {
		memory[0x0941]= 0xad;	// just disable the IRQ digis and stick to the NMI playback
		memory[0x0937]= 0xad;
		memory[0x093C]= 0xad;
	}
}

static void patchSwallowLatestIfNeeded(uint16_t *initAddr) {
	// Comaland_tune_3.sid / Fantasmolytic_tune_2.sid use a player
	// that depends on IRQ/NMI sync (etc).. fortunately they do not 
	// seem to hang the emu and they can be left as is..
}

static void patchMahoneyLatestIfNeeded(uint16_t *initAddr) {
	// Mahoney's latest songs use features (endless IRQs that must be interrupted, etc)
	// that will hang the emu and to avoid an annoying user experience these songs are 
	// better disabled..

	// Musik_Run_Stop (Stereophonik_8580_2SID, Stereophonik_6581_2SID use version 3 RSID which is not supported)
	uint8_t startPattern[]= {0x85,0x01,0x4c,0x00,0xd0};
	if (!memcmp(&(memory[(*initAddr)+3]), startPattern, 5)) {
		
		uint8_t initPattern[]= {0xa2,0x00,0xbd,0x50,0xd0};
		uint16_t initsub= 0xd000;
		if (!memcmp(&(memory[initsub]), initPattern, 5)) {
			memory[(*initAddr)]= 0x60; // RTS
		}
	}

	// We_Are_Demo
	if ((*initAddr) == 0xc60) {
		uint8_t playerPattern[]= {0x8e,0x18,0xd4,0x79,0x00};
		if (!memcmp(&(memory[0xb10]), playerPattern, 5)) {
			memory[(*initAddr)]= 0x60; // RTS
		}
	}
}

static void patchWonderlandIfNeeded(uint16_t *initAddr) {
	/* 
	 Wonderland_XII-Digi* patch: this hack does not work well enough
	 to actually re-create the original 16kHz experience (with all the timing
	 tricks that the digi-player uses, one would need cycle accurate emulation across 
	 CPU, Timers & VIC to get it right.. the main obstacles within the Tiny'R'Sid impl
	 here are: lack of VIC & Timer emulation during CPU handling). so enjoy the 
	 DonaldDuck remix ;-)
	
	 The hack just disables some of the Raster-polling which is originally done
	 within the NMI digi-player routine.. it also turns on hard-coded value for 0xdc04 
	 so that the dynamic Timer driven NMI-vector selection logik is actually fixed at some
	 valid value

	 following sequence is used to ID the player..
	;0E10    0D 12 D0    ORA $D012
	;0E13    C9 2C       CMP #$2C  
	;0E15    D0 52       BNE $0E69		<-- disable	
	*/

	uint8_t wonderPattern[]= {0x0D,0x12,0xD0,0xC9,0x2C,0xD0,0x52};
	if (!memcmp(&(memory[0x0E10]), wonderPattern, 7)) {
		memory[0x15A7]= 0xea;	// IRQ cycle timing patch  (somewhat reduces glitches..)
		memory[0x15A8]= 0xea;
	
		/*
		interstingly there seems to be no benefit in also disabling there two
		instances of NMI D012 polling.. maybe with our hack it is already dead code 
		
		memory[0x0C48]= 0x02;	// hack op: L_A
		memory[0x0C49]= 0x1;	// directly take exit.. no more loop
		memory[0x0C4A]= 0xf5;
		
		memory[0x0D00]= 0x02;	// hack op: L_A
		memory[0x0D01]= 0x01;
		memory[0x0D02]= 0xf4;
		
		also sync during INIT seems to be harmless 
		
		memory[0x1037]= 0xea;	// disable D012 sync in init
		memory[0x1038]= 0xea;
		memory[0x104F]= 0xea;
		memory[0x1050]= 0xea;
		*/
		
		// replacing the CMP $d012 here is crucial 
		memory[0x0D36]= 0x12;	// hack op: C_A
		memory[0x0D37]= 0x02;	// loop 1x before exit
		memory[0x0D38]= 0x0;

		memory[0x0E15]= 0xea;	// disable D012 check (needed!)
		memory[0x0E16]= 0xea;
		
		// optional add-on: fix defective Wonderland_XII-Digi_part_2.sid rip (main prog):
		memory[0x1a8a]= 0xad;	// disable dead JMP 

		memory[0x1b8c]= 0x4c;	// go into loop instead of using dead JSRs
		memory[0x1b8d]= 0x8c;
		memory[0x1b8e]= 0x1b;
		
		setCiaNmiVectorHack();
	}
}

void hackIfNeeded(uint16_t *initAddr) {
	patchMahoneyLatestIfNeeded(initAddr);
	
	patchWonderlandIfNeeded(initAddr);
	
	patchBloodMoneyIfNeeded(initAddr);
	
	patchLameTuneIfNeeded(initAddr);

	patchKnatterIfNeeded(initAddr);
		
	patchThatsAllFolksIfNeeded(initAddr);
}


/**************************************************************************************************
	below add-on HACK for "main loop ocsillator polling"
	
	This is a hack to support specific ocsillator polling from main loop as done by PollyTracker
	e.g. see Instantfunk.sid (the usage pattern is very specific and the below hack 
	covers exactly that - and nothing more).
***************************************************************************************************/

struct simosc3 {
	uint8_t waveform;
	
	// more recent hack for main loop polling
	uint32_t baseCycles;
	uint32_t counter;
	uint32_t freqmul;	
};
static struct simosc3 osc3;

void simStartOscillatorVoice3(uint8_t voice, uint8_t val) {
	if ((voice == 2) && isMainLoopPolling()) {
		osc3.waveform= val & 0xf0;

		// hack: use only for main/pulse cases like Instantfunk.sid
		osc3.baseCycles= sAbsCycles;
		osc3.counter= 0; 
		
		// for some reason the playback is slightly slower than in ACID64
		osc3.freqmul= freqmul*getNumberOfSamplesPerCall()/getCyclesPerScreen();
	}
}
static uint32_t simOsc3Counter() {
	// voice 3 oscillator counter based on elapsed time
	uint32_t diff= sAbsCycles - osc3.baseCycles;
	osc3.baseCycles= sAbsCycles;

	uint32_t f= ((uint32_t)sid.v[2].freq)*osc3.freqmul*diff;		
	osc3.counter=(osc3.counter + f) & 0xfffffff;
	return osc3.counter;
}

static uint8_t simReadSawtoothD41B() {
	// simulate sawtooth voice 3 oscillator level based on elapsed time	
	// (handle busy polling for sid oscillator3 - e.g. Ring_Ring_Ring.sid)

	return (uint8_t) (simOsc3Counter() >> 20);
}

static uint8_t simReadPulsedD41B() {
	// simulate pulse voice 3 oscillator level based on elapsed time	
	uint32_t p= (((uint32_t)sid.v[2].pulse) & 0xfff) << 16;
	return (simOsc3Counter() > p) ? 0 : 1;
}

uint8_t simReadD41B() {
	if (osc3.waveform == 0x40) {
		return  simReadPulsedD41B();
	}
	return simReadSawtoothD41B();
}


/**************************************************************************************************
	below add-on HACK for "main loop timer polling" is independent of the above! 

	This is a hack to support specific timer polling from main loop as done by PollyTracker
	e.g. see Instantfunk.sid (the usage pattern is very specific and the below hack 
	covers exactly that - and nothing more). Actually PollyTracker also reads timer 
	counters to make +1 cycle timing adjustments: since it doesn't make much difference 
	to the playback quality - but a respective add-on feature might break existing
	stuff - it is not implemented.
***************************************************************************************************/

struct dummycia {
	uint8_t isStarted;
	uint32_t baseCycles;
	uint16_t latch;
	uint16_t nextLatch;	
	uint8_t stopStatus;
};
static struct dummycia dcia[4];


void simWriteTABLO(uint8_t ciaIdx, uint8_t timerIdx, uint8_t val) {
	struct dummycia *c= &(dcia[ciaIdx*2+timerIdx]);
	c->nextLatch=  (c->nextLatch & 0xff00) | (0x00ff & val);
}

uint8_t intReadICR(uint8_t ciaIdx, uint8_t timerIdx) {	
	// calc timer underflow status (there is no timer counted down, instead
	// the "time elapsed" since the start of the countdown is tracked)
	struct dummycia *c= &(dcia[ciaIdx*2+timerIdx]);

	if (!c->isStarted ) {
		uint8_t readOnce= c->stopStatus;
		c->stopStatus= 0;
		return readOnce;
	}	
	uint32_t diff= sAbsCycles - c->baseCycles;	
	if (diff >= c->latch) {
		uint32_t overflow= diff - c->latch;		
		c->baseCycles= sAbsCycles - overflow;
		
		// note: status bits are cleared "on read": since base time has  
		// just been updated this status has been cleared
				
		c->latch= c->nextLatch; // underflow triggers "reload" of the counter
		
		return (timerIdx & 1) + 1;
	} else {
		return 0;
	}
}
uint8_t simReadICR_1() {
	// Interrupt Control und Status	- $DC0D
	return intReadICR(0, 0) | intReadICR(0, 1);
}
uint8_t simReadICR_2() {
	// Interrupt Control und Status	- $DD0D
	return intReadICR(1, 0) | intReadICR(1, 1);
}
void simWriteCRAB(uint8_t ciaIdx, uint8_t timerIdx, uint8_t val) {
	// Control Timer: DC0E/F DD0E/F  write start/stop flag (other flags NOT implemented)

	uint8_t i= ciaIdx*2+timerIdx;
	struct dummycia *c= &(dcia[i]);
		
	if (val & 0x1) {	// ignore other flags here
		// start timer
		if (!c->isStarted ) {
			c->baseCycles= sAbsCycles;			// "load latch" here equals "reset base time"
					
			c->latch= c->nextLatch;	
					
			c->isStarted= 1;
		} else {
			// start of already running timer is a NOP
		}
	} else {
		if (!c->isStarted) {
			// stop of already stopped timer is a NOP
		} else {
			// stop (in case there was already an interrupt before, make
			// sure the interrupt status is still updated)

			// 1st check if there is a timer underflow (once stopped the
			// logic in simReadICR* will no longer calc and therefore it must be done here)
			 if (c->isStarted) {	c->stopStatus= intReadICR(ciaIdx, timerIdx); }			
		}		
		c->isStarted= 0;
	}
}
uint8_t simReadCRAB(uint8_t ciaIdx, uint8_t timerIdx) {
	// DC0E/F DD0E/F  read start/stop bit (other flags NOT implemented)
	struct dummycia *c= &(dcia[ciaIdx*2+timerIdx]);
	return c->isStarted;
}

void simWriteTimer(uint16_t addr, uint8_t value) {
	switch (addr) {
		case 0xdc04:	// targetted song are only setting LO byte
		case 0xdc06:
			simWriteTABLO(0, ((addr-0xdc04) >>1), value);
			break;
		case 0xdd04:
		case 0xdd06:
			simWriteTABLO(1, ((addr-0xdd04) >>1), value);
			break;
			
		case 0xdc0e:	
		case 0xdc0f:	
			simWriteCRAB(0, (addr-0xdc0e), value);
			break;
		case 0xdd0e:	
		case 0xdd0f:	
			simWriteCRAB(1, (addr-0xdd0e), value);
			break;
	} 
}

/**************************************************************************************************/

 void initHacks() {
	memSet((uint8_t*)&osc3,0,sizeof(osc3));

	//	memSet( (uint8_t*)&sLastPolled, 0, sizeof(sLastPolled) );	
	memSet((uint8_t*)&dcia,0,sizeof(dcia));
 }


