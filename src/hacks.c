/*
 * Call it cheating... 
 *
 * <p>Tiny'R'Sid (c) 2013 J.Wothke
 * <p>version 0.81
 */

#include <string.h> 

#include "hacks.h"

#include "memory.h"
#include "cia.h"

/*
* Lame_Tune.sid would normally run endless digi-player loop from IRQ.
* as long as emu does not allow to resume suspended IRQ handlers on the next screen,
* the easiest workaround is to directly run the digi-player loop as Main..
*/
static void patchLameTuneIfNeeded(uint16_t *initAddr) {		
	uint8_t lamePattern[]= {0xa2, 0x00,0xbd, 0x00, 0x0c};
	if (((*initAddr) == 0x1000) && memMatch(0x1000, lamePattern, 5)) {
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
	if (((*initAddr) == 0xc800) && (memReadRAM(0xc82a)==0x58)) {
		// seems like we have a problem playing demophase $02 ("lyrics").. as a hack we 
		// just skip it: replace $xx08 NMI vector + 2x inc phase id
		memWriteRAM(0x0e94, 0x00);	// replace $xx08 NMI vector (used for demophase $02 ("lyrics"))								
		// sneak in an extra INC $09 by calling the below $c700 subroutine
		memWriteRAM(0x0e95, 0x20);	// jsr $c700
		memWriteRAM(0x0e96, 0x00);
		memWriteRAM(0x0e97, 0xc7);

		memWriteRAM(0xc700, 0x8d);	// sta $dc05
		memWriteRAM(0xc701, 0x05);
		memWriteRAM(0xc702, 0xdc);
		memWriteRAM(0xc703, 0xe6); 	// inc $09
		memWriteRAM(0xc704, 0x09);
		memWriteRAM(0xc705, 0x60);	// rts	
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
	if (((*initAddr) == 0xc000) && (memReadRAM(0xC056)==0x71)) {
		memWriteRAM(0xa7c4, 0x60);	// replace IRQ2 RTI with RTS	
		memWriteRAM(0xa7ce, 0x60);		
		memWriteRAM(0xa7d6, 0x60);		
		memWriteRAM(0xa547, 0x20);	// use JSR into IRQ2 from IRQ1
		memWriteRAM(0xa548, 0x89);	// IRQ2 entry with skipped register saving
		memWriteRAM(0xa549, 0xe1);	
	}
}

/*
* Thats_All_Folks.sid: here again we have a problem with nested IRQs, in addition the main prog uses
* busy-d012-wait for timing.. (there still remains a speed problem at the very end of the melody but 
* the rest plays with this hack)
*/
static void patchThatsAllFolksIfNeeded(uint16_t *initAddr) {
	if (((*initAddr) == 0x4800) && (memReadRAM(0x4806)==0x48)) {	
		memWriteRAM(0x461C, 0x4c);	// always use main loop at $4685
		memWriteRAM(0x461D, 0x85);
		memWriteRAM(0x461E, 0x46);		
	}
}

/*
* Uwe's original demo version switches between NMI and IRQ bases digi playback in the middle of the screen.
* Unfortunately the emulator's timing glitches make it play more samples than are actually provided.
*/
static void patchBloodMoneyIfNeeded(uint16_t *initAddr) {
	uint8_t bloodPattern[]= {0xa4,0x2e,0x4c,0x0c,0xdc};
	if (((*initAddr) == 0x080d) && memMatch(0x094B, bloodPattern, 5)) {
		memWriteRAM(0x0941, 0xad);	// just disable the IRQ digis and stick to the NMI playback
		memWriteRAM(0x0937, 0xad);
		memWriteRAM(0x093C, 0xad);
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
	if (memMatch((*initAddr)+3, startPattern, 5)) {
		
		uint8_t initPattern[]= {0xa2,0x00,0xbd,0x50,0xd0};
		uint16_t initsub= 0xd000;
		if (memMatch(initsub, initPattern, 5)) {
			memWriteRAM((*initAddr), 0x60); // RTS
		}
	}

	// We_Are_Demo
	if ((*initAddr) == 0xc60) {
		uint8_t playerPattern[]= {0x8e,0x18,0xd4,0x79,0x00};
		if (memMatch(0xb10, playerPattern, 5)) {
			memWriteRAM((*initAddr), 0x60); // RTS
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
	if (memMatch(0x0E10, wonderPattern, 7)) {
		memWriteRAM(0x15A7, 0xea);	// IRQ cycle timing patch  (somewhat reduces glitches..)
		memWriteRAM(0x15A8, 0xea);
	
		// replacing the CMP $d012 here is crucial 
		memWriteRAM(0x0D36, 0x12);	// hack op: C_A
		memWriteRAM(0x0D37, 0x02);	// loop 1x before exit
		memWriteRAM(0x0D38, 0x0);

		memWriteRAM(0x0E15, 0xea);	// disable D012 check (needed!)
		memWriteRAM(0x0E16, 0xea);
		
		// optional add-on: fix defective Wonderland_XII-Digi_part_2.sid rip (main prog):
		memWriteRAM(0x1a8a, 0xad);	// disable dead JMP 

		memWriteRAM(0x1b8c, 0x4c);	// go into loop instead of using dead JSRs
		memWriteRAM(0x1b8d, 0x8c);
		memWriteRAM(0x1b8e, 0x1b);
		
		ciaSetNmiVectorHack();
	}
}


static void patchFileDeletedIfNeeded(uint16_t *initAddr) {
	// "File Deleted" patch
	uint8_t thcmPattern[]= {0x24, 0x00, 0x4C, 0x03, 0x00};
	if (((*initAddr) == 0x080d) && memMatch(0x08c3, thcmPattern, 5)) {
		// NMI interrupting MAIN during init not supported
		// (THMC uses it here to detect old/new CIA chips and make
		// a +1 cycle timing correction..)
		memWriteRAM(0x0B50, 0xea);	// disable NMI stack manipulation
		memWriteRAM(0x0B51, 0xea);
		memWriteRAM(0x0B52, 0xea);		
	}
}

void hackIfNeeded(uint16_t *initAddr) {

	patchFileDeletedIfNeeded(initAddr);
	
	patchMahoneyLatestIfNeeded(initAddr);
	
	patchWonderlandIfNeeded(initAddr);
	
	patchBloodMoneyIfNeeded(initAddr);
	
	patchLameTuneIfNeeded(initAddr);

	patchKnatterIfNeeded(initAddr);
		
	patchThatsAllFolksIfNeeded(initAddr);
}


