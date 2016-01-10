/*
 * Call it cheating... 
 *
 * <p>Tiny'R'Sid (c) 2013 J.Wothke
 * <p>version 0.77
 * 
 * <p>The flaws of the current emulator implemetation (i.e. lack of cycle exact: CIA timer simulation, 
 * D41B register content; no support for cascading interrupts; IRQs must terminate within 1 screen refresh;
 * complete lack of badline timing corrections; etc) makes those exotic songs fail which actually depend on 
 * respective "features" (e.g. Microsleep_tune_06.sid, Lame_Tune.sid).
 *
 * <p>The logic in this file is meant to patch some of the affected music files so that they 
 * can be played in spite of the emulator flaws. The hacks performed here do not always lead to good 
 * playback results, but at least they will avoid annoying player lock-ups..
 *
 * <p>Search for "hack" to find other instances of workarounds..
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
void patchLameTuneIfNeeded(word *initAddr) {		
	unsigned char lamePattern[]= {0xa2, 0x00,0xbd, 0x00, 0x0c};
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
void patchStorebrorIfNeeded(word *initAddr) {
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
void patchKnatterIfNeeded(word *initAddr) {
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
void patchThatsAllFolksIfNeeded(word *initAddr) {
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
void patchBloodMoneyIfNeeded(word *initAddr) {
	unsigned char bloodPattern[]= {0xa4,0x2e,0x4c,0x0c,0xdc};
	if (((*initAddr) == 0x080d) && !memcmp(&(memory[0x094B]), bloodPattern, 5)) {
		memory[0x0941]= 0xad;	// just disable the IRQ digis and stick to the NMI playback
		memory[0x0937]= 0xad;
		memory[0x093C]= 0xad;
	}
}
void patchWonderlandIfNeeded(word *initAddr) {
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

	unsigned char wonderPattern[]= {0x0D,0x12,0xD0,0xC9,0x2C,0xD0,0x52};
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

void hackIfNeeded(word *initAddr) {
	patchWonderlandIfNeeded(initAddr);
	
	patchBloodMoneyIfNeeded(initAddr);
	
	patchLameTuneIfNeeded(initAddr);

//	patchStorebrorIfNeeded(initAddr);
	
	patchKnatterIfNeeded(initAddr);
		
	patchThatsAllFolksIfNeeded(initAddr);
}