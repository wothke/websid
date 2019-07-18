/*
* Call it cheating... 
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*/

#include <string.h> 

#include "hacks.h"

#include "memory.h"
#include "cia.h"

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
* Thats_All_Folks.sid: here again we have a problem with nested IRQs, in addition the main prog uses
* busy-d012-wait for timing..
*/
static void patchThatsAllFolksIfNeeded(uint16_t *initAddr) {
	if (((*initAddr) == 0x4800) && (memReadRAM(0x4806)==0x48)) {	
		memWriteRAM(0x461C, 0x4c);	// always use main loop at $4685
		memWriteRAM(0x461D, 0x85);
		memWriteRAM(0x461E, 0x46);		
	}
}

static void patchSuperCarlingSpider(uint16_t *initAddr) {
	// "Super_Carling_the_Spider" patch (it remains to be seen if other songs are
	// also using this digi approach.. a patch is use here since the song
	// would otherwise block the player)
	uint8_t pattern[]= {0x11, 0x8d, 0x0b, 0xd4, 0xa9, 0x09, 0x8d, 0x12, 0xd4, 0xad, 0xff};
	if (((*initAddr) == 0x080d) && memMatch(0x0d1b, pattern, 11)) {	
	/*
		memWriteRAM(0x081b, 0xa9);	// hardcode timer (the set 0x3 seems to be good for 155 NMIs per frame..)
			
		memWriteRAM(0x08D3, 0x4c);	// use IRQ2 directly as main	
		memWriteRAM(0x08D4, 0x50);	
		memWriteRAM(0x08D5, 0x09);	
	*/
		
		memWriteRAM(0x0c0e, 0xea);	// replace BRK in NMI (the branch timing is incorrect)
		memWriteRAM(0x0c0f, 0xea);
		memWriteRAM(0x0d0e, 0xea);
		memWriteRAM(0x0d0f, 0xea);
		// the alternating 2-voice FREQ modulation digi approach is not currently
		// detected.. patch it to something that is (i.e. use only 1 voice per NMI-vector
		// and use the "standard pattern" with GATE). If this approach was more widely used
		// then it would make sense to extend the detection logic - but for just this one
		// song there is no need.
		memWriteRAM(0x0c1d, 0x0b);
		memWriteRAM(0x0c2b, 0x1);		
		memWriteRAM(0x0d1d, 0x12);
		memWriteRAM(0x0D2b, 0x1);
	}
}


void hackIfNeeded(uint16_t *initAddr) {
//	memWriteRAM(0x1437, 0x82);	// HACK Wonderland_XII-Digi_part_1 to activate NMI rather than dacctivate..


// this patch still makes something play with the new version...
//	patchSuperCarlingSpider(initAddr);	// plays without but without nice bongos
	
	/* XXX FIXME DITCH
		
	patchSuperCarlingSpider(initAddr);
		
	patchBloodMoneyIfNeeded(initAddr);
	
		
	patchThatsAllFolksIfNeeded(initAddr);
	*/
}


