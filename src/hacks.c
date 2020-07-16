/*
* Call it cheating...
*
* The hacks are a means to explore the features that would be
* needed to properly deal with the affacted songs.
*
* WebSid (c) 2020 Jürgen Wothke
* version 0.94
*/

#include <string.h> 

#include "hacks.h"

#include "memory.h"
#include "vic.h"

/*
* Immigrant_Song.sid: hardcore badline timing
*/
static void patchImmigrantSongIfNeeded(uint16_t init_addr) {
	// the timing of this song is absolutely unforgiving.. if NMI fires 1 cycle to
	// soon or one cycle to late then it will eventually hit a badline
	uint8_t pattern[] = {0xd1,0x0b,0x20,0xcc,0x0c,0x20,0x39};
	
	if ((init_addr == 0x080d) && memMatch(0x0826, pattern, 7)) {	
		// just disable the display (wich is causing the trouble in the first place)
		memWriteRAM(0x0821, 0x0b);	
	}
}

/*
* Thats_All_Folks.sid: still some problem with nested IRQs
* (todo: emu should be able to cope with this)
*/
static void patchThatsAllFolksIfNeeded(uint16_t init_addr) {
	if ((init_addr == 0x4800) && (memReadRAM(0x4806)==0x48)) {	
		memWriteRAM(0x461C, 0x4c);	// always use main loop at $4685
		memWriteRAM(0x461D, 0x85);
		memWriteRAM(0x461E, 0x46);		
	}
}

static uint8_t disabledStun(uint8_t x, uint16_t y, uint8_t cpr) {
	// completely disable regular badline logic
	return 0;
}

/*
* Utopia_tune_6.sid: hardcore sprite timing
*/
static void patchUtopia6IfNeeded(uint16_t init_addr) {
	// timing critical song that depends on sprite-delays.. without the proper
	// bad-sprite-cycles the timing is off, and the D011-based badline
	// avoidance logic actually causes MORE badlines (slowing the song down).
		
	uint8_t pattern[] = {0xce, 0x16, 0xd0, 0xee, 0x16, 0xd0};
	if ((init_addr == 0x9200) && memMatch(0x8b05, pattern, 6)) {
		vicSetStunImpl(&disabledStun);

		// disable IRQ ACKN.. causing the IRQ handler to be immediately
		// called again (did not check why this is needed)
		memWriteRAM(0x8E49, 0x0);
	}
}

static uint8_t swallowStun(uint8_t x, uint16_t y, uint8_t cpr) {
	// timing with all 8 sprites active on the line:
	// sprites 3-7 use 10 cycles starting at cycle 0
	// sprites 0-2 use 6 cycles at very end of line
	// before sprite 0 there is 3 cycle "stun on read"
	
	if (memReadIO(0xd015) == 0xff) {
		if (y < 0x10 || y > (0x10 + 0x29*6 + 1)) {
			// see used sprite Y positions of 6 lines of sprites
		} else {
			if ((x < 10)) {
				return 2;
			}
			uint8_t s= cpr - 4 - 3;	// should be 6 not 4! (but sounds better like this)
			if ((x >= s)) {
				if ((x >= (s+3))) {
					return 2;
				} else {
					return 1;
				}
			}
		}
	}
	return 0;
}

static void patchSwallowIfNeeded(uint16_t init_addr) {
	// Comaland_tune_3.sid & Fantasmolytic_tune_2.sid
	
	// timing critical song that depends on sprite-delays.. switches
	// between 0 (0 added bad-cycles) and 8 sprites (~17 added bad cycles)
	// sprites are not shown on all lines, i.e. there are lines without
	// the slowdown (regular badlines seem to be irrelevant here)
	
	uint8_t pattern1[] = {0x8E,0x16,0xD0,0xA5,0xE0,0x69,0x29};
	uint8_t pattern2[] = {0x8E,0x16,0xD0,0xA5,0xC1,0x69,0x29};
	if ((init_addr == 0x2000) &&
		(memMatch(0x28C8, pattern1, 7) || memMatch(0x28C8, pattern2, 7))) {
		vicSetStunImpl(&swallowStun);
	}
}

void hackIfNeeded(uint16_t init_addr) {
	patchThatsAllFolksIfNeeded(init_addr);
	
	patchImmigrantSongIfNeeded(init_addr);
	
	patchUtopia6IfNeeded(init_addr);
	
	patchSwallowIfNeeded(init_addr);
}


