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
* Immigrant_Song.sid: hardcore badline timing
*/
static void patchImmigrantSongIfNeeded(uint16_t *initAddr) {
	// the timing of this song is absolutely unforgiving.. if NMI fires 1 cycle to
	// soon or one cycle to late then it will eventually hit a badline 
	uint8_t pattern[] = {0xd1,0x0b,0x20,0xcc,0x0c,0x20,0x39};
	
	if (((*initAddr) == 0x080d) && memMatch(0x0826, pattern, 7)) {	
		memWriteRAM(0x0821, 0x0b);	// just disable the display (wich is causing the trouble in the first place)
	}
}

/*
* Thats_All_Folks.sid: problem with nested IRQs
*/
static void patchThatsAllFolksIfNeeded(uint16_t *initAddr) {
	if (((*initAddr) == 0x4800) && (memReadRAM(0x4806)==0x48)) {	
		memWriteRAM(0x461C, 0x4c);	// always use main loop at $4685
		memWriteRAM(0x461D, 0x85);
		memWriteRAM(0x461E, 0x46);		
	}
}

void hackIfNeeded(uint16_t *initAddr) {
	patchThatsAllFolksIfNeeded(initAddr);
	
	patchImmigrantSongIfNeeded(initAddr);
}


