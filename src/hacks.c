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
}


