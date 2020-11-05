/*
* Performance optimization only.
*
* This violates the normal encapsulation of the "memory" impl for the 
* purpose of slightly faster access in timing critical logic.
*
* It should not normally be used and the same functionality is available
* via the regular memReadIO and memWriteIO API.
* 
* Rationale: For very small functions (e.g. getters) the overhead involved
* in a function call seems to be significant as compared to the run time
* of the actual functionality. In this context here, the problem may be somewhat
* WebAssemby specific: C normally provides the "inline" directive however
* for some reason code seems to break completely when it is used for WebAssembly
* compilation (i.e. "inline" cannot be used here). Also function call overhead
* may be higher than usual: It might be a contributing factor that some 
* WebAssembly engines seem to perform stack consistency checks before every
* function call. I similarly replaced respective small functions that are 
* critical during single-cycle-clocking in other parts of the implementation 
* (see cia and cpu). Whithout and functional changes this refactoring
* alone lead to a decrease of run time by 10%. 
* 
* <p>WebSid (c) 2020 Jürgen Wothke
* <p>version 0.94
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
 
#ifndef WEBSID_MEM_OPT_H
#define WEBSID_MEM_OPT_H

#include "base.h"

#ifdef __cplusplus
extern "C" {
#endif

extern uint8_t* _io_area;		// MUST NOT BE USED DIRECTLY!

#ifdef __cplusplus
}
#endif

#define	MEM_READ_IO(addr)\
	_io_area[addr - 0xd000]
	
#define	MEM_WRITE_IO(addr, value)\
	_io_area[addr - 0xd000] = value;

#endif