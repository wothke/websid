/*
* System level emulation stuff.
* 
* WebSid (c) 2020 Jürgen Wothke
* version 0.94
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
 
#ifndef WEBSID_SYS_H
#define WEBSID_SYS_H

#include "base.h"

// setup
void 		sysReset();

// system clock related
uint32_t	sysCycles();		// system cycles since starting the song

void 		sysClock();
void		sysClockOpt();
uint8_t		sysClockTimeout();
#ifdef TEST
uint8_t		sysClockTest();
#endif
uint32_t	sysGetClockRate(uint8_t is_ntsc);

// hack used for digi handling only (todo: check if these might no longer be needed)
void	 	sysSetNMIMarker(uint8_t m);
uint8_t 	sysCheckNMIMarker();

#endif