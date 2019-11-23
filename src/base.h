#ifndef WEBSID_BASE_H
#define WEBSID_BASE_H

typedef signed char int8_t;
typedef unsigned char uint8_t;

// 16-bit must be exactly 16-bit!
typedef short int16_t;
typedef unsigned short uint16_t;

typedef signed long int32_t;
typedef unsigned long uint32_t;

// debug: trace what is happending on ADSR level
//#define DBG_TRACE_ADSR	0	// ID of voice to trace


/*
* diagnostic information for GUI use
*/
typedef enum {
	DigiNone=0,
	DigiD418=1,			// legacy 4-bit approach
	DigiMahoneyD418=2,	// Mahoney's "8-bit" D418 samples
	
	// all the filterable digi types
	DigiFM=3,			// 8-bit frequency modulation; e.g. Vicious_SID_2-15638Hz.sid, LMan - Vortex.sid, etc
	DigiPWM= 4,			// older PWM impls, e.g. Bla_Bla.sid, Bouncy_Balls_RCA_Intro.sid
	DigiPWMTest=5		// new test-bit based PWM; e.g. Wonderland_XII-Digi_part_1.sid, GhostOrGoblin.sid, etc
} DigiType;



#endif
