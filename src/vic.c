/*
* Poor man's emulation of the C64's VIC-II (Video-Interface-Chip).
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Different system versions:
* 	PAL:  312 rasters with 63 cycles, system clock: 985248Hz,  frame rate: 50.124542Hz,  19656 cycles per frame
*	NTSC: 263 rasters with 65 cyles,  system clock: 1022727Hz, frame rate: 59.8260895Hz, 17095 cycles per frame
*
* 200 visible lines/25 text lines..	
*
* trivia: "Normally the VIC always uses the 1st phase of each clock cycle (for bus access) while cpu uses the 2nd phase.
* During badline cycles VIC may completely take over 40-43 cycles and stun the CPU. Technically the CPU is told on its 
* "RDY" pin (HIGH) if everything runs normal or if a badline mode is about to start. (The CPU then may still complete 
* its write ops - for a maximum of 3 cycles before it pauses to let VIC take over the bus.) Within an affected
* raster line the "stun phase" starts at cycle 12 and lasts until cycle 55."
*
* useful links:
*
*  - Christian Bauer's: "The MOS 6567/6569 video controller (VIC-II) and its application in the Commodore 64"
*
* LIMITATIONS: Enabled sprites (see D015) normally cause the same kind of "CPU stun" effect as the "bad line" 
*              but this effect has not been implemented here (it does not seem to be relevant for many songs). 
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#include "vic.h"

#include "memory.h"
#include "cpu.h"
#include "env.h"


static double _fps;
static uint8_t _cycles_per_raster;
static uint16_t _lines_per_screen;
static uint32_t _total_cycles_per_screen;

static uint8_t _x;	// in cycles
static uint16_t _y;	// in rasters

static uint32_t _raster_latch;	// optimization: D012 + D011-bit 8 combined

static int16_t _init_raster_PSID;

double vicFPS() {
	return _fps;
}
uint16_t vicLinesPerScreen() {
	return _lines_per_screen;
}
uint32_t vicCyclesPerScreen() {
	return _total_cycles_per_screen;
}


void vicSetModel(uint8_t ntsc_mode) {
	// emulation only supports new PAL & NTSC model (none of the special models)
		
	_x= _y= 0;
	
	// note: clocking is derived from the targetted video standard.
	// 14.31818MHz (NTSC) respectively of 17.734475MHz (PAL) - divide by 4 for respective 
	// color subcarrier: 3.58Mhz NTSC,  4.43MHz PAL.
	// the 8.18MHz (NTSC) respectively 7.88MHz (PAL) "dot clock" is derived from the above
	// system clock is finally "dot clock" divided by 8
	if(ntsc_mode) {
		// NTSC
		_fps= 59.8260895;
		_cycles_per_raster= 65;
		_lines_per_screen = 263;	// with  520 pixels
//		_clockRate= 1022727;	// system clock (14.31818MHz/14)
	} else {
		// PAL
		_fps= 50.124542;
		_cycles_per_raster= 63;	
		_lines_per_screen = 312;	// with 504 pixels
//		_clockRate= 985249;		// system clock (17.734475MHz/18)
	}

	// init to very end so that next tick will create a raster 0 IRQ...
	_x= _cycles_per_raster-1;
	_y= _lines_per_screen-1;
	
		
	// NTSC: 17095	/ PAL: 19656		
	_total_cycles_per_screen= _cycles_per_raster * vicLinesPerScreen();
	
	_init_raster_PSID= -1;
}

static void checkIRQ() {
	// "The test for reaching the interrupt raster line is done in cycle 0 of 
	// every line (for line 0, in cycle 1)."	
	
	if (_y == _raster_latch) {
		uint8_t latch= memReadIO(0xd019) | 0x1;	// always signal (test case: Wally Beben songs that use CIA 1 timer for IRQ but check for this flag)

		uint8_t interrupt_enable= memReadIO(0xd01a) & 0x1;			
		if (interrupt_enable) {
			latch |= 0x80;	// signal VIC interrupt			
		}
		
		memWriteIO(0xd019, latch);
	}
}

void vicClock() {
	if (!_x && !_y) {	// special case: in line 0 it is cycle 1		
		checkIRQ();
	}
	_x+= 1;	
	
	if (_x >= _cycles_per_raster) {
		_x= 0;
		_y+= 1;

		if (_y >= _lines_per_screen) {
			_y= 0;			
		}
				
		if (_y) checkIRQ();	// normal case: check in cycle 0				
	}
}

uint8_t vicIRQ() {	
	return memReadIO(0xd019) & 0x80;
}

/*
	 "A Bad Line Condition is given at any arbitrary clock cycle, if at the
	 negative edge of ø0 at the beginning of the cycle RASTER >= $30 and RASTER
	 <= $f7 and the lower three bits of RASTER are equal to YSCROLL and if the
	 DEN (display enable: $d011, bit 4) bit was set during an arbitrary cycle of raster line $30."
	 
	 KNOWN LIMITATION: Additional 2 cycles per sprite may be needed - which isn't implemented here.
*/
uint8_t vicStunCPU() {
	const uint8_t ctrl= memReadIO(0xd011);
	if (ctrl & 0x10) {	// display enabled
		if ((_y >= 0x30) && (_y <= 0xf7) && ((ctrl & 0x7) == (_y & 0x7))) {
			/*  this is a badline:
				During cycles 15-54(incl) the CPU is *always* completely blocked - BUT it may be blocked up to 3 cycles earlier
				depending on the first used sub-instruction "read-access", i.e. from cycle 12 the CPU is stunned at its first "read-access"
				(i.e. the write-access of current OP is allowed to complete.. usually at the end of OP - max 3 consecutive 
				"write" cycles in any 6502 OP)
				=> this means that "OPs in progress" might be stunned in the middle of the execution, i.e. OP has just been started 
				and then is stunned before it can read the data that is needs.
			*/
			if ((_x >= 11) && (_x <= 53)) {
				if ((_x >= 14)) {
					return 2;	// stun completely
				} else {
					return 1;	// stun on read
				}
			}
		}
	}
	
	// "displayed sprites" cause a similar effect of stunning the CPU - "stealing" ~2 cycles for one sprite
	// and up to ~19 cycles for all 8 sprites (if they are shown on the specific line). Like for the "badline" there 
	// is a 3 cycle "stun" period (during which more or less cycles are lost depending on the current OP) before the bus 
	// is completely blocked for the CPU. For more details see "Missing Cycles" by Pasi 'Albert' Ojala
	
	// note: The limited benefits do not seem to be worth the extra implementation & runtime cost, since very few songs 
	// are actually using this (maybe some hardcode timing demo showing off - e.g. Vicious_SID_2-15638Hz.sid, Fantasmolytic_tune_2).	
	return 0;
}

static void cacheRasterLatch() {
	_raster_latch= memReadIO(0xd012) + (((uint16_t)memReadIO(0xd011)&0x80)<<1);
}

void vicReset(uint8_t is_rsid, uint8_t ntsc_mode) {
	
	vicSetModel(ntsc_mode); 
	
	// by default C64 is configured with CIA1 timer / not raster irq
			
	memWriteIO(0xd019, 0x01); 	// presumable "the machine" has been running for more than one frame before the prog is run
	memWriteIO(0xd01a, 0x00); 	// raster irq not active
	
	if (is_rsid) {
		// RASTER default to 0x137 (see real HW) 
		memWriteIO(0xd011, 0x9B);
		memWriteIO(0xd012, 0x37); 	// raster at line x
	} else {
		memWriteIO(0xd011, 0x1B);
		memWriteIO(0xd012, 0x0); 	// raster must be below 0x100
	}
	cacheRasterLatch();
}

// -----------------------------  VIC I/O -------------------------------------------

static void writeD019(uint8_t value) {
	// ackn vic interrupt, i.e. a setting a bit actually clears it
	
	// note: :some players use "INC D019" (etc) to ackn the interrupt (all Read-Modify-Write instructions write the
	// original value before they write the new value, i.e. the intermediate write does the clearing..)
	// (see cpu.c emulation)
	
	// "The bit 7 in the latch $d019 reflects the inverted state of the IRQ output of the VIC.", i.e. if the
	// source conditions clear, so does the overall output. XXX when is this status updated?
	
	// test-case: some songs only clear the "RASTER" but not the "IRQ" flag (e.g. Ozone.sid - which takes about 12000 cycles)
	
	// PROBLEM: Utopia_tune_6.sid (takes about 25000 cycles) uses the same ACKN as Ozone.sid but it "expects" the IRQ to be
	// immediately retriggered.. what's the difference in this long running IRQ handler? XXX some special case does not
	// seem to be handled correctly yet!

	
	uint8_t v=  memReadIO(0xd019);
	v = v&(~value);					// clear (source) flags directly
	
	if (!(v & 0xf)) {
		v &= 0x7f; 		// all sources are gone.. so IRQ flag should also be cleared
	}
	memWriteIO(0xd019, v);
}

static void writeD01A(uint8_t value) {	
	memWriteIO(0xd01A, value);
		
	if (value & 0x1) {
		// check if RASTER condition has already fired previously
		uint8_t d= memReadIO(0xd019);
		if (d & 0x1) {
			memWriteIO(0xd019, d | 0x80); 	// signal VIC interrupt
		}
	}
}

void vicWriteMem(uint16_t addr, uint8_t value) {
	switch (addr) {
		case 0xd011:
		case 0xd012:
			memWriteIO(addr, value);
			cacheRasterLatch();
			break;
		case 0xd019:
			writeD019(value);
			break;
		case 0xd01A:
			writeD01A(value);
			break;
		default:
			memWriteIO(addr, value);
	}
}

uint8_t vicReadMem(uint16_t addr) {
	switch (addr) {
		case 0xd011:
			return (memReadIO(0xd011) & 0x7f) | ((_y & 0x100) >> 1);
		case 0xd012:					
			return  _y & 0xff;
		case 0xd019:
			return memReadIO(0xd019);
	}
	return memReadIO(addr);
}