/*
* Poor man's emulation of the C64's VIC raster interrupts.
*
* <p>Tiny'R'Sid (c) 2012 J.Wothke
* <p>version 0.81
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#include "vic.h"

#include "memory.h"
#include "cpu.h"
#include "env.h"


static int32_t _timerCarryOverIrq= -1;		// irq pos calculated on previous screen
static int16_t _lastRasterInterrupt= -1;	
static uint16_t _currentRasterPos= 0;		// 	simulated "read" raster position (see d012/d011)
static uint32_t _lastRelativeCyclePos= 0;	// sim progress of rasterline during current code execution
static uint32_t _remainingCyclesThisRaster= 0;

// next interrupt not on this screen; use value bigger than any 16-bit counter for easy comparison
static uint32_t _failMarker;

void vicReset(uint8_t isRsid, uint32_t f) {
	_failMarker= f;

	if (isRsid) {	
		// by default C64 is configured with CIA1 timer / not raster irq
		memWriteIO(0xd01a, 0x00); 	// raster irq not active
		memWriteIO(0xd011, 0x1B);
		memWriteIO(0xd012, 0x00); 	// raster at line x
	}
	
	_timerCarryOverIrq= -1;
	_lastRasterInterrupt= -1;
	_currentRasterPos= 0;
	_lastRelativeCyclePos= 0;
	_remainingCyclesThisRaster= 0;
}

static void setCurrentRasterPos(uint32_t cycleTime) {
// FIXME maybe badlines should be considered here?
	_currentRasterPos= ((uint32_t)((float)cycleTime/envCyclesPerRaster()))%envLinesPerScreen();	
}

static void incCurrentRasterPos() {
	_currentRasterPos+=1;
	if (_currentRasterPos == envLinesPerScreen()) {
		_currentRasterPos= 0;
	}
}

void vicStartRasterSim(uint32_t rasterPosInCycles) {
	setCurrentRasterPos(rasterPosInCycles);
	
	_remainingCyclesThisRaster=rasterPosInCycles-(_currentRasterPos*envCyclesPerRaster());
	
	_lastRelativeCyclePos= cpuCycles();
}

void vicSimRasterline() {
	/* 
	songs like Digital_Music.sid, Thats_All_Folks.sid or Uwe Anfang's stuff use busy-wait 
	for specific rasterlines from their IRQ and Main prog code. in order to avoid endless 
	waiting the progress of the raster is simulated here..
	
	the incremental impl used below seems to work best for the "main prog" handling.. were 
	we do not have the correct timing.. (i.e. the attempt to directly derive a d012 position 
	from the start and current execution time failed )
	*/	
	long cdiff= cpuCycles()-_lastRelativeCyclePos;
	if (cdiff > _remainingCyclesThisRaster) {
		incCurrentRasterPos();					// sim progress of VIC raster line..
		_remainingCyclesThisRaster+= envCyclesPerRaster();	// badlines korrektur?
	}	
	_remainingCyclesThisRaster-= cdiff;
	_lastRelativeCyclePos= cpuCycles();	
}

static void setD019(uint8_t value) {
	// ackn vic interrupt, i.e. a set bit actually clear that bit
	memWriteIO(0xd019, memReadIO(0xd019)&(~value));
}

static void signalVicIrq() {	
	// bit 7: IRQ triggered by VIC
	// bit 0: source was rasterline

	memWriteIO(0xd019, memReadIO(0xd019)|0x81);
}

uint32_t lastDummyInterrupt=0;
static uint8_t getD019() {
	if ((cpuGetProgramMode() == MAIN_OFFSET_MASK) && !(memReadIO(0xd01a) & 0x1)) {
		// might be a main loop polling for interrupt (see Alter_Digi_Piece.sid) -
		// hack: just create one dummy interrupt per screen
		if ((cpuTotalCycles()- lastDummyInterrupt) > envCyclesPerScreen()) {
			lastDummyInterrupt= cpuTotalCycles();	// try to sim a raster interrupt once a screen
			memWriteIO(0xd019, memReadIO(0xd019)|0x1);
			return 1;
		} else {
			return 0;			
		}
	}
	return memReadIO(0xd019);
}

static uint8_t getCurrentD012() {
	return _currentRasterPos & 0xff;
}
static uint8_t getCurrentD011() {
	return (_currentRasterPos & 0x100) >> 1;
}

uint16_t vicGetRasterline() {
	uint16_t rasterline= memReadIO(0xd012) + (((uint16_t)memReadIO(0xd011)&0x80)<<1);
	return rasterline;
}

static uint16_t getCycleTime(uint16_t rasterTime) {
	return rasterTime * envCyclesPerRaster();
}

uint8_t vicIsIrqActive() {
//	return ((memReadIO(0xd01a)&0x1) == 1);  // old imp
	return !cpuIrqFlag() && ((memReadIO(0xd01a)&0x1) == 1);	// XXX needs to be tested
}

/*
* Gets the next 'timer' based on next raster interrupt which will occur on the current screen.
* @return _failMarker if no event on the  current screen
*/
uint32_t vicForwardToNextRaster() {
	if (!vicIsIrqActive()) {
		return _failMarker;
	}
	int32_t timer= 0;
	if (_timerCarryOverIrq >=0) {
		timer= _timerCarryOverIrq;
		_timerCarryOverIrq= -1;
	} else {
		uint16_t nextRasterline= vicGetRasterline(); 
		
		if (_lastRasterInterrupt<0) {	// first run
			timer= getCycleTime(nextRasterline); 
		} else {
			int16_t lineDelta= nextRasterline - _lastRasterInterrupt;
			if (lineDelta > 0){
				timer= getCycleTime(lineDelta);					// next IRQ on same page refresh			
			} else {
				_timerCarryOverIrq= getCycleTime(nextRasterline);// IRQ on next screen
				timer= _failMarker; 								// no event on this screen
			}			
		}
		_lastRasterInterrupt= nextRasterline;

	}
	if (timer != _failMarker) {
		signalVicIrq();	// in case some IRQ routine is checking.. e.g. Galdrumway.sid	
	} 

	return timer;
}


// -----------------------------  VIC I/O -------------------------------------------

void vicWriteMem(uint16_t addr, uint8_t value) {
	switch (addr) {
		case 0xd019:
			setD019(value);
		default:
			memWriteIO(addr, value);
	}
}

uint8_t vicReadMem(uint16_t addr) {
	switch (addr) {
		// VIC
		case 0xd011:
			return getCurrentD011();
		case 0xd012:					
			return getCurrentD012();
		case 0xd019:
			return getD019();
	}
	return memReadIO(addr);
}