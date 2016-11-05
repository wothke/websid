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


static int32_t timerCarryOverIrq= -1;		// irq pos calculated on previous screen
static int16_t lastRasterInterrupt= -1;	
static uint16_t currentRasterPos= 0;		// 	simulated "read" raster position (see d012/d011)
static uint32_t lastRelativeCyclePos= 0;	// sim progress of rasterline during current code execution
static uint32_t remainingCyclesThisRaster= 0;

// next interrupt not on this screen; use value bigger than any 16-bit counter for easy comparison
static uint32_t failMarker;

void vicReset(uint32_t f) {
	failMarker= f;
	
	timerCarryOverIrq= -1;
	lastRasterInterrupt= -1;
	currentRasterPos= 0;
	lastRelativeCyclePos= 0;
	remainingCyclesThisRaster= 0;
}

static void setCurrentRasterPos(uint32_t cycleTime) {
// FIXME maybe badlines should be considered here?
	currentRasterPos= ((uint32_t)((float)cycleTime/envCyclesPerRaster()))%envLinesPerScreen();	
}

static void incCurrentRasterPos() {
	currentRasterPos+=1;
	if (currentRasterPos == envLinesPerScreen()) {
		currentRasterPos= 0;
	}
}

void vicStartRasterSim(uint32_t rasterPosInCycles) {
	setCurrentRasterPos(rasterPosInCycles);
	
	remainingCyclesThisRaster=rasterPosInCycles-(currentRasterPos*envCyclesPerRaster());
	
	lastRelativeCyclePos= cpuCycles();
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
	long cdiff= cpuCycles()-lastRelativeCyclePos;
	if (cdiff > remainingCyclesThisRaster) {
		incCurrentRasterPos();					// sim progress of VIC raster line..
		remainingCyclesThisRaster+= envCyclesPerRaster();	// badlines korrektur?
	}	
	remainingCyclesThisRaster-= cdiff;
	lastRelativeCyclePos= cpuCycles();	
}

static void setD019(uint8_t value) {
	// ackn vic interrupt, i.e. a set bit actually clear that bit

	memWriteIO(0xd019, memReadIO(0xd019)&(~value));
}

static uint8_t getD019() {
	return  memReadIO(0xd019);
}

static void signalVicIrq() {	
	// bit 7: IRQ triggered by VIC
	// bit 0: source was rasterline

	memWriteIO(0xd019, memReadIO(0xd019)|0x81);
}

static uint8_t getCurrentD012() {
	return currentRasterPos & 0xff;
}
static uint8_t getCurrentD011() {
	return (currentRasterPos & 0x100) >> 1;
}

uint16_t vicGetRasterline() {
	uint16_t rasterline= memReadIO(0xd012) + (((uint16_t)memReadIO(0xd011)&0x80)<<1);
	return rasterline;
}

static uint16_t getCycleTime(uint16_t rasterTime) {
	return rasterTime * envCyclesPerRaster();
}

uint8_t vicIsIrqActive() {
	return ((memReadIO(0xd01a)&0x1) == 1);
}

/*
* Gets the next 'timer' based on next raster interrupt which will occur on the current screen.
* @return failMarker if no event on the  current screen
*/
uint32_t vicForwardToNextRaster() {
	if (!vicIsIrqActive()) {
		return failMarker;
	}
	int32_t timer= 0;
	if (timerCarryOverIrq >=0) {
		timer= timerCarryOverIrq;
		timerCarryOverIrq= -1;
	} else {
		uint16_t nextRasterline= vicGetRasterline(); 
		
		if (lastRasterInterrupt<0) {	// first run
			timer= getCycleTime(nextRasterline); 
		} else {
			int16_t lineDelta= nextRasterline - lastRasterInterrupt;
			if (lineDelta > 0){
				timer= getCycleTime(lineDelta);					// next IRQ on same page refresh			
			} else {
				timerCarryOverIrq= getCycleTime(nextRasterline);// IRQ on next screen
				timer= failMarker; 								// no event on this screen
			}			
		}
		lastRasterInterrupt= nextRasterline;

	}
	if (timer != failMarker) {
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