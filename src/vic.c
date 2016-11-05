/*
* Poor man's emulation of the C64's VIC raster interrupts.
*
* <p>Tiny'R'Sid (c) 2012 J.Wothke
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#include "nanocia.h"
#include "sidengine.h"
#include "rsidengine.h"
#include "sidplayer.h"

static int32_t sTimerCarryOverIrq= -1;	// irq pos calculated on previous screen
static int16_t sLastRasterInterrupt= -1;	
static uint16_t sCurrentRasterPos= 0;		// 	simulated "read" raster position (see d012/d011)
static uint32_t sLastRelativeCyclePos= 0;		// sim progression of rasterline during current code execution..
static uint32_t sRemainingCyclesThisRaster= 0;

void resetVic() {
	sTimerCarryOverIrq= -1;
	sLastRasterInterrupt= -1;
	sCurrentRasterPos= 0;
	sLastRelativeCyclePos= 0;
	sRemainingCyclesThisRaster= 0;
}

void setCurrentRasterPos(uint32_t cycleTime) {
// FIXME maybe badlines should be considered here?
	sCurrentRasterPos= ((uint32_t)((float)cycleTime/getCyclesPerRaster()))%getLinesPerScreen();	
}

void incCurrentRasterPos() {
	sCurrentRasterPos+=1;
	if (sCurrentRasterPos == getLinesPerScreen()) {
		sCurrentRasterPos= 0;
	}
}

void initRasterlineSim(uint32_t rasterPosInCycles) {
	setCurrentRasterPos(rasterPosInCycles);
	
	sRemainingCyclesThisRaster=rasterPosInCycles-(sCurrentRasterPos*getCyclesPerRaster());
	
	sLastRelativeCyclePos= sCycles;
}

void simRasterline() {
	// songs like Digital_Music.sid, Thats_All_Folks.sid or Uwe Anfang's stuff use busy-wait 
	// for specific rasterlines from their IRQ and Main prog code. in order to avoid endless waiting
	// the progress of the raster is simulated here..
	
	// the incremental impl used below seems to work best for the "main prog" handling.. were 
	// we do not have the correct timing.. (i.e. the attempt to directly derive a d012 position 
	// from the start and current execution time failed )
		
	long cdiff= sCycles-sLastRelativeCyclePos;
	if (cdiff > sRemainingCyclesThisRaster) {
		incCurrentRasterPos();					// sim progress of VIC raster line..
		sRemainingCyclesThisRaster+= getCyclesPerRaster();	// badlines korrektur?
	}	
	sRemainingCyclesThisRaster-= cdiff;
	sLastRelativeCyclePos= sCycles;	
	
	
/*	
	uint32_t progress= sCycles-sLastRelativeCyclePos;
	if (progress >= getCyclesPerRaster()) {
		incCurrentRasterPos();					// sim progress of VIC raster line..

		uint32_t overflow= progress - getCyclesPerRaster();
		sLastRelativeCyclePos= sCycles - overflow;
	}
	
*/
}

void setD019(uint8_t value) {
	// ackn vic interrupt, i.e. a set bit actually clear that bit	
	io_area[0x0019]&= (~value);
}

uint8_t getD019() {
	return io_area[0x019];
}

void signalVicIrq() {	
	// bit 7: IRQ triggered by VIC
	// bit 0: source was rasterline

	io_area[0x0019] |= 0x81;
}

uint8_t getCurrentD012() {
	return sCurrentRasterPos & 0xff;
}
uint8_t getCurrentD011() {
	return (sCurrentRasterPos & 0x100) >> 1;
}

uint16_t getRasterlineTimer() {
	uint16_t rasterline= io_area[0x0012] + (((uint16_t)io_area[0x0011]&0x80)<<1);	// io_area maps to $d000..

	return rasterline;
}

static uint16_t getCycleTime(uint16_t rasterTime) {
	return rasterTime * getCyclesPerRaster();
}

uint8_t isRasterIrqActive() {
	return ((io_area[0x001a]&0x1) == 1);
}

/*
* Gets the next 'timer' based on next raster interrupt which will occur on the current screen.
* @return NO_INT if no event on the  current screen
*/
uint32_t forwardToNextRasterTimer() {
	if (!isRasterIrqActive()) {
		return NO_INT;
	}
	int32_t timer= 0;
	if (sTimerCarryOverIrq >=0) {
		timer= sTimerCarryOverIrq;
		sTimerCarryOverIrq= -1;
	} else {
		uint16_t nextRasterline= getRasterlineTimer(); 
		
		if (sLastRasterInterrupt<0) {	// first run
			timer= getCycleTime(nextRasterline); 
		} else {
			int16_t lineDelta= nextRasterline - sLastRasterInterrupt;
			if (lineDelta > 0){
				timer= getCycleTime(lineDelta);								// next IRQ on same page refresh			
			} else {
				sTimerCarryOverIrq= getCycleTime(nextRasterline);			// IRQ on next screen
				timer= NO_INT; 													// no event on this screen
			}			
		}
		sLastRasterInterrupt= nextRasterline;

	}
	if (timer != NO_INT) {
		signalVicIrq();	// in case some IRQ routine is checking.. e.g. Galdrumway.sid	
	} 

	return timer;
}
