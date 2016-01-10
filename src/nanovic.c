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

static signed long sTimerCarryOverIrq= -1;	// irq pos calculated on previous screen
static signed int sLastRasterInterrupt= -1;	
static unsigned int sCurrentRasterPos= 0;		// 	simulated "read" raster position (see d012/d011)
static unsigned long sLastRelativeCyclePos= 0;		// sim progression of rasterline during current code execution..
static unsigned long sRemainingCyclesThisRaster= 0;

void resetVic() {
	sTimerCarryOverIrq= -1;
	sLastRasterInterrupt= -1;
	sCurrentRasterPos= 0;
	sLastRelativeCyclePos= 0;
	sRemainingCyclesThisRaster= 0;
}

void setCurrentRasterPos(unsigned long cycleTime) {
// FIXME maybe badlines should be considered here?
	sCurrentRasterPos= ((unsigned long)((float)cycleTime/getCyclesPerRaster()))%getLinesPerScreen();	
}

void incCurrentRasterPos() {
	sCurrentRasterPos+=1;
	if (sCurrentRasterPos == getLinesPerScreen()) {
		sCurrentRasterPos= 0;
	}
}

void initRasterlineSim(unsigned long rasterPosInCycles) {
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
	unsigned long progress= sCycles-sLastRelativeCyclePos;
	if (progress >= getCyclesPerRaster()) {
		incCurrentRasterPos();					// sim progress of VIC raster line..

		unsigned long overflow= progress - getCyclesPerRaster();
		sLastRelativeCyclePos= sCycles - overflow;
	}
	
*/
}

void setD019(unsigned char value) {
	// ackn vic interrupt, i.e. a set bit actually clear that bit	
	io_area[0x0019]&= (~value);
}

unsigned char getD019() {
	return io_area[0x019];
}

void signalVicIrq() {	
	// bit 7: IRQ triggered by VIC
	// bit 0: source was rasterline

	io_area[0x0019] |= 0x81;
}

unsigned char getCurrentD012() {
	return sCurrentRasterPos & 0xff;
}
unsigned char getCurrentD011() {
	return (sCurrentRasterPos & 0x100) >> 1;
}

unsigned int getRasterlineTimer() {
	unsigned int rasterline= io_area[0x0012] + (((unsigned int)io_area[0x0011]&0x80)<<1);	// io_area maps to $d000..

	return rasterline;
}

static unsigned int getCycleTime(unsigned int rasterTime) {
	return rasterTime * getCyclesPerRaster();
}

int isRasterIrqActive() {
	return ((io_area[0x001a]&0x1) == 1);
}

/*
* Gets the next 'timer' based on next raster interrupt which will occur on the current screen.
* @return NO_INT if no event on the  current screen
*/
unsigned long forwardToNextRasterTimer() {
	if (!isRasterIrqActive()) {
		return NO_INT;
	}
	signed long timer= 0;
	if (sTimerCarryOverIrq >=0) {
		timer= sTimerCarryOverIrq;
		sTimerCarryOverIrq= -1;
	} else {
		unsigned int nextRasterline= getRasterlineTimer(); 
		
		if (sLastRasterInterrupt<0) {	// first run
			timer= getCycleTime(nextRasterline); 
		} else {
			signed int lineDelta= nextRasterline - sLastRasterInterrupt;
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
