#ifndef TINYRSID_HACKS_H
#define TINYRSID_HACKS_H

/*
 * Call it cheating... 
 *
 * <p>Tiny'R'Sid (c) 2013 J.Wothke
 * <p>version 0.77
 * 
 * <p>The flaws of the current emulator implemetation (i.e. lack of cycle exact: CIA timer simulation, 
 * D41B register content; no support for cascading interrupts; IRQs must terminate within 1 screen refresh;
 * complete lack of badline timing corrections; etc) makes those exotic songs fail which actually depend on 
 * respective "features" (e.g. Microsleep_tune_06.sid, Lame_Tune.sid).
 *
 * <p>This here is a collection of add-on hacks that mitigate some of the flaws of the underlying emulator
 * design. 
 *
 * <p>Search for "hack" to find other instances of workarounds..
 */


void initHacks();
 
/** 
 * This function is meant to patch the binaries of some of the affected music files so that they 
 * can be played in spite of the emulator flaws. The hacks performed here do not always lead to good 
 * playback results, but at least they will avoid annoying player lock-ups..
 */
void hackIfNeeded(uint16_t *initAddr);	


/** 
 * These functions provide support for SID voice 3 oscillator polling from within the main loop.
 */
uint8_t simReadD41B();
void simStartOscillatorVoice3(uint8_t voice, uint8_t val);


/** 
 * These functions provide support for CIA timer polling from within the main loop,
 * e.g. needed for PollyTracker based songs
 */
void simWriteTimer(uint16_t addr, uint8_t value);
uint8_t simReadICR_1();
uint8_t simReadICR_2();
uint8_t simReadCRAB(uint8_t ciaIdx, uint8_t timerIdx);

#endif