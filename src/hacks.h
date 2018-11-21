/*
 * Call it cheating... 
 *
 * <p>Tiny'R'Sid (c) 2013 J.Wothke
 * <p>version 0.81
 * 
 * <p>The flaws of the current emulator implemetation (i.e. lack of cycle exact: CIA timer simulation, 
 * D41B register content; no support for cascading interrupts; IRQs must terminate within 1 screen refresh;
 * complete lack of badline timing corrections; etc) makes those exotic songs fail which actually depend on 
 * respective "features" (e.g. Microsleep_tune_06.sid, Lame_Tune.sid).
 *
 * <p>Search for "hack" comments to find other instances of workarounds..
 */
#ifndef TINYRSID_HACKS_H
#define TINYRSID_HACKS_H

#include "base.h"

/** 
 * This function applies a collection of add-on hacks that mitigate some of the flaws of 
 * the underlying emulator design by patching the respective songs. 
 * The hacks performed here do not always lead to good 
 * playback results, but at least they will avoid annoying player lock-ups..
 */
void hackIfNeeded(uint16_t *initAddr);	

uint8_t hackEnvFlip();

#endif