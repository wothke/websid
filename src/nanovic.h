#ifndef TINYRSID_NANOVIC_H
#define TINYRSID_NANOVIC_H

#include "defines.h"

void resetVic();
void initRasterlineSim(uint32_t rasterPosInCycles);
void simRasterline();
uint16_t getRasterlineTimer();
void setD019(uint8_t value);
uint8_t getD019();
uint8_t isRasterIrqActive();
uint32_t forwardToNextRasterTimer();
uint8_t getCurrentD011();
uint8_t getCurrentD012();


#endif