#ifndef TINYRSID_NANOVIC_H
#define TINYRSID_NANOVIC_H

void resetVic();
void initRasterlineSim(unsigned long rasterPosInCycles);
void simRasterline();
unsigned int getRasterlineTimer();
void setD019(unsigned char value);
unsigned char getD019();
int isRasterIrqActive();
unsigned long forwardToNextRasterTimer();
unsigned char getCurrentD011();
unsigned char getCurrentD012();


#endif