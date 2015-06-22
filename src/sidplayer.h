#ifndef TINYRSID_PLAYER_H
#define TINYRSID_PLAYER_H

extern word sInitAddr, sPlayAddr, sLoadEndAddr;

extern unsigned char sIsPSID;
extern unsigned long sIrqTimeout;
extern short * sSynthBuffer;
extern unsigned char * sDigiBuffer;

extern unsigned long sTotalCyclesPerScreen;
extern unsigned char sCyclesPerRaster;
extern unsigned int sLinesPerScreen;

int isC64compatible();
int isTimerDrivenPsid();
unsigned char getCurrentSongSpeed();
unsigned char sMainProgStatus;
#endif