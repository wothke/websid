#ifndef TINYRSID_RSIDENGINE_H
#define TINYRSID_RSIDENGINE_H

#define MEMORY_SIZE 65536
extern unsigned char memory[MEMORY_SIZE];

#define KERNAL_SIZE 8192
extern unsigned char kernal_rom[KERNAL_SIZE];	// mapped to $e000-$ffff

#define IO_AREA_SIZE 4096
extern unsigned char io_area[IO_AREA_SIZE];	// mapped to $d000-$dfff

// setup/restart
void resetRSID();

unsigned char isRsid();
unsigned char isPsid();

int isRasterDrivenPsid();
void setPsidMode(unsigned char m);
unsigned int getProgramMode();

unsigned char isMainLoopMode();
unsigned char callMain(unsigned short npc, unsigned char na, unsigned long startTime, signed long cycleLimit);
int processOneScreen(short * synthBuffer, unsigned char *digiBuffer, unsigned long cyclesPerScreen, unsigned int samplesPerCall);
void setProgramStatus(unsigned char s);

// TOD simulation
unsigned long getTimeOfDayMillis();
void updateTimeOfDay10thOfSec(unsigned char value);
void updateTimeOfDaySec(unsigned char value);

// util
void memSet(unsigned char *mem, char val, int len);

#endif