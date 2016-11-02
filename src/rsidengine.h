#ifndef TINYRSID_RSIDENGINE_H
#define TINYRSID_RSIDENGINE_H

#include "defines.h"


#define MEMORY_SIZE 65536
extern uint8_t memory[MEMORY_SIZE];

#define KERNAL_SIZE 8192
extern uint8_t kernal_rom[KERNAL_SIZE];	// mapped to $e000-$ffff

#define IO_AREA_SIZE 4096
extern uint8_t io_area[IO_AREA_SIZE];	// mapped to $d000-$dfff

// setup/restart
void resetRSID();

uint8_t isRsid();
uint8_t isPsid();

int8_t isRasterDrivenPsid();
void setPsidMode(uint8_t m);
uint32_t getProgramMode();
uint8_t isMainLoopPolling();

uint8_t isMainLoopMode();
uint8_t callMain(uint16_t npc, uint8_t na, uint32_t startTime, int32_t cycleLimit);
uint8_t processOneScreen(int16_t * synthBuffer, uint8_t *digiBuffer, uint32_t cyclesPerScreen, uint16_t samplesPerCall);
void setProgramStatus(uint8_t s);

// TOD simulation
uint32_t getTimeOfDayMillis();
void updateTimeOfDay10thOfSec(uint8_t value);
void updateTimeOfDaySec(uint8_t value);

// util
void memSet(uint8_t *mem, int8_t val, uint32_t len);

#endif