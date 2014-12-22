#ifndef TINYRSID_RSIDENGINE_H
#define TINYRSID_RSIDENGINE_H

#define MEMORY_SIZE 65536
extern unsigned char memory[MEMORY_SIZE];

#define KERNAL_SIZE 8192
extern unsigned char kernal_rom[KERNAL_SIZE];	// mapped to $e000-$ffff

#define IO_AREA_SIZE 4096
extern unsigned char io_area[IO_AREA_SIZE];	// mapped to $d000-$dfff

int isRsid();
unsigned char callMain(unsigned short npc, unsigned char na, unsigned long startTime, signed long cycleLimit);
int processOneScreen(unsigned long cyclesPerScreen, unsigned int samplesPerCall);

#endif