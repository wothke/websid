#ifndef TINYRSID_SIDENGINE_H
#define TINYRSID_SIDENGINE_H

#include "defines.h"

#define DIGI_BUF_SIZE 1000

// when recording original digi samples we mark the  timestamps using the below "producer" specific bits
#define NMI_OFFSET_MASK		(0x1 << 24)
#define IRQ_OFFSET_MASK 	(0x1 << 25)
#define MAIN_OFFSET_MASK 	(0x1 << 26)

#define FLAG_N 128
#define FLAG_V 64
#define FLAG_B 16
#define FLAG_D 8
#define FLAG_I 4
#define FLAG_Z 2
#define FLAG_C 1

// fixme restructure code / avoid to expose all these internals..
extern unsigned int sDigiCount;
extern unsigned long sDigiTime[DIGI_BUF_SIZE];	
extern unsigned char sDigiVolume[DIGI_BUF_SIZE];
extern unsigned int sOverflowDigiCount;
extern unsigned long sOverflowDigiTime[DIGI_BUF_SIZE];
extern unsigned char sOverflowDigiVolume[DIGI_BUF_SIZE];
extern unsigned long sCycles;
extern unsigned short pc;
extern word sInitAddr, sPlayAddr;
extern unsigned int sProgramMode;
extern unsigned char a,x,y,s,p;
extern unsigned char sMainLoopOnlyMode;
extern unsigned char sSynthDisabled;
extern unsigned long sTodInMillies;

void initC64Rom();
void synth_init(unsigned long mixfrq);

void reInitEngine();
unsigned short LoadSIDFromMemory(void *pSidData, unsigned short *load_addr, unsigned short *load_end_addr, 
					unsigned short *init_addr, unsigned short *play_addr, unsigned char *subsongs, unsigned char *startsong, unsigned long *speed, unsigned short size);
void cpuParse(void);
void cpuReset(void);
void push(unsigned char val);
unsigned char isIrqBlocked();
unsigned char getmem(unsigned short addr);
void sidPoke(int reg, unsigned char val);
void synth_render (dword *buffer, unsigned long len);
void memSet(unsigned char *mem, char val, int len);

#ifdef DEBUG
void trace(unsigned short addr, char *text);
#endif


#endif