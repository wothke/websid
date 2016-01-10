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

// SID register definition
struct s6581 {
    struct sidvoice {
        unsigned short freq;
        unsigned short pulse;
        unsigned char wave;
        unsigned char ad;
        unsigned char sr;
    } v[3];
    unsigned char ffreqlo;
    unsigned char ffreqhi;
    unsigned char res_ftv;
    unsigned char ftp_vol;
};

extern struct s6581 sid;

// init/reset
void synth_init(unsigned long mixfrq);

// used by CPU side to interact with SID
void sidPoke(int reg, unsigned char val);

// get playback rate (e.g. 44100 samples/sec)
unsigned long getSampleFrequency();

// may be used from hack.c
void setCiaNmiVectorHack();

// used for digis..
void setMute(unsigned char voice);

// exposed rsidengine.c FIXME move all the CPU stuff into one place
// ---------->
extern unsigned long sCycles;
extern unsigned short pc;
extern unsigned char a,x,y,s,p;
extern unsigned char sSynthDisabled;
void incFrameCount();
void cpuParse(void);
void cpuReset(void);
void push(unsigned char val);
unsigned char isIrqBlocked();
unsigned char getmem(unsigned short addr);
void synth_render (short *buffer, unsigned long len);
// <-------------

#ifdef DEBUG
void trace(unsigned short addr, char *text);
#endif

#endif