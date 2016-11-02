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
        uint16_t freq;
        uint16_t pulse;
        uint8_t wave;
        uint8_t ad;
        uint8_t sr;
    } v[3];
    uint8_t ffreqlo;
    uint8_t ffreqhi;
    uint8_t res_ftv;
    uint8_t ftp_vol;
};

extern uint8_t voiceEnableMask;

extern struct s6581 sid;
extern uint32_t  freqmul;

// init/reset
void synth_init(uint32_t mixfrq);

// used by CPU side to interact with SID
void sidPoke(uint8_t reg, uint8_t val);

// get playback rate (e.g. 44100 samples/sec)
uint32_t getSampleFrequency();

// may be used from hack.c
void setCiaNmiVectorHack();

// used for digis..
void setMute(uint8_t voice);

// exposed rsidengine.c FIXME move all the CPU stuff into one place
// ---------->
extern uint32_t sCycles;
extern uint32_t sAbsCycles;
extern uint32_t sLastFrameCycles;
extern uint16_t pc;
extern uint8_t a,x,y,s,p;
extern uint8_t sSynthDisabled;
void incFrameCount();
void cpuParse(void);
void cpuReset(void);
void push(uint8_t val);
uint8_t isIrqBlocked();
uint8_t getmem(uint16_t addr);
void synth_render (int16_t *buffer, uint32_t len);
uint8_t isMainLoopPolling();

// <-------------

#ifdef DEBUG
void trace(uint16_t addr, char *text);
#endif

#endif
