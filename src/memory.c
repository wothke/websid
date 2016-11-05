/*
 * This contains everything to do with the emulation of memory access.
 * 
 * <p>Tiny'R'Sid (c) 2016 Jürgen Wothke
 * <p>version 0.81
 * 
 * known limitation: basic-ROM specific handling not implemented...
 */

#include <string.h>
#include <stdio.h>

#include "memory.h"

#include "vic.h"
#include "cia.h"
#include "sid.h"

#define MEMORY_SIZE 65536
static uint8_t memory[MEMORY_SIZE];

#define KERNAL_SIZE 8192
static uint8_t kernal_rom[KERNAL_SIZE];	// mapped to $e000-$ffff

#define IO_AREA_SIZE 4096
static uint8_t io_area[IO_AREA_SIZE];	// mapped to $d000-$dfff


uint8_t memMatch(uint16_t addr, uint8_t *pattern, uint8_t len) {
	return !memcmp(&(memory[addr]), pattern, len);
}

inline void setMemBank(uint8_t b) {
	memory[0x0001]= b;
}

void memSetDefaultBanks(uint8_t isRsid, uint16_t initAddr, uint16_t loadEndAddr) {
	uint8_t memBankSetting= 0x37;	// default memory config: basic ROM, IO area & kernal ROM visible
	if (!isRsid) {
		// problem: some PSID init routines want to initialize registers in the IO area while others
		// actually expect to use the RAM in that area.. none of them setup the memory banks accordingly :(

		if ((initAddr >= 0xd000) && (initAddr < 0xe000)) {
			memBankSetting= 0x34;	// default memory config: all RAM
			
		} else if ((initAddr >= 0xe000)) {
			// PSIDv2 songs like IK_plus.sid, Pandora.sid use the ROM below the kernal *without* setting 0x0001
			// so obviously here we must use a default like this:
			memBankSetting= 0x35;	// default memory config: IO area visible, RAM $a000-b000 + $e000-$ffff

		} else if (loadEndAddr >= 0xa000) {
			memBankSetting= 0x36;
		} else {
			// normally the kernal ROM should be visible: e.g. A-Maz-Ing.sid uses kernal ROM routines & vectors 
			// without setting $01!
			memBankSetting= 0x37;	// default memory config: basic ROM, IO area & kernal ROM visible			
		}
	}
	setMemBank(memBankSetting);	
}

void memResetPsidBanks(uint8_t isPsid, uint16_t playAddr) {
	if (isPsid) {
		// some PSID actually switch the ROM back on eventhough their code is located there! (e.g. 
		// Ramparts.sid - the respective .sid even claims to be "C64 compatible" - what a joke) 
		
		if ((playAddr >= 0xd000) && (playAddr < 0xe000)) {
			setMemBank(0x34);
		} else if (playAddr >= 0xe000) {
			setMemBank(0x35);
		} else if (playAddr >= 0xa000) {
			setMemBank(0x36);
		} else if (playAddr == 0x0){
			// keep whatever the PSID init setup
		} else {
			setMemBank(0x37);
		}		
	}	
}

/*
* @return 0 if RAM is visible; 1 if ROM is visible
*/ 
static uint8_t isKernalRomVisible() {
	return memory[0x0001] & 0x2;
}

/*
* @return 0 if RAM is visible; 1 if IO area is visible
*/ 
static uint8_t isIoAreaVisible() {
	uint8_t bits= memory[0x0001] & 0x7;	
	return ((bits & 0x4) != 0) && (bits != 0x4);
}

uint8_t memReadIO(uint16_t addr) {
	if ((addr&0xfc00)==0xd400) {			
		return io_area[(addr&0xfc1f) - 0xd000];
	}
	return io_area[addr-0xd000];
}

void memWriteIO(uint16_t addr, uint8_t value) {
	io_area[addr - 0xd000]= value;
}

uint8_t memReadRAM(uint16_t addr) {	// FIXME used?
	return memory[addr];
}
void memWriteRAM(uint16_t addr, uint8_t value) {
	 memory[addr]= value;
}

void memCopyToRAM(uint8_t *src, uint16_t destAddr, uint32_t len) {
	memcpy(&memory[destAddr], src, len);		

}
void memCopyFromRAM(uint8_t *dest, uint16_t srcAddr, uint32_t len) {
	memcpy(dest, &memory[srcAddr], len);
}

uint8_t memGet(uint16_t addr)
{
	if (addr < 0xd000) {
		return  memory[addr];	// basic rom not implemented
	} else if ((addr >= 0xd000) && (addr < 0xe000)) {	// handle I/O area 		
		if (isIoAreaVisible()) {		
			if ((addr >= 0xd000) && (addr < 0xd400)) {
				return vicReadMem(addr);
			} else if ((addr >= 0xd400) && (addr < 0xd800)) {
				return sidReadMem(addr);
			} else if ((addr >= 0xdc00) && (addr < 0xde00)) {
				return ciaReadMem(addr);
			} 
			return memReadIO(addr);
		} else {
			// normal RAM access
			return  memory[addr];
		}
	} else {	// handle kernal ROM
		if (isKernalRomVisible()) {
			return kernal_rom[addr - 0xe000];
		} else {
			// normal RAM access
			return  memory[addr];
		}
	}
}

void memSet(uint16_t addr, uint8_t value)
{
	if ((addr >= 0xd000) && (addr < 0xe000)) {	// handle I/O area 
		if (isIoAreaVisible()) {
			if ((addr >= 0xd000) && (addr < 0xd400)) {			// vic stuff
				vicWriteMem(addr, value);
				return;
			} else if ((addr >= 0xd400) && (addr < 0xd800)) {	// SID stuff
				sidWriteMem(addr, value);
				return;
			} else if ((addr >= 0xdc00) && (addr < 0xde00)) {			// CIA timers
				ciaWriteMem(addr, value);
				return;
			}
			  
			io_area[addr - 0xd000]= value;
		} else {
			// normal RAM access
			memory[addr]=value;
		}
		
	} else {
		// normal RAM or
		// kernal ROM (even if the ROM is visible, writes always go to the RAM)
		memory[addr]=value;
	}
}

const static uint8_t irqHandlerFF48[19] ={0x48,0x8A,0x48,0x98,0x48,0xBA,0xBD,0x04,0x01,0x29,0x10,0xEA,0xEA,0xEA,0xEA,0xEA,0x6C,0x14,0x03};
const static uint8_t irqHandlerEA7E[9] ={0xAD,0x0D,0xDC,0x68,0xA8,0x68,0xAA,0x68,0x40};
const static uint8_t nmiHandlerFE43[4] ={0x78,0x6c,0x18,0x03};

void memResetKernelROM() {
	// we dont have the complete rom but in order to ensure consistent stack handling (regardless of
	// which vector the sid-program is using) we provide dummy versions of the respective standard 
	// IRQ/NMI routines..
	
    fillMem(&kernal_rom[0], 0x0, KERNAL_SIZE);

    memcpy(&kernal_rom[0x1f48], irqHandlerFF48, 19);	// $ff48 irq routine
    fillMem(&kernal_rom[0x0a31], 0xea, 0x4d);			// $ea31 fill some NOPs	
    memcpy(&kernal_rom[0x0a7e], irqHandlerEA7E, 9);	// $ea31 return sequence
    memcpy(&kernal_rom[0x1e43], nmiHandlerFE43, 4);	// $fe43 nmi handler
	
	kernal_rom[0x1ffe]= 0x48;
	kernal_rom[0x1fff]= 0xff;
		
	kernal_rom[0x1ffa]= 0x43;	// standard NMI vectors (this will point into the void at: 0318/19)
	kernal_rom[0x1ffb]= 0xfe;	

	// basic rom init routines (e.g. used by Soundking_V1.sid)
	kernal_rom[0x1D50]= 0x60;	
	kernal_rom[0x1D15]= 0x60;	
	kernal_rom[0x1F5E]= 0x60;	
		
	// kernal vector: initalise screen and keyboard (e.g. used by Voodoo_People_part_1.sid)
	kernal_rom[0x1F81]= 0x60;	
}

void memResetRAM(uint8_t isPsid) {
    fillMem(memory, 0x0, sizeof(memory));

	memory[0x0314]= 0x31;		// standard IRQ vector
	memory[0x0315]= 0xea;

	// Master_Blaster_intro.sid actually checks this:
	memory[0x00cb]= 0x40;		// no key pressed 
	
	// for our PSID friends who don't know how to properly use memory banks lets mirror the kernal ROM into RAM
	if (isPsid) {
		memcpy(&memory[0xe000], &kernal_rom[0], 0x2000);
	}
}

void memResetIO(uint32_t cyclesPerScreen, uint8_t isRsid, uint32_t failMaker) {
    fillMem(&io_area[0], 0x0, IO_AREA_SIZE);

	// Master_Blaster_intro.sid actually checks this:
	io_area[0x0c01]= 0xff;	 	// Port B, keyboard matrix rows and joystick #1

	// CIA 1 defaults	(by default C64 is configured with CIA1 timer / not raster irq)
	memWriteIO(0xdc0d, 0x81);	// interrupt control	(interrupt through timer A)
	memWriteIO(0xdc0e, 0x01); 	// control timer A: start - must already be started (e.g. Phobia, GianaSisters, etc expect it)
	memWriteIO(0xdc0f, 0x08); 	// control timer B (start/stop) means auto-restart
	memWriteIO(0xdc04, cyclesPerScreen&0xff); 	// timer A (1x pro screen refresh)
	memWriteIO(0xdc05, cyclesPerScreen>>8);
	
	if (isRsid) {	
		// by default C64 is configured with CIA1 timer / not raster irq
		memWriteIO(0xd01a, 0x00); 	// raster irq not active
		memWriteIO(0xd011, 0x1B);
		memWriteIO(0xd012, 0x00); 	// raster at line x

		// CIA 2 defaults
		memWriteIO(0xdd0e, 0x08); 	// control timer 2A (start/stop)
		memWriteIO(0xdd0f, 0x08); 	// control timer 2B (start/stop)		
	}

	ciaReset(failMaker);
	vicReset(failMaker);

	io_area[0x0418]= 0xf;					// turn on full volume	
	sidPoke(0x18, 0xf);  
}