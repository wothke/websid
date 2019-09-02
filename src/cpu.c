/*
* Emulates MOS Technology 6510 CPU - as far as needed to play RSID files.
* 
* Implementation was originally based on the code from the old TinySid, but it has meanwhile been rewritten 
* almost completely:
*
*    - added impls for illegal 6510 op codes, fixed errors in V-flag calculation, added handling for 6510 addressing "bugs"
*    - correct total cycle-time calculation for ALL (used) 6510 OP codes
*    - timing of OP internal "bus write" for badline handling
*    - switched from a "per OP" based stepping to a approximated "per clock" mode (as a base for "badline" handling, etc)
*
* Interactions with external components (VIA, CIA, SID) are emulated on a cycle-by-cycle level, however there may be imprecisions
* with regard to intra-OP timing (see  "SUMMARY OF SINGLE CYCLE EXECUTION" in "MOS MCS6500 hardware manual.TXT"), e.g. if a 4 cycle 
* OP would actually perform some read in its 3rd cycle it is emulated here as if it were to do everything at the end in cycle 4.
* 
*
* LIMITATIONS: 
*  - No real sub-instruction accuracy NOR pipeline (overlapping OPs) handling but some (hopefully) "good enough" 
*    approximation. Delays regarding when updated data will be visible on the bus (and to which component) may not be correctly 
*    modeled for all special cases.
*  - flag handling in BCD mode is not implemented (see http://www.oxyron.de/html/opcodes02.html)
*
*
* HW clock timing info:
* 
* Based on the ø0 clock delivered by VIC, the CPU generates the two output clocks ø1 and ø2. The respective phase 
* width and rise/fall times vary slightly resulting in something like this:
*    
* ø0 ¯¯¯¯¯¯\_______________/¯¯¯¯¯¯¯¯¯\___________ 
*
* ø1 __________/¯¯¯¯¯¯¯¯¯\_______________/¯¯¯¯¯¯¯
*
* ø2 ¯¯¯¯¯¯¯¯\_______________/¯¯¯¯¯¯¯¯¯\_________
*
* It seems that ø2 is a "delayed" version of the input ø0 whereas ø1 is an "inverted version" of ø0 (and ø2).
* On sub-cycle level ø1 and ø2 are not completely in sync. While the CPU is clocked with ø2 all the other
* components are clocked with ø1.
*
* From a simplistic software point of view, the above can be thought of as each "system clock" having 2 phases:
* where the 1st phase is always used by "the other components" and the CPU (usually) uses phase 2.
*
* Known issues:
*  - Thats_All_Folks.sid crashes at the transition to the 2nd part
*  - Galdrumway.sid pauses for long periods of time => might be a good test-case for some special-case bug (since first check-in new impl)
*
*
* useful links/docs:
*  
*  http://www.oxyron.de/html/opcodes02.html
*  http://6502.org/tutorials/interrupts.html
*  http://www.zimmers.net/anonftp/pub/cbm/documents/chipdata/64doc
*  http://archive.6502.org/datasheets/mos_6500_mpu_preliminary_may_1976.pdf
*  https://wiki.nesdev.com/w/index.php/CPU_interrupts
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

// NOTE: for some reason EMSCRIPTREN does not seem to cope with the use of "inline" functions here..
  
#include <string.h>
#include <stdio.h>

#include <emscripten.h>

#include "cpu.h"
#include "env.h"

// for interrupt handling
#include "vic.h"
#include "cia.h"

#include "memory.h"

#ifdef TEST
uint8_t test_running= 0;
#endif

// ----------- system clock -----------

static uint32_t _cycles= 0;		// counter of elapsed cycles

uint32_t cpuCycles() {
	return _cycles;
}

void cpuClockSystem() {
	_cycles+= 1;
}


// ----------- CPU state -----------


static uint16_t _pc;			// program counter
static uint8_t _a, _x, _y;		// accumulator & x,y registers
static uint8_t _s; 				// stack pointer

#define FLAG_N 128
#define FLAG_V 64
#define FLAG_B1 32
#define FLAG_B0 16
#define FLAG_D 8
#define FLAG_I 4
#define FLAG_Z 2
#define FLAG_C 1

static uint8_t _p;				// status register (see above flags)

static void setflags(int32_t flag, int32_t cond)
{
    if (cond) _p |= flag;
    else _p &= ~flag;
}



// ---- interrupt handling ---

// "Many references will claim that interrupts are polled during the last cycle of an instruction,
// but this is true only when talking about the output from the edge and level detectors. As can
// be deduced from above, it's really the status of the interrupt lines at the end of the second-to-last
// cycle that matters." (https://wiki.nesdev.com/w/index.php/CPU_interrupts)


// "When an interrupt occurs 2 or more cycles before the current command ends, it is
// executed immediately after the command. Otherwise, the CPU executes the next command
// first before it calls the interrupt handler.

// The only exception to this rule are "taken branches" to the same page which last 3
// cycles. Here, the interrupt must have occurred before clock 1 of the branch command;
// the normal rule says before clock 2. Branches to a different page or branches not taken are behaving normal."
// => i.e. for special case the lead time must be 4 cycles



// line "detectors" run in ø2 phase and activate the respective internal signal in next ø1, i.e. the
// next system clock cycle.
static uint8_t _interrupt_lead_time= 2;

// how the 6502's pipline affects the handling of the I-flag:

// "The RTI instruction affects IRQ inhibition immediately. If an IRQ is pending and an RTI is
// executed that clears the I flag, the CPU will invoke the IRQ handler immediately after RTI
// finishes executing. This is due to RTI restoring the I flag from the stack before polling
// for interrupts.

// The CLI, SEI, and PLP instructions on the other hand change the I flag after polling for
// interrupts (like all two-cycle instructions they poll the interrupt lines at the end of
// the first cycle), meaning they can effectively delay an interrupt until after the next
// instruction. For example, if an interrupt is pending and the I flag is currently set,
// executing CLI will execute the next instruction before the CPU invokes the IRQ handler."

// HACK: with imprecisions of current impl using this delay seems to make things worse
// and since it would just burn extra CPU for nothing it is therefore not used
uint8_t _delayed_cli= 0;
uint8_t _delayed_sei= 0;

// ---- CIA handling ---
static uint8_t _irq_committed= 0;		// CPU is committed to running the IRQ
static uint32_t _irq_line_ts= 0;


void checkForIRQ(void) {
	// test-case: Humphrey_Bogart.sid, Monster_Museum.sid
	
	// as long as the IRQ line stays active new IRQ will trigger as soon as the I-flag 
	// is cleared .. let's presume that the I-flag masking is done initially and 
	// once committed it will no longer matter (like in the NMI case)
	
	if (!(_p & FLAG_I) && (vicIRQ() || ciaIRQ())) {	// test-case: Vicious_SID_2-Escos (needs FLAG_I check)
	
		if (!_irq_line_ts) {
			_irq_committed= 1;		// there is no way back now
			_irq_line_ts= _cycles;	// ts when line was activated
		}
	} else {
		if (!_irq_committed) _irq_line_ts = 0;	// is the IRQ flag really relevant here (see mandatory check in pendingIRQ())
	}
}

int8_t pendingIRQ(void) {
	if (_irq_committed) {
		uint32_t elapsed= _cycles - _irq_line_ts;

		// test-case: Vaakataso.sid, Vicious_SID_2-Carmina_Burana.sid depend on the FLAG_I test
		//            probably compensates for some flaw in the state handling
		
		return (!(_p & FLAG_I)) && (elapsed  >= _interrupt_lead_time); // test-case: "IMR"
	}
	return 0;
}


	// ---- NMI handling ---

static uint8_t _nmi_executing= 0;		// hack for digi handling
	
uint8_t cpuIsInNMI() {
	return _nmi_executing;
}	
	
	
static uint8_t _nmi_committed= 0;		// CPU is committed to running the NMI
static uint8_t _nmi_line= 0;			// state change detection
static uint32_t _nmi_line_ts= 0;		// for scheduling

void checkForNMI(void) {
	// when the CPU detects the "NMI line" activation it "commits" to running that NMI
	// handler and no later state change will stop that NMI from being executed (see above for scheduling)

	// test-case: "ICR01" ("READING ICR=81 MUST PASS NMI"): eventhough the NMI line is immediately
	// acknowledged/cleared in the same cycle that the CIA sets it, the NMI handler should still be called.
	
	if (ciaNMI()) {						// NMI line is active now

		// NMI is different from IRQ in that only the transition from high to low signal triggers an NMI, and
		// the line has to be restored to high (meaning "false" here) before the next NMI can trigger.
	
		if (!_nmi_line) {
			
			_nmi_committed= 1;			// there is no way back now
			_nmi_line= 1;				// using 1 to model HW line "low" signal
			_nmi_line_ts= _cycles;
			
			// "If both an NMI and an IRQ are pending at the end of an instruction, the NMI will be handled and the
			// pending status of the IRQ forgotten (though it's likely to be detected again during later polling)."
	//		_irq_committed= 0;
	//		_irq_line_ts= 0;
		} else {
			// line already/still activated... cannot trigger new NMI before previous one has been acknowledged
		}
	} else {
		_nmi_line= 0;				// NMI has been acknowledged
		if (!_nmi_committed) {
			_nmi_line_ts= 0; 		// still needed until the committed NMI has been scheduled
		}
	}	
}

uint8_t pendingNMI () {

	if (_nmi_committed) {
		uint32_t elapsed= _cycles - _nmi_line_ts;
		return elapsed >= _interrupt_lead_time;
	}
	return 0;
}

uint8_t cpuIsValidPcPSID() {
	// only used to to run PSID INIT separately.. everything else runs 
	// without this limitation 
	
	// for RSIDs there is not really "any" invalid PC
	// test-case: Boot_Zak_v2.sid (uses $0000 for IRQ handler).
	return _pc > 1;
}

// MOS6510 instruction modes
#define imp 0
#define imm 1
#define abs 2
#define abx 3
#define aby 4
#define zpg 6
#define zpx 7
#define zpy 8
#define ind 9
#define idx 10
#define idy 11
#define acc 12
#define rel 13

// enum of all MOS6510 operations
enum {
	adc, alr, anc, and, ane, arr, asl, bcc, bcs, beq, bit, bmi, bne, bpl, brk, bvc, 
    bvs, clc, cld, cli, clv, cmp, cpx, cpy, dcp, dec, dex, dey, eor, inc, inx, iny, 
	isb, jam, jmp, jsr, lae, lax, lda, ldx, ldy, lsr, lxa, nop, ora, pha, php, pla, 
	plp, rla, rol, ror, rra, rti, rts, sax, sbc, sbx, sec, sed, sei, sha, shs, shx, 
	shy, slo, sre, sta, stx, sty, tax, tay, tsx, txa, txs, tya,
	sti, stn, nul	// pseudo OPs (replacing unusable JAM OPs)
};


static uint16_t isClass2(int32_t cmd) {
	switch (cmd) {
		case adc:
		case and:
		case cmp:
		case eor:
		case lae:
		case lax:
		case lda:
		case ldx:
		case ldy:
		case nop:
		case ora:
		case sbc:
			return 1;
		default:
			return 0;
	}
}

// artificial OPs patched in the positions of unsable JAM ops to ease impl:

static uint8_t start_irq_op= 0x02;	// "sti" pseudo OP for interrupt handling
static uint8_t start_nmi_op= 0x12;	// "stn"  "
static uint8_t null_op= 0x22;		// "nul" means "empty main"

static const int32_t _opcodes[256]  = {
	brk,ora,sti,slo,nop,ora,asl,slo,php,ora,asl,anc,nop,ora,asl,slo,
	bpl,ora,stn,slo,nop,ora,asl,slo,clc,ora,nop,slo,nop,ora,asl,slo,
	jsr,and,nul,rla,bit,and,rol,rla,plp,and,rol,anc,bit,and,rol,rla,
	bmi,and,jam,rla,nop,and,rol,rla,sec,and,nop,rla,nop,and,rol,rla,
	rti,eor,jam,sre,nop,eor,lsr,sre,pha,eor,lsr,alr,jmp,eor,lsr,sre,
	bvc,eor,jam,sre,nop,eor,lsr,sre,cli,eor,nop,sre,nop,eor,lsr,sre,
	rts,adc,jam,rra,nop,adc,ror,rra,pla,adc,ror,arr,jmp,adc,ror,rra,
	bvs,adc,jam,rra,nop,adc,ror,rra,sei,adc,nop,rra,nop,adc,ror,rra,
	nop,sta,nop,sax,sty,sta,stx,sax,dey,nop,txa,ane,sty,sta,stx,sax,
	bcc,sta,jam,sha,sty,sta,stx,sax,tya,sta,txs,shs,shy,sta,shx,sha,
	ldy,lda,ldx,lax,ldy,lda,ldx,lax,tay,lda,tax,lxa,ldy,lda,ldx,lax,
	bcs,lda,jam,lax,ldy,lda,ldx,lax,clv,lda,tsx,lae,ldy,lda,ldx,lax,
	cpy,cmp,nop,dcp,cpy,cmp,dec,dcp,iny,cmp,dex,sbx,cpy,cmp,dec,dcp,
	bne,cmp,jam,dcp,nop,cmp,dec,dcp,cld,cmp,nop,dcp,nop,cmp,dec,dcp,
	cpx,sbc,nop,isb,cpx,sbc,inc,isb,inx,sbc,nop,sbc,cpx,sbc,inc,isb,
	beq,sbc,jam,isb,nop,sbc,inc,isb,sed,sbc,nop,isb,nop,sbc,inc,isb
};

static const int32_t _modes[256]  = {
	imp,idx,abs,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,abs,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	abs,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imp,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imp,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,ind,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpy,zpy,imp,aby,imp,aby,abx,abx,aby,aby,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpy,zpy,imp,aby,imp,aby,abx,abx,aby,aby,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx
};


// Cycles per operation (adjustments apply) - note: these timings only consider
// the time to the next OP but not the additional cycles at the OPs end that 
// overlap with the fetching of the next OP (due to pipelining)!

static const int32_t _opbase_frame_cycles[256] = {
	7,6,7,8,3,3,5,5,3,2,2,2,4,4,6,6,
	2,5,7,8,4,4,6,6,2,4,2,7,4,4,7,7,
	6,6,2,8,3,3,5,5,4,2,2,2,4,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7,
	6,6,0,8,3,3,5,5,3,2,2,2,3,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7,
	6,6,0,8,3,3,5,5,4,2,2,2,5,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7,
	2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4,
	2,6,0,5,4,4,4,4,2,5,2,5,5,5,5,5,
	2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4,
	2,5,0,5,4,4,4,4,2,4,2,4,4,4,4,4,
	2,6,2,8,3,3,5,5,2,2,2,2,4,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7,
	2,6,2,8,3,3,5,5,2,2,2,2,4,4,6,6,
	2,5,0,8,4,4,6,6,2,4,2,7,4,4,7,7
};

// Used to emulate "CPU stun" (by VIC): 0 for OPs that don't use "bus writes", else
// cycle (starting at 1) in which "write" is started (with exception of BRK/JSR these are
// then all the remaining steps of the OP)
// note: the relevant OPs are not affected by page boundary crossing, i.e. no further 
// adjustments needed.

static const int32_t _opbase_write_cycle[256] = {
	3,0,3,7,0,0,4,4,3,0,0,0,0,0,5,5,
	0,0,3,7,0,0,5,5,0,0,0,6,0,0,6,6,
	4,0,0,7,0,0,4,4,0,0,0,0,0,0,5,5,
	0,0,0,7,0,0,5,5,0,0,0,6,0,0,6,6,
	0,0,0,7,0,0,4,4,3,0,0,0,0,0,5,5,
	0,0,0,7,0,0,5,5,0,0,0,6,0,0,6,6,
	0,0,0,7,0,0,4,4,0,0,0,0,0,0,5,5,
	0,0,0,7,0,0,5,5,0,0,0,6,0,0,6,6,
	0,6,0,6,3,3,3,3,0,0,0,0,4,4,4,4,
	0,6,0,0,4,4,4,4,0,5,0,0,0,5,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,7,0,0,4,4,0,0,0,0,0,0,5,5,
	0,0,0,7,0,0,5,5,0,0,0,6,0,0,6,6,
	0,0,0,7,0,0,4,4,0,0,0,0,0,0,5,5,
	0,0,0,7,0,0,5,5,0,0,0,6,0,0,6,6
};

// ----------------------------------------------- globale Faulheitsvariablen
static uint8_t _bval;
static uint16_t _wval;

static uint8_t getH(uint8_t opc, int32_t mode)	// hi-byte of addr; used for some obscure illegal ops
{
   uint16_t ad,ad2;  
    switch(mode)
    {
        case abs:
            return memGet(_pc+2);			
        case abx:
        case aby:			
            ad=memGet(_pc+1);
            ad|=memGet(_pc+2)<<8;
            ad2=ad +(mode==abx?_x:_y);
            return ad2 >> 8;
        case zpg:
			ad=memGet(_pc+1);
            return ad >> 8;
        case idx:
			// indexed indirect, e.g. LDA ($10,X)
            ad=memGet(_pc+1);
            ad+=_x;
            ad2=memGet(ad&0xff);
            ad++;
            ad2|=memGet(ad&0xff)<<8;
            return ad2 >> 8;
        case idy:
			// indirect indexed, e.g. LDA ($20),Y
            ad=memGet(_pc+1);
            ad2=memGet(ad);
            ad2|=memGet((ad+1)&0xff)<<8;
            ad=ad2+_y;			
            return ad >> 8;
    }  
    return 0;
}


static uint8_t getaddr(uint8_t opc, int32_t mode)
{
	// reads all the bytes belonging to the operation and advances the pc accordingly;
	// handles any "page boundary crossing" related timing adjustments..
	
    uint16_t ad,ad2;  
    switch(mode)
    {
        case imp:
            return 0;
        case imm:
            return memGet(_pc++);
        case abs:
            ad=memGet(_pc++);
            ad|=memGet(_pc++)<<8;			
            return memGet(ad);
        case abx:
        case aby:			
            ad=memGet(_pc++);
            ad|=memGet(_pc++)<<8;
            ad2=ad +(mode==abx?_x:_y);
            return memGet(ad2);
        case zpg:
			ad=memGet(_pc++);
            return memGet(ad);
        case zpx:
            ad=memGet(_pc++);
            ad+=_x;
            return memGet(ad&0xff);
        case zpy:
            ad=memGet(_pc++);
            ad+=_y;
            return memGet(ad&0xff);
        case idx:
			// indexed indirect, e.g. LDA ($10,X)
            ad=memGet(_pc++);
            ad+=_x;
            ad2=memGet(ad&0xff);
            ad++;
            ad2|=memGet(ad&0xff)<<8;
            return memGet(ad2);
        case idy:
			// indirect indexed, e.g. LDA ($20),Y
            ad=memGet(_pc++);
            ad2=memGet(ad);
            ad2|=memGet((ad+1)&0xff)<<8;
            ad=ad2+_y;			
            return memGet(ad);
        case acc:
            return _a;
    }  
    return 0;
}

static void setaddr(uint8_t opc, int32_t mode, uint8_t val)
{
	// note: orig impl only covered the modes used by "regular" ops but
	// for the support of illegal ops there are some more..
	
	// note: only used after "getaddr" and any addressing mode related "timing" handling
	// is already performed there, i.e. NO timing adjustments (see page boundary crossing) are 
	// REQUIRED here!
    uint16_t ad,ad2;
    switch(mode)
    {
        case abs:
            ad=memGet(_pc-2);
            ad|=memGet(_pc-1)<<8;
            memSet(ad,val);
            return;
        case abx:
        case aby:		// added
            ad=memGet(_pc-2);
            ad|=memGet(_pc-1)<<8;
	        ad2=ad +(mode==abx?_x:_y);
            memSet(ad2,val);
            return;
        case idx:			// added
			// indexed indirect, e.g. LDA ($10,X)
            ad=memGet(_pc-1);	 // original impl pc increment seemed wrong here!!!
            ad+=_x;
            ad2=memGet(ad&0xff);
            ad++;
            ad2|=memGet(ad&0xff)<<8;
			memSet(ad2,val);
            return;
        case idy:			// added
			// indirect indexed, e.g. LDA ($20),Y
            ad=memGet(_pc-1);
            ad2=memGet(ad);
            ad2|=memGet((ad+1)&0xff)<<8;
            ad=ad2+_y;			
			memSet(ad,val);
            return;
        case zpg:
            ad=memGet(_pc-1);
            memSet(ad,val);
            return;
        case zpx:
        case zpy:		// added
            ad=memGet(_pc-1);
	        ad+=(mode==zpx?_x:_y);
            memSet(ad&0xff,val);
            return;
        case acc:
            _a=val;
            return;
    }
}

static int8_t _next_instruction_cycles;	// number of cycles that will be used by the next instruction; fixme - ugly global var

static void addPageBoundaryReadCycle(uint8_t opc, int32_t mode) {
    uint16_t ad,ad2;  
    switch(mode)
    {
        case abx:
        case aby:			
            ad=memGet(_pc++);
            ad|=memGet(_pc++)<<8;
            ad2=ad +(mode==abx?_x:_y);

			if (isClass2(_opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00))) {
				// page boundary crossed
				_next_instruction_cycles++;
			}
			break;
        case idy:
			// indirect indexed, e.g. LDA ($20),Y
            ad=memGet(_pc++);
            ad2=memGet(ad);
            ad2|=memGet((ad+1)&0xff)<<8;
            ad=ad2+_y;
			
			if (isClass2(_opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00)))	{
				// page boundary crossed
				_next_instruction_cycles++;
			}
			break;
    }  
}


static void addBranchOpCycles(uint8_t opc, int32_t flag) {	
    int8_t dist=(int8_t)getaddr(opc, imm);
    _wval=_pc+dist;
    if (flag) {
		uint8_t diff= ((_pc&0x100)!=(_wval&0x100))?2:1;
		
		if (diff == 1) {
			_interrupt_lead_time= 4;	// before 1st cycle (of a 3 cycle OP)
		}
    	_next_instruction_cycles+= diff; // + 1 if branch occurs to same page/ + 2 if branch occurs to different page
	}
}

static void addPageBoundaryWriteCycle(uint8_t opc, int32_t mode) {
	// add additional execution cycle caused by crossed page boundary
    uint16_t ad,ad2;
    switch(mode)
    {
        case abx:
        case aby:
            ad=memGet(_pc++);
            ad|=memGet(_pc++)<<8;				
            ad2=ad +(mode==abx?_x:_y);

			if (isClass2(_opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00))) {
				// page boundary crossed
				_next_instruction_cycles++;
			}				
            break;
		 default:
            break;
    }
}

static void prefetchOP( int16_t *opcode, int8_t *cycles) {
	/* see comment at var decl
	if (_delayed_sei) {
		setflags(FLAG_I,1);
		_delayed_sei= 0;
	}
	if (_delayed_cli) {
		setflags(FLAG_I,0);
		_delayed_cli= 0;
	}
	*/
	
	/* no longer needed - now that PSIDs use proper driver/wrapper
	if (!cpuPcIsValid()) {
		// init returned to non existing "main".. just burn cycles for imaginary "main" and wait for next interrupt
		*opcode= null_op;		
		*cycles= _opbase_frame_cycles[*opcode];	// just burn some cycles
		return;
	}*/
	
	uint16_t _orig_pc= _pc;	// _pc will be used/corrupted via getaddr anyway

    uint8_t opc=memGet(_pc++);
	*opcode= opc;
		
    int32_t mode=_modes[opc];
	
	// get base cycles
	_next_instruction_cycles= _opbase_frame_cycles[opc];
    
	// calc adjustments 
   switch (_opcodes[opc])
    {
        case adc: 
		case alr:
        case anc:
        case and:
        case arr:
        case asl:
			addPageBoundaryReadCycle(opc, mode);
            break;
        case bcc:
            addBranchOpCycles(opc, !(_p&FLAG_C));
            break;
        case bcs:
            addBranchOpCycles(opc, _p&FLAG_C);
            break;
        case bne:
            addBranchOpCycles(opc, !(_p&FLAG_Z));
            break;
        case beq:
            addBranchOpCycles(opc, _p&FLAG_Z);
            break;
        case bpl:
            addBranchOpCycles(opc, !(_p&FLAG_N));
            break;
        case bmi:
            addBranchOpCycles(opc, _p&FLAG_N);
            break;
        case bvc:
            addBranchOpCycles(opc, !(_p&FLAG_V));
            break;
        case bvs:
            addBranchOpCycles(opc, _p&FLAG_V);
            break;
        case bit:
			addPageBoundaryReadCycle(opc, mode);
            break;
        case ane:
        case brk:
        case clc:
        case cld:
        case cli:
        case clv:
            break;
			
        case cmp:
        case cpx:
        case cpy:
        case dcp:
        case dec:
			addPageBoundaryReadCycle(opc, mode);
            break;
			
        case dex:
        case dey:
            break;
			
        case eor:
        case inc:
			addPageBoundaryReadCycle(opc, mode);
            break;
			
        case inx:
        case iny:
            break;
			
        case isb:
			addPageBoundaryReadCycle(opc, mode);
			break;
			
		case jam:
        case jmp:
        case jsr:
            break;
			
		case lax:
		case lae:
		case lxa:
        case lda:
        case ldx:
        case ldy:
        case lsr:      
        case nop:
        case ora:
 			addPageBoundaryReadCycle(opc, mode);
            break;
			
        case pha:
        case php:
        case pla:
        case plp:
            break;
			
        case rla:
        case rol:
        case ror:
        case rra:
			addPageBoundaryReadCycle(opc, mode);
            break;
			
        case rti:
        case rts:		
            break;
        case sbc:
        case sax:
		case sbx:
			addPageBoundaryReadCycle(opc, mode);
			break;
			
        case sec:
        case sed:
        case sei:
            break;
			
        case sha:
        case slo:
        case sre:
        case shx:
        case shy:
			addPageBoundaryReadCycle(opc, mode);
            break;
			
        case sta:
        case stx:
        case sty:
            addPageBoundaryWriteCycle(opc,mode);
            break;
			
        case tax:
        case tay:
        case tsx:
        case txa:
        case txs:
        case tya:
            break; 
			
/* hard to think of a good reason why anybody would ever use this..
		case shs:	// 	TAS						
			addPageBoundaryReadCycle(opc, mode);
            break;
*/						
		default:			
			addPageBoundaryReadCycle(opc, mode);
    }
	
	_pc= _orig_pc;
	
	*cycles= _next_instruction_cycles;
}

uint16_t cpuGetPC() {
	return _pc;
}

uint8_t cpuGetSP() {
	return _s;
}

static void putaddr(uint8_t opc, int32_t mode, uint8_t val) {
    uint16_t ad,ad2;
    switch(mode)
    {
        case abs:
            ad=memGet(_pc++);
            ad|=memGet(_pc++)<<8;
            memSet(ad,val);
            return;
        case abx:
        case aby:
            ad=memGet(_pc++);
            ad|=memGet(_pc++)<<8;				
            ad2=ad +(mode==abx?_x:_y);
            memSet(ad2,val);
            return;
        case zpg:
            ad=memGet(_pc++);
            memSet(ad,val);
            return;
        case zpx:
            ad=memGet(_pc++);
            ad+=_x;
            memSet(ad&0xff,val);
            return;
        case zpy:
            ad=memGet(_pc++);
            ad+=_y;
            memSet(ad&0xff,val);
            return;
        case idx:
            ad=memGet(_pc++);
            ad+=_x;
            ad2=memGet(ad&0xff);
            ad++;
            ad2|=memGet(ad&0xff)<<8;
            memSet(ad2,val);
            return;
        case idy:
            ad=memGet(_pc++);
            ad2=memGet(ad);
            ad2|=memGet((ad+1)&0xff)<<8;
            ad=ad2+_y;
            memSet(ad,val);
            return;
        case acc:
            _a=val;
            return;
    }
}

static void push(uint8_t val)
{
    memSet(0x100+_s,val);	
	_s= (_s-1)&0xff;			// real stack just wraps around...	
}

static uint8_t pop(void)
{
	_s= (_s+1)&0xff;			// real stack just wraps around...	
    return memGet(0x100+_s);	// pos is now the new first free element..
}

static void branch(uint8_t opc, int32_t flag)
{
    int8_t dist;
    dist=(int8_t)getaddr(opc, imm);
    _wval=_pc+dist;
    if (flag) {
		_pc=_wval; 
	}
}

// instruction that is executing in a "cycle-by-cycle manner"
static int16_t _exe_instr_opcode;
static int8_t _exe_instr_cycles;
static int8_t _exe_instr_cycles_remain;

void cpuInit(void)
{
	// timing status
	_cycles= 0;	// still used for digi-sample detection
		
	// cpu status
	_pc=_a= _x= _y= _s= _p= _bval= _wval= 0;
		
	_exe_instr_cycles= _exe_instr_cycles_remain= 0;
	_exe_instr_opcode = -1;
	
	_irq_line_ts= _irq_committed= 0;
	_nmi_line= _nmi_line_ts= _nmi_committed= 0;
	_nmi_executing= 0;
//	_delayed_cli= _delayed_sei= 0;

#ifdef TEST
	test_running= 1;

	_s= 0x0; 	// so this should be equivalent to the 0xfd that the tests expect:
	push(0);	// use as marker to know when to return
	push(0);
	push(0);	
	
	_p= 0x00;	// idiotic advice to set I-flag! (see "irq" tests)
	_pc= 0x0801;
	
#endif
}

static void cpuRegReset(void)
{
    _a=_x=_y=0;
    _p=0;
    _s= 0xff; 
    _pc= 0;
}

void cpuReset(uint16_t npc, uint8_t na) {
	cpuRegReset();
	_a=na;
	_pc=npc;	
	
	push(0);	// marker used to detect when "init" returns to non existing "main"
	push(0);
	
	_cycles= 0;
}



void cpuIrqFlagPSID(uint8_t on) {
	setflags(FLAG_I, on);
}

#ifdef TEST
char _load_filename[32];
#endif
static void runNextOp(void)
{
	_interrupt_lead_time= 2;	// reset (special case for branch timing)

	/*
	note:  Read-Modify-Write instructions (ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP) write the
	originally read value before they then write the updated value. This is used by some programs to acknowledge/clear
	interrupts - since the 1st write will clear all the originally set bits.
	*/
	
	
	
	// The operation MUST BE fetched in the 1st cycle (i.e. when prefetching is none - or the wrong command
	// could be used later .. see "cia1tb123" test - where the command byte is updates by the timer - changing
	// the OP while the instruction is already executed)
    uint8_t opc=memGet(_pc++);	// might be invalid by now
	opc= _exe_instr_opcode;

//	if (_pc == 0x0B43) fprintf(stderr, "%6lu ***** START TEST 8 ********\n", _cycles);

	
    int32_t cmd=_opcodes[opc];
    int32_t mode=_modes[opc];
	
	int32_t c;  // temp for "carry"
	
    switch (cmd) {
		// ideally the most often used OPs should be retriveable most quickly.. but is is unclear what strategy
		// the optimizer will actually be using to implement this (and the optimizer cannot know what programs
		// will be emulated here so it has no clue what might be the most used OPs) .. let's just hope it
		// is using some constant time access scheme.
		
        case adc: {
			uint8_t in1= _a;
			uint8_t in2= getaddr(opc, mode);
			
			// note: The carry flag is used as the carry-in (bit 0) for the operation, and the 
			// resulting carry-out (bit 8) value is stored in the carry flag.
            _wval=(uint16_t)in1+in2+((_p&FLAG_C)?1:0);	// "carry-in"
            setflags(FLAG_C, _wval&0x100);
            _a=(uint8_t)_wval;
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
			
			// calc overflow flag (http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html)
			// also see http://www.6502.org/tutorials/vflag.html
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ _a)&0x80);
			}
            break;
		case alr: 		// aka ASR - Kukle.sid, Raveloop14_xm.sid (that song has other issues though)
			//	ALR #{imm} = AND #{imm} + LSR
            _bval=getaddr(opc, mode);
			_a= _a&_bval;

            setflags(FLAG_C,_a&1);
            _a>>=1;
			
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
			break;
        case anc:	// Kukle.sid, Axelf.sid (Crowther), Whats_Your_Lame_Excuse.sid, Probing_the_Crack_with_a_Hook.sid
            _bval=getaddr(opc, mode);
			_a= _a&_bval;
			
			// http://codebase64.org/doku.php?id=base:some_words_about_the_anc_opcode
            setflags(FLAG_C, _a&0x80);
			
			// supposedly also sets these (http://www.oxyron.de/html/opcodes02.html)
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
            break;
        case and:
            _bval=getaddr(opc, mode);
            _a&=_bval;
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
            break;
        case ane: {// aka XAA; another useless op that is only used in the tests
	        _bval=getaddr(opc, mode);
			const uint8_t con= 0x0; 	// this is HW dependent.. i.e. this OP is bloody useless			
			_a= (_a|con) & _x & _bval;
			
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
		} break; 
        case arr: {		// Whats_Your_Lame_Excurse.sid uses this. sigh "the crappier the song...." & 
						// Probing_the_Crack_with_a_Hook.sid
									
			// AND
            _bval=getaddr(opc, mode);
            _a&=_bval;
			
			// set C+V based on this intermediate state of bits 6+7 (before ROR)
			uint8_t bit7= !!(_a&0x80);
			uint8_t bit6= !!(_a&0x40);

            c= !!(_p&FLAG_C);
			
			setflags(FLAG_V, bit7^bit6);
            setflags(FLAG_C, bit7);
						
			// ROR - C+V not affected here
            _a>>= 1;
			
			if (c) {
				_a |= 0x80;	// exchange bit 7 with carry
			}
            setflags(FLAG_N,_a&0x80);
            setflags(FLAG_Z,!_a);
			}			
            break;
        case asl:
            _wval=getaddr(opc, mode);
            setaddr(opc, mode,(uint8_t)_wval);	// read-modify-write writes original 1st
            _wval<<=1;
            setaddr(opc, mode,(uint8_t)_wval);
            setflags(FLAG_Z,!(_wval&0xff));
            setflags(FLAG_N,_wval&0x80);
            setflags(FLAG_C,_wval&0x100);
            break;
        case bcc:
            branch(opc, !(_p&FLAG_C));
            break;
        case bcs:
            branch(opc, _p&FLAG_C);
            break;
        case bne:
            branch(opc, !(_p&FLAG_Z));
            break;
        case beq:
            branch(opc, _p&FLAG_Z);
            break;
        case bpl:
            branch(opc, !(_p&FLAG_N));
            break;
        case bmi:
            branch(opc, _p&FLAG_N);
            break;
        case bvc:
            branch(opc, !(_p&FLAG_V));
            break;
        case bvs:
            branch(opc, _p&FLAG_V);
            break;
        case bit:
            _bval=getaddr(opc, mode);
            setflags(FLAG_Z,!(_a&_bval));
            setflags(FLAG_N,_bval&0x80);
            setflags(FLAG_V,_bval&0x40);	// bit 6
            break;
        case brk:
#ifdef TEST
			if (_pc == 0xFFD3) {	// print char via FFD2
				memWriteRAM(0x030C, 0);

				EM_ASM_({ window['outputPETSCII'](($0));}, _a);	// easier to deal with this on JavaScript side (pervent optimizer renaming the func)

				_wval=pop();
				_wval|=pop()<<8;	// RTS to where it came from
				_pc=_wval+1;
				break;
			} else if (_pc == 0xE170) {	// load
				// report the next test file (this means that this test was successful)
				uint16_t adr= memReadRAM(0x00bb) | (memReadRAM(0x00bc) <<8);
				uint8_t len= memReadRAM(0x00b7);
				if (len > 31) len= 31;
				for (int i= 0; i<len; i++) {
					_load_filename[i]= memReadRAM(adr++);
				}
				_load_filename[len]= 0;
				
				EM_ASM_({ window['loadFileError'](Pointer_stringify($0));}, _load_filename);	// easier to deal with this on JavaScript side (pervent optimizer renaming the func)

				test_running= 0;
				break;
			} else if (_pc == 0xFFE5) {	// scan keyboard
				_a= 3;			// always return this "key press"
				_wval=pop();
				_wval|=pop()<<8;
				_pc=_wval+1;
				break;
			} else if ((_pc == 0x8001) || (_pc == 0xA475)) {	// exit
				test_running= 0;
				break;
			}	
#endif
			EM_ASM_({ console.log('BRK from:        $' + ($0).toString(16));}, _pc-1);	// less mem than inclusion of fprintf

			// _pc has already been incremented by 1 (see above) 
			// (return address to be stored on the stack is original _pc+2 )
			push((_pc+1)>>8);
			push((_pc+1));
			push(_p | FLAG_B0 | FLAG_B1);	// only in the stack copy
			
			_pc=memGet(0xfffe);
			_pc|=memGet(0xffff)<<8;		// somebody might finger the IRQ vector or the BRK vector at 0316/0317 to use this?
			
			setflags(FLAG_I,1);
            break;
        case clc:
            setflags(FLAG_C,0);
            break;
        case cld:
            setflags(FLAG_D,0);
            break;
        case cli:
            setflags(FLAG_I,0);
//			_delayed_cli= 1;
            break;
        case clv:
            setflags(FLAG_V,0);
            break;
        case cmp:
            _bval=getaddr(opc, mode);
            _wval=(uint16_t)_a-_bval;
            setflags(FLAG_Z,!_wval);		// _a == _bval
            setflags(FLAG_N,_wval&0x80);	// _a < _bval
            setflags(FLAG_C,_a>=_bval);
            break;
        case cpx:
            _bval=getaddr(opc, mode);
            _wval=(uint16_t)_x-_bval;
            setflags(FLAG_Z,!_wval);
            setflags(FLAG_N,_wval&0x80);      
            setflags(FLAG_C,_x>=_bval);
            break;
        case cpy:
            _bval=getaddr(opc,mode);
            _wval=(uint16_t)_y-_bval;
            setflags(FLAG_Z,!_wval);
            setflags(FLAG_N,_wval&0x80);      
            setflags(FLAG_C,_y>=_bval);
            break;
        case dcp:		// used by: Clique_Baby.sid, Musik_Run_Stop.sid
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
			// dec
            _bval--;
            setaddr(opc,mode,_bval);
			// cmp
            _wval=(uint16_t)_a-_bval;
            setflags(FLAG_Z,!_wval);
            setflags(FLAG_N,_wval&0x80);
            setflags(FLAG_C,_a>=_bval);
            break;
        case dec:
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            _bval--;
            setaddr(opc,mode,_bval);
            setflags(FLAG_Z,!_bval);
            setflags(FLAG_N,_bval&0x80);
            break;
        case dex:
            _x--;
            setflags(FLAG_Z,!_x);
            setflags(FLAG_N,_x&0x80);
            break;
        case dey:
            _y--;
            setflags(FLAG_Z,!_y);
            setflags(FLAG_N,_y&0x80);
            break;
        case eor:
            _bval=getaddr(opc, mode);
            _a^=_bval;
            setflags(FLAG_Z,!_a);
            setflags(FLAG_N,_a&0x80);
            break;
        case inc:
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            _bval++;
            setaddr(opc,mode,_bval);
            setflags(FLAG_Z,!_bval);
            setflags(FLAG_N,_bval&0x80);
            break;
        case inx:
            _x++;
            setflags(FLAG_Z,!_x);
            setflags(FLAG_N,_x&0x80);
            break;
        case iny:
            _y++;
            setflags(FLAG_Z,!_y);
            setflags(FLAG_N,_y&0x80);
            break;
        case isb: {	// aka ISC; see 'insz' tests
			// inc
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            _bval++;
            setaddr(opc,mode,_bval);
            setflags(FLAG_Z,!_bval);
            setflags(FLAG_N,_bval&0x80);

			// + sbc			
			uint8_t in1= _a;
			uint8_t in2= (_bval^0xff);	// substract
			
            _wval=(uint16_t)in1+in2+((_p&FLAG_C)?1:0);
            setflags(FLAG_C, _wval&0x100);
            _a=(uint8_t)_wval;
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
			
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ _a)&0x80);			
            }
			break;
		case jam:	// this op would have crashed the C64
		    _pc=0;           // just quit the emulation
            break;
        case jmp:
            _wval=memGet(_pc++);
            _wval|=memGet(_pc++)<<8;
            switch (mode) {
                case abs:
					_pc=_wval;
                    break;
                case ind:
					// 6502 bug: JMP ($12FF) will fetch the low-byte from $12FF and the high-byte from $1200
                    _pc=memGet(_wval);
                    _pc|=memGet((_wval==0xff) ? 0 : _wval+1)<<8;
                    break;
            }
            break;
        case jsr:
			// _pc has already been incremented by 1 (see above) 
			// (return address to be stored on the stack is original _pc+2 )
            push((_pc+1)>>8);
            push((_pc+1));
            _wval=memGet(_pc++);
            _wval|=memGet(_pc++)<<8;
            _pc=_wval;
						
            break;
		case lae:	// aka LAS, aka LAR .. just for the tests
            _bval=getaddr(opc, mode);
			_a= _x= _s= (_bval & _s);
			
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
            break;
		case lax:
			// e.g. Vicious_SID_2-15638Hz.sid, Kukle.sid
            _a=getaddr(opc, mode);
			_x= _a;
            setflags(FLAG_Z,!_a);
            setflags(FLAG_N,_a&0x80);
            break;
		case lxa: {	// Whats_Your_Lame_Excuse.sid - LOL only real dumbshit player uses this op.. 
            _bval=getaddr(opc, mode);
			const uint8_t con= 0xff;
			_a|= con;	// roulette what the specific CPU uses here
			_a&= _bval;
			_x= _a;
            setflags(FLAG_Z,!_a);
            setflags(FLAG_N,_a&0x80);
		} break;				
        case lda:
            _a=getaddr(opc, mode);
            setflags(FLAG_Z,!_a);
            setflags(FLAG_N,_a&0x80);
            break;
        case ldx:
            _x=getaddr(opc, mode);
            setflags(FLAG_Z,!_x);
            setflags(FLAG_N,_x&0x80);
            break;
        case ldy:
            _y=getaddr(opc, mode);
            setflags(FLAG_Z,!_y);
            setflags(FLAG_N,_y&0x80);
            break;
        case lsr:      
            _bval=getaddr(opc, mode); 
			_wval=(uint8_t)_bval;
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            _wval>>=1;
            setaddr(opc,mode,(uint8_t)_wval);
            setflags(FLAG_Z,!_wval);
            setflags(FLAG_N,_wval&0x80);	// always clear?
            setflags(FLAG_C,_bval&1);
            break;
        case nop:
			getaddr(opc, mode);	 // make sure the PC is advanced correctly
            break;
        case ora:
            _bval=getaddr(opc, mode);
            _a|=_bval;
            setflags(FLAG_Z,!_a);
            setflags(FLAG_N,_a&0x80);
            break;
        case pha:
            push(_a);
            break;
        case php:
            push(_p | FLAG_B0 | FLAG_B1);	// only in the stack copy
            break;
        case pla:
            _a=pop();
            setflags(FLAG_Z,!_a);
            setflags(FLAG_N,_a&0x80);
            break;
        case plp: {
			_bval= pop();
			/*
			uint8_t i_flag_new= _bval & FLAG_I;
			uint8_t i_flag_old= _p & FLAG_I;
			if (i_flag_new != i_flag_old) {
				if (i_flag_new) { _delayed_sei= 1; }
				else 			{ _delayed_cli= 1; }
				
				_bval = (_bval & ~FLAG_I) | i_flag_old; // keep old for one more cycle
			}
			*/
            _p=_bval & ~(FLAG_B0 | FLAG_B1);		
		} break;
        case rla:				// see Spasmolytic_part_6.sid
			// rol
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            c=!!(_p&FLAG_C);
            setflags(FLAG_C,_bval&0x80);
            _bval<<=1;
            _bval|=c;
            setaddr(opc,mode,_bval);

			// + and
            _a&=_bval;
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
            break;
        case rol:
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            c=!!(_p&FLAG_C);
            setflags(FLAG_C,_bval&0x80);
            _bval<<=1;
            _bval|=c;
            setaddr(opc,mode,_bval);
            setflags(FLAG_N,_bval&0x80);
            setflags(FLAG_Z,!_bval);
            break;
        case ror:
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            c=!!(_p&FLAG_C);
            setflags(FLAG_C,_bval&1);
            _bval>>=1;
            _bval|= 0x80*c;
            setaddr(opc,mode,_bval);
            setflags(FLAG_N,_bval&0x80);
            setflags(FLAG_Z,!_bval);
            break;
        case rra:
			// ror
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            c=!!(_p&FLAG_C);
            setflags(FLAG_C,_bval&1);
            _bval>>=1;
            _bval|=0x80*c;
            setaddr(opc,mode,_bval);

			// + adc
			uint8_t in1= _a;
			uint8_t in2= _bval;
			
            _wval=(uint16_t)in1+in2+((_p&FLAG_C)?1:0);
            setflags(FLAG_C, _wval&0x100);
            _a=(uint8_t)_wval;
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
			
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ _a)&0x80);
            break;
        case rti:
			// timing hack: some optimized progs use JMP to an RTI that is placed such that
			// the nearby interrupt status register is implicitly read - automatically  
			// acknowledging the interrupt without having to explicitly read the register.
			switch(_pc) {
				case 0xdc0d:
				case 0xdd0d:	// e.g. LMan - Vortex.sid
					memGet(_pc);
					break;
			}
			
			_bval= pop();
            _p=_bval & ~(FLAG_B0 | FLAG_B1);
			
            _wval=pop();
            _wval|=pop()<<8;
            _pc=_wval;	// not like 'rts'! correct address is expected here!
			
			_nmi_executing= 0;	// hack to improve digi output
            break;
        case rts:		
            _wval=pop();
            _wval|=pop()<<8;
			_pc=_wval+1;
            break;
        case sbc:    {
            _bval=getaddr(opc, mode)^0xff;
			
			uint8_t in1= _a;
			uint8_t in2= _bval;
						
            _wval=(uint16_t)in1+in2+((_p&FLAG_C)?1:0);
            setflags(FLAG_C, _wval&0x100);
            _a=(uint8_t)_wval;
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
			
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ _a)&0x80);
			}
            break;
        case sha:    {	// aka AHX; for the benefit of the 'SHAAY' test (etc).. have yet to find a song that uses this
			uint8_t h= getH(opc, mode)+1;		// should be +1 but is seems to make the test happy.. who cares about this OP
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
			
			setaddr(opc, mode,_a&_x&h);
			}
            break;			
        case shx:    {	// for the benefit of the 'SHXAY' test (etc).. have yet to find a song that uses this
			uint8_t h= getH(opc, mode)+1;		// should be +1 but is seems to make the test happy.. who cares about this OP
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
			
			setaddr(opc, mode,_x&h);
			}
            break;			
        case shy:    {	// for the benefit of the 'SHYAY' test (etc).. have yet to find a song that uses this
			uint8_t h= getH(opc, mode);		// should be +1 but is seems to make the test happy.. who cares about this OP
            _bval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
			
			setaddr(opc, mode,_y&h);
			}
            break;			
        case sax:				// aka AXS; e.g. Vicious_SID_2-15638Hz.sid, Kukle.sid
			getaddr(opc, mode);	 // make sure the PC is advanced correctly
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            _bval=_a&_x;
			setaddr(opc,mode,_bval);
			// no flags are affected; registers unchanged
            break;
		case sbx: // somtimes called SAX; used in Kukle.sid, Artefacts.sid, Whats_Your_Lame_Excuse.sid, Probing_the_Crack_with_a_Hook.sid
			// affects N Z and C (like CMP)
			_bval=getaddr(opc, mode);

            setflags(FLAG_C,(_x&_a)>=_bval);	// affects the carry but NOT the overflow
			
			_x=((_x&_a)-_bval) & 0xff;	// _a unchanged (calc not affected by input carry)

            setflags(FLAG_Z,!_x);		// _a == _bval
            setflags(FLAG_N,_x&0x80);	// _a < _bval
			break;
        case sec:
            setflags(FLAG_C,1);
            break;
        case sed:
            setflags(FLAG_D,1);
            break;
        case sei:
            setflags(FLAG_I,1);
//			_delayed_sei= 1;
            break;
		case shs:	// 	aka TAS 
			// instable op; hard to think of a good reason why anybody would ever use this..
			_s= _a&_x;

			uint8_t h= getH(opc, mode) +1;
			_bval= getaddr(opc, mode);	 	// make sure the PC is advanced correctly
			setaddr(opc,mode,_bval);
			setaddr(opc,mode,_s&h);			// setaddr
		
            break;
        case slo:			// see Spasmolytic_part_6.sid
            _wval=getaddr(opc, mode);
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
            _wval<<=1;
            setaddr(opc,mode,(uint8_t)_wval);
            //setflags(FLAG_Z,!_wval);
            //setflags(FLAG_N,_wval&0x80);
            setflags(FLAG_C,_wval&0x100);
			// + ora
            _bval=_wval & 0xff;
            _a|=_bval;
            setflags(FLAG_Z,!_a);
            setflags(FLAG_N,_a&0x80);
            break;
        case sre:      		// aka LSE; see Spasmolytic_part_6.sid, Halv_2_2.sid
			// like SLO but shifting right and with eor
						
			// copied section from 'lsr'
            _bval=getaddr(opc, mode);			
            setaddr(opc, mode,_bval);	// read-modify-write writes original 1st
			_wval=(uint8_t)_bval;
            _wval>>=1;
            setaddr(opc,mode,(uint8_t)_wval);
            setflags(FLAG_Z,!_wval);
            setflags(FLAG_N,_wval&0x80);
            setflags(FLAG_C,_bval&1);
			// + copied section from 'eor'
            _bval=_wval & 0xff;
			
            _a^=_bval;
            setflags(FLAG_Z,!_a);
            setflags(FLAG_N,_a&0x80);
			
            break;
        case sta:
            putaddr(opc,mode,_a);
            break;
        case stx:
            putaddr(opc,mode,_x);
            break;
        case sty:
            putaddr(opc,mode,_y);
            break;
        case tax:
            _x=_a;
            setflags(FLAG_Z, !_x);
            setflags(FLAG_N, _x&0x80);
            break;
        case tay:
            _y=_a;
            setflags(FLAG_Z, !_y);
            setflags(FLAG_N, _y&0x80);
            break;
        case tsx:
			_x=_s;
            setflags(FLAG_Z, !_x);
            setflags(FLAG_N, _x&0x80);
            break;
        case txa:
            _a=_x;
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
            break;
        case txs:
            _s=_x;
            break;
        case tya:
            _a=_y;
            setflags(FLAG_Z, !_a);
            setflags(FLAG_N, _a&0x80);
            break; 
		default:
#ifdef DEBUG
			EM_ASM_({ console.log('op code not implemented: ' + ($0).toString(16) + ' at ' + ($1).toString(16));}, opc, _pc);	// less mem than inclusion of fprintf
#endif
			getaddr(opc, mode);	 // at least make sure the PC is advanced correctly (potentially used in timing)
    }
}

/*
* Notes on VIC "bad lines": VIC may "stun" the CPU for 40 (+up to 3) cycles (or more if sprites are involved - which isn't
* supported here) and this may occur right in the middle of some OP. The "stun" starts whenever the current OP does its next 
* "bus read" - only consecutive "writes" of the current OP are allowed to complete. (Within a 7 cycle BRK OP the code would 
* be allowed to complete the 3 push stack operations - but ONLY if the "OP" was already past the 2 initial cycles..) 
* => see vic.c for more information
* To handle this correctly OPs would need to be simulated at a sub-cycle level. But in the context of SID music players
* this is probably unnecessary overkill (except for the most hardcore RSID files - but those most likely avoid the
* badlines problem altogether since they cannot afford the 40 cycles gap when they want to play hi-fi digi-samples..).
*
* => CPU may run any cycles that DO NOT use the bus.. and there might be special cases that I overlooked
*/
int8_t isStunned(void) {	
	// VIC badline handling (i.e. CPU may be paused/stunned)
//	uint8_t is_stunned= envIsRSID() && vicStunCPU();
	uint8_t is_stunned= vicStunCPU();			// it won't not hurt to also STUN the crappy PSID songs
	if (is_stunned) {
		if ((is_stunned == 2) || (_exe_instr_opcode < 0)) {
			return 1;
		}
		
		uint8_t bus_write= _opbase_write_cycle[_exe_instr_opcode];
		if (!bus_write) {
			return 1;
		} else {
			// this OP may be allowed to still perform "bus write" (if that's the current step):			
			int8_t p= _exe_instr_cycles -_exe_instr_cycles_remain;
			if (p >= bus_write) {
				// allow this "write to complete".. see below
			} else {
				return 1;				
			}
		}		
	}
	return 0;
}

/*
* Simulate what the CPU does within the next system clock cycle.
*
* Note on interrupt handling (see http://6502.org/tutorials/interrupts.html): an interrupt triggers a 7-clock 
* cycles "virtual OP". An interrupt allows the previous OP to complete (it seems reasonable to presume that BRK 
* uses the exact same sequence - and JSR does the same just without pushing the processor status):
*					2 cycles internal
*					1 cycle push stack: return addr-hi
*					1 cycle push stack: return addr-lo
*					1 cycle push stack: processor status regigster
*				=> BTW: these are the max. 3 consecutive writes that may occur on a 6502 (see badline handling) - whereas for 
*                   the other OPs (except JSR) the writes seem to be at the end of the OP they are in the middle here..
*					1 cycle pc-lo: get vector
*					1 cycle pc-hi: get vector
*
* special cases:
*
* unhandled: "When an IRQ occurs while SEIn is executing, the IFlag on the stack will be set." => WTF is this supposed to mean?
* unhandled: "When an NMI occurs before clock 4 of a BRKn command, the BRK is finished as a NMI. In 
*            this case, BFlag on the stack will not be cleared."
"
*/


void cpuClock(void) {
	// note: on the real HW the respective check happends in ø2 phase of the previous CPU cycle and the respective internal
	// interrupt signal then goes high in the ø1 phase after (the potential problem of the below impl is that by performing 
	// the test here, it might incorrectly pick up some CIA change that has just happend in the ø1 phase)
	checkForIRQ();	// check 1st (so NMI can overrule if needed)
	checkForNMI();

	if (isStunned()) return;

	if (_exe_instr_opcode < 0) {	// get next instruction	

		if(pendingNMI()) {								// has higher prio than IRQ
			// some old PlaySID files (with recorded digis) files actually 
			// use NMI settings that must not be used here
			_nmi_executing= 1;
			_nmi_committed= 0;
			_nmi_line_ts= 0;	// make that same trigger unusable (interrupt must be acknowledged before a new one can be triggered)
			
			_exe_instr_opcode= start_nmi_op;
			_exe_instr_cycles= _opbase_frame_cycles[_exe_instr_opcode];
				
		} else if (pendingIRQ()) {	// interrupts are like a BRK command
			_irq_committed= 0;
			_exe_instr_opcode= start_irq_op;
			_exe_instr_cycles= _opbase_frame_cycles[_exe_instr_opcode];
			
		} else {
			// default: start execution of next instruction (determine exact timing)
			prefetchOP( &_exe_instr_opcode, &_exe_instr_cycles);
		}
		_exe_instr_cycles_remain=  _exe_instr_cycles - 1;					// we already are in 1st cycle here
	} else {
		
		// handle "current" instruction
		_exe_instr_cycles_remain--;	
		
		if(_exe_instr_cycles_remain == 0) {
			// complete current instruction

			if (_exe_instr_opcode == start_irq_op) {
				push(_pc>>8);		// where to resume after the interrupt
				push(_pc&0xff);
				push(_p | FLAG_B1);	// only in the stack copy ( will clear I-flag upon RTI...)
				
				setflags(FLAG_I,1);

				_pc=memGet(0xfffe);			// IRQ vector
				_pc|=memGet(0xffff)<<8;
				
			} else if (_exe_instr_opcode == start_nmi_op) {			
				push(_pc>>8);	// where to resume after the interrupt
				push(_pc&0xff);
				push(_p | FLAG_B1);	// only in the stack copy

				// "The 6510 will set the IFlag on NMI, too. 6502 untested."
				setflags(FLAG_I,1);
				
				_pc=memGet(0xfffa);			// NMI vector
				_pc|=memGet(0xfffb)<<8;
				
			} else if (_exe_instr_opcode == null_op) {
				// just burn cycles
			} else {				
				runNextOp();	// use old impl that runs a complete instruction
			}
			_exe_instr_opcode= -1;	// completed current OP
			_exe_instr_cycles_remain=  _exe_instr_cycles= 0;
		}
	}
//	 if (_exe_instr_cycles_remain <0) {
//			fprintf(stderr, "ERROR: _exe_instr_cycles_remain < 0\n");
//	}
}

void cpuResetToPSID(uint16_t npc) {
	_pc= npc;
	
   setflags(FLAG_I, 0);		// make sure the IRQ isn't blocked
}


