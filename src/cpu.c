/*
 * This contains everything to do with the emulation of the basic CPU (MOS Technology 6510).
 * 
 * It is based on the respectice code originally present in TinySid, but
 * it has been improved quite a bit.
 *
 * <p>My later additions:
 *   <ol>
 *    <li>correct cycle-time calculation for ALL 6510 op codes (also illegal ones)
 *    <li>added impls for illegal 6510 op codes, fixed errors in V-flag calculation, added handling for 6510 addressing "bugs"
 *    <li>"cycle limit" feature used to interrupt emulation runs (e.g. main prog can now be suspended/continued)
 *  </ol>
 *
 * <p>version 0.81
 * <p>Tiny'R'Sid (c) 2016 Jürgen Wothke
 */

// useful links:
// http://www.oxyron.de/html/opcodes02.html
 
#include <string.h>
#include <stdio.h>

#include "cpu.h"
#include "memory.h"

#define FLAG_N 128
#define FLAG_V 64
#define FLAG_B 16
#define FLAG_D 8
#define FLAG_I 4
#define FLAG_Z 2
#define FLAG_C 1

static uint32_t cycles= 0;			// counter of burned cpu cycles within current frame
static uint32_t totalCycles= 0;			// counter of burned cpu cycles since start of emu
static uint16_t pc;

// ----------------------------------------------------------------- Register
static uint8_t a,x,y,s,p;	// p= status register


// snapshot of current registers and stack (so we can ignore IRQ/NMI effects)

struct snapshot {
	uint8_t a, x, y, p, stackPtr;
	uint16_t pc;
	uint8_t stack[0xff];
};

static struct snapshot snapshots[2];

static uint32_t programMode= MAIN_OFFSET_MASK;

// hacks
static uint8_t fakeCountD012=0;
static uint8_t fakeLoopD012=0;

uint32_t cpuGetProgramMode() {
	return programMode;
}

void cpuSetProgramMode(uint32_t p) {
	programMode= p;
}

void cpuResetCycles(uint32_t c) {
	cycles= c;
}

uint32_t cpuTotalCycles() {
	return totalCycles;
}

uint32_t cpuCycles() {
	return cycles;
}

uint8_t cpuPcIsValid() {
	return pc > 1;
}

uint8_t cpuIrqFlag() {
	return p & FLAG_I;
}

void cpuRegSave(uint8_t i, uint8_t light) {
	struct snapshot *ss= &(snapshots[i]);
	
    ss->a = a;
    ss->x = x;
    ss->y = y;
	if (!light)  {
		ss->p = p;
		ss->pc = pc;
		
		// songs like: ParanoiaComplex, Another_Feekzoid_Digi_Tune or Demi-Demo_4 currently need this..
		// todo: check what goes wrong and fix the cause... 
		ss->stackPtr = s;	
		memCopyFromRAM(ss->stack, 0x0100, 0xff);	
	}
}

void cpuRegRestore(uint8_t i, uint8_t light) {
	struct snapshot *ss= &(snapshots[i]);
    a = ss->a;
    x = ss->x;
    y = ss->y;
	if (!light)  {
		p = ss->p;
		pc = ss->pc;
		s = ss->stackPtr;	
		memCopyToRAM(ss->stack, 0x0100, 0xff);
	}
}

// c64 instruction modes
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

// enum of all c64 operations
enum {
	adc, alr, anc, and, ane, arr, asl, bcc, bcs, beq, bit, bmi, bne, bpl, brk, bvc, 
    bvs, clc, cld, cli, clv, cmp, cpx, cpy, dcp, dec, dex, dey, eor, inc, inx, iny, 
	isb, jam, jmp, jsr, lae, lax, lda, ldx, ldy, lsr, lxa, nop, ora, pha, php, pla, 
	plp, rla, rol, ror, rra, rti, rts, sax, sbc, sbx, sec, sed, sei, sha, shs, shx, 
	shy, slo, sre, sta, stx, sty, tax, tay, tsx, txa, txs, tya,
	l_a, c_a// additional pseudo ops used for D012 polling hack (replaces 2 jam ops..)
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
		case l_a:	// additional ops introduced for D012-polling hacks 
		case c_a:
			return 1;
		default:
			return 0;
	}
}
	
static const int32_t opcodes[256]  = {
	brk,ora,l_a,slo,nop,ora,asl,slo,php,ora,asl,anc,nop,ora,asl,slo,
	bpl,ora,c_a,slo,nop,ora,asl,slo,clc,ora,nop,slo,nop,ora,asl,slo,
	jsr,and,jam,rla,bit,and,rol,rla,plp,and,rol,anc,bit,and,rol,rla,
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

static const int32_t modes[256]  = {
	imp,idx,abs,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,abs,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	abs,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,abx,abx,abx,abx,abx,
	imp,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imp,idx,imp,idx,zpg,zpg,zpg,zpg,imp,imm,acc,imm,ind,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,abx,zpx,zpx,zpy,zpy,imp,aby,imp,aby,abx,abx,aby,aby,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpy,zpy,imp,aby,imp,aby,abx,abx,aby,aby,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx,
	imm,idx,imm,idx,zpg,zpg,zpg,zpg,imp,imm,imp,imm,abs,abs,abs,abs,
	rel,idy,imp,idy,zpx,zpx,zpx,zpx,imp,aby,imp,aby,abx,abx,abx,abx
};

// cycles per operation (adjustments apply)
static const int32_t opbaseFrameCycles[256] = {
	7,6,4,8,3,3,5,5,3,2,2,2,4,4,6,6,
	2,5,4,8,4,4,6,6,2,4,2,7,4,4,7,7,
	6,6,0,8,3,3,5,5,4,2,2,2,4,4,6,6,
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

// ----------------------------------------------- globale Faulheitsvariablen
static uint8_t bval;
static uint16_t wval;

static uint8_t getaddr(uint8_t opc, int32_t mode)
{
	// reads all the bytes belonging to the operation and advances the pc accordingly
    uint16_t ad,ad2;  
    switch(mode)
    {
        case imp:
            return 0;
        case imm:
            return memGet(pc++);
        case abs:
            ad=memGet(pc++);
            ad|=memGet(pc++)<<8;
            return memGet(ad);
        case abx:
        case aby:			
            ad=memGet(pc++);
            ad|=memGet(pc++)<<8;
            ad2=ad +(mode==abx?x:y);

			if (isClass2(opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00))) {
				// page boundary crossed
				cycles++;
				totalCycles++;
			}
				
            return memGet(ad2);
        case zpg:
			ad=memGet(pc++);
            return memGet(ad);
        case zpx:
            ad=memGet(pc++);
            ad+=x;
            return memGet(ad&0xff);
        case zpy:
            ad=memGet(pc++);
            ad+=y;
            return memGet(ad&0xff);
        case idx:
			// indexed indirect, e.g. LDA ($10,X)
            ad=memGet(pc++);
            ad+=x;
            ad2=memGet(ad&0xff);
            ad++;
            ad2|=memGet(ad&0xff)<<8;
            return memGet(ad2);
        case idy:
			// indirect indexed, e.g. LDA ($20),Y
            ad=memGet(pc++);
            ad2=memGet(ad);
            ad2|=memGet((ad+1)&0xff)<<8;
            ad=ad2+y;
			
			if (isClass2(opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00)))	{
				// page boundary crossed
				cycles++;
				totalCycles++;					
			}
            return memGet(ad);
        case acc:
            return a;
    }  
    return 0;
}

static void setaddr(uint8_t opc, int32_t mode, uint8_t val)
{
	// note: orig impl only covered the modes used by "regular" ops but
	// for the support of illegal ops there are some more..
    uint16_t ad,ad2;
    switch(mode)
    {
        case abs:
            ad=memGet(pc-2);
            ad|=memGet(pc-1)<<8;
            memSet(ad,val);
            return;
        case abx:
        case aby:
            ad=memGet(pc-2);
            ad|=memGet(pc-1)<<8;
	        ad2=ad +(mode==abx?x:y);
            memSet(ad2,val);
            return;
        case idx:
			// indexed indirect, e.g. LDA ($10,X)
            ad=memGet(pc++);
            ad+=x;
            ad2=memGet(ad&0xff);
            ad++;
            ad2|=memGet(ad&0xff)<<8;
			memSet(ad2,val);
            return;
        case idy:
			// indirect indexed, e.g. LDA ($20),Y
            ad=memGet(pc++);
            ad2=memGet(ad);
            ad2|=memGet((ad+1)&0xff)<<8;
            ad=ad2+y;
			
			if (isClass2(opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00))) {
				// page boundary crossed
				cycles++;
				totalCycles++;				
			}
			memSet(ad,val);
            return;
        case zpg:
            ad=memGet(pc-1);
            memSet(ad,val);
            return;
        case zpx:
        case zpy:
            ad=memGet(pc-1);
	        ad+=(mode==zpx?x:y);
            memSet(ad&0xff,val);
            return;
        case acc:
            a=val;
            return;
    }
}

static void putaddr(uint8_t opc, int32_t mode, uint8_t val)
{
    uint16_t ad,ad2;
    switch(mode)
    {
        case abs:
            ad=memGet(pc++);
            ad|=memGet(pc++)<<8;
            memSet(ad,val);
            return;
        case abx:
        case aby:
            ad=memGet(pc++);
            ad|=memGet(pc++)<<8;				
            ad2=ad +(mode==abx?x:y);

			if (isClass2(opcodes[opc]) && ((ad2&0xff00)!=(ad&0xff00))) {
				// page boundary crossed
				cycles++;
				totalCycles++;
			}
				
            memSet(ad2,val);
            return;
        case zpg:
            ad=memGet(pc++);
            memSet(ad,val);
            return;
        case zpx:
            ad=memGet(pc++);
            ad+=x;
            memSet(ad&0xff,val);
            return;
        case zpy:
            ad=memGet(pc++);
            ad+=y;
            memSet(ad&0xff,val);
            return;
        case idx:
            ad=memGet(pc++);
            ad+=x;
            ad2=memGet(ad&0xff);
            ad++;
            ad2|=memGet(ad&0xff)<<8;
            memSet(ad2,val);
            return;
        case idy:
			// no cycle adjustment needed here.. all relevant cases are handled in "getaddr"
            ad=memGet(pc++);
            ad2=memGet(ad);
            ad2|=memGet((ad+1)&0xff)<<8;
            ad=ad2+y;
            memSet(ad,val);
            return;
        case acc:
            a=val;
            return;
    }
}

static void setflags(int32_t flag, int32_t cond)
{
    if (cond) p|=flag;
    else p&=~flag;
}

static void push(uint8_t val)
{
    memSet(0x100+s,val);	
	s= (s-1)&0xff;			// real stack just wraps around...
}

static uint8_t pop(void)
{
	s= (s+1)&0xff;			// real stack just wraps around...	
    return memGet(0x100+s);	// pos is now the new first free element..
}

static void branch(uint8_t opc, int32_t flag)
{
    int8_t dist;
    dist=(int8_t)getaddr(opc, imm);
    wval=pc+dist;
    if (flag) {
		uint8_t diff= ((pc&0x100)!=(wval&0x100))?2:1;
    	cycles+= diff; // + 1 if branch occurs to same page/ + 2 if branch occurs to different page
		totalCycles+= diff;
		pc=wval; 
	}
}

void cpuInit(void)
{
	pc= 0;
	a= x= y= s= p= 0;
	bval= 0;
	wval= 0;
	
	// status
	cycles= 0;
	totalCycles= 0;

	fakeCountD012= 0;
	fakeLoopD012= 0;	
}

void cpuRegReset(void)
{
    a=x=y=0;
    p=0;
    s=255; 
    pc= 0;
}

void cpuReset(uint16_t npc, uint8_t na) {
	cpuRegReset();
	a=na;
	pc=npc;	
	
	push(0);
	push(0);
}

// KNOWN LIMITATION: flag handling in BCD mode is not implemented (see http://www.oxyron.de/html/opcodes02.html)
void cpuParse(void)
{
    uint8_t opc=memGet(pc++);
	
    int32_t cmd=opcodes[opc];
    int32_t mode=modes[opc];
	
	uint8_t diff= opbaseFrameCycles[opc];	// see adjustments in "branch", "putaddr" and "getaddr"
	cycles += diff;
	totalCycles += diff;
    
	int32_t c;  
    switch (cmd)
    {
        case adc: {
			uint8_t in1= a;
			uint8_t in2= getaddr(opc, mode);
			
			// note: The carry flag is used as the carry-in (bit 0) for the operation, and the 
			// resulting carry-out (bit 8) value is stored in the carry flag.
            wval=(uint16_t)in1+in2+((p&FLAG_C)?1:0);	// "carry-in"
            setflags(FLAG_C, wval&0x100);
            a=(uint8_t)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
			
			// calc overflow flag (http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html)
			// also see http://www.6502.org/tutorials/vflag.html
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ a)&0x80);
			}
            break;
        case anc:	// used by Axelf.sid (Crowther), Whats_Your_Lame_Excuse.sid
            bval=getaddr(opc, mode);
			a= a&bval;
			
			// http://codebase64.org/doku.php?id=base:some_words_about_the_anc_opcode
            setflags(FLAG_C, a&0x80);
			
			// supposedly also sets these (http://www.oxyron.de/html/opcodes02.html)
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break;
        case and:
            bval=getaddr(opc, mode);
            a&=bval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break;
        case arr: {		// Whats_Your_Lame_Excurse.sid uses this. sigh "the crappier the song...."
			// AND
            bval=getaddr(opc, mode);
            a&=bval;
			
			// set C+V based on this intermediate state of bits 6+7 (before ROR)
			uint8_t bit7= !!(a&0x80);
			uint8_t bit6= !!(a&0x40);

            c=!!(p&FLAG_C);
			
			setflags(FLAG_V,bit7^bit6);
            setflags(FLAG_C,bit7);
			
			a ^= (-c ^ a) & (1UL << 7);	// "exchange" bit 7 with carry
			
			// ROR - C+V not affected here
            a>>=1;
    		
            setflags(FLAG_N,a&0x80);
            setflags(FLAG_Z,!a);
			}			
            break;
        case asl:
            wval=getaddr(opc, mode);
            wval<<=1;
            setaddr(opc, mode,(uint8_t)wval);
            setflags(FLAG_Z,!(wval&0xff));
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,wval&0x100);
            break;
        case bcc:
            branch(opc, !(p&FLAG_C));
            break;
        case bcs:
            branch(opc, p&FLAG_C);
            break;
        case bne:
            branch(opc, !(p&FLAG_Z));
            break;
        case beq:
            branch(opc, p&FLAG_Z);
            break;
        case bpl:
            branch(opc, !(p&FLAG_N));
            break;
        case bmi:
            branch(opc, p&FLAG_N);
            break;
        case bvc:
            branch(opc, !(p&FLAG_V));
            break;
        case bvs:
            branch(opc, p&FLAG_V);
            break;
        case bit:
            bval=getaddr(opc, mode);
            setflags(FLAG_Z,!(a&bval));
            setflags(FLAG_N,bval&0x80);
            setflags(FLAG_V,bval&0x40);	// bit 6
            break;
        case brk:
			pc= 0; // code probably called non existent ROM routine.. 
			
			/* proper impl would look like this:

			// pc has already been incremented by 1 (see above) 
			// (return address to be stored on the stack is original pc+2 )
			push((pc+1)>>8);
            push((pc+1));
            push(p);
			
			if (cpuGetProgramMode() == NMI_OFFSET_MASK) {
				pc=memGet(0xfffa);
				pc|=memGet(0xfffb)<<8;
 			} else {
				pc=memGet(0xfffe);
				pc|=memGet(0xffff)<<8;
 			}
            setflags(FLAG_I,1);
            setflags(FLAG_B,1);
			*/
            break;
        case clc:
            setflags(FLAG_C,0);
            break;
        case cld:
            setflags(FLAG_D,0);
            break;
        case cli:
            setflags(FLAG_I,0);
            break;
        case clv:
            setflags(FLAG_V,0);
            break;
        case cmp:
            bval=getaddr(opc, mode);
            wval=(uint16_t)a-bval;
            setflags(FLAG_Z,!wval);		// a == bval
            setflags(FLAG_N,wval&0x80);	// a < bval
            setflags(FLAG_C,a>=bval);
            break;
        case cpx:
            bval=getaddr(opc, mode);
            wval=(uint16_t)x-bval;
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);      
            setflags(FLAG_C,x>=bval);
            break;
        case cpy:
            bval=getaddr(opc,mode);
            wval=(uint16_t)y-bval;
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);      
            setflags(FLAG_C,y>=bval);
            break;
        case dcp:		// used by: Clique_Baby.sid, Musik_Run_Stop.sid
            bval=getaddr(opc, mode);
			// dec
            bval--;
            setaddr(opc,mode,bval);
			// cmp
            wval=(uint16_t)a-bval;
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,a>=bval);
            break;
        case dec:
            bval=getaddr(opc, mode);
            bval--;
            setaddr(opc,mode,bval);
            setflags(FLAG_Z,!bval);
            setflags(FLAG_N,bval&0x80);
            break;
        case dex:
            x--;
            setflags(FLAG_Z,!x);
            setflags(FLAG_N,x&0x80);
            break;
        case dey:
            y--;
            setflags(FLAG_Z,!y);
            setflags(FLAG_N,y&0x80);
            break;
        case eor:
            bval=getaddr(opc, mode);
            a^=bval;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case inc:
            bval=getaddr(opc, mode);
            bval++;
            setaddr(opc,mode,bval);
            setflags(FLAG_Z,!bval);
            setflags(FLAG_N,bval&0x80);
            break;
        case inx:
            x++;
            setflags(FLAG_Z,!x);
            setflags(FLAG_N,x&0x80);
            break;
        case iny:
            y++;
            setflags(FLAG_Z,!y);
            setflags(FLAG_N,y&0x80);
            break;
        case isb: {
			// inc
            bval=getaddr(opc, mode);
            bval++;
            setaddr(opc,mode,bval);
            setflags(FLAG_Z,!bval);
            setflags(FLAG_N,bval&0x80);

			// + sbc			
			uint8_t in1= a;
			uint8_t in2= bval;
			
            wval=(uint16_t)in1+in2+((p&FLAG_C)?1:0);
            setflags(FLAG_C, wval&0x100);
            a=(uint8_t)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
			
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ a)&0x80);
            }
			break;
		case jam:	// this op would have crashed the C64
		    pc=0;           // just quit the emulation
            break;
        case jmp:
            wval=memGet(pc++);
            wval|=memGet(pc++)<<8;
            switch (mode) {
                case abs:
					if ((wval==pc-3) && (cpuGetProgramMode() == MAIN_OFFSET_MASK)) {
						pc= 0;	// main loop would steal cycles from NMI/IRQ which it normally would not..
					} else {
						pc=wval;
					}
                    break;
                case ind:
					// 6502 bug: JMP ($12FF) will fetch the low-byte from $12FF and the high-byte from $1200
                    pc=memGet(wval);
                    pc|=memGet((wval==0xff) ? 0 : wval+1)<<8;
                    break;
            }
            break;
        case jsr:
			// pc has already been incremented by 1 (see above) 
			// (return address to be stored on the stack is original pc+2 )
            push((pc+1)>>8);
            push((pc+1));
            wval=memGet(pc++);
            wval|=memGet(pc++)<<8;
            pc=wval;
            break;
		case lax:
			// e.g. Vicious_SID_2-15638Hz.sid
            a=getaddr(opc, mode);
			x= a;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
		case lxa: 	// Whats_Your_Lame_Excuse.sid - LOL only real dumbshit player uses this op..
            bval=getaddr(opc, mode);
			a|= 0xff;	// roulette what the specific CPU uses here
			a&= bval;
			
			x= a;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;				
        case lda:
            a=getaddr(opc, mode);
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case ldx:
            x=getaddr(opc, mode);
            setflags(FLAG_Z,!x);
            setflags(FLAG_N,x&0x80);
            break;
        case ldy:
            y=getaddr(opc, mode);
            setflags(FLAG_Z,!y);
            setflags(FLAG_N,y&0x80);
            break;
        case lsr:      
            bval=getaddr(opc, mode); 
			wval=(uint8_t)bval;
            wval>>=1;
            setaddr(opc,mode,(uint8_t)wval);
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,bval&1);
            break;
        case nop:
			getaddr(opc, mode);	 // make sure the PC is advanced correctly
            break;
        case ora:
            bval=getaddr(opc, mode);
            a|=bval;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case pha:
            push(a);
            break;
        case php:
            push(p);
            break;
        case pla:
            a=pop();
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case plp:
            p=pop();
            break;
        case rla:				// see Spasmolytic_part_6.sid
			// rol
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&0x80);
            bval<<=1;
            bval|=c;
            setaddr(opc,mode,bval);

			// + and
            a&=bval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break;
        case rol:
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&0x80);
            bval<<=1;
            bval|=c;
            setaddr(opc,mode,bval);
            setflags(FLAG_N,bval&0x80);
            setflags(FLAG_Z,!bval);
            break;
        case ror:
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&1);
            bval>>=1;
            bval|= 0x80*c;
            setaddr(opc,mode,bval);
            setflags(FLAG_N,bval&0x80);
            setflags(FLAG_Z,!bval);
            break;
        case rra:
			// ror
            bval=getaddr(opc, mode);
            c=!!(p&FLAG_C);
            setflags(FLAG_C,bval&1);
            bval>>=1;
            bval|=0x80*c;
            setaddr(opc,mode,bval);

			// + adc
			uint8_t in1= a;
			uint8_t in2= bval;
			
            wval=(uint16_t)in1+in2+((p&FLAG_C)?1:0);
            setflags(FLAG_C, wval&0x100);
            a=(uint8_t)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
			
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ a)&0x80);
            break;
        case rti:
			// timing hack: some optimized progs use JMP to an RTI that is placed such that
			// the nearby interrupt status register is implicitly read - automatically  
			// acknowledging the interrupt without having to explicitly read the register.
			switch(pc) {
				case 0xdc0d:
				case 0xdd0d:
					memGet(pc);
					break;
			}

			p=pop();                        
            wval=pop();
            wval|=pop()<<8;
            pc=wval;	// not like 'rts'! correct address is expected here!
			
			// todo: if interrupts where to be handled correctly then we'd need to 
			// clear interrupt flag here (and set it when NMI is invoked...)
            break;
        case rts:
            wval=pop();
            wval|=pop()<<8;
			pc=wval+1;
            break;
        case sbc:    {
            bval=getaddr(opc, mode)^0xff;
			
			uint8_t in1= a;
			uint8_t in2= bval;
						
            wval=(uint16_t)in1+in2+((p&FLAG_C)?1:0);
            setflags(FLAG_C, wval&0x100);
            a=(uint8_t)wval;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
			
			setflags(FLAG_V, (~(in1 ^ in2))&(in1 ^ a)&0x80);
			}
            break;
        case sax:				// e.g. Vicious_SID_2-15638Hz.sid
			getaddr(opc, mode);	 // make sure the PC is advanced correctly
            bval=a&x;
			setaddr(opc,mode,bval);
            break;
		case sbx:	// used in Artefacts.sid, Whats_Your_Lame_Excuse.sid	
			// affects N Z and C (like CMP)
			bval=getaddr(opc, mode);
			
            setflags(FLAG_C,(x&a)>=bval);

			x=(x&a)-bval;

            setflags(FLAG_Z,!x);		// a == bval
            setflags(FLAG_N,x&0x80);	// a < bval		
			break;
        case sec:
            setflags(FLAG_C,1);
            break;
        case sed:
            setflags(FLAG_D,1);
            break;
        case sei:
            setflags(FLAG_I,1);
            break;
        case slo:			// see Spasmolytic_part_6.sid
			// asl
            wval=getaddr(opc, mode);
            wval<<=1;
            setaddr(opc,mode,(uint8_t)wval);
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,wval&0x100);
			// + ora
            bval=wval & 0xff;
            a|=bval;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);			
            break;
        case sre:      		// see Spasmolytic_part_6.sid
			// lsr
            bval=getaddr(opc, mode); 
			wval=(uint8_t)bval;
            wval>>=1;
            setaddr(opc,mode,(uint8_t)wval);
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,bval&1);
			// + eor
            bval=wval & 0xff;
            a^=bval;
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
        case sta:
            putaddr(opc,mode,a);
            break;
        case stx:
            putaddr(opc,mode,x);
            break;
        case sty:
            putaddr(opc,mode,y);
            break;
        case tax:
            x=a;
            setflags(FLAG_Z, !x);
            setflags(FLAG_N, x&0x80);
            break;
        case tay:
            y=a;
            setflags(FLAG_Z, !y);
            setflags(FLAG_N, y&0x80);
            break;
        case tsx:
			x=s;
            setflags(FLAG_Z, !x);
            setflags(FLAG_N, x&0x80);
            break;
        case txa:
            a=x;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break;
        case txs:
            s=x;
            break;
        case tya:
            a=y;
            setflags(FLAG_Z, !a);
            setflags(FLAG_N, a&0x80);
            break; 
/* hard to think of a good reason why anybody would ever use this..
		case shs:	// 	TAS						
			s= a&x;
			bval= (memGet(pc+1) + 1) & s;

			getaddr(opc, mode);	 		// make sure the PC is advanced correctly
			setaddr(opc,mode,bval);		// setaddr
		
            break;
*/			
		// ----- D012 hacks ->	
        case c_a:
			/*
			hack:
			
			this op (0x12) - which is normally illegal (and would freeze the machine)
			can be used to patch programs that use CMP $D012 for comparisons (see 'l_a' 
			for explanation). the op: "0x12 0x11 0x99" allows to supply 2 infos: $11 
			the countdown value and $99 which is unused each use of the op drives the 
			countdown (hack only implemented for abs mode, i.e. 3 byte op)
			*/
            wval=memGet(pc++);	// countdown
            memGet(pc++);  // unused			
			bval= 0;
			if (!fakeCountD012) {
				fakeCountD012= wval & 0xff;
			} 
			if (--fakeCountD012 == 0) {
				bval= a;
			}	
            wval=(uint16_t)a-bval;
            setflags(FLAG_Z,!wval);
            setflags(FLAG_N,wval&0x80);
            setflags(FLAG_C,a>=bval);
            break;			
        case l_a:
			/*
			hack:
			
			this op (0x02) - which is normally illegal (and would freeze the machine)
			can be used to patch programs that poll LDA $D012 for comparisons (see NMI 
			sample player in Wonderland_XII-Digi_part_1.sid). Since VIC is not properly 
			simulated during NMI handling, respective conditions will never work.. for 
			the above case some fixed size loop is probably a better -but far from 
			correct- solution.

			the op: "0x02 0x11 0x22" allows to supply 2 infos: $11 the countdown value 
			and $22 the fake result that is returned at the end of the countdown.. 
			each use of the op drives the countdown
			
			only implemented for abs mode, i.e. 3 byte op. also see "c_a"
			*/
            bval=memGet(pc++);	// countdown
            wval=memGet(pc++);  // success result
			
			a= 0;
			if (!fakeCountD012 && !fakeLoopD012) {
				fakeCountD012= bval;
				fakeLoopD012= 40;	// slow down more
			}
			if (--fakeCountD012 == 0) {
					fakeCountD012= bval;
				if (--fakeLoopD012 == 0) {
					a= wval;
					fakeLoopD012= 40;	// slow down more
				}
			}			
            setflags(FLAG_Z,!a);
            setflags(FLAG_N,a&0x80);
            break;
			
		default:			
#ifdef DEBUG
			fprintf(stderr, "op code not implemented: %d at %d\n", opc, pc);
#endif
			getaddr(opc, mode);	 // at least make sure the PC is advanced correctly (potentially used in timing)
    }
}

void cpuResetToIrq(uint16_t npc) {
	/*
	provide dummy return address - which we use to return from the emulation:
	in case of RTI (e.g. progs implementing $fffe/f vector directly) this will be used "as is".
	if some program was to return with "RTS" (e.g. legacy PSID) the address would be returned as $0001.
	*/
	
	push(0);	// addr high
	push(0);	// addr low
	push(p);	// processor status (processor would do this in case of interrupt...)	
	
	// only set pc and keep current stackpointer and registers in case player directly passes 
	// them between calls (see Look_sharp.sid)
	pc= npc;	
}


