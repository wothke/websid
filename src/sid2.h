/*
* Poor man's emulation of the C64's SID.
*
* <p>Tiny'R'Sid (c) 2018 J.Wothke
* <p>version 0.91
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef TINYRSID_SID2_H
#define TINYRSID_SID2_H

#include "base.h"

// as long as there is still remainders of the old .c code I'll
// keep these.. (FIXME cleanup once the refactoring progresses)
extern "C" uint8_t sidGetWave(uint8_t voice);
extern "C" uint8_t sidGetAD(uint8_t voice);
extern "C" uint8_t sidGetSR(uint8_t voice);
extern "C" void sidFilterSamples (uint8_t *digiBuffer, uint32_t len, int8_t voice);
extern "C" uint32_t sidGetSampleFreq();
extern "C" uint16_t sidGetFreq(uint8_t voice);
extern "C" uint16_t sidGetPulse(uint8_t voice);
extern "C" void sidSetMute(uint8_t voice, uint8_t value);

/**
* This class emulates one "MOS Technology SID" chip (see 6581, 8580 or 6582).
*
* Some aspects of the implementation are delegated to separate helpers, 
* see digi.c, envelope.h, filter.h
* 
* I had almost forgotten what a garbage language C++ is.. 
*/
class SID {
public:
	SID(uint16_t addr);
	
	/**
	* Resets this instance according to the passed params.
	*/
	void reset(uint32_t sampleRate, uint8_t isModel6581);
		
	/**
	* Directly updates one of the SID's registers.
	*
	* <p>DOES NOT reflect in the memory mapped IO area.
	*/
	void poke(uint8_t reg, uint8_t val);
		
	/**
	* Handles read access to the IO area that this SID is mapped into.
	*/
	uint8_t readMem(uint16_t addr);

	/**
	* Handles write access to the IO area that this SID is mapped into.
	*/
	void writeMem(uint16_t addr, uint8_t value);

	/**
	* Gets the type of SID that is emulated.
	*/
	uint8_t isModel6581();
	
	/**
	* Generates sample output data based on the current SID state.
	* 
	* During rendering the SID's state is updated to reflect the 
	* passed time.
	*
	* @param synthTraceBufs when used it must be an array[4] containing
	*                       buffers of length "len"
	*/	
	void synthRender(int16_t *buffer, uint32_t len, int16_t **synthTraceBufs);
protected:
	friend class Envelope;
	
	// API exposed to internal components..
	uint8_t getWave(uint8_t voice);
	uint8_t getAD(uint8_t voice);
	uint8_t getSR(uint8_t voice);
	void filterSamples (uint8_t *digiBuffer, uint32_t len, int8_t voice);

private:
	// convenience accessors & utilities
	void resetEngine(uint32_t sampleRate, uint8_t isModel6581);
	static uint8_t getPreviousVoice(uint8_t voice);
	static uint8_t getNextVoice(uint8_t voice);
	uint32_t getRingModCounter(uint8_t voice);

	void syncRegisterCache();
	
	// oscillator handling
	void syncOscillator(uint8_t voice);
	void advanceOscillators();
		// hack: fake osc3 read-out
	uint8_t useOscPollingHack();
	void simStartOscillatorVoice3(uint8_t voice, uint8_t val);
	uint32_t simOsc3Counter();
	uint8_t simReadSawtoothD41B();
	uint8_t simReadPulsedD41B();
	uint8_t simReadD41B();

	// wave form generation
	uint16_t combinedWF(uint8_t channel, double *wfarray, uint16_t index, uint8_t differ6581);
	uint16_t createTriangleOutput(uint8_t voice);
	uint16_t createSawOutput(uint8_t voice);
	void calcPulseBase(uint8_t voice, uint32_t *tmp, uint32_t *pw);
	uint16_t createPulseOutput(uint8_t voice, uint32_t tmp, uint32_t pw);
	uint16_t createNoiseOutput(uint8_t voice);
	uint16_t createWaveOutput(int8_t voice);	// main entry
	
protected:
	friend uint8_t sidGetWave(uint8_t voice);
	friend uint8_t sidGetAD(uint8_t voice);
	friend uint8_t sidGetSR(uint8_t voice);
	friend void sidFilterSamples (uint8_t *digiBuffer, uint32_t len, int8_t voice);
	friend uint32_t sidGetSampleFreq();
	friend uint16_t sidGetFreq(uint8_t voice);
	friend uint16_t sidGetPulse(uint8_t voice);
	friend void sidSetMute(uint8_t voice, uint8_t value);

	struct SidState *_sid;	// as long as the legacy C accessors are still there
private:
	struct Oscillator *_osc[3];
	struct SimOsc3 *_osc3sim;
	
	class Envelope *_env[3];
	class Filter *_filter;
	
	uint16_t _addr;		// start memory address that the SID is mapped to
};


#endif
