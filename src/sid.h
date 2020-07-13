/*
* Poor man's emulation of the C64's SID.
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef WEBSID_SID_H
#define WEBSID_SID_H

extern "C" {
#include "base.h"
}

/**
* This class emulates the "MOS Technology SID" chips (see 6581, 8580 or 6582).
*
* Some aspects of the implementation are delegated to separate helpers, 
* see digi.h, envelope.h, filter.h
*/
class SID {
public:
	SID();
	
	/**
	* Gets the base memory address that this SID is mapped to.
	*/
	uint16_t getBaseAddr();

	/**
	* Resets this instance according to the passed params.
	*/
	void resetModel(uint8_t is_6581);
	void reset(uint16_t addr, uint32_t sample_rate, uint8_t is_6581, uint8_t compatibility, uint8_t output_channel);
	void resetStatistics();
		
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
	* Gets the last value actually written to this address (even for write-only regs).
	*/
	uint8_t peekMem(uint16_t addr);

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
	* @param synth_trace_bufs when used it must be an array[4] containing
	*                       buffers of at least length "offset"
	*/		
	void synthSample(int16_t *buffer, int16_t **synth_trace_bufs, uint32_t offset, double *scale, uint8_t do_clear);
	void synthSampleStripped(int16_t *buffer, int16_t **synth_trace_bufs, uint32_t offset, double *scale, uint8_t do_clear);

	/**
	* Measures the length if one sample in system cycles.
	*/
	static double getCyclesPerSample();

	/**
	* Clocks this instance by one system cycle.
	*/	
	void clock();
	
	// ------------- class level functions ----------------------
	
	/**
	* Rescaling that should be used on the output signal (depending on the used number of SID chips).
	*/
	static double getScale();

	/**
	* Total number of SID chips used in the current song.
	*/
	static uint8_t getNumberUsedChips();
		
	/**
	* Resets all used SID chips.
	*/
	static void resetAll(uint32_t sample_rate, uint8_t compatibility, uint8_t resetVol);

	/**
	* Clock all used SID chips.
	*/
	static void	clockAll();
		
	/**
	* Gets the type of digi samples used in the current song.
	*/	
	static DigiType getGlobalDigiType();
	
	/**
	* Gets textual representation the type of digi samples used in the current song.
	*/	
	static const char* getGlobalDigiTypeDesc();
	

	/**
	* Resets whatever is is that might be counted.
	*/	
	static void	resetGlobalStatistics();
	
	/**
	* Gets rate of digi samples used in the current song.
	*/	
	static uint16_t getGlobalDigiRate();
	
	/**
	* Allows to mute/unmute a spectific voice.
	*/
	static void	setMute(uint8_t sid_idx, uint8_t voice, uint8_t value);
		
	/**
	* Reconfigures all used chips to the specified model.
	* @param is_6581 array with one byte corresponding to each available chip
	*/
	static void	setModels(uint8_t *is_6581);

	static uint8_t isAudible();

	/**
	* Renders the combined output of all currently used SIDs.
	*/
	static void	synthSample(int16_t *buffer, int16_t **synth_trace_bufs, double* scale, uint32_t offset);
	static void	synthSampleStripped(int16_t *buffer, int16_t **synth_trace_bufs, double* scale, uint32_t offset);
	
protected:
	friend class Envelope;
	friend class DigiDetector;
	
	// API exposed to internal (SID related) components..
	uint8_t		getWave(uint8_t voice);
	uint8_t		getAD(uint8_t voice);
	uint8_t		getSR(uint8_t voice);
	uint16_t	getFreq(uint8_t voice);
	uint16_t	getPulse(uint8_t voice);

	DigiType	getDigiType();
	const char*	getDigiTypeDesc();
	uint16_t	getDigiRate();
		
	static uint32_t	getSampleFreq();
	void		setMute(uint8_t voice, uint8_t value);
private:
	// convenience accessors & utilities
	void		resetEngine(uint32_t sample_rate, uint8_t is_6581);
	uint32_t	getRingModCounter(uint8_t voice);
	
	// oscillator handling
	void		syncOscillator(uint8_t voice);
	
	void		clockOscillators();
		
	// wave form generation
	void		updateFreqCache(uint8_t voice);
	
	uint16_t	combinedWF(uint8_t channel, double *wfarray, uint16_t index, uint8_t differ6581);
	uint16_t	createTriangleOutput(uint8_t voice);
	uint16_t	createSawOutput(uint8_t voice);
	void		calcPulseBase(uint8_t voice, uint32_t *tmp, uint32_t *pw);
	uint16_t	createPulseOutput(uint8_t voice, uint32_t tmp, uint32_t pw);
	uint16_t	createNoiseOutput(uint8_t voice);
	uint16_t	createWaveOutput(int8_t voice);	// main entry
	
protected:
	struct SidState *_sid;	// as long as the legacy C accessors are still there
	class DigiDetector *_digi;
private:
	struct Oscillator *_osc[3];
	
	class Envelope *_env[3];
	class Filter *_filter;
	
	uint16_t _addr;			// start memory address that the SID is mapped to
	uint8_t _dest_channel;		// which stereo channel to output to
};


#endif
