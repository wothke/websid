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
* Struct used to configure the number/types of used SID chips.
*
* Used to "write through" to the underlying state information.
*/
class SIDConfigurator {
public:
	SIDConfigurator();
	
	void configure(uint8_t is_ext_file, uint8_t sid_file_version, uint16_t flags, uint8_t* addr_list);
protected:
	void init(uint16_t* addr, uint8_t* is_6581, uint8_t* target_chan, uint8_t* second_chan_idx, 
				uint8_t* ext_multi_sid_mode);
				
	static uint16_t getSidAddr(uint8_t center_byte);
	
	friend class SID;
private:
	uint16_t* _addr;				// array of addresses
	uint8_t* _is_6581;			// array of models
	
	// use of stereo:
	uint8_t* _target_chan;		// array of used channel-idx (for stereo scenarios)
	uint8_t* _second_chan_idx;	// single int: index of the 1st SID that maps to the 2nd stereo channel
	uint8_t* _ext_multi_sid_mode;// single int: activates "extended sid file" handling	
};

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
	void reset(uint16_t addr, uint32_t sample_rate, uint8_t is_6581, uint32_t clock_rate, 
				 uint8_t is_rsid, uint8_t is_compatible, uint8_t output_channel);
	void resetStatistics();
		
	/**
	* Directly updates one of the SID's registers.
	*
	* DOES NOT reflect in the memory mapped IO area.
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
	void synthSample(int16_t* buffer, int16_t** synth_trace_bufs, uint32_t offset, double* scale, uint8_t do_clear);
	void synthSampleStripped(int16_t* buffer, int16_t** synth_trace_bufs, uint32_t offset, double* scale, uint8_t do_clear);

	/**
	* Clocks this instance by one system cycle.
	*/	
	void clock();
	
	// ------------- class level functions ----------------------
	
	/**
	* Measures the length if one sample in system cycles.
	*/
	static double getCyclesPerSample();

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
	static void resetAll(uint32_t sample_rate, uint32_t clock_rate, uint8_t is_rsid, 
							uint8_t is_compatible);

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
		
	static uint8_t isAudible();

	/**
	* Renders the combined output of all currently used SIDs.
	*/
	static void	synthSample(int16_t* buffer, int16_t** synth_trace_bufs, double* scale, uint32_t offset);
	static void	synthSampleStripped(int16_t* buffer, int16_t** synth_trace_bufs, double* scale, uint32_t offset);

	
	// ---------- HW configuration -----------------
	static struct SIDConfigurator* getHWConfigurator();
	
	static uint8_t isSID6581();
	/**
	* Manually override original "SID model" setting from music file.
	*/
	static uint8_t setSID6581(uint8_t is6581);
	
#ifdef PSID_DEBUG_ADSR
	void debugVoice(uint8_t voice);
#endif
protected:
	friend class Envelope;
	friend class DigiDetector;
	
	// API exposed to internal (SID related) components..
	uint8_t		getWave(uint8_t voice);
	uint8_t		getAD(uint8_t voice);
	uint8_t		getSR(uint8_t voice);
	uint16_t	getFreq(uint8_t voice);
	uint16_t	getPulse(uint8_t voice);

	static uint8_t isExtMultiSidMode();
	static uint8_t peek(uint16_t addr);
	
	DigiType	getDigiType();
	const char*	getDigiTypeDesc();
	uint16_t	getDigiRate();
		
	static uint32_t	getSampleFreq();
	void		setMute(uint8_t voice, uint8_t value);
private:
	/**
	* Reconfigures all used chips to the specified model.
	*
	* @param is_6581 array with one byte corresponding to each available chip
	*/
	static void	setModels(const uint8_t* is_6581);
	
	// convenience accessors & utilities
	void		resetEngine(uint32_t sample_rate, uint8_t is_6581, uint32_t clock_rate);
	uint32_t	getRingModCounter(uint8_t voice);
	
	// oscillator handling
	void		syncOscillator(uint8_t voice);
	
	void		clockOscillators();
		
	// wave form generation
	void		updateFreqCache(uint8_t voice);
	
	uint16_t	combinedWF(uint8_t channel, double* wfarray, uint16_t index, uint8_t differ6581);
	uint16_t	createTriangleOutput(uint8_t voice);
	uint16_t	createSawOutput(struct Oscillator* osc);
	void		calcPulseBase(struct Oscillator* osc, uint32_t* tmp, uint32_t* pw);
	uint16_t	createPulseOutput(struct Oscillator* osc, uint32_t tmp, uint32_t pw);
	uint16_t	createNoiseOutput(struct Oscillator* osc);
	uint16_t	createWaveOutput(int8_t voice);	// main entry
	
protected:
	struct SidState* _sid;	// as long as the legacy C accessors are still there
	class DigiDetector* _digi;
private:
	struct Oscillator* _osc[3];
	
	class Envelope* _env[3];
	class Filter* _filter;
	
	uint16_t _addr;			// start memory address that the SID is mapped to
	uint8_t _dest_channel;		// which stereo channel to output to
};

#endif
