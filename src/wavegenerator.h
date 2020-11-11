/*
* Handles the wave output of a single voice of the C64's "MOS Technology SID" (see 6581, 8580 or 6582).
*
* WebSid (c) 2020 Jürgen Wothke
* version 0.95
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef WEBSID_VOICE_H
#define WEBSID_VOICE_H

#include "base.h"


// XXX new resampling seems to cause problems: see We_Are_Demo_tune_2.sid!
// other test cases: Synthesis.sid
#define USE_HERMIT_ANTIALIAS

class WaveGenerator {
protected:
	friend class SID;								// the only user of Voice
	
	WaveGenerator(class SID* sid, uint8_t voice_idx);
	
	void reset();
	
	// oscillator handling
	void		clockOscillator();
	void		syncOscillator();

	void		setMute(uint8_t is_muted);
	uint8_t		isMuted();
	
	// access to related registers
	void		setWave(uint8_t wave);
	uint8_t		getWave();

	void		setPulseWidthLow(uint8_t val);
	void		setPulseWidthHigh(uint8_t val);
	uint16_t	getPulse();

	void		setFreqLow(uint8_t val, double cycles_per_sample);
	void		setFreqHigh(uint8_t val, double cycles_per_sample);
	uint16_t	getFreq();
	
	// waveform generation
	uint16_t	getOutput();
	
private:
	// utils for waveform generation
	void		updateFreqCache(double cycles_per_sample);
	uint16_t	combinedWF(double* wfarray, uint16_t index, uint8_t differ6581);
	uint16_t	createTriangleOutput();
	uint16_t	createSawOutput();
	
#ifdef USE_HERMIT_ANTIALIAS	
	void		calcPulseBase(uint32_t* tmp, uint32_t* pw);
	uint16_t	createPulseOutput(uint32_t tmp, uint32_t pw);
#else	
	void		updatePulseCache();
	uint16_t	createPulseOutput();
#endif
	uint16_t	createNoiseOutput(uint16_t outv, uint8_t is_combined);
		
private:
	class SID*	_sid;
	uint8_t		_voice_idx;

	uint8_t		_is_muted;			// player's separate "mute" feature
	
	// base oscillator state
    uint32_t	_counter;			// aka "accumulator" (24-bit)
	uint16_t	_freq;				// counter increment per cycle
	uint8_t		_msb_rising;		// hard sync handling
	
	// waveform generation
    uint8_t		_ctrl;				// waveform control register
		// performance opt: redundant flags from _ctrl
	uint8_t		_test_bit;
	uint8_t		_sync_bit;
	uint8_t		_ring_bit;
	
	double		_freq_inc_sample;

		// pulse waveform
	uint16_t	_pulse_width;		// 12-bit "pulse width" from respective SID registers
#ifdef USE_HERMIT_ANTIALIAS
	uint32_t	_pulse_out;			// 16-bit "pulse width" _pulse_width<<4 shifted for convenience)
	uint32_t	_freq_pulse_base;	// Hermit's "anti-aliasing"
	double		_freq_pulse_step;	// Hermit's "anti-aliasing"
	
	// saw waveform
	double		_freq_saw_step;		// Hermit's "anti-aliasing"	
#else
	double		_freq_inc_sample_inv;
	double		_ffff_freq_inc_sample_inv;
	uint32_t	_pulse_width12;
	uint32_t	_pulse_width12_neg;
	uint32_t	_pulse_width12_plus;
	
		// saw waveform
	uint16_t	_saw_range;
	uint16_t	_saw_base;
#endif

		// noise waveform
	uint8_t		_noise_oversample;	// number of noise-updates per sample
    uint32_t	_noisepos;			// update trigger
    uint32_t	_noise_LFSR;
    uint16_t	_noiseout;			// wave output
		

	// add-ons snatched from Hermit's implementation
	double		_prev_wav_data;		// combined waveform handling
	uint16_t 	_prev_wave_form_out;// floating DAC handling
};

#endif