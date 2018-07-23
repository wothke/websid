/*
* Poor man's emulation of the C64's SID.
*
* <p>Tiny'R'Sid (c) 2018 J.Wothke
* <p>version 0.81
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef TINYRSID_ENVELOPE_H
#define TINYRSID_ENVELOPE_H

#include "base.h"


/**
* This class handles the envelope generation for one specific voice of the SID chip.
*
* It is a construct exclusively used by the SID class and access is restricted accordingly.
*/
class Envelope {
protected:
	friend class SID;
	Envelope(class SID *sid, uint8_t voice);
	
	/**
	* Static configuration.
	*/
	static void resetConfiguration(uint32_t sampleRate);
	
	/**
	* Reinitialize a specific instance to reuse it.
	*/
	void reset();

	/**
	* Forwards time by the duration of one sample.
	*/
	void updateEnvelope();
	
	/**
	* Updates redundantly kept state information 
	*/
	void syncState();
	
	/**
	* Handle those SID writes that impact the envelope generator.
	*/
	void poke(uint8_t reg, uint8_t val);
	
	/** 
	* Hack uses for ADSR-bug detection.
	*/
	void snapshotLFSR();

	
	/**
	* Gets the currently valid output envelope level.
	*/
	uint8_t getOutput();

	/**
	* Gets the raw AD register.
	*/
	uint8_t getAD();
	/**
	* Gets the raw SR register.
	*/
	uint8_t getSR();	
private:
	uint8_t triggerLFSR_Threshold(uint16_t threshold, uint16_t *end);
	uint8_t handleExponentialDelay();
		// --- ADSR-bug
	uint16_t getCurrentThreshold();
	void simGateAdsrBug(uint8_t scenario, uint16_t newRate);
	int32_t clocksToSamples(int32_t clocks);

	
private:
	friend struct EnvelopeState* getState(Envelope *e);

	void *_state;	// don't want this header file cluttered with all the implementation details..
	class SID* _sid;
	uint8_t _voice;

	static double sCyclesPerSample;	// redundancy used to avoid repeated calcs
	static int32_t sLimitLFSR; // the original cycle counter would be 15-bit (here samples are counted & counter is rescaled accordingly)
	static int32_t sCounterPeriod[16];
	static uint8_t sExponentialDelays[256];
};


#endif
