/*
* Poor man's emulation of the SID's digi sample playback features.
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

#ifndef WEBSID_DIGI_H
#define WEBSID_DIGI_H

extern "C" {
#include "base.h"
}

// FM detector states
typedef enum {
    FreqIdle=0,
    FreqPrep=1,
    FreqSet=2,
	
    FreqVariant1=3,
    FreqVariant2=4,

} FreqDetectState;

// PWM detector states
typedef enum {
    PulseIdle=0,
	// variant 2
    PulsePrep=1,
    PulseConfirm=2,
	
	// variant 2
	PulsePrep2= 3,
	PulseConfirm2= 4
} PulseDetectState;


class DigiDetector {
protected:
	friend class SID;
	
	DigiDetector(class SID *sid);
	
	// setup
	void reset(uint8_t compatibility, uint8_t is_6581);
	void resetModel(uint8_t is_6581);
	void resetCount();

	// result accessors
	uint8_t getSample(); // get whatever digi-sample has last been written
	uint8_t getSource();
	uint8_t isFiltered();
	int32_t genPsidSample(int32_t sample_in);	// legacy PSID digis

	// detection of sample playback
	uint8_t detectSample(uint16_t addr, uint8_t value);

	// diagnostics
	DigiType getType();
	const char * getTypeDesc();
	uint16_t getRate();
private:
	// what a fucking joke language to have all those impl details in the interface
	void recordSample(uint8_t sample, uint8_t voice);
	uint8_t assertSameSource(uint8_t voice_plus);
	
	uint8_t isWithinFreqDetectTimeout(uint8_t voice);
	uint8_t recordFreqSample(uint8_t voice, uint8_t sample);
	uint8_t handleFreqModulationDigi(uint8_t voice, uint8_t reg, uint8_t value);
	
	uint8_t isWithinPulseDetectTimeout(uint8_t voice);
	uint8_t recordPulseSample(uint8_t voice, uint8_t sample);
	uint8_t handlePulseModulationDigi(uint8_t voice, uint8_t reg, uint8_t value);

	uint8_t handleIceGuysDigi(uint8_t voice, uint8_t reg, uint8_t value);

	uint8_t isMahoneyDigi();
	
	uint8_t setSwallowMode(uint8_t voice, uint8_t m);
	uint8_t handleSwallowDigi(uint8_t voice, uint8_t reg, uint16_t addr, uint8_t value);

	uint8_t getD418Sample( uint8_t value);

private:
	SID *_sid;
	uint16_t _baseAddr;
	
	int8_t _digiSource;	// lo-nibble: voice +1 
	
	uint16_t _digiCount;
	uint8_t _isC64compatible;

	// diagnostic information for GUI use
	DigiType _usedDigiType;

	// last detected
	uint8_t _currentDigiSample;
	uint8_t _currentDigiSrc;

	// FM: tracked timing state
	uint8_t _fm_count;
	FreqDetectState _freqDetectState[3];
	uint32_t _freqDetectTimestamp[3];
	uint8_t _freqDetectDelayedSample[3];
	
	// PWM: tracked timing state
	PulseDetectState _pulseDetectState[3];
	uint32_t _pulseDetectTimestamp[3];
	uint8_t _pulseDetectDelayedSample[3];
	uint8_t _pulseDetectMode[3];	// 2= Pulse width LO/ 3= Pulse width HI
	
	// swallow's PWM
	uint16_t _swallowPWM[3];
	
	// nmi d418 (hack)
	uint8_t _use_non_nmi_D418;
	uint8_t _non_nmi_count_D418;
	
};


#endif