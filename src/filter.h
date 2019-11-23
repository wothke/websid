/*
* Poor man's emulation of the C64 SID's filter.
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef WEBSID_FILTER_H
#define WEBSID_FILTER_H

extern "C" {
#include "base.h"
}
/**
* This class handles the filter of a SID chip.
*
* It is a construct exclusively used by the SID class and access is restricted accordingly.
*/
class Filter {
protected:
	Filter(class SID *sid);
	
	void reset(uint32_t sampleRate);
	int32_t getOutput(int32_t *in, int32_t *out, double cyclesPerSample);
	int32_t simOutput(uint8_t voice, uint8_t isFiltered, int32_t *in, int32_t *out);

	double runFilter(double in, double output, double *prevbandpass, double *prevlowpass);
	
	/**
	* @return true if voice is filtered
	*/
	uint8_t routeSignal(int32_t *voiceOut, int32_t *outf, int32_t *outo, uint8_t voice, uint8_t *voiceEnabled);
	/**
	* Handle those SID writes that impact the filter.
	*/
	void poke(uint8_t reg, uint8_t val);
	
	uint8_t getVolume();
protected:
	void resetInput6581(int32_t filtin);
	void resetInput8580();
private:
	friend class SID;
	friend class DigiDetector;
	friend struct FilterState* getState(Filter *e);

	void *_state;	// don't want this header file cluttered with all the implementation details..
	class SID* _sid;
};


#endif
