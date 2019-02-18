/*
* Poor man's emulation of the C64's SID.
*
* <p>Tiny'R'Sid (c) 2018 J.Wothke
* <p>version 0.81
* 
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
#ifndef TINYRSID_FILTER_H
#define TINYRSID_FILTER_H

#include "base.h"


/**
* This class handles the filter of a SID chip.
*
* It is a construct exclusively used by the SID class and access is restricted accordingly.
*/
class Filter {
protected:
	friend class SID;
	Filter(class SID *sid);
	
	void reset(uint32_t sampleRate);
	int32_t getOutput(int32_t *in, int32_t *out, double cutoff, double resonance, double cyclesPerSample);
	int32_t simOutput(uint8_t voice, uint8_t isFiltered, int32_t *in, int32_t *out, double cutoff, double resonance);

	void filterSamples(uint8_t *digiBuffer, uint32_t len, int8_t voice);
	double runFilter(double in, double output, double *prevbandpass, double *prevlowpass, double cutoff, double resonance);
	void setupFilterInput(double *cutoff, double *resonance);
	
	/**
	* @return true if voice is filtered
	*/
	uint8_t routeSignal(int32_t *voiceOut, int32_t *outf, int32_t *outo, uint8_t voice, uint8_t *voiceEnabled);
	/**
	* Handle those SID writes that impact the filter.
	*/
	void poke(uint8_t reg, uint8_t val);
	
	uint8_t getVolume();

private:
private:
	friend struct FilterState* getState(Filter *e);

	void *_state;	// don't want this header file cluttered with all the implementation details..
	class SID* _sid;
};


#endif
