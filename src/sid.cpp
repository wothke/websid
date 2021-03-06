/*
* This emulates the C64's "MOS Technology SID" (see 6581, 8580 or 6582).
*
* It handles the respective SID voices and filters. Envelope generator with
* ADSR-delay bug support, waveform generation including combined waveforms.
* Supports all SID features like ring modulation, hard-sync, etc.
*
* Emulation is performed on a system clock cycle-by-cycle basis.
*
* Anything related to the handling of 'digi samples' is kept separately in digi.c
*
*
* Known limitations:
*
*  - effects of the digital-to-analog conversion (e.g. respective 6581 flaws)
*    are NOT handled
*
* Credits:
* 
*  - TinySid (c) 1999-2012 T. Hinrichs, R. Sinsch (with additional fixes from
*             Markus Gritsch) originally provided the starting point - though
*             very little of that code remains here
* 
*  - Hermit's jsSID.js provided a variant of "resid filter implementation", an
*             "anti aliasing" for "pulse" and "saw" waveforms, and a rather
*             neat approach to generate combined waveforms
*             (see http://hermit.sidrip.com/jsSID.html)
*
* Links:
*
*  - http://www.waitingforfriday.com/index.php/Commodore_SID_6581_Datasheet
*  - http://www.sidmusic.org/sid/sidtech2.html
*
*
* WebSid (c) 2019 Jürgen Wothke
* version 0.93
*
* Terms of Use: This software is licensed under a CC BY-NC-SA
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
 
// todo: run performance tests to check how cycle based oversampling fares as compared to
// current once-per-sample approach

// todo: with the added external filter Hermit's antialiasing might no longer make sense
// and the respective handling may be the source of high-pitched noise during PWM/FM digis..

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "sid.h"
#include "envelope.h"
#include "filter.h"
#include "digi.h"
#include "wavegenerator.h"
#include "memory_opt.h"

extern "C" {
#include "base.h"
#include "memory.h"
};


// --------- HW configuration ----------------

static uint16_t	_sid_addr[MAX_SIDS];		// start addr of SID chips (0 means NOT available)
static uint8_t 	_sid_is_6581[MAX_SIDS];		// models of installed SID chips
static uint8_t 	_sid_target_chan[MAX_SIDS];	// output channel for the SID chips
static uint8_t 	_sid_2nd_chan_idx;			// stereo sid-files: 1st chip using 2nd channel
static uint8_t 	_ext_multi_sid;				// use extended multi-sid mode

SIDConfigurator::SIDConfigurator() {
}

void SIDConfigurator::init(uint16_t* addr, uint8_t* is_6581, uint8_t* target_chan, uint8_t* second_chan_idx,
			uint8_t* ext_multi_sid_mode) {
	_addr = addr;
	_is_6581 = is_6581;
	_target_chan = target_chan;
	_second_chan_idx = second_chan_idx;
	_ext_multi_sid_mode = ext_multi_sid_mode;
}

uint16_t SIDConfigurator::getSidAddr(uint8_t center_byte) {
	if (((center_byte >= 0x42) && (center_byte <= 0xFE)) &&
		!((center_byte >= 0x80) && (center_byte <= 0xDF)) &&
		!(center_byte & 0x1)) {

		return ((uint16_t)0xD000) | (((uint16_t)center_byte) << 4);
	}
	return 0;
}

void SIDConfigurator::configure(uint8_t is_ext_file, uint8_t sid_file_version, uint16_t flags, uint8_t* addr_list) {
	(*_second_chan_idx) = 0;
	
	_addr[0] = 0xd400;
	_is_6581[0] = (flags >> 4) & 0x3;
	_is_6581[0] = !((_is_6581[0] >> 1) & 0x1); 	// only use 8580 when bit is explicitly set
	
	if (!is_ext_file) {	// allow max of 3 SIDs
		(*_ext_multi_sid_mode) = 0;
	
		_target_chan[0] = 0;	// no stereo support
		
		// standard PSID maxes out at 3 sids
		_addr[1] = getSidAddr((addr_list && (sid_file_version>2)) ? addr_list[0x0] : 0);
		_is_6581[1] = (flags >> 6) & 0x3;
		_is_6581[1] = !_is_6581[1] ? _is_6581[0] : !((_is_6581[1] >> 1) & 0x1);
		_target_chan[1] = 0;
		
		_addr[2] = getSidAddr((addr_list && (sid_file_version>3)) ? addr_list[0x1] : 0);
		_is_6581[2] = (flags >> 8) & 0x3;
		_is_6581[2] = !_is_6581[2] ? _is_6581[0] : !((_is_6581[2] >> 1) & 0x1);
		_target_chan[2] = 0;
		
		_addr[3] = 0;
		_is_6581[3] = 0;
		_target_chan[3] = 0;
		
	} else {	// allow max of 10 SIDs
		(*_ext_multi_sid_mode) = 1;
		
		_target_chan[0] = (flags >> 6) & 0x1;
		
		uint8_t prev_chan = _target_chan[0];
		
		// is at even offset so there should be no alignment issue
		uint16_t *addr_list2 = (uint16_t*)addr_list;
		
		uint8_t i, ii;
		for (i= 0; i<(MAX_SIDS-1); i++) {
			uint16_t flags2 = addr_list2[i];	// bytes are flipped here
			if (!flags2) break;
			
			ii = i + 1;
			
			_addr[ii] = getSidAddr(flags2 & 0xff);
			_is_6581[ii] = (flags2 >> 12) & 0x3;
			_target_chan[ii] = (flags2 >> 14) & 0x1;

			if (!(*_second_chan_idx)) {
				if (prev_chan != _target_chan[ii] ) {
					(*_second_chan_idx) = ii;	// 0 is the $d400 SID
					
					prev_chan = _target_chan[ii];
				}
			}
			
			// default to whatever main SID is using
			if (!_is_6581[ii]) _is_6581[ii] = _is_6581[0];
		}
		for (; i<(MAX_SIDS-1); i++) {	// mark as unused
			ii = i + 1;
			
			_addr[ii] = 0;
			_is_6581[ii] = 0;
			_target_chan[ii] = 0;
		}
	}
}

static SIDConfigurator _hw_config;


static uint8_t _used_sids = 0;
static uint8_t _is_audible = 0;

static SID _sids[MAX_SIDS];	// allocate the maximum

// globally shared by all SIDs
static double		_cycles_per_sample;
static uint32_t		_sample_rate;				// target playback sample rate


/**
* This class represents one specific MOS SID chip.
*/
SID::SID() {
	_addr = 0;		// e.g. 0xd400
	
	_env_generators[0] = new Envelope(this, 0);
	_env_generators[1] = new Envelope(this, 1);
	_env_generators[2] = new Envelope(this, 2);
		
	_wave_generators[0] = new WaveGenerator(this, 0);
	_wave_generators[1] = new WaveGenerator(this, 1);
	_wave_generators[2] = new WaveGenerator(this, 2);
		
	_filter = new Filter(this);
	_digi = new DigiDetector(this);
}

WaveGenerator* SID::getWaveGenerator(uint8_t voice_idx) {
	return _wave_generators[voice_idx];
}


void SID::resetEngine(uint32_t sample_rate, uint8_t is_6581, uint32_t clock_rate) {
	// note: structs are NOT packed and contain additional padding..
	
	resetModel(is_6581);
	
	_sample_rate = sample_rate;
	_cycles_per_sample = ((double)clock_rate) / sample_rate;	// corresponds to Hermit's clk_ratio

	for (uint8_t i= 0; i<3; i++) {
		_wave_generators[i]->reset(_cycles_per_sample);
	}
	
	// reset envelope generator
	for (uint8_t i= 0; i<3; i++) {
		_env_generators[i]->reset();
	}

	// reset filter
	_filter->reset(sample_rate);
}

void SID::clockOscillators() {
	// forward oscillators one CYCLE (required to properly time HARD SYNC)
	for (uint8_t voice_idx= 0; voice_idx<3; voice_idx++) {
		WaveGenerator* wave_gen = _wave_generators[voice_idx];
		wave_gen->clockOscillator();
	}
	
	// handle oscillator HARD SYNC (quality wise it isn't worth the trouble to
	// use this correct impl..)
	for (uint8_t voice_idx= 0; voice_idx<3; voice_idx++) {
		WaveGenerator* wave_gen = _wave_generators[voice_idx];
		wave_gen->syncOscillator();
	}
}

double SID::getCyclesPerSample() {
	return _cycles_per_sample;
}

// ------------------------- public API ----------------------------

struct SIDConfigurator* SID::getHWConfigurator() {
	// let loader directly manipulate respective state
	_hw_config.init(_sid_addr, _sid_is_6581, _sid_target_chan, &_sid_2nd_chan_idx, &_ext_multi_sid);
	return &_hw_config;
}

uint8_t SID::isSID6581() {
	return _sid_is_6581[0];		// only for the 1st chip
}

uint8_t SID::setSID6581(uint8_t is6581) {
	// this minimal update should allow to toggle the
	// used filter without disrupting playback in progress
	
	_sid_is_6581[0] = _sid_is_6581[1] = _sid_is_6581[2] = is6581;
	SID::setModels(_sid_is_6581);
	return 0;
}

// who knows what people might use to wire up the output signals of their
// multi-SID configurations... it probably depends on the song what might be
// "good" settings here.. alternatively I could just let the clipping do its
// work (without any rescaling). but for now I'll use Hermit's settings - this
// will make for a better user experience in DeepSID when switching players..

// todo: only designed for even number of SIDs.. if there actually are 3SID
// (etc) stereo songs then something more sophisticated would be needed...

static double _vol_map[] = { 1.0f, 0.6f, 0.4f, 0.3f, 0.3f, 0.3f, 0.3f, 0.3f };
static double _vol_scale;

double SID::getScale() {
	return _vol_scale;
}

uint16_t SID::getBaseAddr() {
	return _addr;
}

void SID::resetModel(uint8_t is_6581) {
	_is_6581 = is_6581;
		
	_bus_write = 0;
		
	// On the real hardware all audio outputs would result in some (positive)
	// voltage level and different chip models would use different base
	// voltages to begin with. Effects like the D418 based output modulation used
	// for digi playback would always scale some positive input voltage.
	
	// On newer chip models the respective base voltage would actually be quite small -
	// causing D418-digis to be barely audible - unless a better carrier signal was
	// specifically created via the voice-outputs. But older chip models would use
	// a high base voltage which always provides a strong carrier signal for D418-digis.
	
	// The current use of signed values to represent audio signals causes a problem
	// here, since the D481-scaling of negative numbers inverts the originally intended
	// effect. (For the purpose of D418 output scaling, the respective input
	// should always be positive.)
	
	// todo: since all calculations already use 32-bit numbers, there is enough unused
	// range to always use positive values.. update impl accordingly and try to
	// also ditch one(or both) of the below offsets to simplify calculations..
	// (nobody cares if 8580 plays digis that would normally not be audible..)
	// also recalibrate the "scope" output such that it is always centered..
	
	if (_is_6581) {
		_wf_zero = -0x3800;
		_dac_offset = 0x8000 * 0xff;
		
		_filter->resetInput6581(0);
	} else {
		_wf_zero = -0x8000;
		_dac_offset = -0x1000 * 0xff;
		
		_filter->resetInput8580();
	}
}

void SID::reset(uint16_t addr, uint32_t sample_rate, uint8_t is_6581, uint32_t clock_rate,
				 uint8_t is_rsid, uint8_t is_compatible, uint8_t output_channel) {
	
	_addr = addr;
	_dest_channel = output_channel;
	
	resetEngine(sample_rate, is_6581, clock_rate);
		
	_digi->reset(clock_rate, is_rsid, is_compatible);
		
	// turn on full volume
	memWriteIO(getBaseAddr() + 0x18, 0xf);
	poke(0x18, 0xf);
}

uint8_t SID::isModel6581() {
	return _is_6581;
}

uint8_t SID::peekMem(uint16_t addr) {
	return MEM_READ_IO(addr);
}

uint8_t SID::readMem(uint16_t addr) {
	uint16_t offset = addr - _addr;
	
	switch (offset) {
	case 0x1b:	// "oscillator" .. docs once again are wrong since this is WF specific!
		return _wave_generators[2]->getOsc();
		
	case 0x1c:	// envelope
		return _env_generators[2]->getOutput();
	}
	
	// reading of "write only" registers returns whatever has been last
	// written to the bus (register independent) and that value normally
	// would also fade away eventually.. only few songs use this and NONE
	// seem to depend on "fading" to be actually implemented;
	// testcase: Mysterious_Mountain.sid
	
	return _bus_write;
}

#ifdef PSID_DEBUG_ADSR
int16_t _frame_count;	// redundant

bool isDebug(uint8_t voice_idx) {
	return (voice_idx == PSID_DEBUG_VOICE) &&
		(_frame_count >= PSID_DEBUG_FRAME_START) && (_frame_count <= PSID_DEBUG_FRAME_END);
}
#endif

void SID::poke(uint8_t reg, uint8_t val) {
    uint8_t voice_idx = 0;
	if (reg < 7) {}
    else if (reg <= 13) { voice_idx = 1; reg -= 7; }
    else if (reg <= 20) { voice_idx = 2; reg -= 14; }

	// writes that impact the envelope generator
	if ((reg >= 0x4) && (reg <= 0x6)) {
		_env_generators[voice_idx]->poke(reg, val);
	}
	
	// writes that impact the filter
    switch (reg) {
		case 0x15:
		case 0x16:
		case 0x17:
		case 0x18:
			_filter->poke(reg, val);
            break;
	}
	
    switch (reg) {
        case 0x0: {
			WaveGenerator* wave_gen = _wave_generators[voice_idx];
			wave_gen->setFreqLow(val);
            break;
        }
        case 0x1: {
			WaveGenerator* wave_gen = _wave_generators[voice_idx];
			wave_gen->setFreqHigh(val);
            break;
        }
        case 0x2: {
			WaveGenerator* wave_gen = _wave_generators[voice_idx];
			wave_gen->setPulseWidthLow(val);
            break;
        }
        case 0x3: {
			WaveGenerator* wave_gen = _wave_generators[voice_idx];
			wave_gen->setPulseWidthHigh(val);
            break;
        }
        case 0x4: {
			WaveGenerator *wave_gen = _wave_generators[voice_idx];
			wave_gen->setWave(val);

			break;
		}
    }
	
#ifdef PSID_DEBUG_ADSR
	if (isDebug(voice_idx) && ((reg >= 0x4) && (reg <= 0x6))) {
		fprintf(stderr, "    %02X ", _wave_generators[voice_idx]->getWave());
		_env_generators[voice_idx]->debug();
		fprintf(stderr, "\n");
	}
#endif
    return;
}

#ifdef RPI4 
// extension callback used by the RaspberryPi4 version to play on an actual SID chip
extern void recordPokeSID(uint32_t ts, uint8_t reg, uint8_t value);
#include "system.h"
#endif

void SID::writeMem(uint16_t addr, uint8_t value) {
	_digi->detectSample(addr, value);

	_bus_write = value;
	
	// no reason anymore to NOT always write (unlike old/un-synced version)
	
	const uint16_t reg = addr & 0x1f;
#ifdef RPI4
	recordPokeSID(SYS_CYCLES(), reg, value);
#endif
	
	poke(reg, value);
	memWriteIO(addr, value);

	// some crappy songs like Aliens_Symphony.sid actually use d5xx instead
	// of d4xx for their SID settings.. i.e. they use mirrored addresses that
	// might actually be used by additional SID chips. always map to standard
	// address to ease debug output handling, e.g. visualization in DeepSid
	
	if (_used_sids == 1) {
		addr = 0xd400 | reg;
		memWriteIO(addr, value);
	}
}

void SID::clock() {
	clockOscillators();		// for all 3 voices
				
	for (uint8_t voice_idx= 0; voice_idx<3; voice_idx++) {
		_env_generators[voice_idx]->clockEnvelope();
	}
}

// clipping (filter, multi-SID as well as PSID digi may bring output over the edge)
#define RENDER_CLIPPED(dest, final_sample) \
	const int32_t clip_value = 32767; \
	if ( final_sample < -clip_value ) { \
		final_sample = -clip_value; \
	} else if ( final_sample > clip_value ) { \
		final_sample = clip_value; \
	} \
	*(dest)= (int16_t)final_sample

void SID::synthSample(int16_t* buffer, int16_t** synth_trace_bufs, uint32_t offset, double* scale, uint8_t do_clear) {
	// generate the two output signals (filtered / non-filtered)
	int32_t outf = 0, outo = 0;	// outf and outo here end up with the sum of 3 voices..
			
	// create output sample based on current SID state
	for (uint8_t voice_idx= 0; voice_idx<3; voice_idx++) {
		
		WaveGenerator* wave_gen = _wave_generators[voice_idx];
		
		int32_t voice_out;
		
		if (wave_gen->isMuted() || _filter->isSilencedVoice3(voice_idx)) {
			voice_out = 0;
		} else {
			uint8_t env_out = _env_generators[voice_idx]->getOutput();
			int32_t outv = ((wave_gen)->*(wave_gen->getOutput))(); // crappy C++ syntax for calling the "getOutput" method
			
			// the ideal voice_out would be signed 16-bit - but more bits are actually needed - particularily
			// for the massively shifted 6581 signal (needed for D418 digis to work correctly)
			
			// note: the _wf_zero ofset *always* creates some wave-output that will be modulated via the
			// envelope (even when 0-waveform is set it will cause audible clicks and distortions in
			// the scope views)
			
			voice_out = (*scale) * ( env_out * (outv + _wf_zero) + _dac_offset);
			
			_filter->routeSignal(&voice_out, &outf, &outo, voice_idx);	// route to either the non-filtered or filtered channel
		}
		
		
		// trace output (always make it 16-bit)
		if (synth_trace_bufs) {
			// when filter is active then muted voices may still show some effect
			// in the trace... but there is no point to slow things down with additional
			// checks here
			int16_t *voice_trace_buffer = synth_trace_bufs[voice_idx];

			if(wave_gen->isMuted()) {
				// never filter
				*(voice_trace_buffer + offset) = voice_out;
			} else {
				int32_t o = 0, f = 0;	// isolated from other voices
				uint8_t is_filtered = _filter->routeSignal(&voice_out, &f, &o, voice_idx);	// redundant.. see above
				
				*(voice_trace_buffer + offset) = (int16_t)_filter->simOutput(voice_idx, is_filtered, &f, &o);
			}			
		}
	}

	int32_t digi_out = 0;
	int8_t dvoice_idx = _digi->routeDigiSignal(_filter, &digi_out, &outf, &outo);
	
	if (synth_trace_bufs) {
		int16_t *voice_trace_buffer = synth_trace_bufs[3];	// digi track
		if (dvoice_idx == -1) {
			// non-filterable digi approach
			*(voice_trace_buffer + offset) = digi_out;			// save the trouble of filtering
		} else {
			// digi approach based on a filterable voice (that voice should be muted, i.e. can use its simOutput) 
			int32_t o = 0, f = 0;	// isolated from other voices
			uint8_t is_filtered = _filter->routeSignal(&digi_out, &f, &o, dvoice_idx);	// redundant.. see above
			*(voice_trace_buffer + offset) = (int16_t)_filter->simOutput(dvoice_idx, is_filtered, &f, &o);
		}
	}
	
	int32_t final_sample = _filter->getOutput(&outf, &outo, _cycles_per_sample);	// note: external filter is included here

#ifdef PSID_DEBUG_ADSR
	if (!isDebug(PSID_DEBUG_VOICE)) final_sample = 0;		// only play what is shown in debug output
#endif
	
	final_sample = _digi->genPsidSample(final_sample);		// recorded PSID digis are merged in directly

	int16_t *dest = buffer + (offset << 1) + _dest_channel; 	// always use interleaved stereo buffer
	
	if (!do_clear) final_sample += *(dest);

	RENDER_CLIPPED(dest, final_sample);
}

// same as above but without digi & no filter for trace buffers - once faster
// PCs are more widely in use, then this optimization may be ditched..
void SID::synthSampleStripped(int16_t* buffer, int16_t** synth_trace_bufs, uint32_t offset, double* scale, uint8_t do_clear) {
	int32_t outf = 0, outo = 0;	// outf and outo here end up with the sum of 3 voices..
	
	for (uint8_t voice_idx= 0; voice_idx<3; voice_idx++) {
		
		uint8_t env_out = _env_generators[voice_idx]->getOutput();
		WaveGenerator *wave_gen= _wave_generators[voice_idx];
		int32_t outv = ((wave_gen)->*(wave_gen->getOutput))(); // crappy C++ syntax for calling the "getOutput" method
		
		int32_t voice_out = (*scale) * ( env_out * (outv + _wf_zero) + _dac_offset);
		_filter->routeSignal(&voice_out, &outf, &outo, voice_idx);
		
		// trace output (always make it 16-bit)
		if (synth_trace_bufs) {
			// performance note: specially for multi-SID (e.g. 8SID) song's use
			// of the filter for trace buffers has a significant runtime cost
			// (e.g. 8*3= 24 additional filter calculations - per sample - instead
			// of just 1). switching to display of non-filtered data may make the
			// difference between "still playing" and "much too slow" -> like on
			// my old machine
			
			int16_t *voice_trace_buffer = synth_trace_bufs[voice_idx];
			*(voice_trace_buffer + offset) = (int16_t)(voice_out);
		}
	}
	
	int32_t final_sample = _filter->getOutput(&outf, &outo, _cycles_per_sample);
	int16_t *dest= buffer + (offset << 1) + _dest_channel; 	// always use interleaved stereo buffer
	
	if (!do_clear) final_sample += *(dest);

	RENDER_CLIPPED(dest, final_sample);
}

// "friends only" accessors
uint8_t SID::getWave(uint8_t voice_idx) {
	return _wave_generators[voice_idx]->getWave();
}

uint16_t SID::getFreq(uint8_t voice_idx) {
	return _wave_generators[voice_idx]->getFreq();
}

uint16_t SID::getPulse(uint8_t voice_idx) {
	return _wave_generators[voice_idx]->getPulse();
}

uint8_t SID::getAD(uint8_t voice_idx) {
	return _env_generators[voice_idx]->getAD();
}

uint8_t SID::getSR(uint8_t voice_idx) {
	return _env_generators[voice_idx]->getSR();
}

uint32_t SID::getSampleFreq() {
	return _sample_rate;
}

DigiType  SID::getDigiType() {
	return _digi->getType();
}

const char*  SID::getDigiTypeDesc() {
	return _digi->getTypeDesc();
}

uint16_t  SID::getDigiRate() {
	return _digi->getRate();
}
		
void SID::setMute(uint8_t voice_idx, uint8_t is_muted) {
	if (voice_idx > 3) voice_idx = 3; 	// no more than 4 voices per SID
	
	if (voice_idx == 3) {
		_digi->setEnabled(!is_muted);
		
	} else {
		_wave_generators[voice_idx]->setMute(is_muted);
	}
}

void SID::resetStatistics() {
	_digi->resetCount();
}

/**
* Use a simple map to later find which IO access matches which SID (saves annoying if/elses later):
*/
const int MEM_MAP_SIZE = 0xbff;
static uint8_t _mem2sid[MEM_MAP_SIZE];	// maps memory area d400-dfff to available SIDs

void SID::setMute(uint8_t sid_idx, uint8_t voice_idx, uint8_t is_muted) {
	if (sid_idx > 9) sid_idx = 9; 	// no more than 10 sids supported

	_sids[sid_idx].setMute(voice_idx, is_muted);
}

DigiType SID::getGlobalDigiType() {
	return _sids[0].getDigiType();
}

const char* SID::getGlobalDigiTypeDesc() {
	return _sids[0].getDigiTypeDesc();
}

uint16_t SID::getGlobalDigiRate() {
	return _sids[0].getDigiRate();
}

void SID::clockAll() {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid = _sids[i];		
		sid.clock();
	}
}

void SID::synthSample(int16_t* buffer, int16_t** synth_trace_bufs, double* scale, uint32_t offset) {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid = _sids[i];
		int16_t **sub_buf = !synth_trace_bufs ? 0 : &synth_trace_bufs[i << 2];	// synthSample uses 4 entries..
		sid.synthSample(buffer, sub_buf, offset, scale, !i);
	}
}

void SID::synthSampleStripped(int16_t* buffer, int16_t** synth_trace_bufs, double* scale, uint32_t offset) {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid = _sids[i];
		int16_t **sub_buf = !synth_trace_bufs ? 0 : &synth_trace_bufs[i << 2];	// synthSample uses 4 entries..
		sid.synthSampleStripped(buffer, sub_buf, offset, scale, (i == 0) || (i == _sid_2nd_chan_idx));
	}
}

void SID::resetGlobalStatistics() {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid = _sids[i];
		sid.resetStatistics();
	}
}

void SID::setModels(const uint8_t* is_6581) {
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid = _sids[i];
		sid.resetModel(is_6581[i]);
	}
}

uint8_t SID::getNumberUsedChips() {
	return _used_sids;
}

uint8_t SID::isAudible() {
	return _is_audible;
}

void SID::resetAll(uint32_t sample_rate, uint32_t clock_rate, uint8_t is_rsid, 
					uint8_t is_compatible) {

	_used_sids = 0;
	memset(_mem2sid, 0, MEM_MAP_SIZE); // default is SID #0

	_is_audible = 0;
	
	// determine the number of used SIDs
	for (uint8_t i= 0; i<MAX_SIDS; i++) {
		if (_sid_addr[i]) {
			_used_sids++;
		}
	}
	// setup the configured SID chips & make map where to find them
	for (uint8_t i= 0; i<_used_sids; i++) {
		SID &sid = _sids[i];
		sid.reset(_sid_addr[i], sample_rate, _sid_is_6581[i], clock_rate, is_rsid, is_compatible, _sid_target_chan[i]);	// stereo only used for my extended sid-file format
			
		if (i) {	// 1st entry is always the regular default SID
			memset((void*)(_mem2sid + _sid_addr[i] - 0xd400), i, 0x1f);
		}
	}
	
	if (_ext_multi_sid) {
		_vol_scale = _vol_map[_sid_2nd_chan_idx ? _used_sids >> 1 : _used_sids - 1] / 0xff;
	} else {
		_vol_scale = _vol_map[_used_sids - 1] / 0xff;	// 0xff serves to normalize the 8-bit envelope
	}
}

uint8_t SID::isExtMultiSidMode() {
	return _ext_multi_sid;
}

// gets what has actually been last written (even for write-only regs)
uint8_t SID::peek(uint16_t addr) {
	uint8_t sid_idx = _mem2sid[addr - 0xd400];
	return _sids[sid_idx].peekMem(addr);
}

// -------------- API used by C code --------------

#ifdef PSID_DEBUG_ADSR
#include <stdio.h>

void SID::debugVoice(uint8_t voice_idx) {
	// hack to manually check stuff
	fprintf(stderr, "%03d %02X ", _frame_count, _wave_generators[voice_idx].getWave());
	_env_generators[voice_idx]->debug();
	fprintf(stderr, "\n");
}

extern "C" void sidDebug(int16_t frame_count) {	// PSID specific
	_frame_count = frame_count;
	
	uint8_t voice_idx = PSID_DEBUG_VOICE;
	if (isDebug(voice_idx)) {
		SID &sid = _sids[0];
		sid.debugVoice(voice_idx);
	}
}
#endif

extern "C" uint8_t sidReadMem(uint16_t addr) {
	uint8_t sid_idx = _mem2sid[addr - 0xd400];
	return _sids[sid_idx].readMem(addr);
}

extern "C" void sidWriteMem(uint16_t addr, uint8_t value) {
	_is_audible |= value;	// detect use by the song
	
	uint8_t sid_idx = _mem2sid[addr - 0xd400];
	_sids[sid_idx].writeMem(addr, value);
}
