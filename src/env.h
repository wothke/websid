#ifndef TINYRSID_PLAYER_H
#define TINYRSID_PLAYER_H

#include "defines.h"

// infos derived from the *.sid file's meta data
uint16_t getSidPlayAddr();
uint32_t getCyclesPerScreen();
uint32_t getCyclesPerSec();
uint8_t getCyclesPerRaster();
uint16_t getLinesPerScreen();
uint8_t getCurrentSongSpeed();
uint16_t getNumberOfSamplesPerCall();

#endif