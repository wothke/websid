#ifndef TINYRSID_PLAYER_H
#define TINYRSID_PLAYER_H

// infos derived from the *.sid file's meta data
word getSidPlayAddr();
unsigned long getCyclesPerScreen();
unsigned long getCyclesPerSec();
unsigned char getCyclesPerRaster();
unsigned int getLinesPerScreen();
unsigned char getCurrentSongSpeed();
#endif