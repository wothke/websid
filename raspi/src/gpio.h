/**
* GPIO configuration & utils.
*
* The pin configuration is identical to the one used 
* in SidBerry, i.e. it should be possible to use all 
* respective devices with this configuration. 
*/
#ifndef RPI4_GPIO_UTILS_H
#define RPI4_GPIO_UTILS_H

#include  <stdint.h>

#define CS		3
#define A0		8
#define A1		9
#define A2		7
#define A3		0
#define A4		2
#define D0		15
#define D1		16
#define D2		1
#define D3		4
#define D4		5
#define D5		6
#define D6		10
#define D7		11

void gpioInit();
void pokeSID(uint8_t addr, uint8_t value);

#endif