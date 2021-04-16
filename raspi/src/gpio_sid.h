/**
* API for controlling the SID chip via GPIO.
*
* The pin configuration is identical to the one used 
* in SidBerry, i.e. it should be possible to use all 
* respective devices with this configuration. 
*/
#ifndef RPI4_GPIO_SID_H
#define RPI4_GPIO_SID_H

#include  <stdint.h>

void gpioInitSID();
void gpioPokeSID(uint8_t addr, uint8_t value);

#endif