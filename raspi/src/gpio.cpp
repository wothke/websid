#include "gpio.h"

#include <wiringPi.h>
#include "rpi4_utils.h"

void gpioInit(){
	wiringPiSetup();

	pinMode(D0, OUTPUT);
	pinMode(D1, OUTPUT);
	pinMode(D2, OUTPUT);
	pinMode(D3, OUTPUT);
	pinMode(D4, OUTPUT);
	pinMode(D5, OUTPUT);
	pinMode(D6, OUTPUT);
	pinMode(D7, OUTPUT);
	
	pinMode(A0, OUTPUT);
	pinMode(A1, OUTPUT);
	pinMode(A2, OUTPUT);
	pinMode(A3, OUTPUT);
	pinMode(A4, OUTPUT);
	
	pinMode(CS, OUTPUT);
	
	digitalWrite(D0, LOW);
	digitalWrite(D1, LOW);
	digitalWrite(D2, LOW);
	digitalWrite(D3, LOW);
	digitalWrite(D4, LOW);
	digitalWrite(D5, LOW);
	digitalWrite(D6, LOW);
	digitalWrite(D7, LOW);
	
	digitalWrite(A0, LOW);
	digitalWrite(A1, LOW);	
	digitalWrite(A2, LOW);
	digitalWrite(A3, LOW);
	digitalWrite(A4, LOW);

	digitalWrite(CS, HIGH);
}

void pokeSID(uint8_t addr, uint8_t value) {
	/*
	Timing wise the below WiringPi based impl seems to be adequate, i.e. measurements show
	that on average the below write sequence takes slighly more than 1 micro (including the 
	intentional 1 micro wait);
	
		playing [00:00]mi 1 mx 2 s 1.058000
		playing [00:01]mi 1 mx 2 s 1.031000
		playing [00:02]mi 1 mx 5 s 1.026000
		playing [00:02]mi 1 mx 5 s 1.019500
		playing [00:03]mi 1 mx 5 s 1.015600
		playing [00:04]mi 1 mx 5 s 1.013000
		playing [00:04]mi 1 mx 5 s 1.011143
		playing [00:05]mi 1 mx 5 s 1.010000
		playing [00:06]mi 1 mx 5 s 1.008889
		playing [00:06]mi 1 mx 5 s 1.008000
		playing [00:07]mi 1 mx 5 s 1.007455
		
	However there are again storadic outliers where the same sequence may suddenly spike to
	7 micros. This is a problem for songs that use techiques like PWM or FREQM based digi 
	playback (etc) and where individual SID writes happen as little as 4 CPU cycles (i.e.
	4 micros) apart. The resulting distortions are painfully obvious in respective songs.
	
	Most likely the effect is again caused by the linux scheduler that strikes with 100Hz
	frequency. The regular frequency of the glitches makes them stick out in an audio signal.
	*/
	static uint32_t ts;

	/*	
	respective constants from wiringPi:
	#define	LOW			 0
	#define	HIGH		 1
	.. generate directly to avoid branches
	*/
	
	// set address
	digitalWrite(A0, (addr & 0x01));
	digitalWrite(A1, (addr & 0x02) >> 1);
	digitalWrite(A2, (addr & 0x04) >> 2);
	digitalWrite(A3, (addr & 0x08) >> 3);
	digitalWrite(A4, (addr & 0x10) >> 4);
	
	// set data
	digitalWrite(D0, (value & 0x01));
	digitalWrite(D1, (value & 0x02) >> 1);
	digitalWrite(D2, (value & 0x04) >> 2);
	digitalWrite(D3, (value & 0x08) >> 3);
	digitalWrite(D4, (value & 0x10) >> 4);
	digitalWrite(D5, (value & 0x20) >> 5);
	digitalWrite(D6, (value & 0x40) >> 6);
	digitalWrite(D7, (value & 0x80) >> 7);
	
	digitalWrite(CS, LOW);
	
	ts= SYS_COUNT_LOW(); ts+= 1;	// 1 cycle should be enough..
	while (SYS_COUNT_LOW() <  ts) {}

	digitalWrite(CS, HIGH);
}