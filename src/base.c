#include "base.h"

void fillMem(uint8_t *mem, int8_t val, uint32_t len) {
	// for some reason 'memset' does not seem to work in Alchemy...
	for (uint32_t i= 0; i<len; i++) {
		mem[i]= val;
	}
}
