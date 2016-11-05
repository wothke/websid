#ifndef TINYRSID_BASE_H
#define TINYRSID_BASE_H

typedef signed char int8_t;
typedef unsigned char uint8_t;

// 16-bit must be exactly 16-bit!
typedef short int16_t;
typedef unsigned short uint16_t;

typedef signed long int32_t;
typedef unsigned long uint32_t;


// replacement for memset (was needed for old Alchemy stuff)
void fillMem(uint8_t *mem, int8_t val, uint32_t len);

#endif
