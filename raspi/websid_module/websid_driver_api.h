#ifndef _WEBSID_DRIVER_API_H
#define _WEBSID_DRIVER_API_H

// This kernel module (aka device driver) interacts with the "userland" by 
// means of a directly shared kernel memory area. This allows to make
// the interface as direct as possible and does not introduce additional 
// dependencies (e.g. using "ioctl" would involve additional kernel
// threads that might run on other CPU cores and might unnecessarily cause
// issues in parts of the system than need not even be touched..)

// This design is NOT AT ALL foolproof/robust and any flaws in the kernel 
// module or in the "userland" part may well cause a crash of the system, 
// i.e. it is by no means a general purpose solution suitable for other usecases!
// But for my single user RaspberryPi toy project it seems perfectly good 
// enough. (or as Mr Hammer says.. "Trust me, I know what I am doing.")

// The shared area starts with some flags and then contains two buffers 
// that are used to pass actual data back and forth. Any thread 
// "synchronization" is done via busy-polling of flags. The assumption is, that
// any changes to respective u32 flags are sufficiently "atomic" and the 
// possible state-changes would even allow for certain delays without breaking
// the logic. The two buffers are managed such that at any moment a buffer is
// "owned" by either the "kernel" or the "userland" side, i.e. there is NEVER
// any concurrent access into the buffers. (One problem might be the caches
// maintained on different CPU cores which may delay the visibility of 
// changes performed by a different core.)

// As for the two flags the main distinction is "null" and "not null". In both 
// cases one side has the exclusive ability to set the flag to the "not null"
// state while the other side detects/consumes that state and resets the state
// to "null". There is one flag for each "direction": the "fetch" flag signals 
// that the "kernel" wants data, while the "feed" flag signals that the 
// "userland" has data.

#ifdef RPI4
// for the benefit of the userland code
#define u32 uint32_t
#endif

// part of the shared area used for signaling:

#define RESERVED_ENTRIES	2
	// offsets of the two signaling flags:
#define FETCH_FLAG_OFFSET	0
#define FEED_FLAG_OFFSET	1


// part of the shared area used for buffers

#define NUM_BUFFERS 2

#define MAX_ENTRIES	1500	/* fixme: should be adapted to used userland "chunk_size"; testcase Comaland_tune_3.sid */
#define INTS_PER_ENTRY 2

// total size of the shared memory area
#define AREA_SIZE ((RESERVED_ENTRIES * sizeof(u32)) + (MAX_ENTRIES+1) * INTS_PER_ENTRY  * sizeof(u32) * NUM_BUFFERS)


/*
i.e. the used (as a u32 buffer) layout of the shared memory area is:

[0] fetch flag (contains "available fetch buffer offset")
[1] feed flag (contains "available feed buffer offset")
[ .. ] buffer 1
[ .. ] buffer 2

the contained buffers are addresses via their offset from the 
_shared_area beginning, i.e. these offsets are never 0 for the 
two buffers and 0 can therefore be used as an "unavailable" flag
*/	

#endif