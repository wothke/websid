/*

various observed effects "while crashing" :

1) websid prints nothing but "detected : Raspberry.." line but nothing else, i.e. the "cout" stuff seems to
	be blocked.. but the music is still starting to play (before it completely crashes after 10 secs)
	
2) ... 
*/


#include <fcntl.h>
#include <unistd.h>		// close
#include <iostream>		// cout
#include <unistd.h>		// sleep
#include <csignal>
#include <sys/mman.h>

#include "../websid_module/websid_driver_api.h"

#include "rpi4_utils.h"

#include "device_driver_handler.h"


using namespace std;


void detectDeviceDriver(int *fd, volatile uint32_t **buffer_base) {
	// check of device driver is installed on the machine
	*fd = open("/dev/websid", O_RDWR);
	
	if((*fd) >= 0) {		
		// reminder: an incorrect size passed to the mmap API will lead to cross-core cache update issues
		*buffer_base = (uint32_t*) mmap(0, AREA_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED, *fd, 0);
		if ((*buffer_base) == MAP_FAILED)	{
			// device driver does not seem to be available
			close(*fd);
			*fd = -1;
			*buffer_base = 0;
		}
	}
}

// conceptually these are all DeviceDriverHandler instance vars, but since there is never 
// more than one DeviceDriverHandler instance .. I'll rather keep the *.h file as an 
// interface without cluttering it with irrelevant implementation details..

static int _device_fd = -1;

volatile uint8_t *_buffer_base;
static int32_t _buffer_offset;		// offset of the currently used buffer (in bytes, relative to _buffer_base)

// flags used to send signals between userland and kernel space
volatile uint32_t *_fetch_flag_ptr = 0;	
volatile uint32_t *_feed_flag_ptr = 0;


DeviceDriverHandler::DeviceDriverHandler(int device_fd, volatile uint32_t *buffer_base)  : PlaybackHandler() {
	_device_fd = device_fd;
	_buffer_base = (volatile uint8_t *)buffer_base;		// page aligned, i.e. also aligned for int32_t use
	
	if (_buffer_base == 0) {
		cout << "error: no valid device buffer available" << endl;
	}
	
	_fetch_flag_ptr = (volatile uint32_t *)(_buffer_base + (FETCH_FLAG_OFFSET << 2));
	_feed_flag_ptr = (volatile uint32_t *)(_buffer_base + (FEED_FLAG_OFFSET << 2));
	

	// new kernel version seems to have a new kind of RT throttling in place
	// that will slow the driver thread on core #3 (try to get rid of that throttling):
		
	// Defines the period in μs (microseconds) to be considered as 100% of CPU bandwidth.
	system("sudo echo '600000000' > /proc/sys/kernel/sched_rt_period_us");			//  default: 1000000
	
	// Setting the value to -1 means that real-time tasks may use up to 100% of CPU times
	system("sudo echo '-1' > /proc/sys/kernel/sched_rt_runtime_us");			//  default: 950000
		

	// disable to avoid having the logs filled with these:
	system("sudo echo '600' > /proc/sys/kernel/hung_task_timeout_secs");			//  default: 120
	system("sudo echo '1' >  /sys/module/rcupdate/parameters/rcu_cpu_stall_suppress");	//  default: 0
	
}

DeviceDriverHandler::~DeviceDriverHandler() {

	// the device driver "close" of the current user will cause the current kthread to exit
	// and an "open" from the next user will re-init the _fetch_flag_ptr & _feed_flag_ptr flags 
	// and therefore there is no need for respective cleanups here
	
    if( (_buffer_base == 0) || munmap((void*)_buffer_base, AREA_SIZE) ) {
		cout << "error: failed to unmap device" << endl;
	} else {
		_buffer_base = 0;
//		cout << "successfully unmapped device" << endl;
	}
	
	close(_device_fd);	
}

// timeout during playback might use 20ms but since PSID "INIT" calls may 
// take longer, keep it simple
#define FETCH_TIMEOUT 10000000		/*just avoid infinite blocking */

uint32_t DeviceDriverHandler::fetchBuffer() {
	if (_buffer_base == 0) return 0;	// just in case
	
	// blocks until a buffer is available in the kernal driver.. 

	// typically new buffers must be supplied every 17-20ms,
	// i.e. if it takes longer to fetch a new buffer then the
	// playback is doomed anyway
	
	uint32_t start, now, timeout, overflow; 
	uint32_t addr_offset = 0;// means "unavailable" (must be handled as an error in "userland")
	
	start = now =  micros();
	overflow = (0xffffffff - now) <= FETCH_TIMEOUT;	// couter will overflow during timeout window
	timeout = start + FETCH_TIMEOUT;

	while ((!overflow && (now<timeout)) || (overflow && ((now>start) || (now<timeout))) ) {	
		// availability of the buffer should be detected as quickly as possible since any
		// time wasted here substracts from the 17-20ms available to produce the next buffer
		
		if ((volatile uint32_t)(*_fetch_flag_ptr)) {
			// pass "ownership" of the buffer to "userland"
			addr_offset = (volatile uint32_t)(*_fetch_flag_ptr);
			(*_fetch_flag_ptr) = 0;
			break;
		}
		now = micros();
	}	
	return addr_offset;
}

void DeviceDriverHandler::flushBuffer(uint32_t addr_offset) {
	// it might be prudent to signal that the respective buffer area has been updated
	// and respective caches (if any) need to be reloaded.. (though it seems to not make any difference...)
	// note: with the current buffer layout, the end of buffer 1 and the beginning of buffer 2
	// may be within the same page. so syncing might also overwrite the "other" buffer which 
	// has not been updated here.. however, since it is only the producer here that is writing into 
	// the buffers (and the kernel module is only reading) that should not matter
	
	// XXX The 1st page which contains the 2 flags + parts of the 1st buffer might be more 
	// problematic, e.g. supposing userland writes back that page (to deliver the buffer) and 
	// thereby overwrites the more recent "fetch" flag that the kernel might meanwhile have updated
	// but which might not yet be reflected in the userland's cache. todo: check if this might be
	// a real problem
	
	
	// hack: hard coded page size of 4096
	const uint32_t page_offset_mask =	0x00000fff;	// offset from beginning of a page
	const uint32_t page_mask = 			0xfffff000;
	
	uint32_t buffer = ((uint32_t)(_buffer_base + addr_offset));
	void* start_page = (void*)(buffer & page_mask);
	uint32_t page_offset = buffer & page_offset_mask;	
	
	// 8 bytes per entry and a 4-byte end marker
	uint32_t len = ((_script_len)<<3)+sizeof(uint32_t) + page_offset;	
	
	// MS_SYNC always seems to throw an EINVAL error here, so there is no knowing how delayed 
	// the kernel might actually get this..	supposedly:
	//  "EINVAL addr is not a multiple of PAGESIZE; or any bit other than
	//			MS_ASYNC | MS_INVALIDATE | MS_SYNC is set in flags; or
	//			both MS_SYNC and MS_ASYNC are set in flags." 
	// => but this doc is once again incorrect garbage

	if(msync(start_page, len, MS_INVALIDATE)) 
	{
		cout << "error msync: start: "<< (uint32_t)start_page << " len: " << len << " errno: " << errno << endl;
	}
}

void DeviceDriverHandler::flushFlags(uint32_t addr_offset) {
	if((addr_offset != RESERVED_ENTRIES * sizeof(u32))) {
		msync((void*)_buffer_base, RESERVED_ENTRIES * sizeof(u32), MS_INVALIDATE);
	}
}

void DeviceDriverHandler::feedBuffer(uint32_t addr_offset) {
	if (_buffer_base == 0) return;	// just in case
	
	// this might use timer based waiting instead.. no micro latency needed here	
	while ((volatile uint32_t)(*_feed_flag_ptr)) { /* wait until kernel retrieved the previous one */ }

	if ((*_feed_flag_ptr) > AREA_SIZE) {
		// this should never happen.. it would have to be the kernel-side "SID reset" script, 
		// but by the time that script gets used we should not be here!
		
		cout << "error: feedBuffer illegal state" << endl;		
	} else {
		flushBuffer(addr_offset);
				
		(*_feed_flag_ptr) = addr_offset;
		
		flushFlags(addr_offset) ;		
	}
}

void DeviceDriverHandler::recordBegin() {
	_buffer_offset =  fetchBuffer();
	if (_buffer_offset == 0) {
		cout << "timeout: device driver is not responding" << endl;
		raise (SIGINT);			// this should trigger a regular teardown..
		_script_buf = 0;		// disable write ops until SIGINT performs its job
	} else {
		_script_buf = (uint32_t*)(_buffer_base + _buffer_offset);
	}
	_script_len = 0;
}

void DeviceDriverHandler::recordEnd() {
	if (!_script_buf) {
		// SIGINT has already been sent (see recordBegin())
	} else {
		// buffer is never reset and it will still contain the
		// garbage from longer previous scripts: set an "end marker"
		_script_buf[_script_len<<1] = 0;
		
		feedBuffer(_buffer_offset);

		_script_buf = 0;	// redundant
		_script_len = 0;	// redundant	
	}
}
