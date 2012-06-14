#ifndef LISZTTIME_H_
#define LISZTTIME_H_

#include "CycleTimer.h"

#ifdef __CUDA_ARCH__

#endif


#ifdef __GNUC__
// #ifdef __linux__
// 
// #include <time.h>
// inline static float wall_time() {
// 	timespec time1;
// 	clock_gettime(CLOCK_REALTIME, &time1);
// 	return time1.tv_sec / 1000.0f + time1.tv_nsec * 1000000.f;
// }
// 
// #else
#include <sys/time.h>


#include <time.h>
inline static double cpu_time() {
	clock_t ctime = clock();
	return (double) ctime / ((double) CLOCKS_PER_SEC / 1000.0);
}

class Timer {
public:

    struct timeval lastTime;
    Timer() {
		gettimeofday(&lastTime, 0);
	}
    virtual ~Timer() {
		
	}

    double elapsed() {
		timeval currentTime;
		gettimeofday(&currentTime, 0);
		double deltaT = (double) ((currentTime.tv_sec - lastTime.tv_sec) + 1e-6
            * (currentTime.tv_usec - lastTime.tv_usec));
		return deltaT;
	}
};

static Timer globaltimer;
inline static double wall_time() {
	//only accurate to about 0.02 seconds but is consistent across processors and machines and all that stuff so useful for long-run times
	return globaltimer.elapsed();
}

static ::vtal::CycleTimer cycletimer;
inline static double processor_time() {
	//uses actual processor ticks for highest possible accuracy, but produces incorrect results if thread hops between processors.
	return cycletimer.currentSeconds();
}


// double currentTime(void) {
//     struct timeval tv;
//     gettimeofday(&tv, NULL);
//     double wtime = tv.tv_sec + (double) tv.tv_usec / 1000000.0;
//     return wtime;
// }

// 
// #endif
#endif

#endif /* LISZTTIME_H_ */