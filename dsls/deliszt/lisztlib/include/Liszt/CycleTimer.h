#ifndef _VTAL_CYCLE_TIMER_H_
#define _VTAL_CYCLE_TIMER_H_

#include <string>

#if defined(__APPLE__)
#include <mach/mach.h>
#include <mach/mach_time.h>
#elif _WIN32
#include <windows.h>
#include <time.h>
#else
#include <cstdlib>
#include <cstdio>
#include <string.h>
#include <sys/time.h>
#endif


namespace vtal {
  // This uses the cycle counter of the processor.	Different
  // processors in the system will have different values for this.	If
  // you process moves across processors, then the delta time you
  // measure will likely be incorrect.	This is mostly for fine
  // grained measurements where the process is likely to be on the
  // same processor.  For more global things you should use the
  // Time interface.

  // Also note that if you processors' speeds change (i.e. processors
  // scaling) or if you are in a heterogenous environment, you will
  // likely get spurious results.
class CycleTimer {
public:
	typedef unsigned long long SysClock;
	
	CycleTimer() {
		initialized = false;
		secondsPerTick_val = 1e-9;
		initialize();
	}
	
	//////////
	// Return the current CPU time, in terms of clock ticks.
	// Time zero is at some arbitrary point in the past.
	SysClock currentTicks();

	//////////
	// Return the current CPU time, in terms of seconds.
	// This is slower than currentTicks().	Time zero is at
	// some arbitrary point in the past.
	double currentSeconds() {
	  return currentTicks() * secondsPerTick_val;
	}

	//////////
	// Return the conversion from seconds to ticks.
	double ticksPerSecond() {
	  return 1.0/secondsPerTick_val;
	}

	//////////
	// Return the conversion from ticks to seconds.
	double secondsPerTick() {
	  return secondsPerTick_val;
	}

	//////////
	// Requeries the processor speed to recompute the ticksPerSecond.
	void resetScale();

	void initialize();

private:
	bool initialized;
	double secondsPerTick_val;
};

inline void CycleTimer::initialize()
{
  resetScale();
  initialized = true;
}

inline void CycleTimer::resetScale()
{
#if defined(__APPLE__)
  mach_timebase_info_data_t time_info;
  mach_timebase_info(&time_info);

  // Scales to nanoseconds without 1e-9f
  secondsPerTick_val = (1e-9*static_cast<double>(time_info.numer))/
    static_cast<double>(time_info.denom);
#elif defined(_WIN32)
  LARGE_INTEGER qwTicksPerSec;
  QueryPerformanceFrequency(&qwTicksPerSec);
  secondsPerTick_val = 1.0/static_cast<double>(qwTicksPerSec.QuadPart);
#else
  FILE *fp = fopen("/proc/cpuinfo","r");
  char input[1024];
  if (!fp) {
    throw "CycleTimer::resetScale failed: couldn't find /proc/cpuinfo. Is this not a linux machine?";
  }
  // In case we don't find it, e.g. on the N900
  secondsPerTick_val = 1e-9;
  while (!feof(fp) && fgets(input, 1024, fp)) {
    // NOTE(boulos): Because reading cpuinfo depends on dynamic frequency scaling it's better to read the @ sign first
    float GHz, MHz;
    if (strstr(input, "model name")) {
      char* at_sign = strstr(input, "@");
      if (at_sign) {
        char* after_at = at_sign + 1;
        char* GHz_str = strstr(after_at, "GHz");
        char* MHz_str = strstr(after_at, "MHz");
        if (GHz_str) {
          *GHz_str = '\0';
          if (1 == sscanf(after_at, "%f", &GHz)) {
            //printf("GHz = %f\n", GHz);
            secondsPerTick_val = 1e-9f / GHz;
            break;
          }
        } else if (MHz_str) {
          *MHz_str = '\0';
          if (1 == sscanf(after_at, "%f", &MHz)) {
            //printf("MHz = %f\n", MHz);
            secondsPerTick_val = 1e-6f / GHz;
            break;
          }
        }
      }
    } else if (1 == sscanf(input, "cpu MHz : %f", &MHz)) {
      //printf("MHz = %f\n", MHz);
      secondsPerTick_val = 1e-6f / MHz;
      break;
    }
  }
  fclose(fp);
#endif
}

//////////
// Return the current CPU time, in terms of clock ticks.
// Time zero is at some arbitrary point in the past.
#if defined(__APPLE__)
/////////////////////////////
// Apple (PowerPC)
/////////////////////////////
inline CycleTimer::SysClock CycleTimer::currentTicks()
{
  if (!initialized) initialize();

  // NOTE(boulos): On recent Apple systems using assembly won't give
  // you the proper scaling factor, so we should be using
  // mach_absolute_time.  If this thing doesn't scale though, we
  // should try to find some other solution.
  return mach_absolute_time();
}
#elif defined (_WIN32)
/////////////////////////////
// Win32
/////////////////////////////
inline CycleTimer::SysClock CycleTimer::currentTicks()
{
  if (!initialized) initialize();
  LARGE_INTEGER qwTime;
  QueryPerformanceCounter(&qwTime);
  return qwTime.QuadPart;
}

#elif defined(__x86_64__)
inline CycleTimer::SysClock CycleTimer::currentTicks()
{
  if (!initialized) initialize();

  unsigned int a, d;
  asm volatile("rdtsc" : "=a" (a), "=d" (d));
  return static_cast<unsigned long long>(a) |
        (static_cast<unsigned long long>(d) << 32);
}
#elif defined(__ARM_NEON__) && 0
inline CycleTimer::SysClock CycleTimer::currentTicks()
{
  if (!initialized) initialize();

  unsigned int val;
  asm volatile("mrc p15, 0, %0, c9, c13, 0" : "=r"(val));
  return val;
}
#else
inline CycleTimer::SysClock CycleTimer::currentTicks()
{
  if (!initialized) initialize();
  timespec spec;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &spec);
  return CycleTimer::SysClock(static_cast<float>(spec.tv_sec) * 1e9 + static_cast<float>(spec.tv_nsec));
}

/////////////////////////////
// Non-Win32 x86
/////////////////////////////

#endif // defined(__APPLE__) && defined(__POWERPC__)


}
#endif // #ifndef _VTAL_CYCLE_TIMER_H_
