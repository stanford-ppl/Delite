#ifndef __DELITE_CUDA_TIMER_H__
#define __DELITE_CUDA_TIMER_H__

#include <time.h>
#include <sys/time.h>
class DeliteCudaTimer {
  public:
    struct timespec _start;
    struct timespec _end;

    DeliteCudaTimer(){
      clock_gettime(CLOCK_MONOTONIC, &_start);
    }

    virtual ~DeliteCudaTimer(){}

    inline void restart(){
      clock_gettime(CLOCK_MONOTONIC, &_start);
    }

    inline float elapsed(){
      clock_gettime(CLOCK_MONOTONIC, &_end);
      return (_end.tv_sec - _start.tv_sec) + (_end.tv_nsec - _start.tv_nsec) / 1000000000.0;
    }
};

void nanotic(void);
void nanotoc(void);

#endif
