#include <map>
#include <vector>
#include <string>
#include <iostream>
#include <sys/time.h>
#include <jni.h>
#include <stdint.h>
#include <cuda.h>

#ifndef DELITE_NUM_CUDA
#define DELITE_NUM_CUDA 1
#endif

typedef struct {
  struct timeval start;
  struct timeval end;
} cudatimer_t;

void InitDeliteCudaTimer(int32_t tid);
void DeliteCudaTimerStart(int32_t tid, std::string name);
void DeliteCudaTimerStop(int32_t tid, std::string name);
void DeliteCudaTimerDump(int32_t tid, int32_t rid, JNIEnv* env);
