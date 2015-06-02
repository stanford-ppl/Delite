#ifndef __DELITE_CPP_PROFILER_H__
#define __DELITE_CPP_PROFILER_H__

#include <map>
#include <vector>
#include <string>
#include <iostream>
#include <sys/time.h>
#include <stdint.h>
#include "DeliteNamespaces.h"

#ifndef __DELITE_CPP_STANDALONE__
#include <jni.h>
#endif

typedef struct {
  struct timeval start;
  struct timeval end;
  bool isKernel;
} cpptimer_t;

void InitDeliteCppTimer(int32_t numThreads);
void DeliteCppTimerStart(int32_t tid, string name, bool isKernel = true);
void DeliteCppTimerStop(int32_t tid, string name);
#ifndef __DELITE_CPP_STANDALONE__
void DeliteCppTimerDump(int32_t offset, JNIEnv* env);
#else
void DeliteCppTimerDump();
#endif

#endif
