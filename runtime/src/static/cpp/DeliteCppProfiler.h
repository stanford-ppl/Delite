#ifndef __DELITE_CPP_PROFILER_H__
#define __DELITE_CPP_PROFILER_H__

#include <map>
#include <vector>
#include <string>
#include <iostream>
#include <ios>
#include <fstream>
#include <sys/time.h>
#include <jni.h>
#include <stdint.h>
#include <stack>
#include <stdio.h>
#include "DeliteNamespaces.h"

#include "pcmHelper.h"

typedef struct {
  const char* name;
  struct timeval start;
  struct timeval end;
  bool isKernel;
} cpptimer_t;

typedef struct {
  void* startAddr;
  uint64_t size;
  std::string sourceContext;
} cpparray_layout_info;

void InitDeliteCppTimer(int32_t lowestCppTid, int32_t numCppThreads);
void DeliteCppTimerStart(int32_t tid, string name, bool isKernel = true);
void DeliteCppTimerStop(int32_t tid, string name);
void DeliteCppTimerClose();
void DeliteUpdateMemoryAccessStats( int32_t tid, std::string sourceContext, PCMStats* stats );
void DeliteSendMemoryAccessStatsToJVM( int32_t offset, JNIEnv* env );
void DeliteLogArrayAllocation(int32_t tid, void* startAddr, int32_t length, std::string elemType, std::string sourceContext);

#endif
