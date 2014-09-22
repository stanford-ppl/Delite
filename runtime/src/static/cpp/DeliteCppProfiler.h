#include <map>
#include <vector>
#include <string>
#include <iostream>
#include <sys/time.h>
#include <jni.h>
#include <stdint.h>
#include "DeliteNamespaces.h"

typedef struct {
  struct timeval start;
  struct timeval end;
} cpptimer_t;

void InitDeliteCppTimer(int32_t tid);
void DeliteCppTimerStart(int32_t tid, string name);
void DeliteCppTimerStop(int32_t tid, string name);
void DeliteCppTimerDump(int32_t tid, int32_t rid, JNIEnv* env);
