#ifndef __CPP_INIT_H__
#define __CPP_INIT_H__

#include <jni.h>
#include <sched.h>

#ifdef __DELITE_CPP_NUMA__
#include <numa.h>
#endif

#ifdef __sun
#include <sys/processor.h>
#endif

void initializeThread(int threadId);

#endif
