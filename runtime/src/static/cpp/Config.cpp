#ifndef _DELITE_CONFIG_
#define _DELITE_CONFIG_

#include <stdlib.h>
#include <iostream>
#include <pthread.h>
#include "Config.h"
#include "DeliteDatastructures.h"
#include "DeliteCpp.h"
#include "cppInit.h"
#include "pcmHelper.h"

Config* config = NULL;
resourceInfo_t* resourceInfos = NULL;
pthread_mutex_t init_mtx = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t init_cond = PTHREAD_COND_INITIALIZER;

// heavy-handed, but doesn't appear there is another good way
int getCpuInfo(FILE* pipe) {
  if (!pipe) return -1;

  char buffer[128];
  buffer[0] = 0;
  fgets(buffer, 128, pipe);
  pclose(pipe);
  return atoi(buffer);
}

int getNumCoresPerSocket() {
  FILE* pipe = popen("grep 'cores' /proc/cpuinfo 2> /dev/null | head -1 | cut -d ':' -f 2 | tr -d ' '", "r");
  return getCpuInfo(pipe);
}

int getNumSockets() {
  FILE* pipe = popen("grep 'physical id' /proc/cpuinfo 2> /dev/null | tail -1 | cut -d ':' -f 2 | tr -d ' '", "r");
  return getCpuInfo(pipe) + 1;
}

void initializeConfig(int numThreads) {
  config = new Config(numThreads);
    
  //detect physical config
  int numSockets = getNumSockets();
  int numCoresPerSocket = getNumCoresPerSocket();
  int numCores = numCoresPerSocket * numSockets;

  //detect numa config
  #ifdef __DELITE_CPP_NUMA__
  if (numa_available() >= 0) {
    int numCpus = numa_num_configured_cpus();
    if (numCoresPerSocket <= 0) {
        VERBOSE("WARNING: Unable to automatically determine number of physical cores, assuming %d\n", numCpus);
        numCores = numCpus;
    }

    int numNodes = numa_num_configured_nodes();
    if (numSockets > 0 && numSockets != numNodes) {
      VERBOSE("WARNING: Found %d sockets but %d NUMA nodes. Using %d nodes\n", numSockets, numNodes, numNodes);
    }
    numSockets = numNodes;
    numCoresPerSocket = numCores / numSockets; //potentially re-distribute cores across nodes
  }
  #endif

  if (numSockets > 0 && numCoresPerSocket > 0) {
    config->numSockets = numSockets;
    config->numCoresPerSocket = numCoresPerSocket;
    VERBOSE("Detected machine configuration of %d socket(s) with %d core(s) per socket.\n", config->numSockets, config->numCoresPerSocket);
  }
  else {
    VERBOSE("WARNING: Unable to automatically detect machine configuration.  Assuming %d socket(s) with %d core(s) per socket.\n", config->numSockets, config->numCoresPerSocket);
  }
}

void initializeGlobal(int lowestCppTid, int numCppThreads, size_t heapSize, bool enablePCM) {
  pthread_mutex_lock(&init_mtx); 
  if (!config) {
    InitDeliteCppTimer(lowestCppTid, numCppThreads);
    initializeConfig(numCppThreads);
    resourceInfos = new resourceInfo_t[numCppThreads];
    for (int i=0; i<numCppThreads; i++) {
      resourceInfos[i] = resourceInfo_t(i, numCppThreads, config->threadToSocket(i), config->numSockets);
      resourceInfos[i].rand = new DeliteCppRandom(i);
    }
    DeliteHeapInit(numCppThreads, heapSize);
    initializeThreadPool(numCppThreads);
  }

  pcmInit(lowestCppTid);

  pthread_mutex_unlock(&init_mtx);
}

#ifndef __DELITE_CPP_STANDALONE__
void freeGlobal(int numThreads, int offset, JNIEnv *env) {
#else
void freeGlobal(int numThreads) {
#endif
  pthread_mutex_lock(&init_mtx);
  if (config) {
    DeliteSendMemoryAccessStatsToJVM(offset, env);
    SendKernelMemUsageStatsToJVM(env);
    DeliteSendStartTimeToJVM(env);
    DeliteCppTimerClose();
    DeliteHeapClear(numThreads);
    delete[] resourceInfos;
    delete config;
    config = NULL;
  }

  pcmCleanup();
  pthread_mutex_unlock(&init_mtx);
}

void initializeAll(int threadId, int lowestCppTid, int numCppThreads, int numLiveThreads, size_t heapSize, bool enablePCM) {
  initializeGlobal(lowestCppTid, numCppThreads, heapSize, enablePCM);
  initializeThread(threadId);
  delite_barrier(numLiveThreads); //ensure fully initialized before any continue
}

#ifndef __DELITE_CPP_STANDALONE__
void clearAll(int numThreads, int numLiveThreads, int offset, JNIEnv *env) {
#else
void clearAll(int numThreads, int numLiveThreads) {
#endif
  delite_barrier(numLiveThreads); //first wait for all threads to arrive
  #ifndef __DELITE_CPP_STANDALONE__
  freeGlobal(numThreads, offset, env);
  #else
  freeGlobal(numThreads);
  #endif
}

#endif
