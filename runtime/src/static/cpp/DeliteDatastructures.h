#ifndef __DELITE_DATASTRUCTURES_H__
#define __DELITE_DATASTRUCTURES_H__

#include "DeliteCppRandom.h"

// structure to keep thread-local resource information
typedef struct {
  int threadId;
  int numThreads;
  int socketId;
  int numSockets;
  //int slaveId;
  //int numSlaves;
  int availableThreads;
  //TODO: move thread-local rand to somewhere
  DeliteCppRandom *rand;
} resourceInfo_t;

#endif
