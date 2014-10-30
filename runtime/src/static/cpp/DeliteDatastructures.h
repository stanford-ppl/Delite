#ifndef __DELITE_DATASTRUCTURES_H__
#define __DELITE_DATASTRUCTURES_H__

#include "DeliteCppRandom.h"

// structure to keep thread-local resource information
typedef struct {
  int thread_id;
  int socket_id;
  //TODO: move thread-local rand to somewhere
  DeliteCppRandom *rand;
} resourceInfo_t;

#endif
