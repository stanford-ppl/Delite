#ifndef __DELITE_DATASTRUCTURES_H__
#define __DELITE_DATASTRUCTURES_H__

// structure to keep thread-local resource information
typedef struct {
  int thread_id;
  int socket_id;
} resourceInfo_t;

#endif
