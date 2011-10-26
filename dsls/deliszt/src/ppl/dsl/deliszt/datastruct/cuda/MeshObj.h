#ifndef _MESHOBJ_H_
#define _MESHOBJ_H_

#include "BitReverse.h"

typedef int MeshObj;
typedef int Cell;
typedef int Edge;
typedef int Face;
typedef int Vertex;

/* global device methods on mesh objects */

__device__ __host__ int internalId(MeshObj id) {
   return id & ~MASK;
}

__device__ __host__ bool reversed(MeshObj id) {
 return BitReverse_reversed(id);
}

#endif