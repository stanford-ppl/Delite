#ifndef _MESHSET_H_
#define _MESHSET_H_

#include <cuda.h>
#include "MeshObj.h"
#include "BitReverse.h"

template <class T>
class MeshSet {
public:
    T *data;
    int size;
    int dir;

    // Accessor Functions
    __host__ __device__ T apply(MeshObj idx) {
        return data[idx*dir] ^ (dir & MASK);
    }

    // DeliteCoolection
    __host__ __device__ int dcSize() {
        return size;
    }

    __host__ __device__ T dcApply(MeshObj idx) {
        return data[idx*dir] ^ (dir & MASK);
    }
};

//TODO: Deal with CellSet (No zero cell)

template <class T>
class BoundarySet : public MeshSet<T>
{

};

#endif