#ifndef _MESHSET_H_
#define _MESHSET_H_

#include <cuda.h>
#include "MeshObj.h"

template <class T>
class MeshSet {
public:
    T *data;
    int size;

    // Accessor Functions
    __host__ __device__ T apply(MeshObj idx) {
        return data[idx];
    }

    //TODO: Is MeshSet mutable?
    __host__ __device__ void update(MeshObj idx, T newVal) {
        data[idx] = newVal;
    }

    // DeliteCoolection
    __host__ __device__ int dcSize() {
        return size;
    }

    __host__ __device__ T dcApply(MeshObj idx) {
        return data[idx];
    }

    __host__ __device__ void dcUpdate(MeshObj idx, T value) {
        data[idx] = value;
    }

};

//TODO: Deal with CellSet (No zero cell)

#endif