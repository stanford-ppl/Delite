#ifndef _FIELD_H_
#define _FIELD_H_

#include <cuda.h>
#include "MeshObj.h"
#include "VecImpl.h"

template <class T>
class Field {
public:
    T *data;
    int size;

    // Constructors
    __host__ __device__ Field() {
        size = 0;
        data = NULL;
    }

    __host__ __device__ Field(int _size, T *_data) {
        size = _size;
        data = _data;
    }
    
    // Accessor Functions
    __host__ __device__ T apply(MeshObj mo) {
        return data[internalId(mo)];
    }

    __host__ __device__ void update(MeshObj mo, T newVal) {
        data[internalId(mo)] = newVal;
    }

    // DeliteCoolection
    __host__ __device__ int dcSize() {
        return size;
    }

    //TODO: Is dcApply/dcUpdate different from normal apply/update above?
    __host__ __device__ T dcApply(int idx) {
        return data[idx];
    }

    __host__ __device__ void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
};


template <class T, int N>
class VecField {
public:
    T *data;
    int size;

    // Accessor Functions
    __host__ __device__ Vec<T,N> apply(MeshObj mo) {
        Vec<T,N> ret;
        int idx = internalId(mo);
        for(int i=0; i<N; i++) { ret.data[i] = data[idx*N+i]; }
        return ret;
    }

    // DeliteCoolection
    __host__ __device__ int dcSize() {
        return size;
    }
};

#endif
