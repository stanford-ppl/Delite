#ifndef _VECIMPL_H_
#define _VECIMPL_H_

/*
#include <stdio.h>

template <class T>
class Vec {
public:
    T *data;
    int length;

    // Constructors
    __host__ __device__ Vec() {
        length = 0;
        data = NULL;
    }

    __host__ __device__ Vec(int _length, T *_data) {
        length = _length;
        data = _data;
    }

    // Accessor Functions
    __host__ __device__ T apply(int idx) {
        return data[idx];
    }

    __host__ __device__ void update(int idx, T newVal) {
        data[idx] = newVal;
    }

    // DeliteCoolection
    __host__ __device__ int size() {
        return length;
    }

    __host__ __device__ T dcApply(int idx) {
        return data[idx];
    }

    __host__ __device__ void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
    
};
*/
#endif
