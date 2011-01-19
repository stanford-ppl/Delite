#ifndef _VECTORIMPL_H_
#define _VECTORIMPL_H_

#include <stdio.h>

template <class T>
class Vector {
public:
    T *data;
    int length;
    bool isRow;

    // Constructors
    __host__ __device__ Vector() {
        length = 0;
        isRow = true;
        data = NULL;
    }

    __host__ __device__ Vector(int _length, bool _isRow, T *_data) {
        length = _length;
        isRow = _isRow;
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

#endif
