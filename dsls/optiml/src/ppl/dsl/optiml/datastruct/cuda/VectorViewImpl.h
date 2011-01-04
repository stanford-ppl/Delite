#ifndef _VECTORVIEWIMPL_H_
#define _VECTORVIEWIMPL_H_

#include <stdio.h>

template <class T>
class VectorView {
public:
    T *data;
    int length;
    bool isRow;
    int start;
    int stride;

    // Constructors
    __host__ __device__ Vector() {
        length = 0;
        isRow = true;
        data = NULL;
        start = 0;
        stride = 1;
    }

    __host__ __device__ Vector(int _length, bool _isRow, T *_data, int _start, int _stride) {
        length = _length;
        isRow = _isRow;
        data = _data;
        start = _start;
        stride = _stride;
    }

    // Accessor Functions
    __host__ __device__ T apply(int idx) {
        return data[start+idx*stride];
    }

    __host__ __device__ void update(int idx, T newVal) {
        data[start+idx*stride] = newVal;
    }

    // DeliteCoolection
    __host__ __device__ int size() {
        return length;
    }

    __host__ __device__ T dcApply(int idx) {
        return data[start+idx*stride];
    }

    __host__ __device__ void dcUpdate(int idx, T value) {
        data[start+idx*stride] = value;
    }
    
};

#endif
