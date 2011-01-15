#ifndef _INDEXVECTORIMPL_H_
#define _INDEXVECTORIMPL_H_

#include <stdio.h>

class IndexVector {
public:
    int *data;
    int length;
    bool isRow;

    // Constructors
    __host__ __device__ IndexVector() {
        length = 0;
        isRow = true;
        data = NULL;
    }

    __host__ __device__ IndexVector(int _length, bool _isRow, int *_data) {
        length = _length;
        isRow = _isRow;
        data = _data;
    }

    // Accessor Functions
    __host__ __device__ int apply(int idx) {
        return data[idx];
    }

    __host__ __device__ void update(int idx, int newVal) {
        data[idx] = newVal;
    }

    // DeliteCoolection
    __host__ __device__ int size() {
        return length;
    }

    __host__ __device__ int dcApply(int idx) {
        return data[idx];
    }

    __host__ __device__ void dcUpdate(int idx, int value) {
        data[idx] = value;
    }
    
};

#endif
