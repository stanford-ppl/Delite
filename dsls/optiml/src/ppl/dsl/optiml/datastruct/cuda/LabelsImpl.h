#ifndef _LABELSIMPL_H_
#define _LABELSIMPL_H_

#include <stdio.h>

template <class L>
class Labels {
public:
    L *data;
    int length;
    bool isRow;
    int numLabels(void) { return length; }

    // Constructors
    __host__ __device__ Labels() {
        data = NULL;
        length = 0;
        isRow = true;
    }

    __host__ __device__ Labels(L *_data, int _length, bool _isRow) {
        data = _data;
        length = _length;
        isRow = _isRow;
    }

    // Accessor Functions
    __host__ __device__ L apply(int idx) {
        return data[idx];
    }

    __host__ __device__ void update(int idx, L newVal) {
        data[idx] = newVal;
    }

    // Delite Collection
    __host__ __device__ int size() {
        return length;
    }

    __host__ __device__ L dcApply(int idx) {
        return data[idx];
    }

    __host__ __device__ void dcUpdate(int idx, L value) {
        data[idx] = value;
    }
    
};

#endif
