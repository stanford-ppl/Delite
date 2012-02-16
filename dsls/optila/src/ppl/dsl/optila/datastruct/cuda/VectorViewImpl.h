#ifndef _VECTORVIEWIMPL_H_
#define _VECTORVIEWIMPL_H_

#include "DeliteCuda.h"
#include "DeliteArray.h"

template <class T>
class VectorView {
public:
    T *data;
    int length;
    bool isRow;
    int start;
    int stride;

    // Constructors
    __host__ __device__ VectorView() {
        length = 0;
        isRow = true;
        data = NULL;
        start = 0;
        stride = 1;
    }

    __host__ __device__ VectorView(DeliteArray<T> _da, int _start, int _stride, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        data = _da.data;
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