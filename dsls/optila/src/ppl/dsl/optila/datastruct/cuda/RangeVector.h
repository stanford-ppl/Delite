#ifndef _RANGEVECTOR_H_
#define _RANGEVECTOR_H_

#include "DeliteCuda.h"

class RangeVector {
public:
    int start;
    int end;
    int stride;
    bool isRow;

    // Constructor
    __host__ RangeVector() { }

    __host__ RangeVector(int _start, bool _end, int _stride, bool _isRow) {
        start = _start;
        end = _end;
        stride = _stride;
        isRow = _isRow;
    }

    // Accessor Functions
    __host__ __device__ int apply(int idx) {
        return start + idx*stride;
    }

    // DeliteCoolection
    __host__ __device__ int size() {
        return (end - start + stride - 1) / stride;
    }

    __host__ __device__ int dcApply(int idx) {
        return start + idx*stride;
    }
    
};

#endif
