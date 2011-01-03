#ifndef _RANGEVECTORIMPL_H_
#define _RANGEVECTORIMPL_H_

#include <stdio.h>
//#include "VectorImpl.h"

//template <class T>
//class RangeVector : public Vector<T> {
class RangeVector {
public:
    int start;
    int end;
    int stride;
    bool isRow;
    int length;

    // Constructors
    RangeVector() {
        start = 0;
        end = 0;
        stride = 1;
        isRow = true;
        length = 0;
    }

    RangeVector(int _start, int _end, int _stride, bool _isRow) {
        start = _start;
        end = _end;
        stride = _stride;
        isRow = _isRow;
        length = _end - _start + _stride - 1;
    }

    // Accessor Functions
    __host__ __device__ int apply(int idx) {
        return start+stride*idx;
    }

    // DeliteCoolection
    __host__ __device__ int size() {
        return length;
    }

    __host__ __device__ int dcApply(int idx) {
        return start+stride*idx;
    }

};

#endif
