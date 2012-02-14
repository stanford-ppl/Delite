#ifndef _DELITEARRAY_H_
#define _DELITEARRAY_H_

#include "DeliteCuda.h"

template <class T>
class DeliteArray {
public:
    T *data;
    int length;

    // Constructor
    __host__ DeliteArray(int _length) {
        length = _length;
        DeliteCudaMalloc((void**)&data,length*sizeof(T));
    }

    __host__ __device__ DeliteArray(int _length, T *_data) {
        length = _length;
        data = _data;
    }

    __host__ __device__ T apply(int idx) {
        return data[idx];
    }

    __host__ __device__ void update(int idx, T value) {
        data[idx] = value;
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
