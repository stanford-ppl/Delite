#ifndef _CUDAARRAYLIST_H_
#define _CUDAARRAYLIST_H_

#include <cuda.h>

template <class T>
class CudaArrayList {
public:
    T *data;
    int length;

    // Constructors
    __host__ __device__ CudaArrayList() {
        data = NULL;
        length = 0;
    }
    __host__ __device__ CudaArrayList(T *_data, int _length) {
        data = _data;
        length = _length;
    }

    // Accessor Functions
    __host__ __device__ T apply(int idx) {
        return data[idx];
    }
    __host__ __device__ void update(int idx, T newVal) {
        data[idx] = newVal;
    }
};

#endif