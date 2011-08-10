#ifndef _FIELDIMPL_H_
#define _FIELDIMPL_H_

#include <stdio.h>

template <class MO, class T>
class Field {
public:
    T *data;
    int length;

    // Constructors
    __host__ __device__ Field() {
        length = 0;
        data = NULL;
    }

    __host__ __device__ Field(int _length, T *_data) {
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
    
    // Accessor Functions
    __host__ __device__ T apply(MO mo) {
        return data[mo.internalId()];
    }

    __host__ __device__ void update(MO mo, T newVal) {
        data[mo.internalId()] = newVal;
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
