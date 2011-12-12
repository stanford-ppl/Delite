#ifndef _FIELD_H_
#define _FIELD_H_

#include <cuda.h>
#include "BitReverse.h"
#include "MeshObj.h"
#include "VecImpl.h"
#include "MatImpl.h"

template <class T>
class Field {
public:
    T *data;
    int size;

    // Constructors
    __host__ __device__ Field() {
        size = 0;
        data = NULL;
    }

    __host__ __device__ Field(int _size, T *_data) {
        size = _size;
        data = _data;
    }
    
    // Accessor Functions
    __host__ __device__ T apply(int moIdx) {
        return data[moIdx];
    }

    __host__ __device__ void update(int moIdx, T newVal) {
        data[moIdx] = newVal;
    }

    // DeliteCoolection
    __host__ __device__ int dcSize() {
        return size;
    }

    //TODO: Is dcApply/dcUpdate different from normal apply/update above?
    __host__ __device__ T dcApply(int idx) {
        return data[idx];
    }

    __host__ __device__ void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
};


template <class T, int N>
class VecField {
public:
    T *data;
    int size;

    // Accessor Functions
    __host__ __device__ Vec<T,N> apply(MeshObj mo) {
        Vec<T,N> ret;
        int idx = internal(mo);
        for(int i=0; i<N; i++) { ret.data[i] = data[idx*N+i]; }
        return ret;
    }
    __host__ __device__ void update(MeshObj mo, Vec<T,N> in) {
        int idx = internal(mo);
        for(int i=0; i<N; i++) { data[idx*N+i] = in.data[i]; }
    }
    __host__ __device__ T raw_apply(MeshObj mo, int offset) {
        int idx = internal(mo);
        return data[idx*N+offset];
    }
    __host__ __device__ void raw_update(MeshObj mo, int offset, T in) {
        int idx = internal(mo);
        data[idx*N+offset] = in;
    }

    // DeliteCoolection
    __host__ __device__ int dcSize() {
        return size;
    }
};

template <class T, int R, int C>
class MatField {
public:
    T *data;
    int size;

    // Accessor Functions
    __host__ __device__ Mat<T,R,C> apply(MeshObj mo) {
        Mat<T,R,C> ret;
        int idx = internal(mo);
        for(int i=0; i<R*C; i++) { ret.data[i] = data[idx*R*C+i]; }
        return ret;
    }
    __host__ __device__ void update(MeshObj mo, Mat<T,R,C> in) {
        int idx = internal(mo);
        for(int i=0; i<R*C; i++) { data[idx*R*C+i] = in.data[i]; }
    }

    // DeliteCoolection
    __host__ __device__ int dcSize() {
        return size;
    }
};

#endif
