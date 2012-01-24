#ifndef _MATRIXIMPL_H_
#define _MATRIXIMPL_H_

#include "DeliteCuda.h"

template <class T>
class Matrix {
public:
    T *data;
	int numRows;
	int numCols;
	
	// Constructors
	__host__ __device__ Matrix() {}

	__host__ __device__ Matrix(int _numRows, int _numCols) {
        numRows = _numRows;
        numCols = _numCols;
        DeliteCudaMalloc((void**)&data,numRows*numCols*sizeof(T));
	}

	// Accessor Functions
	__host__ __device__ T apply(int idxR, int idxC) {
		return data[idxR*numCols+idxC];
	}
	__host__ __device__ void update(int idxR, int idxC, T newVal) {
		data[idxR*numCols+idxC] = newVal;
	}

    // DeliteCollection
    __host__ __device__ int size() {
        return numRows*numCols;
    }
    __host__ __device__ T dcApply(int idx) {
        return data[idx];
    }
    __host__ __device__ void dcUpdate(int idx, T value) {
        data[idx] = value;
    }

    // unsafeSetData
    __host__ void unsafeSetData(DeliteArray<T> *da, int _length) {
        data = da->data;
        //length = _length;
    }
};

#endif

