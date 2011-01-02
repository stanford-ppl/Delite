#ifndef _MATRIXIMPL_H_
#define _MATRIXIMPL_H_

#include <stdio.h>

template <class T>
class Matrix {
public:
    T *data;
	int numRows;
	int numCols;
	
	// Constructors
	Matrix() {
		numRows = 0;
		numCols = 0;
		data = NULL;
	}

	Matrix(int _numRows, int _numCols, T *_data) {
		numRows = _numRows;
		numCols = _numCols;
		data = _data;
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
	
};

#endif

