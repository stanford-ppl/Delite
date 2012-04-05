#ifndef _DENSEMATRIX_H_
#define _DENSEMATRIX_H_

#include "DeliteCuda.h"
#include "DeliteArray.h"
#include "VectorViewImpl.h"

template <class T>
class DenseMatrix {
public:
    T *data;
	int numRows;
	int numCols;
	
	// Constructors
	__host__ __device__ DenseMatrix() {}

	__host__ __device__ DenseMatrix(int _numRows, int _numCols) {
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

    __host__  __device__ VectorView<T> getRow(int row) {
      VectorView<T> v(numCols,true,data,row*numCols,1);
      return v;
    }

    __device__ DeliteArray<T> getdata(void) {
      DeliteArray<T> da(numRows*numCols, data);
      return da;
    }

    __device__ void setdata(DeliteArray<T> da) {
      data = da.data;
    }

};

#endif

