#ifndef _DENSEMATRIX_H_
#define _DENSEMATRIX_H_

//#include "VectorViewImpl.h"

#include <stdlib.h>

template <class T>
class DenseMatrix {
public:
    T *data;
	  int numRows;
 	  int numCols;
	
	  // Constructors
	  //DenseMatrix() {}

    DenseMatrix(int _numRows, int _numCols) {
      numRows = _numRows;
      numCols = _numCols;
      data = (T *)malloc(numRows*numCols*sizeof(T));
	}

    DenseMatrix(T *_data, int _numRows, int _numCols) {
      numRows = _numRows;
      numCols = _numCols;
      data = _data;
	}
    /*
	  // Accessor Functions
	  T apply(int idxR, int idxC) {
		  return data[idxR*numCols+idxC];
	  }
	  void update(int idxR, int idxC, T newVal) {
		  data[idxR*numCols+idxC] = newVal;
 	  }

    // DeliteCollection
    int size() {
        return numRows*numCols;
    }
    T dcApply(int idx) {
        return data[idx];
    }
    void dcUpdate(int idx, T value) {
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
    */

};

#endif

