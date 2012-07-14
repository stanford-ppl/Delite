#ifndef _DENSEMATRIX_H_
#define _DENSEMATRIX_H_

//#include "VectorViewImpl.h"
#include <DeliteArray.h>
#include <stdlib.h>

template <class T>
class DenseMatrix {
public:
    DeliteArray<T> *da;
    T *data;
	  int numRows;
 	  int numCols;
	
	  // Constructors
    DenseMatrix(int _numRows, int _numCols) {
      numRows = _numRows;
      numCols = _numCols;
      da = new DeliteArray<T>(numRows*numCols);
      data = da->data;
	 }

    DenseMatrix(DeliteArray<T> *_da, int _numRows, int _numCols) {
      numRows = _numRows;
      numCols = _numCols;
      da = _da;
      data = da->data;
	  }

    DenseMatrix(T *_data, int _numRows, int _numCols) {
      numRows = _numRows;
      numCols = _numCols;
      da = new DeliteArray<T>(_data, _numRows*_numCols);
      data = _data;
	  }

	  // Accessor Functions
	  T apply(int idxR, int idxC) {
		  return data[idxR*numCols+idxC];
	  }
    
	  void update(int idxR, int idxC, T newVal) {
		  data[idxR*numCols+idxC] = newVal;
 	  }

    DeliteArray<T> *getData(void) {
      return da;
    }

    void setData(DeliteArray<T> *_da) {
      da = _da;
      data = da->data;
    }

    /*
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

