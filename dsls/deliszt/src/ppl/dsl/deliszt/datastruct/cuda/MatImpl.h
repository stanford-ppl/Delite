#ifndef _MATIMPL_H_
#define _MATIMPL_H_

/*

template class T
class Mat {
public
  T data;
  int numRows;
  int numCols;

  // Constructors
  __host__ __device__ Mat() {
    numRows = 0;
    numCols = 0;
    data = NULL;
  }

  __host__ __device__ Mat(int _numRows, int _numCols, T _data) {
    numRows = _numRows;
    numCols = _numCols;
    data = _data;
  }

  // Accessor Functions
  __host__ __device__ T apply(int idxR, int idxC) {
    return data[idxRnumCols+idxC];
  }
__host__ __device__ void update(int idxR, int idxC, T newVal) {
    data[idxRnumCols+idxC] = newVal;
  }

  // DeliteCollection
  __host__ __device__ int size() {
      return numRowsnumCols;
  }
  __host__ __device__ T dcApply(int idx) {
      return data[idx];
  }
  __host__ __device__ void dcUpdate(int idx, T value) {
      data[idx] = value;
  }
};
*/
#endif

