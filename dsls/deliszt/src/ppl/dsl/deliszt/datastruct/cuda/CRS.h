#ifndef _CRS_H_
#define _CRS_H_

#include <cuda.h>

class CRS {
public:
  int *rows;
  int *values;

  __device__ __host__ int row(int r) {
    return rows[r];
  }

  __device__ __host__ int apply(int row, int i) {
    return values[rows[row] + i];
  }

  __device__ __host__ void update(int row, int i, int v) {
    values[rows[row] + i] = v;
  }

  __device__ __host__ int len(int row) {
    return rows[row+1] - rows[row];
  }
};

#endif