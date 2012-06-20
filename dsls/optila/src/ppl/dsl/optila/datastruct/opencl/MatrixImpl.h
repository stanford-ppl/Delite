#ifndef _MATRIXIMPL_H_
#define _MATRIXIMPL_H_

#include <stdio.h>
#include <CL/cl.h>
#include "DeliteOpenCL.h"

class DenseMatrix_double {
public:
    cl_mem data;
    int numRows;
    int numCols;

    // Constructors
    DenseMatrix_double() {
        numRows = 0;
        numCols = 0;
    }

    DenseMatrix_double(int _numRows, int _numCols) {
        numRows = _numRows;
        numCols = _numCols;
        data = DeliteOpenCLMalloc(sizeof(double)*numRows*numCols);
    }

    // DeliteCoolection
    int dcSize() {
        return numRows*numCols;
    }
};

class DenseMatrix_float {
public:
    cl_mem data;
    int numRows;
    int numCols;

    // Constructors
    DenseMatrix_float() {
        numRows = 0;
        numCols = 0;
    }

    DenseMatrix_float(int _numRows, int _numCols) {
        numRows = _numRows;
        numCols = _numCols;
        data = DeliteOpenCLMalloc(sizeof(float)*numRows*numCols);
    }

    // DeliteCoolection
    int dcSize() {
        return numRows*numCols;
    }

};

class DenseMatrix_int {
public:
    cl_mem data;
    int numRows;
    int numCols;

    // Constructors
    DenseMatrix_int() {
        numRows = 0;
        numCols = 0;
    }

    DenseMatrix_int(int _numRows, int _numCols) {
        numRows = _numRows;
        numCols = _numCols;
        data = DeliteOpenCLMalloc(sizeof(int)*numRows*numCols);
    }

    // DeliteCoolection
    int dcSize() {
        return numRows*numCols;
    }

};

class DenseMatrix_bool {
public:
    cl_mem data;
    int numRows;
    int numCols;

    // Constructors
    DenseMatrix_bool() {
        numRows = 0;
        numCols = 0;
    }

    DenseMatrix_bool(int _numRows, int _numCols) {
        numRows = _numRows;
        numCols = _numCols;
        data = DeliteOpenCLMalloc(sizeof(bool)*numRows*numCols);
    }

    // DeliteCoolection
    int dcSize() {
        return numRows*numCols;
    }

};

#endif
