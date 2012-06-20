#ifndef _VECTORIMPL_H_
#define _VECTORIMPL_H_

#include <stdio.h>
#include <CL/cl.h>
#include "DeliteOpenCL.h"

class DenseVector_double {
public:
    cl_mem data;
    int length;
    bool isRow;

    // Constructors
    DenseVector_double() {
        length = 0;
        isRow = true;
    }

    DenseVector_double(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        data = DeliteOpenCLMalloc(sizeof(double)*length);
    }

    // DeliteCoolection
    int dcSize() {
        return length;
    }
};

class DenseVector_float {
public:
    cl_mem data;
    int length;
    bool isRow;

    // Constructors
    DenseVector_float() {
        length = 0;
        isRow = true;
    }

    DenseVector_float(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        data = DeliteOpenCLMalloc(sizeof(float)*length);
    }

    // DeliteCoolection
    int dcSize() {
        return length;
    }

};

class DenseVector_int {
public:
    cl_mem data;
    int length;
    bool isRow;

    // Constructors
    DenseVector_int() {
        length = 0;
        isRow = true;
    }

    DenseVector_int(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        data = DeliteOpenCLMalloc(sizeof(int)*length);
    }

    // DeliteCoolection
    int dcSize() {
        return length;
    }
};

class DenseVector_bool {
public:
    cl_mem data;
    int length;
    bool isRow;

    // Constructors
    DenseVector_bool() {
        length = 0;
        isRow = true;
    }

    DenseVector_bool(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        data = DeliteOpenCLMalloc(sizeof(bool)*length);
    }

    // DeliteCoolection
    int dcSize() {
        return length;
    }
};
#endif
