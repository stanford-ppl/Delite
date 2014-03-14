#ifndef _DELITEARRAY_H_
#define _DELITEARRAY_H_

#include "DeliteOpenCL.h"

class DeliteArray_bool {
public:
    cl_mem data;
    int length;

    // Constructor
    DeliteArray_bool(int _length) {
        length = _length;
        data = DeliteOpenCLMalloc(length*sizeof(bool));
    }
};

class DeliteArray_char {
public:
    cl_mem data;
    int length;

    // Constructor
    DeliteArray_char(int _length) {
        length = _length;
        data = DeliteOpenCLMalloc(length*sizeof(char));
    }
};

class DeliteArray_CHAR {
public:
    cl_mem data;
    int length;

    // Constructor
    DeliteArray_CHAR(int _length) {
        length = _length;
        data = DeliteOpenCLMalloc(length*sizeof(CHAR));
    }
};

class DeliteArray_short {
public:
    cl_mem data;
    int length;

    // Constructor
    DeliteArray_short(int _length) {
        length = _length;
        data = DeliteOpenCLMalloc(length*sizeof(short));
    }
};

class DeliteArray_int {
public:
    cl_mem data;
    int length;

    // Constructor
    DeliteArray_int(int _length) {
        length = _length;
        data = DeliteOpenCLMalloc(length*sizeof(int));
    }
};

class DeliteArray_long {
public:
    cl_mem data;
    int length;

    // Constructor
    DeliteArray_long(int _length) {
        length = _length;
        data = DeliteOpenCLMalloc(length*sizeof(long));
    }
};

class DeliteArray_float {
public:
    cl_mem data;
    int length;

    // Constructor
    DeliteArray_float(int _length) {
        length = _length;
        data = DeliteOpenCLMalloc(length*sizeof(float));
    }
};

class DeliteArray_double {
public:
    cl_mem data;
    int length;

    // Constructor
    DeliteArray_double(int _length) {
        length = _length;
        data = DeliteOpenCLMalloc(length*sizeof(double));
    }
};

#endif
