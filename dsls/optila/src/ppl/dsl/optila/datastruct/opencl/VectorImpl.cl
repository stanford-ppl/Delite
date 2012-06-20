#ifndef _VECTORIMPL_CL_H_
#define _VECTORIMPL_CL_H_

struct DenseVector_boolStruct {
    __global bool *data;
    int length;
    bool isRow;
};

struct DenseVector_intStruct {
    __global int *data;
    int length;
    bool isRow;
};

struct DenseVector_floatStruct {
    __global float *data;
    int length;
    bool isRow;
};

struct DenseVector_doubleStruct {
    __global double *data;
    int length;
    bool isRow;
};

typedef struct DenseVector_boolStruct DenseVector_bool;
typedef struct DenseVector_intStruct DenseVector_int;
typedef struct DenseVector_floatStruct DenseVector_float;
typedef struct DenseVector_doubleStruct DenseVector_double;

// Static methods on data types
bool DenseVector_bool_dcApply(DenseVector_bool vec, int idx) {
    return vec.data[idx];
}
int DenseVector_int_dcApply(DenseVector_int vec, int idx) {
    return vec.data[idx];
}
float DenseVector_float_dcApply(DenseVector_float vec, int idx) {
    return vec.data[idx];
}
double DenseVector_double_dcApply(DenseVector_double vec, int idx) {
    return vec.data[idx];
}

void DenseVector_bool_dcUpdate(DenseVector_bool vec, int idx, bool value) {
    vec.data[idx] = value;
}
void DenseVector_int_dcUpdate(DenseVector_int vec, int idx, int value) {
    vec.data[idx] = value;
}
void DenseVector_float_dcUpdate(DenseVector_float vec, int idx, float value) {
    vec.data[idx] = value;
}
void DenseVector_double_dcUpdate(DenseVector_double vec, int idx, double value) {
    vec.data[idx] = value;
}

int DenseVector_bool_dcSize(DenseVector_bool vec) { return vec.length; }
int DenseVector_int_dcSize(DenseVector_int vec) { return vec.length; }
int DenseVector_float_dcSize(DenseVector_float vec) { return vec.length; }
int DenseVector_double_dcSize(DenseVector_double vec) { return vec.length; }


#endif
