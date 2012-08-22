#ifndef _MATRIXIMPL_CL_H_
#define _MATRIXIMPL_CL_H_

struct DenseMatrix_boolStruct {
    __global bool *data;
    int numRows;
    int numCols;
};

struct DenseMatrix_intStruct {
    __global int *data;
    int numRows;
    int numCols;
};

struct DenseMatrix_floatStruct {
    __global float *data;
    int numRows;
    int numCols;
};

struct DenseMatrix_doubleStruct {
    __global double *data;
    int numRows;
    int numCols;
};

typedef struct DenseMatrix_boolStruct DenseMatrix_bool;
typedef struct DenseMatrix_intStruct DenseMatrix_int;
typedef struct DenseMatrix_floatStruct DenseMatrix_float;
typedef struct DenseMatrix_doubleStruct DenseMatrix_double;

// Static methods on data types
bool DenseMatrix_bool_dcApply(DenseMatrix_bool mat, int idx) {
    return mat.data[idx];
}
int DenseMatrix_int_dcApply(DenseMatrix_int mat, int idx) {
    return mat.data[idx];
}
float DenseMatrix_float_dcApply(DenseMatrix_float mat, int idx) {
    return mat.data[idx];
}
double DenseMatrix_double_dcApply(DenseMatrix_double mat, int idx) {
    return mat.data[idx];
}

void DenseMatrix_bool_dcUpdate(DenseMatrix_bool mat, int idx, bool value) {
    mat.data[idx] = value;
}
void DenseMatrix_int_dcUpdate(DenseMatrix_int mat, int idx, int value) {
    mat.data[idx] = value;
}
void DenseMatrix_float_dcUpdate(DenseMatrix_float mat, int idx, float value) {
    mat.data[idx] = value;
}
void DenseMatrix_double_dcUpdate(DenseMatrix_double mat, int idx, double value) {
    mat.data[idx] = value;
}

int DenseMatrix_bool_dcSize(DenseMatrix_bool mat) { return mat.numRows * mat.numCols; }
int DenseMatrix_int_dcSize(DenseMatrix_int mat) { return mat.numRows * mat.numCols; }
int DenseMatrix_float_dcSize(DenseMatrix_float mat) { return mat.numRows * mat.numCols; }
int DenseMatrix_double_dcSize(DenseMatrix_double mat) { return mat.numRows * mat.numCols; }


#endif
