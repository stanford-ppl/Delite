#ifndef _MATRIXIMPL_CL_H_
#define _MATRIXIMPL_CL_H_

struct BooleanMatrixStruct {
    __global bool *data;
    int numRows;
    int numCols;
};

struct IntMatrixStruct {
    __global int *data;
    int numRows;
    int numCols;
};

struct FloatMatrixStruct {
    __global float *data;
    int numRows;
    int numCols;
};

struct DoubleMatrixStruct {
    __global double *data;
    int numRows;
    int numCols;
};

typedef struct BooleanMatrixStruct BooleanMatrix;
typedef struct IntMatrixStruct IntMatrix;
typedef struct FloatMatrixStruct FloatMatrix;
typedef struct DoubleMatrixStruct DoubleMatrix;

// Static methods on data types
bool BooleanMatrix_dcApply(BooleanMatrix mat, int idx) {
    return mat.data[idx];
}
int IntMatrix_dcApply(IntMatrix mat, int idx) {
    return mat.data[idx];
}
float FloatMatrix_dcApply(FloatMatrix mat, int idx) {
    return mat.data[idx];
}
double DoubleMatrix_dcApply(DoubleMatrix mat, int idx) {
    return mat.data[idx];
}

void BooleanMatrix_dcUpdate(BooleanMatrix mat, int idx, bool value) {
    mat.data[idx] = value;
}
void IntMatrix_dcUpdate(IntMatrix mat, int idx, int value) {
    mat.data[idx] = value;
}
void FloatMatrix_dcUpdate(FloatMatrix mat, int idx, float value) {
    mat.data[idx] = value;
}
void DoubleMatrix_dcUpdate(DoubleMatrix mat, int idx, double value) {
    mat.data[idx] = value;
}

int BooleanMatrix_dcSize(BooleanMatrix mat) { return mat.numRows * mat.numCols; }
int IntMatrix_dcSize(IntMatrix mat) { return mat.numRows * mat.numCols; }
int FloatMatrix_dcSize(FloatMatrix mat) { return mat.numRows * mat.numCols; }
int DoubleMatrix_dcSize(DoubleMatrix mat) { return mat.numRows * mat.numCols; }


#endif
