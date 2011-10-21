#ifndef _MATRIXROWIMPL_CL_H_
#define _MATRIXROWIMPL_CL_H_

struct BooleanMatrixRowStruct {
    __global bool *data;
    int length;
    bool isRow;
};

struct IntMatrixRowStruct {
    __global int *data;
    int length;
    bool isRow;
};

struct FloatMatrixRowStruct {
    __global float *data;
    int length;
    bool isRow;
};

struct DoubleMatrixRowStruct {
    __global double *data;
    int length;
    bool isRow;
};

typedef struct BooleanMatrixRowStruct BooleanMatrixRow;
typedef struct IntMatrixRowStruct IntMatrixRow;
typedef struct FloatMatrixRowStruct FloatMatrixRow;
typedef struct DoubleMatrixRowStruct DoubleMatrixRow;

// Static methods on data types
bool BooleanMatrixRow_dcApply(BooleanMatrixRow vec, int idx) {
    return vec.data[idx];
}
int IntMatrixRow_dcApply(IntMatrixRow vec, int idx) {
    return vec.data[idx];
}
float FloatMatrixRow_dcApply(FloatMatrixRow vec, int idx) {
    return vec.data[idx];
}
double DoubleMatrixRow_dcApply(DoubleMatrixRow vec, int idx) {
    return vec.data[idx];
}

void BooleanMatrixRow_dcUpdate(BooleanMatrixRow vec, int idx, bool value) {
    vec.data[idx] = value;
}
void IntMatrixRow_dcUpdate(IntMatrixRow vec, int idx, int value) {
    vec.data[idx] = value;
}
void FloatMatrixRow_dcUpdate(FloatMatrixRow vec, int idx, float value) {
    vec.data[idx] = value;
}
void DoubleMatrixRow_dcUpdate(DoubleMatrixRow vec, int idx, double value) {
    vec.data[idx] = value;
}

int BooleanMatrixRow_dcSize(BooleanMatrixRow vec) { return vec.length; }
int IntMatrixRow_dcSize(IntMatrixRow vec) { return vec.length; }
int FloatMatrixRow_dcSize(FloatMatrixRow vec) { return vec.length; }
int DoubleMatrixRow_dcSize(DoubleMatrixRow vec) { return vec.length; }


#endif
