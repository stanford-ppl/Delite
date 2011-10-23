#ifndef _VECTORIMPL_CL_H_
#define _VECTORIMPL_CL_H_

struct BooleanVectorStruct {
    __global bool *data;
    int length;
    bool isRow;
};

struct IntVectorStruct {
    __global int *data;
    int length;
    bool isRow;
};

struct FloatVectorStruct {
    __global float *data;
    int length;
    bool isRow;
};

struct DoubleVectorStruct {
    __global double *data;
    int length;
    bool isRow;
};

typedef struct BooleanVectorStruct BooleanVector;
typedef struct IntVectorStruct IntVector;
typedef struct FloatVectorStruct FloatVector;
typedef struct DoubleVectorStruct DoubleVector;

// Static methods on data types
bool BooleanVector_dcApply(BooleanVector vec, int idx) {
    return vec.data[idx];
}
int IntVector_dcApply(IntVector vec, int idx) {
    return vec.data[idx];
}
float FloatVector_dcApply(FloatVector vec, int idx) {
    return vec.data[idx];
}
double DoubleVector_dcApply(DoubleVector vec, int idx) {
    return vec.data[idx];
}

void BooleanVector_dcUpdate(BooleanVector vec, int idx, bool value) {
    vec.data[idx] = value;
}
void IntVector_dcUpdate(IntVector vec, int idx, int value) {
    vec.data[idx] = value;
}
void FloatVector_dcUpdate(FloatVector vec, int idx, float value) {
    vec.data[idx] = value;
}
void DoubleVector_dcUpdate(DoubleVector vec, int idx, double value) {
    vec.data[idx] = value;
}

int BooleanVector_dcSize(BooleanVector vec) { return vec.length; }
int IntVector_dcSize(IntVector vec) { return vec.length; }
int FloatVector_dcSize(FloatVector vec) { return vec.length; }
int DoubleVector_dcSize(DoubleVector vec) { return vec.length; }


#endif
