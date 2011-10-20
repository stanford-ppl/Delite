#ifndef _LABELSIMPL_CL_H_
#define _LABELSIMPL_CL_H_

struct BooleanLabelsStruct {
    __global bool *data;
    int length;
    bool isRow;
};

struct IntLabelsStruct {
    __global int *data;
    int length;
    bool isRow;
};

struct FloatLabelsStruct {
    __global float *data;
    int length;
    bool isRow;
};

struct DoubleLabelsStruct {
    __global double *data;
    int length;
    bool isRow;
};

typedef struct BooleanLabelsStruct BooleanLabels;
typedef struct IntLabelsStruct IntLabels;
typedef struct FloatLabelsStruct FloatLabels;
typedef struct DoubleLabelsStruct DoubleLabels;

// Static methods on data types
bool BooleanLabels_dcApply(BooleanLabels vec, int idx) {
    return vec.data[idx];
}
int IntLabels_dcApply(IntLabels vec, int idx) {
    return vec.data[idx];
}
float FloatLabels_dcApply(FloatLabels vec, int idx) {
    return vec.data[idx];
}
double DoubleLabels_dcApply(DoubleLabels vec, int idx) {
    return vec.data[idx];
}

int BooleanLabels_dcSize(BooleanLabels vec) { return vec.length; }
int IntLabels_dcSize(IntLabels vec) { return vec.length; }
int FloatLabels_dcSize(FloatLabels vec) { return vec.length; }
int DoubleLabels_dcSize(DoubleLabels vec) { return vec.length; }


#endif
