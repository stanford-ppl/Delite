#ifndef _TRAININGSETIMPL_CL_H_
#define _TRAININGSETIMPL_CL_H_

//TODO: How to make sure that Labels is printed beforehand?
//#include "LabelsImpl.cl"

struct FloatFloatTrainingSetStruct {
    __global float *data;
    __global float *data_labels;
    int numRows;
    int numCols;
};
struct FloatIntTrainingSetStruct {
    __global float *data;
    __global int *data_labels;
    int numRows;
    int numCols;
};
struct DoubleDoubleTrainingSetStruct {
    __global double *data;
    __global double *data_labels;
    int numRows;
    int numCols;
};
struct DoubleIntTrainingSetStruct {
    __global double *data;
    __global int *data_labels;
    int numRows;
    int numCols;
};
typedef struct FloatFloatTrainingSetStruct FloatFloatTrainingSet;
typedef struct FloatIntTrainingSetStruct FloatIntTrainingSet;
typedef struct DoubleDoubleTrainingSetStruct DoubleDoubleTrainingSet;
typedef struct DoubleIntTrainingSetStruct DoubleIntTrainingSet;

// Static methods on data types
float FloatFloatTrainingSet_dcApply(FloatFloatTrainingSet ts, int idx) {
    return ts.data[idx];
}
int FloatFloatTrainingSet_dcSize(FloatFloatTrainingSet ts) { return ts.numRows * ts.numCols; }
FloatFloatTrainingSet FloatFloatTrainingSet_transposed(FloatFloatTrainingSet ts) {
    FloatFloatTrainingSet ret;
    ret.data = ts.data;
    ret.data_labels = ts.data_labels;
    ret.numRows = ts.numCols;
    ret.numCols = ts.numRows;
    return ret;
}
FloatLabels FloatFloatTrainingSet_labels(FloatFloatTrainingSet ts) {
    FloatLabels ret;
    ret.data = ts.data_labels;
    ret.length = ts.numCols;
    ret.isRow = true;
    return ret;
}

float FloatIntTrainingSet_dcApply(FloatIntTrainingSet ts, int idx) {
    return ts.data[idx];
}
int FloatIntTrainingSet_dcSize(FloatIntTrainingSet ts) { return ts.numRows * ts.numCols; }
FloatIntTrainingSet FloatIntTrainingSet_transposed(FloatIntTrainingSet ts) {
    FloatIntTrainingSet ret;
    ret.data = ts.data;
    ret.data_labels = ts.data_labels;
    ret.numRows = ts.numCols;
    ret.numCols = ts.numRows;
    return ret;
}
IntLabels FloatIntTrainingSet_labels(FloatIntTrainingSet ts) {
    IntLabels ret;
    ret.data = ts.data_labels;
    ret.length = ts.numCols;
    ret.isRow = true;
    return ret;
}

float DoubleDoubleTrainingSet_dcApply(DoubleDoubleTrainingSet ts, int idx) {
    return ts.data[idx];
}
int DoubleDoubleTrainingSet_dcSize(DoubleDoubleTrainingSet ts) { return ts.numRows * ts.numCols; }
DoubleDoubleTrainingSet DoubleDoubleTrainingSet_transposed(DoubleDoubleTrainingSet ts) {
    DoubleDoubleTrainingSet ret;
    ret.data = ts.data;
    ret.data_labels = ts.data_labels;
    ret.numRows = ts.numCols;
    ret.numCols = ts.numRows;
    return ret;
}
DoubleLabels DoubleDoubleTrainingSet_labels(DoubleDoubleTrainingSet ts) {
    DoubleLabels ret;
    ret.data = ts.data_labels;
    ret.length = ts.numCols;
    ret.isRow = true;
    return ret;
}

float DoubleIntTrainingSet_dcApply(DoubleIntTrainingSet ts, int idx) {
    return ts.data[idx];
}
int DoubleIntTrainingSet_dcSize(DoubleIntTrainingSet ts) { return ts.numRows * ts.numCols; }
DoubleIntTrainingSet DoubleIntTrainingSet_transposed(DoubleIntTrainingSet ts) {
    DoubleIntTrainingSet ret;
    ret.data = ts.data;
    ret.data_labels = ts.data_labels;
    ret.numRows = ts.numCols;
    ret.numCols = ts.numRows;
    return ret;
}
IntLabels DoubleIntTrainingSet_labels(DoubleIntTrainingSet ts) {
    IntLabels ret;
    ret.data = ts.data_labels;
    ret.length = ts.numCols;
    ret.isRow = true;
    return ret;
}
#endif
