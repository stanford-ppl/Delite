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
typedef struct FloatFloatTrainingSetStruct FloatFloatTrainingSet;
typedef struct FloatIntTrainingSetStruct FloatIntTrainingSet;

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
#endif
