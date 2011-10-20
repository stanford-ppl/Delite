#ifndef _INDEXVECTORIMPL_CL_H_
#define _INDEXVECTORIMPL_CL_H_

struct IndexVectorStruct {
    __global int *data;
    int length;
    bool isRow;
};
typedef struct IndexVectorStruct IndexVector;

// Static methods on data types
int IndexVector_dcApply(IndexVector vec, int idx) {
    return vec.data[idx];
}
int IndexVector_dcSize(IndexVector vec) {
    return vec.length;
}
int IndexVector_length(IndexVector vec) {
    return vec.length;
}

#endif
