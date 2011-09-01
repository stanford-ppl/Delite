#ifndef _RANGEVECTORIMPL_CL_H_
#define _RANGEVECTORIMPL_CL_H_

struct RangeVectorStruct {
    int start;
    int end;
    int stride;
    bool isRow;
};

typedef struct RangeVectorStruct RangeVector;

// Static methods on data types
int RangeVector_dcApply(RangeVector vec, int idx) {
    return vec.start+vec.stride*idx;
}
int RangeVector_dcSize(RangeVector vec) {
    return (vec.end - vec.start + vec.stride - 1);
}
int RangeVector_length(RangeVector vec) {
    return (vec.end - vec.start + vec.stride - 1);
}

#endif
