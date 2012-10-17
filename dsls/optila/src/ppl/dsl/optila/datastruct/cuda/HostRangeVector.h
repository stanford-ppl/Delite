#ifndef _HOST_RANGEVECTOR_H_
#define _HOST_RANGEVECTOR_H_

class HostRangeVector {
public:
    int start;
    int end;
    int stride;
    bool isRow;

    // Constructor
    //RangeVector() { }

    HostRangeVector(int _start, bool _end, int _stride, bool _isRow) {
        start = _start;
        end = _end;
        stride = _stride;
        isRow = _isRow;
    }

    /*
    // Accessor Functions
    int apply(int idx) {
        return start + idx*stride;
    }

    // DeliteCoolection
    int size() {
        return (end - start + stride - 1) / stride;
    }

    int dcApply(int idx) {
        return start + idx*stride;
    }
    */
    
};

#endif
