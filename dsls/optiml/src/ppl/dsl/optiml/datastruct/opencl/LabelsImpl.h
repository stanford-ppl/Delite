#ifndef _LABELSIMPL_H_
#define _LABELSIMPL_H_

#include <stdio.h>
#include <CL/cl.h>

class DoubleLabels {
public:
    cl_mem data;
    int length;
    bool isRow;
    int numLabels(void) { return length; }

    // Constructors
    DoubleLabels() {
        length = 0;
        isRow = true;
    }

    DoubleLabels(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
    }

    // Delite Collection
     int dcSize() {
        return length;
    }
};


class FloatLabels {
public:
    cl_mem data;
    int length;
    bool isRow;
    int numLabels(void) { return length; }

    // Constructors
    FloatLabels() {
        length = 0;
        isRow = true;
    }

    FloatLabels(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
    }

    // Delite Collection
     int dcSize() {
        return length;
    }
};

class IntLabels {
public:
    cl_mem data;
    int length;
    bool isRow;
    int numLabels(void) { return length; }

    // Constructors
    IntLabels() {
        length = 0;
        isRow = true;
    }

    IntLabels(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
    }

    // Delite Collection
     int dcSize() {
        return length;
    }
};
#endif
