#ifndef _VECTORIMPL_H_
#define _VECTORIMPL_H_

#include <stdio.h>
#include <CL/cl.h>

class DoubleVector {
public:
    cl_mem data;
    int length;
    bool isRow;

    // Constructors
    DoubleVector() {
        length = 0;
        isRow = true;
        //data = NULL;
    }

    DoubleVector(int _length, bool _isRow, cl_mem _data) {
        length = _length;
        isRow = _isRow;
        data = _data;
    }

/*
    // Accessor Functions
    double apply(int idx) {
        return data[idx];
    }

    void update(int idx, double newVal) {
        data[idx] = newVal;
    }
*/
    // DeliteCoolection
    int dcSize() {
        return length;
    }
    /*
    double dcApply(int idx) {
        return data[idx];
    }

    void dcUpdate(int idx, double value) {
        data[idx] = value;
    }
    */

};

class FloatVector {
public:
    cl_mem data;
    int length;
    bool isRow;

    // Constructors
    FloatVector() {
        length = 0;
        isRow = true;
        //data = NULL;
    }

    FloatVector(int _length, bool _isRow, cl_mem _data) {
        length = _length;
        isRow = _isRow;
        data = _data;
    }

/*
    // Accessor Functions
    double apply(int idx) {
        return data[idx];
    }

    void update(int idx, double newVal) {
        data[idx] = newVal;
    }
*/
    // DeliteCoolection
    int dcSize() {
        return length;
    }
    /*
    double dcApply(int idx) {
        return data[idx];
    }

    void dcUpdate(int idx, double value) {
        data[idx] = value;
    }
    */

};

class IntVector {
public:
    cl_mem data;
    int length;
    bool isRow;

    // Constructors
    IntVector() {
        length = 0;
        isRow = true;
        //data = NULL;
    }

    IntVector(int _length, bool _isRow, cl_mem _data) {
        length = _length;
        isRow = _isRow;
        data = _data;
    }

/*
    // Accessor Functions
    double apply(int idx) {
        return data[idx];
    }

    void update(int idx, double newVal) {
        data[idx] = newVal;
    }
*/
    // DeliteCoolection
    int dcSize() {
        return length;
    }
    /*
    double dcApply(int idx) {
        return data[idx];
    }

    void dcUpdate(int idx, double value) {
        data[idx] = value;
    }
    */

};



/*
template <class T>
class Vector {
public:
    T *data;
    int length;
    bool isRow;

    // Constructors
    Vector() {
        length = 0;
        isRow = true;
        data = NULL;
    }

    Vector(int _length, bool _isRow, T *_data) {
        length = _length;
        isRow = _isRow;
        data = _data;
    }

    // Accessor Functions
    T apply(int idx) {
        return data[idx];
    }

    void update(int idx, T newVal) {
        data[idx] = newVal;
    }

    // DeliteCoolection
    int size() {
        return length;
    }

    T dcApply(int idx) {
        return data[idx];
    }

    void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
    
};
*/

//class BooleanVector:  public Vector<bool> ]};
//class IntVector:  public Vector<int> {};
//class FloatVector:  public Vector<float> {};
//class DoubleVector:  public Vector<double> {};

#endif
