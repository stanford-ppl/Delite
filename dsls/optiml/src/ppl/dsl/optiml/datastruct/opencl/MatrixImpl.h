#ifndef _MATRIXIMPL_H_
#define _MATRIXIMPL_H_

#include <stdio.h>
#include <CL/cl.h>

class DoubleMatrix {
public:
    cl_mem data;
    int numRows;
    int numCols;

    // Constructors
    DoubleMatrix() {
        numRows = 0;
        numCols = 0;
    }

    DoubleMatrix(int _numRows, int _numCols, cl_mem _data) {
        numRows = _numRows;
        numCols = _numCols;
        data = _data;
    }

    // DeliteCoolection
    int dcSize() {
        return numRows*numCols;
    }

    //TODO: What if helper functions need to access the data fields?
    /*
    double dcApply(int idx) {
        return data[idx];
    }

    void dcUpdate(int idx, double value) {
        data[idx] = value;
    }
    */

};

class FloatMatrix {
public:
    cl_mem data;
    int numRows;
    int numCols;

    // Constructors
    FloatMatrix() {
        numRows = 0;
        numCols = 0;
    }

    FloatMatrix(int _numRows, int _numCols, cl_mem _data) {
        numRows = _numRows;
        numCols = _numCols;
        data = _data;
    }

    // DeliteCoolection
    int dcSize() {
        return numRows*numCols;
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
class Matrix {
public:
    T *data;
    int length;
    bool isRow;

    // Constructors
    Matrix() {
        length = 0;
        isRow = true;
        data = NULL;
    }

    Matrix(int _length, bool _isRow, T *_data) {
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

//class BooleanMatrix:  public Matrix<bool> ]};
//class IntMatrix:  public Matrix<int> {};
//class FloatMatrix:  public Matrix<float> {};
//class DoubleMatrix:  public Matrix<double> {};

#endif
