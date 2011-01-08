#ifndef _MATRIXIMPL_H_
#define _MATRIXIMPL_H_

#include <stdio.h>

template <class T>
class Matrix {
public:
    T *data;
	int numRows;
	int numCols;
	
	// Constructors
	Matrix() {
		numRows = 0;
		numCols = 0;
		data = NULL;
	}

	Matrix(int _numRows, int _numCols, T *_data) {
		numRows = _numRows;
		numCols = _numCols;
		data = _data;
	}

	// Accessor Functions
	T apply(int idxR, int idxC) {
		return data[idxR*numCols+idxC];
	}
	void update(int idxR, int idxC, T newVal) {
		data[idxR*numCols+idxC] = newVal;
	}

    // DeliteCollection
    int size() {
        return numRows*numCols;
    }
    T dcApply(int idx) {
        return data[idx];
    }
    void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
	
};

#endif

