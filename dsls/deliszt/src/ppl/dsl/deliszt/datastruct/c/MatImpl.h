#ifndef _MATIMPL_H_
#define _MATIMPL_H_

#include stdio.h

template class T
class Mat {
public
    T data;
	int numRows;
	int numCols;
	
	 Constructors
	Mat() {
		numRows = 0;
		numCols = 0;
		data = NULL;
	}

	Mat(int _numRows, int _numCols, T _data) {
		numRows = _numRows;
		numCols = _numCols;
		data = _data;
	}

	 Accessor Functions
	T apply(int idxR, int idxC) {
		return data[idxRnumCols+idxC];
	}
	void update(int idxR, int idxC, T newVal) {
		data[idxRnumCols+idxC] = newVal;
	}

     DeliteCollection
    int size() {
        return numRowsnumCols;
    }
    T dcApply(int idx) {
        return data[idx];
    }
    void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
	
};

#endif

