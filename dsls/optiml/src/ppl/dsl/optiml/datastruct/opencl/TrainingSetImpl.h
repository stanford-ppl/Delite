#ifndef _TRAININGSETIMPL_H_
#define _TRAININGSETIMPL_H_

#include <stdio.h>
#include <CL/cl.h>
#include "MatrixImpl.h"
#include "LabelsImpl.h"

class FloatFloatTrainingSet {
public:
	cl_mem data;
	int numRows;
	int numCols;
  	cl_mem data_labels;

   	int numSamples(void) { return numRows; }
   	int numFeatures(void) { return numCols; }

    FloatFloatTrainingSet transposed(void) {
        FloatFloatTrainingSet ret;
        ret.data = data;
        ret.numRows = numCols;
        ret.numCols = numRows;
        ret.data_labels = data_labels;
        return ret;
    }

    FloatLabels labels(void) {
        FloatLabels ret;
        ret.data = data_labels;
        ret.length = numCols;
        ret.isRow = true;
        return ret;
    }

	// Constructors
    FloatFloatTrainingSet() {
		data = NULL;
		numRows = 0;
		numCols = 0;
	}

	FloatFloatTrainingSet(cl_mem _data, int _numRows, int _numCols, cl_mem _data_labels) {
		data = _data;
		numRows = _numRows;
		numCols = _numCols;
		data_labels = _data_labels;
	}

    // DeliteCollection
    int dcSize() {
        return numRows*numCols;
    }
};

class FloatIntTrainingSet {
public:
	cl_mem data;
	int numRows;
	int numCols;
  	cl_mem data_labels;

   	int numSamples(void) { return numRows; }
   	int numFeatures(void) { return numCols; }

    FloatIntTrainingSet transposed(void) {
        FloatIntTrainingSet ret;
        ret.data = data;
        ret.numRows = numCols;
        ret.numCols = numRows;
        ret.data_labels = data_labels;
        return ret;
    }

    IntLabels labels(void) {
        IntLabels ret;
        ret.data = data_labels;
        ret.length = numCols;
        ret.isRow = true;
        return ret;
    }

	// Constructors
    FloatIntTrainingSet() {
		data = NULL;
		numRows = 0;
		numCols = 0;
	}

	FloatIntTrainingSet(cl_mem _data, int _numRows, int _numCols, cl_mem _data_labels) {
		data = _data;
		numRows = _numRows;
		numCols = _numCols;
		data_labels = _data_labels;
	}

    // DeliteCollection
    int dcSize() {
        return numRows*numCols;
    }
};

class DoubleDoubleTrainingSet {
public:
	cl_mem data;
	int numRows;
	int numCols;
  	cl_mem data_labels;

   	int numSamples(void) { return numRows; }
   	int numFeatures(void) { return numCols; }

    DoubleDoubleTrainingSet transposed(void) {
        DoubleDoubleTrainingSet ret;
        ret.data = data;
        ret.numRows = numCols;
        ret.numCols = numRows;
        ret.data_labels = data_labels;
        return ret;
    }

    DoubleLabels labels(void) {
        DoubleLabels ret;
        ret.data = data_labels;
        ret.length = numCols;
        ret.isRow = true;
        return ret;
    }

	// Constructors
    DoubleDoubleTrainingSet() {
		data = NULL;
		numRows = 0;
		numCols = 0;
	}

	DoubleDoubleTrainingSet(cl_mem _data, int _numRows, int _numCols, cl_mem _data_labels) {
		data = _data;
		numRows = _numRows;
		numCols = _numCols;
		data_labels = _data_labels;
	}

    // DeliteCollection
    int dcSize() {
        return numRows*numCols;
    }
};

class DoubleIntTrainingSet {
public:
	cl_mem data;
	int numRows;
	int numCols;
  	cl_mem data_labels;

   	int numSamples(void) { return numRows; }
   	int numFeatures(void) { return numCols; }

    DoubleIntTrainingSet transposed(void) {
        DoubleIntTrainingSet ret;
        ret.data = data;
        ret.numRows = numCols;
        ret.numCols = numRows;
        ret.data_labels = data_labels;
        return ret;
    }

    IntLabels labels(void) {
        IntLabels ret;
        ret.data = data_labels;
        ret.length = numCols;
        ret.isRow = true;
        return ret;
    }

	// Constructors
    DoubleIntTrainingSet() {
		data = NULL;
		numRows = 0;
		numCols = 0;
	}

	DoubleIntTrainingSet(cl_mem _data, int _numRows, int _numCols, cl_mem _data_labels) {
		data = _data;
		numRows = _numRows;
		numCols = _numCols;
		data_labels = _data_labels;
	}

    // DeliteCollection
    int dcSize() {
        return numRows*numCols;
    }
};

#endif

