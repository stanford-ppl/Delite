#ifndef _TRAININGSETIMPL_H_
#define _TRAININGSETIMPL_H_

#include <stdio.h>
#include "DenseMatrix.h"
#include "LabelsImpl.h"

template <class T, class L>
class TrainingSet;

template <class T, class L>
class TrainingSet {
public:
	T *data;
	int numRows;
	int numCols;
   	int numSamples(void) { return numRows; }
   	int numFeatures(void) { return numCols; }
  	Labels<L> labels;
  	TrainingSet<T,L> *transposed;

	// Constructors
	__host__ __device__ TrainingSet() {
		data = NULL;
		numRows = 0;
		numCols = 0;
		//labels = NULL;
		//transposed = NULL;
	}

	__host__ __device__ TrainingSet(DenseMatrix<T> mat, TrainingSet<T,L> *set_t, Labels<L> lab) {
		data = mat.data;
		numRows = mat.numRows;
		numCols = mat.numCols;
		labels = lab;
		transposed = set_t;
	}

	// Accessor Functions
	__host__ __device__ T apply(int idxR, int idxC) {
		return data[idxR*numCols+idxC];
	}
	__host__ __device__ void update(int idxR, int idxC, T newVal) {
		data[idxR*numCols+idxC] = newVal;
	}

    	// DeliteCollection
    	__host__ __device__ int size() {
        	return numRows*numCols;
    	}
    	__host__ __device__ T dcApply(int idx) {
        	return data[idx];
    	}
   	__host__ __device__ void dcUpdate(int idx, T value) {
        	data[idx] = value;
   	}

};

#endif

