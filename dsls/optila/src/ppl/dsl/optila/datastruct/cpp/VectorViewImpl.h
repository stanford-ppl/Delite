#ifndef _VECTORVIEWIMPL_H_
#define _VECTORVIEWIMPL_H_

template <class T>
class DenseVectorView {
public:
    T *data;
    int length;
    bool isRow;
    int start;
    int stride;

    // Constructors
    /*
    VectorView() {
        length = 0;
        isRow = true;
        data = NULL;
        start = 0;
        stride = 1;
    }
    */

    DenseVectorView(DeliteArray<T> *_da, int _start, int _stride, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        data = _da->data;
        start = _start;
        stride = _stride;
    }

    // Accessor Functions
    T apply(int idx) {
        return data[start+idx*stride];
    }

    void update(int idx, T newVal) {
        data[start+idx*stride] = newVal;
    }

    /*
    // DeliteCoolection
    int size() {
        return length;
    }

    T dcApply(int idx) {
        return data[start+idx*stride];
    }

    void dcUpdate(int idx, T value) {
        data[start+idx*stride] = value;
    }
    */
    
};

#endif
