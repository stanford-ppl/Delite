#ifndef _HOST_DENSEVECTOR_H_
#define _HOST_DENSEVECTOR_H_

#include <HostDeliteArray.h>
#include <stdlib.h>

template <class T>
class HostDenseVector {
public:
    HostDeliteArray<T> *da;
    T *data;
    int length;
    bool isRow;

    // Constructor
    HostDenseVector(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = new HostDeliteArray<T>(length);
        data = da->data;
    }

    HostDenseVector(HostDeliteArray<T> *_da, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = _da;
        data = _da->data;
    }

    HostDenseVector(T *_data, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = new HostDeliteArray<T>(_data, _length);
        data = _data;
    }

    // Accessor Functions
    T apply(int idx) {
        return data[idx];
    }

    void update(int idx, T newVal) {
        data[idx] = newVal;
    }

    HostDeliteArray<T> *getData(void) {
      return da;
    }

    void setData(HostDeliteArray<T> *_da) {
      da = _da;
      data = da->data;
    }

    /*
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

    // unsafeSetData
    void unsafeSetData(DeliteArray<T> *da, int _length) {
        data = da->data;
        length = _length;
    }

    DeliteArray<T> getdata(void) {
      DeliteArray<T> da(length, data);
      return da;
    }

    void setdata(DeliteArray<T> da) {
      data = da.data;
    }
    */
};

#endif
