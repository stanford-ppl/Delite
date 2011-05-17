#ifndef _LISZT_ARRAY_H
#define _LISZT_ARRAY_H
#include <cstring>

//constant length array with size, suitable to use in smart pointers
template<typename T>
class Array {
public:
	Array() {
		size_ = 0;
		elems = NULL;
		owns_elems = 0;
	}
	Array(size_t sz) {
		size_ = sz;
		owns_elems = 1;
		elems = new T[size_];
	}
	~Array() {
		size_ = 0;
		if(owns_elems) {
			delete [] elems;
			elems = NULL;
		}
	}
	void reset() {
		if(owns_elems)
			delete [] elems;
		size_ = 0;
		elems = NULL;
		owns_elems = 0;
	}
	void reset(size_t sz) {
		if(owns_elems)
			delete [] elems;
		size_ = sz;
		elems = new T[size_];
		owns_elems = 1;
	}
	operator T *() {
		return elems;
	}
	operator const T *() const {
		return elems;
	}
	size_t size() const {
		return size_;
	}
	T * begin() {
		return elems;
	}
	T * end() {
		return elems + size();
	}
	const T * begin() const {
		return elems;
	}
	const T * end() const {
		return elems + size();
	}
	T * get() {
		return elems;
	}
	const T * get() const {
		return elems;
	}
	typedef T value_type;
	//extract range [b,e)
	void slice(size_t b, size_t e, Array<T> * result) {
		result->reset();
		result->size_ = e - b;
		result->elems = begin() + b;
		result->owns_elems = 0;
	}
private:
	Array(const Array<T> & lhs);
	T & operator=(const Array<T> & rhs);
	T * elems;
	unsigned long long int size_ : 63;
	unsigned long long int owns_elems : 1;
};

#endif