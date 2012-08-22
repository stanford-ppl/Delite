#ifndef AUTO_PTR_ARRAY_ADAPTER_H
#define AUTO_PTR_ARRAY_ADAPTER_H

/*
 * Use this as a wrapper for arrays that you want to store inside auto_ptr;
 * Inspired by boost::scoped_array
 *
 * Example use case: 
 * auto_ptr< auto_ptr_array_adapter<T> > p2( new auto_ptr_array_adapter<T>(new T[n]));
 * (*p2)[i]
 *
 * If this happens to be a performance block because of the extra indirection, we have several options:
 * - write our own auto_array, clone auto_ptr (must keep it compatible with all the compilers)
 * - switch to vectors (bleaggg)
 */
template <typename T>
class auto_ptr_array_adapter {
public:
	explicit auto_ptr_array_adapter(T* ptr): _ptr(ptr) {}
	~auto_ptr_array_adapter() { delete[] _ptr; }
	
	void reset(T* ptr = 0) { delete[] _ptr; _ptr = ptr; }
	T & operator[](const unsigned int i) const { return _ptr[i]; }
	T * get() const { return _ptr; }
	void swap(auto_ptr_array_adapter & b) { T* temp = _ptr; _ptr = b._ptr; b._ptr = temp; }
	
	const T &operator*() const { return *_ptr; }
	
private:
	T* _ptr;
};

template<typename T> 
void swap(auto_ptr_array_adapter<T> & a, auto_ptr_array_adapter<T> & b) {
	a.swap(b);
}

#endif /* AUTO_PTR_ARRAY_ADAPTER_H */