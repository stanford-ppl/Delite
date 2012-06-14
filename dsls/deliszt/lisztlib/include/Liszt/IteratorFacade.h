#ifndef _LISZT_ITERATOR_FACADE_H
#define _LISZT_ITERATOR_FACADE_H

using namespace std;

template<typename SelfType, typename RefType>
class liszt_iterator_facade {
public:
	SelfType & operator++(int) {
		me()->increment();
		return *me(); 
	}
	SelfType & operator++() {
		me()->increment();
		return *me(); 
	}
	SelfType & operator--(int) {
		me()->decrement();
		return *me(); 
	}
	SelfType & operator--() {
		me()->decrement();
		return *me(); 
	}
	SelfType & operator+=(ptrdiff_t t) {
		me()->advance(t);
		return *me();
	}
	SelfType & operator-=(ptrdiff_t t) {
		me()->advance(-t);
		return *me();
	}
	RefType operator*() const {
		return me()->dereference();
	}
	const SelfType * me() const {
		return static_cast<const SelfType*>(this);
	}
	SelfType * me() {
		return static_cast<SelfType*>(this);
	}
	template<typename T>
	struct ReallyCpp {
		T t;
		T * operator->() { return &t; }
	};
	
	ReallyCpp<RefType> operator->() const {
		ReallyCpp<RefType> seriously = { me()->dereference() };
		return seriously;
	}

};

template<typename SelfType, typename RefType>
bool operator==(const liszt_iterator_facade<SelfType,RefType> & lhs, 
                const liszt_iterator_facade<SelfType,RefType> & rhs) {
	return lhs.me()->equal(*rhs.me());
}

template<typename SelfType, typename RefType>
bool operator!=(const liszt_iterator_facade<SelfType,RefType> & lhs, 
                const liszt_iterator_facade<SelfType,RefType> & rhs) {
	return !(lhs == rhs);
}

#endif