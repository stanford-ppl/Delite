#ifndef _TYPE_TRAITS_H_
#define _TYPE_TRAITS_H_

namespace Liszt {

struct true_type {
	static const bool value = true;
	typedef true_type type;
};
struct false_type {
	static const bool value = false;
	typedef false_type type;
};

template<typename T>
struct is_arithmetic {
	typedef false_type type;
};

//here we define the actual arithmetic types which has a meta-true is_arithmetic value
#define MAKE_ARITHMETIC_TYPE(t) template<> struct is_arithmetic<t> { typedef true_type type; };
MAKE_ARITHMETIC_TYPE(int)
MAKE_ARITHMETIC_TYPE(float)
MAKE_ARITHMETIC_TYPE(double)
#undef MAKE_ARITHMETIC_TYPE

template<size_t N, typename T>
struct is_arithmetic< vec<N,T> > : public is_arithmetic<T> {};

template<size_t R, size_t C, typename T>
struct is_arithmetic< matrix<R,C,T> > : public is_arithmetic<T> {};

template<typename T>
struct is_boolean : public false_type {};

template<>
struct is_boolean<bool> : public true_type {};

template<typename T>
struct is_integral {
	typedef false_type type;
};

//here we define the actual integral types which has a meta-true is_integral value
#define MAKE_INTEGRAL_TYPE(t) template<> struct is_integral<t> { typedef true_type type; };
MAKE_INTEGRAL_TYPE(int)
#undef MAKE_INTEGRAL_TYPE

} /* namespace Liszt */

#endif /* _TYPE_TRAITS_H_ */