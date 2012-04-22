#ifndef _LISZT_STATIC_ASSERT_H
#define _LISZT_STATIC_ASSERT_H

#ifndef static_assert

template<bool stmt>
struct static_assert_t;
template<>
struct static_assert_t<true> {};

#define static_assert(x) static_assert_t<(x)>()

#endif

#endif