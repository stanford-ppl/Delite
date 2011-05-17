/*
 * ForAll.h
 *
 *  Created on: Mar 9, 2009
 *      Author: zdevito
 */

#ifndef FORALL_H_
#define FORALL_H_

#ifndef _OPENMP

#define FORALL_SET(object,set) \
    { \
    typedef __typeof__(set) SetType; \
    typedef SetType::iterator IterType; \
    typedef IterType::value_type ValueType; \
    for(IterType __msh_it = (set).iter(); __msh_it.hasNext();) { \
        ValueType object = __msh_it.next();

#define FORALL_SET_TM(object,set) \
    { \
    typedef __typeof__(set) SetType; \
    typedef typename SetType::iterator IterType; \
    typedef typename IterType::value_type ValueType; \
    for(IterType __msh_it = (set).iter(); __msh_it.hasNext();) { \
        ValueType object = __msh_it.next();

#define FORALL_SET_PAR(object,set) FORALL_SET(object,set)
#define FORALL_SET_SER(object,set) FORALL_SET(object,set)
#define FORALL_SET_SER_TM(object,set) FORALL_SET_TM(object,set)
#define FORALL_SET_NESTED(object,set) FORALL_SET(object,set)
#define FORALL_SET_NESTED_TM(object,set) FORALL_SET_TM(object,set)


#else

#define FORALL_SET(object,set) \
    { \
        typedef __typeof__(set) SetType; \
        typedef SetType::iterator IterType; \
        typedef IterType::value_type ValueType; \
        IterType __msh_it = set.iter(); \
        int __size = set.size(); \
        _Pragma("omp for schedule(static) nowait") \
        for(int __i = 0; __i < __size; ++__i) { \
            ValueType object = __msh_it[__i];

#define FORALL_SET_TM(object,set) \
    { \
        typedef __typeof__(set) SetType; \
        typedef typename SetType::iterator IterType; \
        typedef typename IterType::value_type ValueType; \
        IterType __msh_it = set.iter(); \
        int __size = set.size(); \
        _Pragma("omp for schedule(static) nowait") \
        for(int __i = 0; __i < __size; ++__i) { \
            ValueType object = __msh_it[__i];

#define FORALL_SET_PAR(object,set) FORALL_SET(object,set)

#define FORALL_SET_SER(object,set) \
    { \
    typedef __typeof__(set) SetType; \
    typedef SetType::iterator IterType; \
    typedef IterType::value_type ValueType; \
    for(IterType __msh_it = (set).iter(); __msh_it.hasNext();) { \
        ValueType object = __msh_it.next();

#define FORALL_SET_SER_TM(object,set) \
    { \
    typedef __typeof__(set) SetType; \
    typedef typename SetType::iterator IterType; \
    typedef typename IterType::value_type ValueType; \
    for(IterType __msh_it = (set).iter(); __msh_it.hasNext();) { \
        ValueType object = __msh_it.next();

#define FORALL_SET_NESTED(object,set) FORALL_SET_SER(object,set)
#define FORALL_SET_NESTED_TM(object,set) FORALL_SET_SER_TM(object,set)

#endif

// Syrah foralls
#define FORALL_SET_SYRAH_BEGIN(object, setObj, vecwidth) \
  { \
    typedef __typeof__(setObj) SetType; \
    typedef SetType::iterator IterType; \
    typedef IterType::value_type ValueType; \
    IterType __set_it = setObj.iter(); \
    int __size = setObj.size(); \
    SIMDObject<(vecwidth), ValueType> object; \
    int __simd_end = __size & (~((vecwidth)-1));

#define FORALL_SET_SYRAH_END }

#define FORALL_SET_SYRAH_BOUNDARY_ITER_NOMASK(object, vecwidth) \
  { \
    for (int __i = 0; __i < __simd_end; __i += (vecwidth)) {            \
      for (int __c = 0; __c < (vecwidth); __c++) { object[__c] = __set_it.next(); }

#define FORALL_SET_SYRAH_BOUNDARY_ITER_CLEANUP(object, vecwidth, loopmask) \
  { \
  if (__simd_end != __size) { \
    int __i = __simd_end; \
    int __loop_end = __size - __i; \
    for (int __c = 0; __c < __loop_end; __c++) { object[__c] = __set_it.next(); } \
    for (int __c = __loop_end; __c < (vecwidth); __c++) { object[__c] = object[__loop_end-1]; } \
    FixedVectorMask<(vecwidth)> loopmask = FixedVectorMask<(vecwidth)>::FirstN(__loop_end);


#define FORALL_SET_SYRAH_NOMASK(object, vecwidth) \
  { \
    for (int __i = 0; __i < __simd_end; __i += (vecwidth)) { \
      for (int __c = 0; __c < (vecwidth); __c++) { object[__c] = __set_it[__i + __c]; } \

#define FORALL_SET_SYRAH_CLEANUP(object, vecwidth, loopmask) \
  { \
     if (__simd_end != __size) { \
        int __i = __simd_end; \
        int __loop_end = (__size - __i); \
        for (int __c = 0; __c < __loop_end; __c++) { object[__c] = __set_it[__i + __c]; } \
        for (int __c = __loop_end; __c < (vecwidth); __c++) { object[__c] = object[__loop_end-1]; } \
        FixedVectorMask<(vecwidth)> loopmask = FixedVectorMask<(vecwidth)>::FirstN(__loop_end);

#define FORALL_SET_SYRAH_VECTORIZED(object, setObj, vecwidth, loopmask, inmask)    \
  { \
    typedef __typeof__(setObj[0]) SetType; \
    typedef SetType::iterator IterType; \
    typedef IterType::value_type ValueType; \
    SIMDObject<(vecwidth), IterType> __iters; \
    FixedVectorMask<(vecwidth)> loopmask = inmask; \
    SIMDObject<(vecwidth), ValueType> object; \
    for (int __i = 0; __i < (vecwidth); ++__i) { \
      __iters[__i] = setObj[__i].iter(); \
      object[__i] = __iters[__i].next(); \
    } \
    while (Any(loopmask)) {


#define ENDSET_SYRAH_NESTED(object, vecwidth, loopmask) \
      for (int __i = 0; __i < (vecwidth); ++__i) { \
        loopmask.set(__i, __iters[__i].hasNext()); \
        if (loopmask.get(__i)) { \
          object[__i] = __iters[__i].next();     \
        } \
      } \
      } }



#define ENDSET } }

#endif /* FORALL_H_ */
