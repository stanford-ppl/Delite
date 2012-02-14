package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

/* IndexVector is an IntVector whose elements represent indices (e.g., of another vector).
 * It is either backed by a discrete sequence of integers (e.g. 1,5,10) or a continouous RangeVector.
 *
 * IndexVectors can be used for scatter/gather operations.
 *
 * They also provide a vector construction operator { } that takes a function mapping an index to a value,
 * producing a new vector.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Dec 27, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class IndexVectorDense(__length: Int) { //extends DenseVector[Int](__length, true) with IndexVectorDense
  var _length = __length
  var _isRow = true
  var _data: Array[Int] = new Array[Int](_length)
  
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[Int], len: Int) {
    _data = xs
    _length = len
  }
  
  def Clone = { 
    val v = new DenseVector[Int](_length, _isRow);
    v._data = _data.clone
    v
  }    
}

class IndexVectorRange(__start: Int, __end: Int) { //extends RangeVector(_start, _end, 1, true) with IndexVectorRange
  var _start = __start
  var _end = __end
  var _stride = 1
  var _isRow = true
    
  // val length = (end-start + stride - 1) / stride
  // def isRow = _isRow
  // 
  // def apply(n: Int) : Int = {
  //   start + n*stride
  // }

  //def Clone = { val v = new DenseVector[Int](0, isRow); v.insertAll(0, this); v }

  // def unsafeSetData(xs: Array[Int], len: Int) {
  //   throw new IllegalArgumentException("RangeVector cannot be updated")
  // }
}
