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

class IndexVectorDense(__length: Int, __isRow: Boolean) { 
  var _length = __length
  var _isRow = __isRow
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

class IndexVectorRange(__start: Int, __end: Int) { 
  var _start = __start
  var _end = __end
  var _stride = 1
  var _isRow = true
  
  def unsafeSetData(xs: Array[Int], len: Int) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }
    
  def Clone = { 
    val len = _end - _start
    val v = new DenseVector[Int](len, _isRow)
    var i = 0
    while (i < len) {
      v._data(i) = _start + i
      i += 1
    }
    v
  }  
}
