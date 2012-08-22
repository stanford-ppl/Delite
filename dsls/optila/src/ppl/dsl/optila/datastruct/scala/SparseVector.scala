package ppl.dsl.optila.datastruct.scala

/**
 * This is the actual class that gets instantiated in the generated code. Ops corresponding to public operations
 * here must have CodeGen methods defined by the DSL on them.
 *
 * Alternatively, everything in this class could be lifted, and we could generate a concrete class to be instantiated
 * in the generated code.
 */

class SparseVector[@specialized T: Manifest](__length: Int, __isRow: Boolean) { 
  var _length = __length
  var _isRow = __isRow
  var _data = new Array[T](32)
  var _indices = new Array[Int](32)
  var _nnz = 0

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException("unsafeSetData in SparseVector not supported")  
  
  def Clone = { 
    val v = new SparseVector[T](_length, _isRow);
    v._data = _data.clone
    v._indices = _indices.clone
    v._nnz = _nnz
    v
  }  
}
