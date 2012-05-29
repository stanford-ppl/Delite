package ppl.dsl.optila.datastruct.scala

/**
 * This is the actual class that gets instantiated in the generated code. Ops corresponding to public operations
 * here must have CodeGen methods defined by the DSL on them.
 *
 * Alternatively, everything in this class could be lifted, and we could generate a concrete class to be instantiated
 * in the generated code.
 */

class DenseVector[@specialized T: Manifest](var _data: Array[T], __isRow: Boolean) { 
  def this(__length: Int, __isRow: Boolean) = this(new Array[T](__length), __isRow)

  var _length = _data.length
  var _isRow = __isRow

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) {
    _data = xs
    _length = len
  }
  
  def Clone = { 
    val v = new DenseVector[T](_length, _isRow);
    v._data = _data.clone
    v
  }  
}
