package ppl.dsl.optisdr.datastruct.scala

import ppl.dsl.optila.datastruct.scala.DenseVector

object FakeStreamVector {
  def ofLength[T:Manifest](length: Int) = {
    new FakeStreamVectorImpl[T](length)
  }
  
  def withData[T:Manifest](data : Array[T]) = {
    new FakeStreamVectorImpl[T](data)
  }
  
  def apply[T:Manifest](xs: T*) = {
    new FakeStreamVectorImpl[T](xs.toArray)
  }
}

class FakeStreamVectorImpl[@specialized T: Manifest](var _data : Array[T]) extends Stream[T] {
  def this(length : Int) = this(new Array[T](length))
  
  def _length = _data.length

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) {
    _data = xs
  }
  
  def Clone = new FakeStreamVectorImpl[T](_data)
  
  override def toString() = {
    "FakeStreamVector(" + _data.map(_.toString).reduceLeft(_ + "," + _) + ")"
  }
}
