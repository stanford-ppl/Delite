package ppl.dsl.optisdr.datastruct.scala

import ppl.dsl.optila.datastruct.scala.DenseVector

object FakeStreamVector {
  def ofLength[T:Manifest](length: Int) = {
    new FakeStreamVectorImpl[T](length)
  }
  
  def withData[T:Manifest](data: Array[T]) = {
    new FakeStreamVectorImpl[T](data)
  }
  
  def fromVector[T:Manifest](v: DenseVector[T]) = {
    new FakeStreamVectorImpl[T](v._data)
  }
  
  def apply[T:Manifest](xs: T*) = {
    new FakeStreamVectorImpl[T](xs.toArray)
  }
}

class FakeStreamVectorImpl[@specialized T: Manifest](var data: Array[T]) extends Stream[T] {
  def this(length : Int) = this(new Array[T](length))
  
  def length = data.length
  def apply(n: Int) = data(n)
  def update(n: Int, y: T) { data(n) = y }
  
  def drop(n: Int) = new FakeStreamVectorImpl[T](data.drop(n))

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) {
    data = xs
  }
  
  def Clone = new FakeStreamVectorImpl[T](data)
  
  override def toString() = {
    "FakeStreamVector(" + data.map(_.toString).reduceLeft(_ + "," + _) + ")"
  }
}
