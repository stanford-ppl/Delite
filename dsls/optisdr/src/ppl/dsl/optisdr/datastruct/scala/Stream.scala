package ppl.dsl.optisdr.datastruct.scala

import ppl.dsl.optila.datastruct.scala.DenseVector

trait Stream[T] {
  def length: Int
  
  def apply(n: Int): T
  
  def drop(n: Int): Stream[T]
  def grouped(n: Int)(implicit m: Manifest[T]): Stream[DenseVector[T]] = GroupedStream[T](this,n)
  def slice(begin: Int, end: Int)(implicit m: Manifest[T]): DenseVector[T] = {
    val v = new DenseVector[T](end-begin-1,false)
  
    for(i <- begin until end) {
      v._data(i-begin) = this(i)
    }
    
    v
  }
}