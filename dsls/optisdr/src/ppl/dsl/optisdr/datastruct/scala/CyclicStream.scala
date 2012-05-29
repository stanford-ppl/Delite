package ppl.dsl.optisdr.datastruct.scala

import ppl.dsl.optila.datastruct.scala.DenseVector

object CyclicStream {
  def apply[T:Manifest](x: DenseVector[T], offset: Int) = {
    new CyclicStream[T](x._data, offset)
  }
  
  def apply[T:Manifest](x: Array[T], offset: Int) = {
    new CyclicStream[T](x, offset)
  }
}

class CyclicStream[@specialized T: Manifest](var _data : Array[T], val offset: Int) extends Stream[T] {
  def _length = Int.MaxValue

  def apply(n: Int) = _data((n + offset) % _data.length)
  
  override def toString() = {
    "CyclicStream(" + _data.map(_.toString).reduceLeft(_ + "," + _) + ")"
  }
}
