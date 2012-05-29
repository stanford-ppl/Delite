package ppl.dsl.optisdr.datastruct.scala

import ppl.dsl.optila.datastruct.scala.DenseVector

trait CyclicStream[T] extends Stream[T]

object CyclicStream {
  def apply[T:Manifest](x: DenseVector[T], offset: Int) = {
    new CyclicStreamImpl[T](x._data, offset)
  }
  
  def apply[T:Manifest](x: Array[T], offset: Int) = {
    new CyclicStreamImpl[T](x, offset)
  }
}

class CyclicStreamImpl[@specialized T: Manifest](var data: Array[T], val _offset: Int) extends CyclicStream[T] {
  def length = Int.MaxValue
  val offset = _offset % data.length

  def apply(n: Int) = data((n + offset) % data.length)
  def drop(n: Int) = new CyclicStreamImpl[T](data, offset + n)
  def grouped(n: Int) = new GroupedCyclicStreamImpl[T](data, n, offset)
  
  override def toString() = {
    "CyclicStream(" + data.map(_.toString).reduceLeft(_ + "," + _) + ")"
  }
}

class GroupedCyclicStreamImpl[@specialized T: Manifest](var data: Array[T], grouping: Int, val _offset: Int) extends CyclicStream[DenseVector[T]] {
  def length = Int.MaxValue
  val offset = _offset % data.length

  def apply(n: Int) = {
    val v = new DenseVector[T](grouping,false)

    val begin = offset + n*grouping
    val end = offset + (n+1)*grouping
    
    for(i <- begin until end) {
      v._data(i-begin) = data(i % data.length)
    }
    
    v
  }

  def drop(n: Int) = new GroupedCyclicStreamImpl[T](data, grouping, offset + n*grouping)
  
  override def toString() = {
    "CyclicStream(" + data.map(_.toString).reduceLeft(_ + "," + _) + ")"
  }
}