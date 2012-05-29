package ppl.dsl.optisdr.datastruct.scala

import ppl.dsl.optila.datastruct.scala.DenseVector

trait GroupedStream[T] extends Stream[DenseVector[T]]

object GroupedStream {
  def apply[T:Manifest](x: Stream[T], grouping: Int) = {
    new GroupedStreamImpl[T](x, grouping)
  }
}

class GroupedStreamImpl[@specialized T: Manifest](var stream: Stream[T], val grouping: Int) extends GroupedStream[T] {
  def length = if(stream.length != Int.MaxValue) {(stream.length - 1) / grouping + 1} else Int.MaxValue

  def apply(n: Int) = stream.slice(n*grouping, (n+1)*grouping)
  def drop(n: Int) = new GroupedStreamImpl[T](stream.drop(grouping), grouping)
  def grouped(n: Int): Stream[DenseVector[DenseVector[T]]] = new GroupedStreamImpl[DenseVector[T]](this, n)
  
  override def toString() = {
    "GroupedStream"
  }
}