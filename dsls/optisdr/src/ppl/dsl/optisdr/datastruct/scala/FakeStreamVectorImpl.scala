import ppl.dsl.optila.datastruct.scala.DenseVector

object FakeStreamVectorImpl {
  def ofSize[T:Manifest](size: Int) = {
    new FakeStreamVectorImpl[T](size)
  }
  
  def withData[T:Manifest](data : Array[T]) = {
    new FakeStreamVectorImpl[T](data)
  }
  
  def apply[T:Manifest](xs: T*) = {
    new FakeStreamVectorImpl[T](xs.toArray)
  }
}

class FakeStreamVectorImpl[@specialized T: Manifest](var _data : Array[T]) extends DenseVector[T](_data.length, false) {
  def this(size : Int) = this(new Array[T](size))
  
  def Clone = new FakeStreamVectorImpl[T](_data)
  
  override def toString() = {
    "FakeStreamVector(" + _data.map(_.toString).reduceLeft(_ + "," + _) + ")"
  }
}
