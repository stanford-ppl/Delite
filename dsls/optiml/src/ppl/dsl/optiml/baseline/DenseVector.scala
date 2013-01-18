package ppl.dsl.optiml.baseline

object DenseVector {
  def apply[A:Numeric:Manifest](length: Int) = new DenseVector[A](length)
}

class DenseVector[@specialized T:Numeric:Manifest](__length: Int) extends Vector[T] {
  var _length = __length    
  var _data: Array[T] = new Array[T](_length)

  def length = _length
  def newVector[B:Numeric:Manifest](len: Int): Vector[B] = DenseVector[B](len)  
  
  def apply(i: Int): T = _data(i)
  def update(i: Int, x: T): Unit = {
    _data(i) = x
  }
}

  
  