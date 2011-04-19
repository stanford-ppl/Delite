package generated.scala

class VectorImpl[@specialized(Int, Double) T:Manifest](val length: Int) extends Vector[T] {

  val data = new Array[T](length)

  def apply(idx: Int) = data(idx)
  def update(idx: Int, value: T) { data(idx) = value }

}
