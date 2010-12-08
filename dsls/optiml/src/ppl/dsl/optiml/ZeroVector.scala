package ppl.dsl.optiml

object IntZeroVector extends Vector[Int] {
  def apply(i: Int) = 0
  def length : Int = throw new UnsupportedOperationException()
  def is_row : Boolean = throw new UnsupportedOperationException()
  def update(index: Int, x: Int) = throw new UnsupportedOperationException()  
}

object DoubleZeroVector extends Vector[Double] {
  def apply(i: Int) = 0
  def length : Int = throw new UnsupportedOperationException()
  def is_row : Boolean = throw new UnsupportedOperationException()
  def update(index: Int, x: Double) = throw new UnsupportedOperationException()  
}