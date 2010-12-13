package ppl.dsl.optiml.datastruct.scala

/**
 * This unfortunateness is due to the fact that Vectors are mutable, so they cannot be covariant.
 */
object NilVectorIntImpl extends NilVector[Int] {
  def apply(i: Int) = throw new UnsupportedOperationException()
  def length : Int = throw new UnsupportedOperationException()
  def is_row : Boolean = throw new UnsupportedOperationException()
  def update(index: Int, x: Int) = throw new UnsupportedOperationException()
}

object NilVectorDoubleImpl extends NilVector[Double] {
  def apply(i: Int) = throw new UnsupportedOperationException()
  def length : Int = throw new UnsupportedOperationException()
  def is_row : Boolean = throw new UnsupportedOperationException()
  def update(index: Int, x: Double) = throw new UnsupportedOperationException()
}
