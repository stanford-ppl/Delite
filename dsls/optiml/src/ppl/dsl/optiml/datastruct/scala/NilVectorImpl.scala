package ppl.dsl.optiml.datastruct.scala

/**
 * This unfortunateness is due to the fact that Vectors are mutable, so they cannot be covariant.
 */
object NilVectorIntImpl extends NilVector[Int] {
  def length : Int = 0
  def apply(i: Int) = throw new UnsupportedOperationException()
  def isRow : Boolean = throw new UnsupportedOperationException()
  def update(index: Int, x: Int) = throw new UnsupportedOperationException()
}

object NilVectorDoubleImpl extends NilVector[Double] {
  def length : Int = 0
  def apply(i: Int) = throw new UnsupportedOperationException()
  def isRow : Boolean = throw new UnsupportedOperationException()
  def update(index: Int, x: Double) = throw new UnsupportedOperationException()
}
