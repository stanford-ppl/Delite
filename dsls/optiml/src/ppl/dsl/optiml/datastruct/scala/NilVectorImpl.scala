package ppl.dsl.optiml.datastruct.scala

/**
 * This unfortunateness is due to the fact that Vectors are mutable, so they cannot be covariant.
 */

object NilVectorIntImpl extends NilVector[Int] {
  override def apply(i: Int) = 0
}
object NilVectorDoubleImpl extends NilVector[Double] {
  override def apply(i: Int) = 0.0
}
