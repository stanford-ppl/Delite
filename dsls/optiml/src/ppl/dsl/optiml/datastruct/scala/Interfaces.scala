package ppl.dsl.optiml.datastruct.scala

/**
 * Delite
 */

trait DeliteOpMapReduce[A,R]
trait DeliteCollection[T]

/**
 * Vector
 */

trait Vector[T] extends ppl.delite.framework.DeliteCollection[T] {
  // fields required on real underlying data structure impl
  def length : Int
  def is_row : Boolean
  def apply(n: Int) : T
  def update[A <: T](index: Int, x: A)

  // DeliteCollection
  def size = length
}

trait NilVector[T] extends Vector[T]

trait VectorView[T] extends Vector[T]

/**
 * Matrix
 */
trait Matrix[T]


/**
 * Ref
 */

case class Ref[T](v: T) {
  var _v = v

  def get = _v
  def set(v: T) = _v = v
}
