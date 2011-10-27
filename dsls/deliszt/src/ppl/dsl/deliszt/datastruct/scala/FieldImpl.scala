package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class FieldImpl[@specialized(Boolean, Int, Long, Float, Double) T](val data : Array[T]) extends Field[T] {
  def apply(idx: Int) = data(Mesh.internal(idx))
  def update(idx: Int, x: T) = {
    data(Mesh.internal(idx)) = x
  }
  def size = data.length
}
