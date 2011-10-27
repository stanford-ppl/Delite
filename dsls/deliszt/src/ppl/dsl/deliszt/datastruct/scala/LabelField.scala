package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 05/12/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait LabelField[T] extends Field[T] { 
  def update(i: Int, x: T) = throw new RuntimeException()
}
