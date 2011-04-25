package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Field[A <: MeshObj, VT] extends ppl.delite.framework.DeliteCollection[A] {
  def apply(a : A) : VT
  def update(a : A, v : VT)

  def size : Int
  def dcApply(idx: Int)
  def dcUpdate(idx: Int, x: A) = {} // Not writing MeshObjects here...
}