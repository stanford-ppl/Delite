package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait DeLisztSet[MO <: MeshObj] extends ppl.delite.framework.DeliteCollection[MO] {
  def apply(i : Int) : MO
  def size : Int

  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: MO) = {} // Read only, bitches
}