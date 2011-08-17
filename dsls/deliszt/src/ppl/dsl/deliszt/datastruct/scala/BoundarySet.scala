package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 05/17/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait BoundarySet[MO<:MeshObj] extends MeshSet[MO] {
  def apply(i : Int) : MO
  def size : Int
}
