package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 05/12/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait LabelField[MO <: MeshObj, VT] extends Field[MO,VT] {
  def apply(e: MO) : VT
  def update(e: MO, v: VT) = throw new RuntimeException()

  def size : Int
  def dcApply(idx: Int)
  def dcUpdate(idx: Int, x: MO) = throw new RuntimeException()
}