package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Field[MO <: MeshObj, VT] extends ppl.delite.framework.datastruct.scala.DeliteCollection[VT] {
  def apply(a : MO) : VT
  def update(a : MO, v : VT)

  def size : Int
  def dcApply(idx: Int) : VT
  def dcUpdate(idx: Int, x: VT) : Unit
}