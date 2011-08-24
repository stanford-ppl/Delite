package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Field[MO <: MeshObj, VT] extends ppl.delite.framework.datastruct.scala.DeliteCollection[VT] {
  def apply(a : MO) : VT = apply(a.internalId)
  def update(a : MO, v : VT) : Unit = update(a.internalId, v)

  def apply(i : Int) : VT
  def update(i : Int, v : VT) : Unit

  def size : Int
  
  def dcApply(idx: Int) : VT = apply(idx)
  def dcUpdate(idx: Int, x: VT) : Unit = update(idx, x)
  def dcSize : Int = size
}
