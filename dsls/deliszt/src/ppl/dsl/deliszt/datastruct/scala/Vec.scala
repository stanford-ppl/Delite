package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Vec[N <: IntM, VT] extends DeliteCollection[VT] {
  def apply[TT <: IntM](n : TT)(implicit f : EnsureSize[TT,N]) : VT
  def update[TT <: IntM](n : TT, v : VT)(implicit f : EnsureSize[TT,N]) : Unit

  def apply(n : Int) : VT
  def update(n : Int, v : VT) : Unit

  def size = null
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: VT) = update(idx, x)
}
