package ppl.dsl.deliszt.datastruct.scala

import MetaInteger._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Vec[N <: IntM, VT] extends DeliteCollection[VT] {
  def apply[TT <: IntM](n : TT)(implicit f : EnsureSize[TT,N]) = apply(MIntDepth[TT])
  def update[TT <: IntM](n : TT, v : VT)(implicit f : EnsureSize[TT,N]) = update(MIntDepth[TT], v)

  def apply(n : Int) : VT
  def update(n : Int, v : VT) : Unit

  def size : Int
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: VT) = update(idx, x)
}
