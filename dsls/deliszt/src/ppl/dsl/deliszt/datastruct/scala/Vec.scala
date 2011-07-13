package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Vec[N<:IntM,VT] extends DeliteCollection[VT] with MetaInteger {
  def apply[TT<:IntM:MVal](n:TT) : VT = apply(MIntDepth[TT])
  def update[TT<:IntM:MVal](n:TT, v:VT) : Unit = update(MIntDepth[TT], v)

  def apply(n:Int) : VT
  def update(n:Int, v : VT) : Unit

  def size : Int
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: VT) = update(idx, x)
}
