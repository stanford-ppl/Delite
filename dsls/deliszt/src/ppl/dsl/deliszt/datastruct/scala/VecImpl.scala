package ppl.dsl.deliszt.datastruct.scala

import MetaInteger._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object VecImpl {
  def apply[N <: IntM, VT : Manifest] = {
    new VecImpl[N,VT](MIntDepth[N])
  }
}

class VecImpl[N <: IntM, VT : Manifest](val size : Int) extends Vec[N, VT] {
  val data = new Array[VT](size)

  def apply[TT <: IntM](n : TT)(implicit f : EnsureSize[TT,N]) = apply(MIntDepth[TT])
  def update[TT <: IntM](n : TT, v : VT)(implicit f : EnsureSize[TT,N]) = update(MIntDepth[TT], v)

  def apply(n : Int) = data(n)
  def update(n : Int, v : VT) = {
    data(n) = v
  }
}