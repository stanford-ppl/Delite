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
  def apply[N<:IntM:MVal, T:Manifest]() = {
    new VecImpl[N,T](MIntDepth[N])
  }
}

class VecImpl[N<:IntM:MVal, @specialized T: ClassManifest](val size : Int) extends Vec[N, T] {
  val data = new Array[T](size)

  def apply[TT<:IntM](n : TT)(implicit mv: MVal[TT], f : EnsureSize[TT,N]) : T = apply(MIntDepth[TT])
  def update[TT<:IntM](n : TT, v : T)(implicit mv: MVal[TT], f : EnsureSize[TT,N]) : Unit = update(MIntDepth[TT], v)

  def apply(n : Int) = data(n)
  def update(n : Int, v : T) = {
    data(n) = v
  }
}
