package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object VecImpl extends MetaInteger {
  def apply[N<:IntM:MVal, VT:Manifest]() = {
    new VecImpl[N,VT](MIntDepth[N])
  }
}

class VecImpl[N<:IntM:MVal, VT : Manifest](val size : Int) extends Vec[N, VT] {
  val data = new Array[VT](size)

  def apply[TT<:IntM](n : TT)(implicit mv: MVal[TT], f : EnsureSize[TT,N]) = apply(MIntDepth[TT])
  def update[TT<:IntM](n : TT, v : VT)(implicit mv: MVal[TT], f : EnsureSize[TT,N]) = update(MIntDepth[TT], v)

  def apply(n : Int) = data(n)
  def update(n : Int, v : VT) = {
    data(n) = v
  }
}