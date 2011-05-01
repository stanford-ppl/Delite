package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class VecImpl[N <: IntM, VT] extends Vec[N, VT] {
  val data = new Array[VT]()

  def apply[TT <: IntM](n : TT)(implicit f : EnsureSize[TT,N]) : VT
  def update[TT <: IntM](n : TT, v : VT)(implicit f : EnsureSize[TT,N]) : Unit

  def apply(n : Int) = data(n)
  def update(n : Int, v : VT) = {
    data(n) = v
  }
}