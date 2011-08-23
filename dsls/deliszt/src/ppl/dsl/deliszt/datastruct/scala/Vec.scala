package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Vec[N<:IntM,@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] with MetaInteger {
  def apply[TT<:IntM:MVal](n:T) : T = apply(MIntDepth[TT])
  def update[TT<:IntM:MVal](n:TT, v:T) : Unit = update(MIntDepth[TT], v)

  def apply(n:Int) : T
  def update(n:Int, v : T) : Unit

  def size : Int
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)
  def dcSize : Int = size
}
