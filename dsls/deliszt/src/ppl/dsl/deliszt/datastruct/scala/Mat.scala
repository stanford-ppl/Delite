package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Mat[R<:IntM,C<:IntM,@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] with MetaInteger {
  def apply[RR<:IntM:MVal, CC<:IntM:MVal](r: RR, c: CC) : T = apply(MIntDepth[RR], MIntDepth[CC])
  def update[RR<:IntM:MVal, CC<:IntM:MVal](r: RR, c: CC, v : T) : Unit = update(MIntDepth[RR], MIntDepth[CC], v)

  def apply(n: Int, m: Int) : T
  def update(n: Int, m: Int, v : T) : Unit
  def size : Int

  def row(n: Int) : MatRow[C,T]
  def col(n: Int) : MatCol[R,T]

  def numRows : Int
  def numCols : Int

    // DeliteCollection
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T): Unit
  def dcSize : Int = size
}

trait VecView[N<:IntM,@specialized(Boolean, Int, Long, Float, Double) T] extends Vec[N,T] with MetaInteger

trait MatRow[C<:IntM,@specialized(Boolean, Int, Long, Float, Double) T] extends VecView[C,T] with MetaInteger

trait MatCol[R<:IntM,@specialized(Boolean, Int, Long, Float, Double) T] extends VecView[R,T] with MetaInteger
