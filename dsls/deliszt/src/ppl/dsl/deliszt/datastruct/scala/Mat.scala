package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Mat[R<:IntM,C<:IntM,@specialized(Boolean, Int, Long, Float, Double)VT] extends DeliteCollection[VT] with MetaInteger {
  def apply[RR<:IntM:MVal, CC<:IntM:MVal](r: RR, c: CC) : VT = apply(MIntDepth[RR], MIntDepth[CC])
  def update[RR<:IntM:MVal, CC<:IntM:MVal](r: RR, c: CC, v : VT) : Unit = update(MIntDepth[RR], MIntDepth[CC], v)

  def apply(n: Int, m: Int) : VT
  def update(n: Int, m: Int, v : VT) : Unit

  def row(n: Int) : MatRow[C,VT]
  def col(n: Int) : MatCol[R,VT]

  def numRows : Int
  def numCols : Int

    // DeliteCollection
  def dcApply(idx: Int): VT
  def dcUpdate(idx: Int, x: VT): Unit
}

trait VecView[N<:IntM,@specialized(Boolean, Int, Long, Float, Double) VT] extends Vec[N,VT] with MetaInteger

trait MatRow[C<:IntM,@specialized(Boolean, Int, Long, Float, Double) VT] extends VecView[C,VT] with MetaInteger

trait MatCol[R<:IntM,@specialized(Boolean, Int, Long, Float, Double) VT] extends VecView[R,VT] with MetaInteger