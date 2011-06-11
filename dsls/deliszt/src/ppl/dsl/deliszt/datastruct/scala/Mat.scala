package ppl.dsl.deliszt.datastruct.scala

import MetaInteger._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Mat[R<:IntM, C<:IntM, @specialized(Boolean, Int, Long, Float, Double)VT] extends DeliteCollection[VT] {
  def apply[RR<:IntM, CC<:IntM](r: RR, c: CC)(implicit f: EnsureSize[RR,R], ff : EnsureSize[CC,C]) : VT
  def update[RR<:IntM, CC<:IntM](r: RR, c: CC, v : VT)(implicit f: EnsureSize[RR,R], f2 : EnsureSize[CC,C]) : Unit

  def apply(n: Int, m: Int) : VT
  def update(n: Int, m: Int, v : VT) : Unit

  def row(n: Int) : MatRow[C,VT]
  def col(n: Int) : MatCol[R,VT]

  def rows : Int
  def cols : Int

    // DeliteCollection
  def dcApply(idx: Int): VT
  def dcUpdate(idx: Int, x: VT): Unit
}

trait VecView[N<:IntM, @specialized(Boolean, Int, Long, Float, Double) VT] extends Vec[N,VT]

trait MatRow[C<:IntM, @specialized(Boolean, Int, Long, Float, Double) VT] extends VecView[C,VT]

trait MatCol[R<:IntM, @specialized(Boolean, Int, Long, Float, Double) VT] extends VecView[R,VT]