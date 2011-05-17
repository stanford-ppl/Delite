package ppl.dsl.deliszt.datastruct.scala

import MetaInteger._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/30/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object MatImpl {
  def apply[R<:IntM, C<:IntM, VT: Manifest] = {
    new MatImpl[R,C,VT](MIntDepth[R],MIntDepth[C])
  }
}

class MatImpl[R <: IntM, C <: IntM, VT](val rows : Int, val cols : Int) extends Mat[R,C,VT] {
  val data = new Array[VT](rows * cols)

  def apply[RR<:IntM, CC<:IntM](r: RR, c: CC)(implicit f: EnsureSize[RR,R], ff : EnsureSize[CC,C]) = apply(MIntDepth[RR], MIntDepth[CC])
  def update[RR<:IntM, CC<:IntM](r: RR, c: CC, v : VT)(implicit f: EnsureSize[RR,R], f2 : EnsureSize[CC,C]) = update(MIntDepth[RR], MIntDepth[CC], v)

  def apply(r: Int, c: Int) = data(r*rows+c)
  def update(r: Int, c: Int, v: VT) = {
    data(r*rows+c) = v
  }

  def size = rows * cols

  def row(n : Int) = new MatRowImpl[C](this, n)
  def col(n : Int) = new MatColImpl[C](this, n)

  def dcApply(idx: Int) = data(idx)
  def dcUpdate(idx: Int, x: VT) = {
    data(idx) = x
  }
}

class MatRowImpl[R <: Int, C <: IntM, VT](mat: Mat[R,C,VT], idx: Int) extends MatRow[C,VT] {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: VT) = mat.update(idx,n,v)
  val size = MIntDepth[C]
}

class MatColImpl[R <: Int, C <: IntM, VT](mat: Mat[R,C,VT], idx: Int) extends MatCol[R,VT] {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: VT) = mat.update(idx,n,v)
  val size = MIntDepth[R]
}