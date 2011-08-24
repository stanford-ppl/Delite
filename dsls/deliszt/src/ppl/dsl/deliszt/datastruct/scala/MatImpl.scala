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
  def apply[R<:IntM:MVal, C<:IntM:MVal, T: Manifest] = {
    new MatImpl[R,C,T](MIntDepth[R],MIntDepth[C])
  }
}

class MatImpl[R<:IntM,C<:IntM, @specialized T: ClassManifest](val numRows : Int, val numCols : Int) extends Mat[R,C,T] {
  val data = new Array[T](numRows * numCols)

  def apply(r: Int, c: Int) = data(r*numRows+c)
  def update(r: Int, c: Int, v: T) = {
    data(r*numRows+c) = v
  }

  def size = numRows * numCols

  def row(n : Int) = new MatRowImpl(this, n)
  def col(n : Int) = new MatColImpl(this, n)

  def dcApply(idx: Int) = data(idx)
  def dcUpdate(idx: Int, x: T) = {
    data(idx) = x
  }
}

class MatRowImpl[C<:IntM, @specialized T: ClassManifest](mat: Mat[_,C,T], idx: Int) extends MatRow[C,T] {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: T) = mat.update(idx,n,v)
  val size = mat.numCols
}

class MatColImpl[R<:IntM, @specialized T: ClassManifest](mat: Mat[R,_,T], idx: Int) extends MatCol[R,T] {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: T) = mat.update(idx,n,v)
  val size = mat.numRows
}
