package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/30/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object MatImpl extends MetaInteger {
  def apply[R<:IntM:MVal, C<:IntM:MVal, VT: Manifest] = {
    new MatImpl[R,C,VT](MIntDepth[R],MIntDepth[C])
  }
}

class MatImpl[R<:IntM,C<:IntM,VT:Manifest](val numRows : Int, val numCols : Int) extends Mat[R,C,VT] {
  val data = new Array[VT](numRows * numCols)

  def apply(r: Int, c: Int) = data(r*numRows+c)
  def update(r: Int, c: Int, v: VT) = {
    data(r*numRows+c) = v
  }

  def size = numRows * numCols

  def row(n : Int) = new MatRowImpl(this, n)
  def col(n : Int) = new MatColImpl(this, n)

  def dcApply(idx: Int) = data(idx)
  def dcUpdate(idx: Int, x: VT) = {
    data(idx) = x
  }
}

class MatRowImpl[C<:IntM, VT](mat: Mat[_,C,VT], idx: Int) extends MatRow[C,VT] {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: VT) = mat.update(idx,n,v)
  val size = mat.numCols
}

class MatColImpl[R<:IntM,VT](mat: Mat[R,_,VT], idx: Int) extends MatCol[R,VT] {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: VT) = mat.update(idx,n,v)
  val size = mat.numRows
}