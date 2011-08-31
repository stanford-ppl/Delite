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

class MatImpl[R<:IntM,C<:IntM, @specialized T: ClassManifest](val numRows : Int, val numCols : Int, val data : Array[T]) extends Mat[R,C,T] with Copyable {
  def this(numRows : Int, numCols : Int) = this(numRows, numCols, new Array[T](numRows * numCols))

  def apply(r: Int, c: Int) = data(r*numRows+c)
  def update(r: Int, c: Int, v: T) = {
    data(r*numRows+c) = v
  }

  def size = numRows * numCols
  
  def apply(idx: Int) = dcApply(idx)
  def update(idx: Int, v: T) = dcUpdate(idx, v)

  def row(n : Int) = new MatRowImpl(this, n)
  def col(n : Int) = new MatColImpl(this, n)

  def dcApply(idx: Int) = data(idx)
  def dcUpdate(idx: Int, x: T) = {
    data(idx) = x
  }
  
  def cloneL = {
    new MatImpl[R,C,T](numRows, numCols, data.clone)
  }
  
  def copy() = {
    val m = new MatImpl[R,C,T](numRows, numCols)
  
    if(classManifest[T] <:< classManifest[Copyable]) {
      for(i <- 0 until size) {
        m(i) = data(i).asInstanceOf[Copyable].copy.asInstanceOf[T]
      }
    }
    else {
      for(i <- 0 until size) {
        m(i) = data(i)
      }
    }
    
    m
  }
  
  override def toString() = {
    var s = "Mat[" + numRows + "," + numCols + "]"
    
    for(i <- 0 until numRows) {
      s += "(" + (0 until numCols).map( this(i,_).toString ).reduceLeft(_ + "," + _) + ")"
    }
    
    s
  }
}

class MatRowImpl[C<:IntM, @specialized T: ClassManifest](mat: Mat[_,C,T], idx: Int) extends MatRow[C,T] with Copyable {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: T) = mat.update(idx,n,v)
  val size = mat.numCols
  
    
  def cloneL = {
    val v = new VecImpl[C,T](size)
  
    for(i <- 0 until size) {
      v(i) = this(i)
    }
    
    v
  }
  
  def copy = cloneL
  
  override def toString() = {
    "MatRow " + idx + "[" + size + "](" + (0 until size).map(this(_).toString).reduceLeft(_ + "," + _) + ")"
  }
}

class MatColImpl[R<:IntM, @specialized T: ClassManifest](mat: Mat[R,_,T], idx: Int) extends MatCol[R,T] with Copyable {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: T) = mat.update(idx,n,v)
  val size = mat.numRows
  
  def cloneL = {
    val v = new VecImpl[R,T](size)
  
    for(i <- 0 until size) {
      v(i) = this(i)
    }
    
    v
  }
  
  def copy = cloneL
  
  override def toString() = {
    "MatCol " + idx + "[" + size + "](" + (0 until size).map(this(_).toString).reduceLeft(_ + "," + _) + ")"
  }
}
