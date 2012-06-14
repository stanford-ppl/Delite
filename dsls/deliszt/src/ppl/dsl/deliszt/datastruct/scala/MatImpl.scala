package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/30/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object MatImpl {
  def ofSize[T: Manifest](rows: Int, cols: Int) = {
    new MatImpl[T](rows, cols)
  }
  
  def apply[T: Manifest](xs: Vec[T]*) = {
    new MatImpl[T](xs.size, xs(0).size, xs.flatten.toArray)
  }
}
 
class MatImpl[@specialized T: ClassManifest](val numRows : Int, val numCols : Int, var data : Array[T]) extends Mat[T] with Copyable {
  def this(numRows : Int, numCols : Int) = this(numRows, numCols, new Array[T](numRows * numCols))

  def apply(r: Int, c: Int) = data(r*numRows+c)
  def update(r: Int, c: Int, v: T) = {
    data(r*numRows+c) = v
  }

  override val size = numRows * numCols
  
  def apply(idx: Int) = dcApply(idx)
  def update(idx: Int, v: T) = dcUpdate(idx, v)

  def row(n : Int) = new MatRowImpl[T](this, n)
  def col(n : Int) = new MatColImpl[T](this, n)

  def dcApply(idx: Int) = data(idx)
  def dcUpdate(idx: Int, x: T) = {
    data(idx) = x
  }
  
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) {
    data = xs
  }
  
  def cloneL = {
    new MatImpl[T](numRows, numCols, data.clone)
  }
  
  def copy() = {
    val m = new MatImpl[T](numRows, numCols)
  
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

class MatRowImpl[@specialized T: ClassManifest](mat: Mat[T], idx: Int) extends MatRow[T] with Copyable {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: T) = mat.update(idx,n,v)
  override val size = mat.numCols
    
  def cloneL = {
    val v = new VecImpl[T](size)
  
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

class MatColImpl[@specialized T: ClassManifest](mat: Mat[T], idx: Int) extends MatCol[T] with Copyable {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: T) = mat.update(idx,n,v)
  override val size = mat.numRows
  
  def cloneL = {
    val v = new VecImpl[T](size)
  
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
