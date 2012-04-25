package ppl.dsl.deliszt.datastruct.scala

object Mat3x3Impl {
  def apply[T: Manifest](v0: Vec[T], v1: Vec[T], v2: Vec[T]) = {
    new Mat3x3Impl[T](v0(0), v0(1), v0(2), v1(0), v1(1), v1(2), v2(0), v2(1), v2(2))
  }
}

class Mat3x3Impl[@specialized T: ClassManifest](var v00:T, var v01:T, var v02:T, var v10:T, var v11:T, var v12:T, 
    var v20:T, var v21:T, var v22:T) extends Mat[T] with Copyable {
   
  def apply(r: Int, c: Int) = dcApply(r*3 + c)
  def update(r: Int, c: Int, v: T) = dcUpdate(r*3 + c, v)

  override val size = 9
  def numRows = 3
  def numCols = 3
  
  def apply(idx: Int) = dcApply(idx)
  def update(idx: Int, v: T) = dcUpdate(idx, v)

  def row(n : Int) = new Mat3x3RowImpl[T](this, n)
  def col(n : Int) = new Mat3x3ColImpl[T](this, n)

  def dcApply(idx: Int) = {
    /*unsafe.UnsafeAccessor.unsafe.getT(this, 16 + idx*UNSAFE_SIZE)*/ v00
  }
  
  def dcUpdate(idx: Int, x: T) = {
    /*unsafe.UnsafeAccessor.unsafe.putT(this, 16 + idx*UNSAFE_SIZE, x)*/
  }
  
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) {
    if (len < 9) throw new RuntimeException("code gen error: unsafeSetData to mat3 with array length less than 9")
    
    // row-major
    v00 = xs(0)
    v01 = xs(1)
    v02 = xs(2)
    v10 = xs(3)
    v11 = xs(4)
    v12 = xs(5)
    v20 = xs(6)
    v21 = xs(7)
    v22 = xs(8)
  }
  
  def cloneL = {
    new Mat3x3Impl[T](v00, v01, v02, v10, v11, v12, v20, v21, v22)
  }
  
  def copy() = {
    new Mat3x3Impl[T](v00, v01, v02, v10, v11, v12, v20, v21, v22)
  }
  
  override def toString() = {
    var s = "Mat[" + numRows + "," + numCols + "]"
    s += "(" + v00 + "," + v01 + "," + v02 + ")"
    s += "(" + v10 + "," + v11 + "," + v12 + ")"
    s += "(" + v20 + "," + v21 + "," + v22 + ")"
    
    s
  }
}

class Mat3x3RowImpl[@specialized T: ClassManifest](mat: Mat3x3Impl[T], idx: Int) extends MatRow[T] with Copyable {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: T) = mat.update(idx,n,v)
  override val size = 3

  def cloneL = {
    new Vec3Impl[T](this(0), this(1), this(2))
  }
  def copy = cloneL

  override def toString() = {
    "MatRow " + idx + "[" + size + "](" + this(0) + "," + this(1) + "," + this(2) + ")"
  }
}

class Mat3x3ColImpl[@specialized T: ClassManifest](mat: Mat3x3Impl[T], idx: Int) extends MatCol[T] with Copyable {
  def apply(n: Int) = mat.apply(idx,n)
  def update(n: Int, v: T) = mat.update(idx,n,v)
  override val size = 3

  def cloneL = {
    new Vec3Impl[T](this(0), this(1), this(2))
  }
  def copy = cloneL

  override def toString() = {
    "MatCol " + idx + "[" + size + "](" + this(0) + "," + this(1) + "," + this(2) + ")"
  }
}
