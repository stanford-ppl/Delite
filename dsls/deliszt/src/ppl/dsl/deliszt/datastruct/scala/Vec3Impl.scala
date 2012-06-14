package ppl.dsl.deliszt.datastruct.scala

object Vec3Impl {
  def apply[T:ClassManifest](v0: T, v1: T, v2: T) = {
    new Vec3Impl[T](v0, v1, v2)
  }
}

class Vec3Impl[@specialized T: ClassManifest](var v0: T, var v1: T, var v2: T) extends Vec[T] with Copyable {
  
  override def size = 3

  // ! unsafe comments are intentional - used as wildcards when specializing datastructures !

  def apply(n : Int) = {
    /*unsafe.UnsafeAccessor.unsafe.getT(this, 16 + n*UNSAFE_SIZE)*/ v0
  }
  
  def update(n : Int, v : T) = {
    /*unsafe.UnsafeAccessor.unsafe.putT(this, 16 + n*UNSAFE_SIZE, v)*/
  }
  
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) {
    if (len < 3) throw new RuntimeException("code gen error: unsafeSetData to vec3 with array length less than 3")
    v0 = xs(0)
    v1 = xs(1)
    v2 = xs(2)
  }
  
  def cloneL = {
    new Vec3Impl[T](v0, v1, v2)
  }
  
  def copy() = cloneL
  
  override def toString() = {
    "Vec[" + size + "](" + v0 + "," + v1 + "," + v2 + ")"
  }
}
