package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class VecImpl[@specialized T: ClassManifest](val data : Array[T]) extends Vec[T] with Copyable {
  def this(size : Int) = this(new Array[T](size))
  
  override def size = data.length

  def apply(n : Int) = data(n)
  def update(n : Int, v : T) = {
    data(n) = v
  }
  
  def cloneL = {
    new VecImpl[T](data.clone)
  }
  
  def copy() = {
    val v = new VecImpl[T](size)
  
    if(classManifest[T] <:< classManifest[Copyable]) {
      for(i <- 0 until size) {
        v(i) = data(i).asInstanceOf[Copyable].copy.asInstanceOf[T]
      }
    }
    else {
      for(i <- 0 until size) {
        v(i) = data(i)
      }
    }
    
    v
  }
  
  override def toString() = {
    "Vec[" + size + "](" + data.map(_.toString).reduceLeft(_ + "," + _) + ")"
  }
}
