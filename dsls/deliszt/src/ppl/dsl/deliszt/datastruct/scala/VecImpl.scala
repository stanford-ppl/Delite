package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
object VecImpl {
  def ofSize[T:ClassManifest](size: Int) = {
    new VecImpl[T](size)
  }
  
  def withData[T:ClassManifest](data : Array[T]) = {
    new VecImpl[T](data)
  }
  
  def apply[T:ClassManifest](xs: T*) = {
    new VecImpl[T](xs.toArray)
  }
}

class VecImpl[@specialized T: ClassManifest](var data : Array[T]) extends Vec[T] with Copyable {
  def this(size : Int) = this(new Array[T](size))
  
  override def size = data.length

  def apply(n : Int) = data(n)
  def update(n : Int, v : T) = {
    data(n) = v
  }
  
  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen. 
   */    
  def unsafeSetData(xs: Array[T], len: Int) {
    data = xs
  }
  
  def cloneL = {
    new VecImpl[T](data.clone)
  }
  
  def copy() = {  
    if(classManifest[T] <:< classManifest[Copyable]) {
      val v = new VecImpl[T](size)
      
      for(i <- 0 until size) {
        v(i) = data(i).asInstanceOf[Copyable].copy.asInstanceOf[T]
      }
      
      v
    }
    else {
      val copy = new Array[T](size)
      Array.copy(data, 0, copy, 0, size)
      
      new VecImpl[T](copy)
    }
  }
  
  override def toString() = {
    "Vec[" + size + "](" + data.map(_.toString).reduceLeft(_ + "," + _) + ")"
  }
}

/*
class ThreeVecImpl[@specialized T: ClassManifest](val data : Array[T]) extends Vec[T] with Copyable {
  override var x : T
  override var y : T
  override var z : T
  
  override def size = 3

  def apply(n : Int) = {
    n match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ =>
    }
  }
  
  def update(n : Int, v : T) = {
    n match {
      case 0 => {x = v}
    }
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
*/