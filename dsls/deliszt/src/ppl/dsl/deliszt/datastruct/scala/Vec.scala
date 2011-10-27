package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait Vec[@specialized(Boolean, Int, Long, Float, Double) T] extends DeliteCollection[T] with Traversable[T] {
  def apply(n:Int) : T
  def update(n:Int, v : T) : Unit

  def size : Int
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)
  def dcSize : Int = size
  
  def cloneL : Vec[T]
  
  def foreach[U](f: T => U) = for(i <- 0 until size) f(this(i))
}

object Vec {
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
