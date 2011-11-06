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
  def x = apply(0)
  def y = apply(1)
  def z = apply(2)
  def w = apply(3)
  
  def apply(n:Int) : T
  def update(n:Int, v : T) : Unit

  def size : Int
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)
  def dcSize : Int = size
  
  def cloneL : Vec[T]
  
  def foreach[U](f: T => U) = {
    var i = 0
    while(i < size) {
       f(this(i))
       i += 1
    }
  }
}
