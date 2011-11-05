package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
trait Field[T] extends DeliteCollection[T] {
  def apply(i: Int) : T
  def update(i: Int, v: T) : Unit

  def size : Int
  
  def dcApply(idx: Int) : T = apply(idx)
  def dcUpdate(idx: Int, x: T) : Unit = update(idx, x)
  def dcSize : Int = size
  def fill(v: T) : Unit
}
