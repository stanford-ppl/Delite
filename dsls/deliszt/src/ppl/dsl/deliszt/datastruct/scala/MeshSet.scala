package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait MeshSet extends DeliteCollection[Int] with Traversable[Int] {
  def apply(i : Int) : Int

  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: Int) = {} // Read only, bitches
  def dcSize : Int = size
  
  def foreach[U](f: Int => U) = {
     var i = 0
     while(i < size) {
        f(this(i))
        i += 1
     }
  }
}
