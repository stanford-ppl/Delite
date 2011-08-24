package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait DeLisztSet[MO <: MeshObj] extends DeliteCollection[MO] {
  def apply(i : Int) : MO
  def size : Int

  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: MO) = {} // Read only, bitches
  def dcSize : Int = size
}