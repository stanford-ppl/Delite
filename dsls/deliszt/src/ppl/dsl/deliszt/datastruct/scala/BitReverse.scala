package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object BitReverse {
  val MASK = 0x80000000

  def reversed(id : Int) = {(id & MASK) != 0}
  def reverse(id : Int) = {id ^ MASK}
  def internal(id : Int) = {id & ~MASK}
}

