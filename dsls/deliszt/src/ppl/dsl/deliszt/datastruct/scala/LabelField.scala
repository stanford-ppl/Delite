package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 05/12/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait LabelField[MO <: MeshObj, VT] extends Field[MO,VT] { 
  def size : Int
  def dcApply(idx: Int) : VT
  def dcUpdate(idx: Int, x: VT) = throw new RuntimeException()
}
