package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait MeshObj {
  def id : Int
  def internalId : Int
}

trait Vertex extends MeshObj
trait Edge extends MeshObj
trait Face extends MeshObj
trait Cell extends MeshObj