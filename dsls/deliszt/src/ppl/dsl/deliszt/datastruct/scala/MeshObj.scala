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
  def internalId : Int = id & ~BitReverse.MASK
  def reversed = BitReverse.reversed(id)
  def typeName : String
  override def toString = typeName + " " + internalId
}

trait Cell extends MeshObj {
  def typeName = "Cell"
}

trait Edge extends MeshObj {
  def typeName = "Edge"
}

trait Face extends MeshObj {
  def typeName = "Face"
}

trait Vertex extends MeshObj {
  def typeName = "Vertex"
}