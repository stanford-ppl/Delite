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
  override def hashCode = typeName.hashCode + id
}

object Cell {
  def unapply(c: Cell) = Some(c.internalId)
}

trait Cell extends MeshObj {
  def typeName = "Cell"
  
  override def equals(that: Any) = that match {
    case other: Cell => other.internalId == internalId
    case _ => false
  }
}

trait Edge extends MeshObj {
  def typeName = "Edge"
  
  override def equals(that: Any) = that match {
    case other: Edge => other.internalId == internalId
    case _ => false
  }
}

trait Face extends MeshObj {
  def typeName = "Face"
  
  override def equals(that: Any) = that match {
    case other: Face => other.internalId == internalId
    case _ => false
  }
}

trait Vertex extends MeshObj {
  def typeName = "Vertex"
  
  override def equals(that: Any) = that match {
    case other: Vertex => other.internalId == internalId
    case _ => false
  }
}