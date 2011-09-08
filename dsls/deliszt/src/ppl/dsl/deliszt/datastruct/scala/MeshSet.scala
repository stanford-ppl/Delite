package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object MeshSet {
  // Use special CellSetImpl, don't expose 0 cell
  implicit def cellSet : MeshSet[Cell] = new CellSetImpl(Mesh.mesh.ncells)
  implicit def edgeSet : MeshSet[Edge] = new MeshSetImpl(Mesh.mesh.nedges)
  implicit def faceSet : MeshSet[Face] = new MeshSetImpl(Mesh.mesh.nfaces)
  implicit def vertexSet : MeshSet[Vertex] = new MeshSetImpl(Mesh.mesh.nvertices)
}

trait MeshSet[MO <: MeshObj] extends DeliteCollection[MO] with Traversable[MO] {
  def apply(i : Int) : MO

  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: MO) = {} // Read only, bitches
  def dcSize : Int = size
  
  def foreach[U](f: MO => U) = for(i <- 0 until size) f(this(i))
}