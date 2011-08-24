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
  implicit def cellSet : MeshSet[Cell] = Mesh.mesh.cells
  implicit def edgeSet : MeshSet[Edge] = Mesh.mesh.edges
  implicit def faceSet : MeshSet[Face] = Mesh.mesh.faces
  implicit def vertexSet : MeshSet[Vertex] = Mesh.mesh.vertices
}

trait MeshSet[MO <: MeshObj] extends DeliteCollection[MO] {
  def apply(i : Int) : MO
  def size : Int

  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: MO) = {} // Read only, bitches
  def dcSize : Int = size
}