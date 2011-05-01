package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object FieldImpl {
  def apply[A <: MeshObj : Manifest, VT : Manifest](mesh: Mesh) = {
    if(manifest[A] <:< manifest[Cell]) {
       new FieldImpl(new Array[VT](mesh.ncells))
    }
    else if(manifest[A] <:< manifest[Face]) {
      new FieldImpl(new Array[VT](mesh.nfaces))
    }
    else if(manifest[A] <:< manifest[Edge]) {
      new FieldImpl(new Array[VT](mesh.nedges))
    }
    else if(manifest[A] <:< manifest[Vertex]) {
      new FieldImpl(new Array[VT](mesh.nvertices))
    }
  }
}

class FieldImpl[A <: MeshObj, VT](data : Array[VT]) extends Field[A, VT] {
  def apply(a : A) : VT = data(a.internalId)
  def update(a : A, v : VT) = {
    data(a.internalId) = v
  }

  def size = data.length
  def dcApply(idx: Int) = data(idx)
  def dcUpdate(idx: Int, x: A) = {
    data(idx) = x
  }
}