package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object FieldImpl {
  def apply[A <: MeshObj : Manifest, VT : Manifest](mesh: Mesh)(implicit mA : Manifest[A]) = {
    var size = 0

    if(mA <:< Manifest[Cell]) {
      size = mesh.ncells
    }
    else if(mA <:< Manifest[Face]) {
      size = mesh.nfaces
    }
    else if(mA <:< Manifest[Edge]) {
      size = mesh.nedges
    }
    else if(mA <:< Manifest[Vertex]) {
      size = mesh.nvertices
    }

    new FieldImpl(new Array[VT](size))
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