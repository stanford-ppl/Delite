package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object FieldImpl {
  def apply[MO<:MeshObj:Manifest,VT:Manifest]() : Field[MO,VT] = {
    if(manifest[MO] <:< manifest[Cell]) {
       new FieldImpl(new Array[VT](Mesh.mesh.ncells))
    }
    else if(manifest[MO] <:< manifest[Face]) {
      new FieldImpl(new Array[VT](Mesh.mesh.nfaces))
    }
    else if(manifest[MO] <:< manifest[Edge]) {
      new FieldImpl(new Array[VT](Mesh.mesh.nedges))
    }
    else if(manifest[MO] <:< manifest[Vertex]) {
      new FieldImpl(new Array[VT](Mesh.mesh.nvertices))
    }
    else {
      throw new RuntimeException("Invalid MeshObj type")
      null
    }
  }
}

class FieldImpl[MO<:MeshObj:Manifest, VT:Manifest](data : Array[VT]) extends Field[MO,VT] {
  def apply(idx: Int) = data(idx)
  def update(idx: Int, x: VT) = {
    data(idx) = x
  }
  def size = data.length
}
