package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object FieldImpl {
  def apply[MO<:MeshObj:Manifest,T:Manifest]() : Field[MO,T] = {
    if(manifest[MO] <:< manifest[Cell]) {
       new FieldImpl(new Array[T](Mesh.mesh.ncells))
    }
    else if(manifest[MO] <:< manifest[Face]) {
      new FieldImpl(new Array[T](Mesh.mesh.nfaces))
    }
    else if(manifest[MO] <:< manifest[Edge]) {
      new FieldImpl(new Array[T](Mesh.mesh.nedges))
    }
    else if(manifest[MO] <:< manifest[Vertex]) {
      new FieldImpl(new Array[T](Mesh.mesh.nvertices))
    }
    else {
      throw new RuntimeException("Invalid MeshObj type")
      null
    }
  }
  
  def withConst[MO<:MeshObj:Manifest,T:Manifest](v: T) : Field[MO,T] = {
    val f = FieldImpl[MO,T]()
    
    for(i <- 0 until f.size) {
      f(i) = if(v.isInstanceOf[Copyable]) v.asInstanceOf[Copyable].copy.asInstanceOf[T] else v
    }
    
    f
  }
}

class FieldImpl[MO<:MeshObj:Manifest, T:Manifest](data : Array[T]) extends Field[MO,T] {
  def apply(idx: Int) = data(idx)
  def update(idx: Int, x: T) = {
    data(idx) = x
  }
  def size = data.length
}
