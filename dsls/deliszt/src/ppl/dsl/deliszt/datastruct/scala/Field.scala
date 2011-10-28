package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
trait Field[T] extends DeliteCollection[T] {
  def apply(i : Int) : T
  def update(i : Int, v : T) : Unit

  def size : Int
  
  def dcApply(idx: Int) : T = apply(idx)
  def dcUpdate(idx: Int, x: T) : Unit = update(idx, x)
  def dcSize : Int = size
  
  def fill(v: T) {
    for(i <- 0 until size) {
      this(i) = if(v.isInstanceOf[Copyable]) v.asInstanceOf[Copyable].copy.asInstanceOf[T] else v
    }
  }
}

object Field {
  def ofCell[T:Manifest]() : Field[T] = {
    new FieldImpl(new Array[T](Mesh.mesh.ncells))
  }
  
  def ofEdge[T:Manifest]() : Field[T] = {
    new FieldImpl(new Array[T](Mesh.mesh.nedges))
  }
  
  def ofFace[T:Manifest]() : Field[T] = {
    new FieldImpl(new Array[T](Mesh.mesh.nfaces))
  }
  
  def ofVertex[T:Manifest]() : Field[T] = {
    new FieldImpl(new Array[T](Mesh.mesh.nvertices))
  }
  
  def cellWithConst[T:Manifest](v: T) : Field[T] = {
    val f = Field.ofCell[T]()
    f.fill(v)
    f
  }
  
  def edgeWithConst[T:Manifest](v: T) : Field[T] = {
    val f = Field.ofEdge[T]()
    f.fill(v)
    f
  }
  
  def faceWithConst[T:Manifest](v: T) : Field[T] = {
    val f = Field.ofFace[T]()
    f.fill(v)
    f
  }
  
  def vertexWithConst[T:Manifest](v: T) : Field[T] = {
    val f = Field.ofVertex[T]()
    f.fill(v)
    f
  }
}