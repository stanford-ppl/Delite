package ppl.dsl.deliszt.datastruct.scala

class MeshObjImpl(val id : Int) extends MeshObj {
  def internalId = id & ~0x80000000
}

class CellImpl(id : Int) extends MeshObjImpl(id)
class EdgeImpl(id : Int) extends MeshObjImpl(id)
class FaceImpl(id : Int) extends MeshObjImpl(id)
class VertexImpl(id : Int) extends MeshObjImpl(id)
