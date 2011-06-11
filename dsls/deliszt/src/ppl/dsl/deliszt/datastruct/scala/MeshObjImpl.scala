package ppl.dsl.deliszt.datastruct.scala

object MeshObjImpl {
  abstract class MeshObjConstruct[MO <: MeshObj] {
    def apply(id: Int) : MO
  }

  implicit object CellConstruct extends MeshObjConstruct[Cell] {
    def apply(id : Int) = new CellImpl(id)
  }

  implicit object EdgeConstruct extends MeshObjConstruct[Edge] {
    def apply(id : Int) = new EdgeImpl(id)
  }

  implicit object FaceConstruct extends MeshObjConstruct[Face] {
    def apply(id : Int) = new EdgeImpl(id)
  }

  implicit object VertexConstruct extends MeshObjConstruct[Vertex] {
    def apply(id : Int) = new VertexImpl(id)
  }

  def apply[MO <: MeshObj : Manifest](id : Int)(implicit con : MeshObjConstruct[MO]) : MO = con(id)
}

class CellImpl(val id : Int) extends Cell
class EdgeImpl(val id : Int) extends Edge
class FaceImpl(val id : Int) extends Face
class VertexImpl(val id : Int) extends Vertex
