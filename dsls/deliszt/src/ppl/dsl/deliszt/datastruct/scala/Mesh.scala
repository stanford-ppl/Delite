package ppl.dsl.deliszt.datastruct.scala

import collection.mutable.{Map, HashMap}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Mesh {
  var mesh: Mesh = null

  def vertices(e: Mesh): DeLisztSet[Vertex] = MeshObjSetImpl(mesh.nvertices)
  def vertices(e: Vertex): DeLisztSet[Vertex] = IndexSetImpl(mesh.vtov, e)
  def vertices(e: Edge): DeLisztSet[Vertex] = IndexSetImpl(mesh.etov, e)
  def vertices(e: Face): DeLisztSet[Vertex] = IndexSetImpl(mesh.ftov, e)
  def vertices(e: Cell): DeLisztSet[Vertex] = IndexSetImpl(mesh.ctov, e)

  def verticesCCW(e: Face): DeLisztSet[Vertex] = IndexSetImpl(mesh.ftov, e)
  def verticesCW(e: Face): DeLisztSet[Vertex] = CWIndexSetImpl(mesh.ftov, e)

  def cells(e: Mesh): DeLisztSet[Cell] = MeshObjSetImpl(mesh.ncells)
  def cells(e: Vertex): DeLisztSet[Cell] = IndexSetImpl(mesh.vtoc, e)
  def cells(e: Edge): DeLisztSet[Cell] = IndexSetImpl(mesh.etoc, e)
  def cells(e: Face): DeLisztSet[Cell] = IndexSetImpl(mesh.ftoc, e)
  def cells(e: Cell): DeLisztSet[Cell] = IndexSetImpl(mesh.ctoc, e)

  def cellsCCW(e: Edge): DeLisztSet[Cell] = IndexSetImpl(mesh.etoc, e)
  def cellsCW(e: Edge): DeLisztSet[Cell] = CWIndexSetImpl(mesh.etoc, e)

  def edges(e: Mesh): DeLisztSet[Edge] = MeshObjSetImpl(mesh.nedges)
  def edges(e: Vertex): DeLisztSet[Edge] = IndexSetImpl(mesh.vtoe, e)
  def edges(e: Face): DeLisztSet[Edge] = IndexSetImpl(mesh.ftoe, e)
  def edges(e: Cell): DeLisztSet[Edge] = IndexSetImpl(mesh.ctoe, e)

  def edgesCCW(e: Face): DeLisztSet[Edge] = IndexSetImpl(mesh.ftoe, e)
  def edgesCW(e: Face): DeLisztSet[Edge] = IndexSetImpl(mesh.ftoe, e)

  def faces(e: Mesh): DeLisztSet[Face] = MeshObjSetImpl(mesh.nfaces)
  def faces(e: Vertex): DeLisztSet[Face] = IndexSetImpl(mesh.vtof, e)
  def faces(e: Edge): DeLisztSet[Face] = IndexSetImpl(mesh.etof, e)
  def faces(e: Cell): DeLisztSet[Face] = IndexSetImpl(mesh.ctof, e)

  def facesCCW(e: Edge): DeLisztSet[Face] = IndexSetImpl(mesh.etof, e)
  def facesCW(e: Edge): DeLisztSet[Face] = IndexSetImpl(mesh.etof, e)

  def head(e: Edge): Vertex = new VertexImpl(mesh.etov.apply(e, if(e.reversed) 0 else 1))
  def tail(e: Edge): Vertex = new VertexImpl(mesh.etov.apply(e, if(e.reversed) 1 else 0))

  def inside(e: Face): Cell = new CellImpl(mesh.ftoc.apply(e, if(e.reversed) 1 else 0))
  def outside(e: Face): Cell = new CellImpl(mesh.ftoc.apply(e, if(e.reversed) 0 else 1))

  def flip(e: Edge): Edge = new EdgeImpl(BitReverse.reverse(e.id))
  def flip(e: Face): Face = new FaceImpl(BitReverse.reverse(e.id))

  def towards(e: Edge, v: Vertex): Edge = {
    val facing = BitReverse.internal(mesh.etov.apply(e, 0)) == v.internalId
    val id = if(facing) e.id else BitReverse.reverse(e.id)
    new EdgeImpl(id)
  }

  def towards(e: Face, c: Cell): Face = {
    val facing = BitReverse.internal(mesh.ftoc.apply(e, 0)) == c.internalId
    val id = if(facing) e.id else BitReverse.reverse(e.id)
    new FaceImpl(id)
  }
}

class LabelData[MO<:MeshObj] {
  val data: Map[String,Array[Object]] = new HashMap[String,Array[Object]]()
  val fns: Map[String,Object => Object] = new HashMap[String,Object => Object]()
}

class Mesh extends MeshObj with MetaInteger {
  val id = 0

  var nvertices: Int = 0
	var nedges: Int = 0
	var nfaces: Int = 0
	var ncells: Int = 0

	var vtov: CRS = null
	var vtoe: CRS = null
	var vtof: CRS = null
	var vtoc: CRS = null

	var etov: CRS = null
	var etof: CRS = null
	var etoc: CRS = null

	var ftov: CRS = null
	var ftoe: CRS = null
	var ftoc: CRS = null

	var ctov: CRS = null
	var ctoe: CRS = null
	var ctof: CRS = null
	var ctoc: CRS = null

  implicit val cellData = new LabelData[Cell]
  implicit val edgeData = new LabelData[Edge]
  implicit val faceData = new LabelData[Face]
  implicit val vertexData = new LabelData[Vertex]

  implicit def cellSet : MeshObjSet[Cell] = new MeshObjSetImpl[Cell](ncells)
  implicit def edgeSet : MeshObjSet[Edge] = new MeshObjSetImpl[Edge](nedges)
  implicit def faceSet : MeshObjSet[Face] = new MeshObjSetImpl[Face](nfaces)
  implicit def vertexSet : MeshObjSet[Vertex] = new MeshObjSetImpl[Vertex](nvertices)

  implicit val cellBounds : Map[String,MeshObjSet[Cell]] = new HashMap[String,MeshObjSet[Cell]]()
  implicit val edgeBounds : Map[String,MeshObjSet[Edge]] = new HashMap[String,MeshObjSet[Edge]]()
  implicit val faceBounds : Map[String,MeshObjSet[Face]] = new HashMap[String,MeshObjSet[Face]]()
  implicit val vertexBounds : Map[String,MeshObjSet[Vertex]] = new HashMap[String,MeshObjSet[Vertex]]()

  def positionToVec(v: Object) : Object = {
      val vec = VecImpl[_3, Double]()
      val a = v.asInstanceOf[Array[Object]]
      vec(0) = a.apply(0)
      vec(1) = a.apply(1)
      vec(2) = a.apply(2)
      vec
  }

  vertexData.fns("position") = positionToVec

  def label[MO<:MeshObj,VT](url: String)(implicit ld: LabelData[MO], mm: Manifest[MO], mv: Manifest[VT]) : LabelField[MO,VT] = {
    //TODO: clean this up
    ld.data.get(url) match {
      case Some(data) => new LabelFieldImpl[MO,VT](data, ld.fns.get(url).getOrElse(null))
      case None => null
    }
  }

  def meshSet[MO<:MeshObj](implicit ms: MeshObjSet[MO], mm: Manifest[MO]) = ms
  def boundarySet[MO<:MeshObj](url: String)(implicit bm: Map[String,MeshObjSet[MO]], mm: Manifest[MO]) : MeshObjSet[MO] = {
    bm.get(url) match {
      case Some(bs) => bs.asInstanceOf[MeshObjSet[MO]]
      case None => null
    }
  }
}