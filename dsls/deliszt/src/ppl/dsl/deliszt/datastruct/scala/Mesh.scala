package ppl.dsl.deliszt.datastruct.scala

import collection.mutable.{Map, HashMap}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Mesh extends MeshObjImpl {
  var mesh: Mesh = null
  var loader: MeshLoader = null

  def vertices(e: Mesh): MeshSet[Vertex] = MeshSetImpl(mesh.nvertices)
  def vertices(e: Vertex): MeshSet[Vertex] = IndexSetImpl(mesh.vtov, e)
  def vertices(e: Edge): MeshSet[Vertex] = IndexSetImpl(mesh.etov, e)
  def vertices(e: Face): MeshSet[Vertex] = IndexSetImpl(mesh.ftov, e)
  def vertices(e: Cell): MeshSet[Vertex] = IndexSetImpl(mesh.ctov, e)

  def verticesCCW(e: Face): MeshSet[Vertex] = IndexSetImpl(mesh.ftov, e)
  def verticesCW(e: Face): MeshSet[Vertex] = CWIndexSetImpl(mesh.ftov, e)
  
  def vertex(e: Cell, i: Int): Vertex = { val set = IndexSetImpl[Vertex](mesh.etov, e); set(i) }

  def cells(e: Mesh): MeshSet[Cell] = MeshSetImpl(mesh.ncells - 1)
  def cells(e: Vertex): MeshSet[Cell] = IndexSetImpl(mesh.vtoc, e)
  def cells(e: Edge): MeshSet[Cell] = IndexSetImpl(mesh.etoc, e)
  def cells(e: Face): MeshSet[Cell] = IndexSetImpl(mesh.ftoc, e)
  def cells(e: Cell): MeshSet[Cell] = IndexSetImpl(mesh.ctoc, e)

  def cellsCCW(e: Edge): MeshSet[Cell] = IndexSetImpl(mesh.etoc, e)
  def cellsCW(e: Edge): MeshSet[Cell] = CWIndexSetImpl(mesh.etoc, e)

  def edges(e: Mesh): MeshSet[Edge] = MeshSetImpl(mesh.nedges)
  def edges(e: Vertex): MeshSet[Edge] = IndexSetImpl(mesh.vtoe, e)
  def edges(e: Face): MeshSet[Edge] = IndexSetImpl(mesh.ftoe, e)
  def edges(e: Cell): MeshSet[Edge] = IndexSetImpl(mesh.ctoe, e)

  def edgesCCW(e: Face): MeshSet[Edge] = IndexSetImpl(mesh.ftoe, e)
  def edgesCW(e: Face): MeshSet[Edge] = CWIndexSetImpl(mesh.ftoe, e)

  def faces(e: Mesh): MeshSet[Face] = MeshSetImpl(mesh.nfaces)
  def faces(e: Vertex): MeshSet[Face] = IndexSetImpl(mesh.vtof, e)
  def faces(e: Edge): MeshSet[Face] = IndexSetImpl(mesh.etof, e)
  def faces(e: Cell): MeshSet[Face] = IndexSetImpl(mesh.ctof, e)

  def facesCCW(e: Edge): MeshSet[Face] = IndexSetImpl(mesh.etof, e)
  def facesCW(e: Edge): MeshSet[Face] = CWIndexSetImpl(mesh.etof, e)
  
  def face(e: Edge, i: Int): Face = { val set = IndexSetImpl[Face](mesh.ctof, e); set(i) }

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
  
  // Todo
  def wall_time() = 0.0
  def processor_time() = 0.0
}

class LabelData[MO<:MeshObj] {
  val data: Map[String,Array[Object]] = new HashMap[String,Array[Object]]()
  val fns: Map[String,Object => Object] = new HashMap[String,Object => Object]()
}

class Mesh extends MeshObj with MetaInteger with MeshObjImpl {
  def typeName = "Mesh"

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

  // Use special CellSetImpl, don't expose 0 cell
  implicit def cellSet : MeshSet[Cell] = new CellSetImpl(ncells-1)
  implicit def edgeSet : MeshSet[Edge] = new MeshSetImpl[Edge](nedges)
  implicit def faceSet : MeshSet[Face] = new MeshSetImpl[Face](nfaces)
  implicit def vertexSet : MeshSet[Vertex] = new MeshSetImpl[Vertex](nvertices)

  implicit val cellBounds : Map[String,MeshSet[Cell]] = new HashMap[String,MeshSet[Cell]]()
  implicit val edgeBounds : Map[String,MeshSet[Edge]] = new HashMap[String,MeshSet[Edge]]()
  implicit val faceBounds : Map[String,MeshSet[Face]] = new HashMap[String,MeshSet[Face]]()
  implicit val vertexBounds : Map[String,MeshSet[Vertex]] = new HashMap[String,MeshSet[Vertex]]()

  def positionToVec(v: Object) : Object = {
      val vec = VecImpl[_3, Double]()
      val a = v.asInstanceOf[Array[Object]]
      vec(_0) = a.apply(0).asInstanceOf[Double]
      vec(_1) = a.apply(1).asInstanceOf[Double]
      vec(_2) = a.apply(2).asInstanceOf[Double]
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

  def meshSet[MO<:MeshObj](implicit ms: MeshSet[MO]) = ms
  def boundarySet[MO<:MeshObj:MeshObjConstruct](name: String) : MeshSet[MO] = {
    Mesh.loader.loadBoundarySet(name)
  }
}
