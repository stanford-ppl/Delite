package ppl.dsl.deliszt.datastruct.scala

import ppl.dsl.deliszt.datastruct.scala.MetaInteger._
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
  var loader: MeshLoader = null
  
  val OUTSIDE = 0
  val INSIDE = 1
  val HEAD = 0
  val TAIL = 1

  def vertices(e: Mesh): MeshSet[Vertex] = MeshSetImpl(mesh.nvertices)
  def vertices(e: Vertex): MeshSet[Vertex] = IndexSetImpl(mesh.vtov, e)
  def vertices(e: Edge): MeshSet[Vertex] = IndexSetImpl(mesh.etov, e)
  def vertices(e: Face): MeshSet[Vertex] = IndexSetImpl(mesh.ftov, e)
  def vertices(e: Cell): MeshSet[Vertex] = IndexSetImpl(mesh.ctov, e)

  def verticesCCW(e: Face): MeshSet[Vertex] = {
    val c = outside(e)
    if(mesh.ftoc.apply(e.internalId, OUTSIDE) == c.internalId) {
      IndexSetImpl(mesh.ftov, e)
    } else {
      CWIndexSetImpl(mesh.ftov, e)
    }
  }
  
  def verticesCW(e: Face): MeshSet[Vertex] = {
    val c = outside(e)
    if(mesh.ftoc.apply(e.internalId, INSIDE) == c.internalId) {
      IndexSetImpl(mesh.ftov, e)
    } else {
      CWIndexSetImpl(mesh.ftov, e)
    }
  }
  
  def vertex(e: Cell, i: Int): Vertex = { val set = IndexSetImpl[Vertex](mesh.etov, e); set(i) }

  def cells(e: Mesh): MeshSet[Cell] = new CellSetImpl(mesh.ncells)
  def cells(e: Vertex): MeshSet[Cell] = IndexSetImpl(mesh.vtoc, e)
  def cells(e: Edge): MeshSet[Cell] = IndexSetImpl(mesh.etoc, e)
  def cells(e: Face): MeshSet[Cell] = IndexSetImpl(mesh.ftoc, e)
  def cells(e: Cell): MeshSet[Cell] = IndexSetImpl(mesh.ctoc, e)

  def cellsCCW(e: Edge): MeshSet[Cell] = {
    val v = head(e)
    if(mesh.etov.apply(e.internalId, HEAD) == v.internalId) {
      IndexSetImpl(mesh.etoc, e)
    } else {
      CWIndexSetImpl(mesh.etoc, e)
    }
  }
  
  def cellsCW(e: Edge): MeshSet[Cell] = {
    val v = head(e)
    if(mesh.etov.apply(e.internalId, TAIL) == v.internalId) {
      IndexSetImpl(mesh.etoc, e)
    } else {
      CWIndexSetImpl(mesh.etoc, e)
    }
  }

  def edges(e: Mesh): MeshSet[Edge] = MeshSetImpl(mesh.nedges)
  def edges(e: Vertex): MeshSet[Edge] = IndexSetImpl(mesh.vtoe, e)
  def edges(e: Face): MeshSet[Edge] = IndexSetImpl(mesh.ftoe, e)
  def edges(e: Cell): MeshSet[Edge] = IndexSetImpl(mesh.ctoe, e)

  def edgesCCW(e: Face): MeshSet[Edge] = {
    val c = outside(e)
    if(mesh.ftoc.apply(e.internalId, OUTSIDE) == c.internalId) {
      System.out.println("EDDGES NORMAL")
      IndexSetImpl(mesh.ftoe, e)
    } else {
      System.out.println("EDDGES CW")
      CWIndexSetImpl(mesh.ftoe, e)
    }
  }
  
  def edgesCW(e: Face): MeshSet[Edge] = {
    val c = outside(e)
    if(mesh.ftoc.apply(e.internalId, INSIDE) == c.internalId) {
      IndexSetImpl(mesh.ftoe, e)
    } else {
      CWIndexSetImpl(mesh.ftoe, e)
    }
  }
  
  def faces(e: Mesh): MeshSet[Face] = MeshSetImpl(mesh.nfaces)
  def faces(e: Vertex): MeshSet[Face] = IndexSetImpl(mesh.vtof, e)
  def faces(e: Edge): MeshSet[Face] = IndexSetImpl(mesh.etof, e)
  def faces(e: Cell): MeshSet[Face] = IndexSetImpl(mesh.ctof, e)

  def facesCCW(e: Edge): MeshSet[Face] = {
    val v = head(e)
    if(mesh.etov.apply(e.internalId, HEAD) == v.internalId) {
      IndexSetImpl(mesh.etof, e)
    } else {
      CWIndexSetImpl(mesh.etof, e)
    }
  }
  
  def facesCW(e: Edge): MeshSet[Face] = {
    val v = head(e)
    if(mesh.etov.apply(e.internalId, TAIL) == v.internalId) {
      IndexSetImpl(mesh.etoc, e)
    } else {
      CWIndexSetImpl(mesh.etoc, e)
    }
  }
  
  def face(e: Edge, i: Int): Face = { val set = IndexSetImpl[Face](mesh.ctof, e); set(i) }

  def head(e: Edge): Vertex = new VertexImpl(mesh.etov.apply(e, if(e.reversed) 1 else 0))
  def tail(e: Edge): Vertex = new VertexImpl(mesh.etov.apply(e, if(e.reversed) 0 else 1))

  def outside(e: Face): Cell = new CellImpl(mesh.ftoc.apply(e, if(e.reversed) 1 else 0))
  def inside(e: Face): Cell = new CellImpl(mesh.ftoc.apply(e, if(e.reversed) 0 else 1))

  def flip(e: Edge): Edge = new EdgeImpl(BitReverse.reverse(e.id))
  def flip(e: Face): Face = new FaceImpl(BitReverse.reverse(e.id))

  def towards(e: Edge, v: Vertex): Edge = {
    val facing = BitReverse.internal(mesh.etov.apply(e, HEAD)) == v.internalId
    val id = if(facing) e.id else BitReverse.reverse(e.id)
    new EdgeImpl(id)
  }

  def towards(e: Face, c: Cell): Face = {
    val facing = BitReverse.internal(mesh.ftoc.apply(e, OUTSIDE)) == c.internalId
    val id = if(facing) e.id else BitReverse.reverse(e.id)
    new FaceImpl(id)
  }
  
  // Todo
  def wall_time() = 0.0
  def processor_time() = 0.0
  
  def label[MO<:MeshObj,VT](url: String)(implicit ld: LabelData[MO], mm: Manifest[MO], mv: Manifest[VT]) : LabelField[MO,VT] = {
    //TODO: clean this up
    ld.data.get(url) match {
      case Some(data) => new LabelFieldImpl[MO,VT](data, ld.fns.get(url).getOrElse(null))
      case None => null
    }
  }

  def meshSet[MO<:MeshObj](implicit ms: MeshSet[MO]) = ms
  def boundarySet[MO<:MeshObj:MeshObjConstruct](name: String) : BoundarySet[MO] = {
    Mesh.loader.loadBoundarySet(name)
  }
}

object LabelData {
  implicit def cellData = Mesh.mesh.cellData
  implicit def edgeData = Mesh.mesh.edgeData
  implicit def faceData = Mesh.mesh.faceData
  implicit def vertexData = Mesh.mesh.vertexData
}

class LabelData[MO<:MeshObj] {
  val data: Map[String,Array[Object]] = new HashMap[String,Array[Object]]()
  val fns: Map[String,Object => Object] = new HashMap[String,Object => Object]()
}

class Mesh extends MeshObj {
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

  val cellData = new LabelData[Cell]
  val edgeData = new LabelData[Edge]
  val faceData = new LabelData[Face]
  val vertexData = new LabelData[Vertex]

  // Use special CellSetImpl, don't expose 0 cell
  val cells : MeshSet[Cell] = new CellSetImpl(ncells-1)
  val edges : MeshSet[Edge] = new MeshSetImpl[Edge](nedges)
  val faces : MeshSet[Face] = new MeshSetImpl[Face](nfaces)
  val vertices : MeshSet[Vertex] = new MeshSetImpl[Vertex](nvertices)
  
  def positionToVec(p: Object) : Object = {
    val a = p.asInstanceOf[Array[Double]]
   
    val v = VecImpl[_3, Float]()
    v(0) = a(0).asInstanceOf[Float]
    v(1) = a(1).asInstanceOf[Float]
    v(2) = a(2).asInstanceOf[Float]
    
    v
  }

  vertexData.fns("position") = positionToVec
}
