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
  var loader: MeshLoader = null
  
  val MASK = 0x80000000

  def reversed(id : Int) = {(id & MASK) != 0}
  def reverse(id : Int) = {id ^ MASK}
  def internal(id : Int) = {id & ~MASK}
  
  val OUTSIDE = 0
  val INSIDE = 1
  val HEAD = 0
  val TAIL = 1

  def verticesMesh(e: Int): MeshSet = MeshSetImpl(mesh.nvertices)
  def verticesVertex(e: Int): MeshSet = IndexSetImpl(mesh.vtov, e)
  def verticesEdge(e: Int): MeshSet = IndexSetImpl(mesh.etov, e)
  def verticesFace(e: Int): MeshSet = IndexSetImpl(mesh.ftov, e)
  def verticesCell(e: Int): MeshSet = IndexSetImpl(mesh.ctov, e)

  def verticesCCW(e: Int): MeshSet = {
    val c = outside(e)
    if(mesh.ftoc.apply(internal(e), OUTSIDE) == internal(c)) {
      IndexSetImpl(mesh.ftov, e)
    } else {
      CWIndexSetImpl(mesh.ftov, e)
    }
  }
  
  def verticesCW(e: Int): MeshSet = {
    val c = outside(e)
    if(mesh.ftoc.apply(internal(e), INSIDE) == internal(c)) {
      IndexSetImpl(mesh.ftov, e)
    } else {
      CWIndexSetImpl(mesh.ftov, e)
    }
  }
  
  def vertex(e: Int, i: Int): Int = { val set = IndexSetImpl(mesh.etov, e); set(i) }

  def cellsMesh(e: Int): MeshSet = new CellSetImpl(mesh.ncells)
  def cellsVertex(e: Int): MeshSet = IndexSetImpl(mesh.vtoc, e)
  def cellsEdge(e: Int): MeshSet = IndexSetImpl(mesh.etoc, e)
  def cellsFace(e: Int): MeshSet = IndexSetImpl(mesh.ftoc, e)
  def cellsCell(e: Int): MeshSet = IndexSetImpl(mesh.ctoc, e)

  def cellsCCW(e: Int): MeshSet = {
    val v = head(e)
    if(mesh.etov.apply(internal(e), HEAD) == internal(v)) {
      IndexSetImpl(mesh.etoc, e)
    } else {
      CWIndexSetImpl(mesh.etoc, e)
    }
  }
  
  def cellsCW(e: Int): MeshSet = {
    val v = head(e)
    if(mesh.etov.apply(internal(e), TAIL) == internal(v)) {
      IndexSetImpl(mesh.etoc, e)
    } else {
      CWIndexSetImpl(mesh.etoc, e)
    }
  }

  def edgesMesh(e: Int): MeshSet = MeshSetImpl(mesh.nedges)
  def edgesVertex(e: Int): MeshSet = IndexSetImpl(mesh.vtoe, e)
  def edgesFace(e: Int): MeshSet = IndexSetImpl(mesh.ftoe, e)
  def edgesCell(e: Int): MeshSet = IndexSetImpl(mesh.ctoe, e)

  def edgesCCW(e: Int): MeshSet = {
    val c = outside(e)
    if(mesh.ftoc.apply(internal(e), OUTSIDE) == internal(c)) {
      IndexSetImpl(mesh.ftoe, e)
    } else {
      CWIndexSetImpl(mesh.ftoe, e)
    }
  }
  
  def edgesCW(e: Int): MeshSet = {
    val c = outside(e)
    if(mesh.ftoc.apply(internal(e), INSIDE) == internal(c)) {
      IndexSetImpl(mesh.ftoe, e)
    } else {
      CWIndexSetImpl(mesh.ftoe, e)
    }
  }
  
  def facesMesh(e: Int): MeshSet = MeshSetImpl(mesh.nfaces)
  def facesVertex(e: Int): MeshSet = IndexSetImpl(mesh.vtof, e)
  def facesEdge(e: Int): MeshSet = IndexSetImpl(mesh.etof, e)
  def facesCell(e: Int): MeshSet = IndexSetImpl(mesh.ctof, e)

  def facesCCW(e: Int): MeshSet = {
    val v = head(e)
    if(mesh.etov.apply(internal(e), HEAD) == internal(v)) {
      IndexSetImpl(mesh.etof, e)
    } else {
      CWIndexSetImpl(mesh.etof, e)
    }
  }
  
  def facesCW(e: Int): MeshSet = {
    val v = head(e)
    if(mesh.etov.apply(internal(e), TAIL) == internal(v)) {
      IndexSetImpl(mesh.etoc, e)
    } else {
      CWIndexSetImpl(mesh.etoc, e)
    }
  }
  
  def face(e: Int, i: Int): Int = { val set = IndexSetImpl(mesh.ctof, e); set(i) }

  def head(e: Int): Int = mesh.etov.apply(e, if(reversed(e)) 1 else 0)
  def tail(e: Int): Int = mesh.etov.apply(e, if(reversed(e)) 0 else 1)

  def outside(e: Int): Int = mesh.ftoc.apply(e, if(reversed(e)) 1 else 0)
  def inside(e: Int): Int = mesh.ftoc.apply(e, if(reversed(e)) 0 else 1)

  def flip(e: Int): Int = {e ^ MASK}

  def towardsEdgeVertex(e: Int, v: Int): Int = {
    val facing = internal(mesh.etov.apply(e, HEAD)) == internal(v)
    if(facing) e else flip(e)
  }

  def towardsFaceCell(e: Int, c: Int): Int = {
    val facing = internal(mesh.ftoc.apply(e, OUTSIDE)) == internal(c)
    if(facing) e else flip(e)
  }
  
  // Todo
  def wall_time() = 0.0
  def processor_time() = 0.0
  
  def label[T](ld: LabelData, url: String) : LabelField[T] = {
    ld.data.get(url) match {
      case Some(data) => new LabelFieldImpl[T](data, ld.fns.get(url).getOrElse(null))
      case None => null
    }
  }
  
  def labelCells[T](url: String) : LabelField[T] = label(mesh.cellData, url)
  def labelEdges[T](url: String) : LabelField[T] = label(mesh.edgeData, url)
  def labelFaces[T](url: String) : LabelField[T] = label(mesh.faceData, url)
  def labelVertices[T](url: String) : LabelField[T] = label(mesh.vertexData, url)

  // def meshSet[MO<:MeshObj](implicit ms: MeshSet[MO]) = ms
  
  def boundarySetCells(name: String) : BoundarySet = {
    Mesh.loader.loadBoundarySet(name, MeshObj.CELL_TYPE)
  }
  
  def boundarySetEdges(name: String) : BoundarySet = {
    Mesh.loader.loadBoundarySet(name, MeshObj.EDGE_TYPE)
  }
  
  def boundarySetFaces(name: String) : BoundarySet = {
    Mesh.loader.loadBoundarySet(name, MeshObj.FACE_TYPE)
  }
  
  def boundarySetVertices(name: String) : BoundarySet = {
    Mesh.loader.loadBoundarySet(name, MeshObj.VERTEX_TYPE)
  }
}

class LabelData {
  val data: Map[String,Array[Object]] = new HashMap[String,Array[Object]]()
  val fns: Map[String,Object => Object] = new HashMap[String,Object => Object]()
}

class Mesh {
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

  val cellData = new LabelData
  val edgeData = new LabelData
  val faceData = new LabelData
  val vertexData = new LabelData

  // Use special CellSetImpl, don't expose 0 cell
  val cells : MeshSet = new CellSetImpl(ncells-1)
  val edges : MeshSet = new MeshSetImpl(nedges)
  val faces : MeshSet = new MeshSetImpl(nfaces)
  val vertices : MeshSet = new MeshSetImpl(nvertices)
  
  def positionToVec(p: Object) : Object = {
    val a = p.asInstanceOf[Array[Double]]
   
    val v = Vec.ofSize[Float](3)
    v(0) = a(0).asInstanceOf[Float]
    v(1) = a(1).asInstanceOf[Float]
    v(2) = a(2).asInstanceOf[Float]
    
    v
  }

  vertexData.fns("position") = positionToVec
}
