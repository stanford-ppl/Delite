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
  val FORWARD = 1
  val REVERSE = -1

  def reversed(id : Int) = {(id & MASK) != 0}
  def reverse(id : Int) = {id ^ MASK}
  def internal(id : Int) = {id & ~MASK}
  
  def flip(e: Int): Int = {e ^ MASK}
  
  val OUTSIDE = 0
  val INSIDE = 1
  val HEAD = 0
  val TAIL = 1
  
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
  
  // Todo
  def wall_time() = 0.0
  def processor_time() = 0.0
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
  
  def verticesMesh: MeshSet = MeshSetImpl(nvertices)
  def verticesVertex(e: Int): MeshSet = IndexSetImpl(vtov, e)
  def verticesEdge(e: Int): MeshSet = IndexSetImpl(etov, e)
  def verticesFace(e: Int): MeshSet = IndexSetImpl(ftov, e)
  def verticesCell(e: Int): MeshSet = IndexSetImpl(ctov, e)

  def verticesCCW(e: Int): MeshSet = {
    val c = outside(e)
    if(ftoc.apply(Mesh.internal(e), Mesh.OUTSIDE) == Mesh.internal(c)) {
      IndexSetImpl(ftov, e)
    } else {
      CWIndexSetImpl(ftov, e)
    }
  }
  
  def verticesCW(e: Int): MeshSet = {
    val c = outside(e)
    if(ftoc.apply(Mesh.internal(e), Mesh.INSIDE) == Mesh.internal(c)) {
      IndexSetImpl(ftov, e)
    } else {
      CWIndexSetImpl(ftov, e)
    }
  }
  
  def vertex(e: Int, i: Int): Int = { val set = IndexSetImpl(etov, e); set(i) }

  def cellsMesh: MeshSet = new CellSetImpl(ncells)
  def cellsVertex(e: Int): MeshSet = IndexSetImpl(vtoc, e)
  def cellsEdge(e: Int): MeshSet = IndexSetImpl(etoc, e)
  def cellsFace(e: Int): MeshSet = IndexSetImpl(ftoc, e)
  def cellsCell(e: Int): MeshSet = IndexSetImpl(ctoc, e)

  def cellsCCW(e: Int): MeshSet = {
    val v = head(e)
    if(etov.apply(Mesh.internal(e), Mesh.HEAD) == Mesh.internal(v)) {
      IndexSetImpl(etoc, e)
    } else {
      CWIndexSetImpl(etoc, e)
    }
  }
  
  def cellsCW(e: Int): MeshSet = {
    val v = head(e)
    if(etov.apply(Mesh.internal(e), Mesh.TAIL) == Mesh.internal(v)) {
      IndexSetImpl(etoc, e)
    } else {
      CWIndexSetImpl(etoc, e)
    }
  }

  def edgesMesh: MeshSet = MeshSetImpl(nedges)
  def edgesVertex(e: Int): MeshSet = IndexSetImpl(vtoe, e)
  def edgesFace(e: Int): MeshSet = IndexSetImpl(ftoe, e)
  def edgesCell(e: Int): MeshSet = IndexSetImpl(ctoe, e)

  def edgesCCW(e: Int): MeshSet = {
    val c = outside(e)
    if(ftoc.apply(Mesh.internal(e), Mesh.OUTSIDE) == Mesh.internal(c)) {
      IndexSetImpl(ftoe, e)
    } else {
      CWIndexSetImpl(ftoe, e)
    }
  }
  
  def edgesCW(e: Int): MeshSet = {
    val c = outside(e)
    if(ftoc.apply(Mesh.internal(e), Mesh.INSIDE) == Mesh.internal(c)) {
      IndexSetImpl(ftoe, e)
    } else {
      CWIndexSetImpl(ftoe, e)
    }
  }
  
  def facesMesh: MeshSet = MeshSetImpl(nfaces)
  def facesVertex(e: Int): MeshSet = IndexSetImpl(vtof, e)
  def facesEdge(e: Int): MeshSet = IndexSetImpl(etof, e)
  def facesCell(e: Int): MeshSet = IndexSetImpl(ctof, e)

  def facesCCW(e: Int): MeshSet = {
    val v = head(e)
    if(etov.apply(Mesh.internal(e), Mesh.HEAD) == Mesh.internal(v)) {
      IndexSetImpl(etof, e)
    } else {
      CWIndexSetImpl(etof, e)
    }
  }
  
  def facesCW(e: Int): MeshSet = {
    val v = head(e)
    if(etov.apply(Mesh.internal(e), Mesh.TAIL) == Mesh.internal(v)) {
      IndexSetImpl(etoc, e)
    } else {
      CWIndexSetImpl(etoc, e)
    }
  }
  
  def face(e: Int, i: Int): Int = etof.apply(Mesh.internal(e), i)

  def head(e: Int): Int = etov.apply(Mesh.internal(e), if(Mesh.reversed(e)) 1 else 0)
  def tail(e: Int): Int = etov.apply(Mesh.internal(e), if(Mesh.reversed(e)) 0 else 1)

  def outside(e: Int): Int = ftoc.apply(Mesh.internal(e), if(Mesh.reversed(e)) 1 else 0)
  def inside(e: Int): Int = ftoc.apply(Mesh.internal(e), if(Mesh.reversed(e)) 0 else 1)

  def towardsEdgeVertex(e: Int, v: Int): Int = {
    val facing = Mesh.internal(etov.apply(Mesh.internal(e), Mesh.HEAD)) == Mesh.internal(v)
    if(facing) e else Mesh.flip(e)
  }

  def towardsFaceCell(e: Int, c: Int): Int = {
    val facing = Mesh.internal(ftoc.apply(Mesh.internal(e), Mesh.OUTSIDE)) == Mesh.internal(c)
    if(facing) e else Mesh.flip(e)
  }
  
  def label[T](ld: LabelData, url: String) : LabelField[T] = {
    ld.data.get(url) match {
      case Some(data) => new LabelFieldImpl[T](data, ld.fns.get(url).getOrElse(null))
      case None => null
    }
  }
  
  def labelCells[T](url: String) : LabelField[T] = label(cellData, url)
  def labelEdges[T](url: String) : LabelField[T] = label(edgeData, url)
  def labelFaces[T](url: String) : LabelField[T] = label(faceData, url)
  def labelVertices[T](url: String) : LabelField[T] = label(vertexData, url)

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
