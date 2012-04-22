package ppl.dsl.deliszt.datastruct.scala

import collection.mutable.{Map, HashMap}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class LabelData[MO<:MeshObj] {
  val data: Map[String,Array[Object]] = new HashMap[String,Array[Object]]()
  val fns: Map[String,Object => Object] = new HashMap[String,Object => Object]()
} 

trait MeshObj {
  def id : Int
}

trait Vertex extends MeshObj
trait Edge extends MeshObj
trait Face extends MeshObj
trait Cell extends MeshObj
 
class Mesh {
  implicit val vertexData = new LabelData[Vertex]
  
  var nvertices : Int = 0
	var nedges : Int = 0
	var nfaces : Int = 0
	var ncells : Int = 0

	var vtov : CRS = null
	var vtoe : CRS = null
	var vtof : CRS = null
	var vtoc : CRS = null

	var etov : CRS = null
	var etof : CRS = null
	var etoc : CRS = null

	var ftov : CRS = null
	var ftoe : CRS = null
	var ftoc : CRS = null

	var ctov : CRS = null
	var ctoe : CRS = null
	var ctof : CRS = null
	var ctoc : CRS = null
}