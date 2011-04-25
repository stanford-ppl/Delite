package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Mesh {
  var mesh : Mesh = null
}

class Mesh {
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