package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._
import collection.mutable.{HashMap}

class Graph[VD:Manifest,ED:Manifest] {
  // TODO: we are storing a lot of data here. investigate reducing the footprint vs. performance.
  var _edgeToVertices = HashMap[Edge[VD,ED], (Vertex[VD,ED], Vertex[VD,ED])]()
  // var verticesToEdges = HashMap[(V, V), E]()

  // this is used only during construction (before frozen), for fast sorting
  var _adjacencies = HashMap[Vertex[VD,ED], List[(Edge[VD,ED], Vertex[VD,ED])]]()

  // this is used only after construction (after frozen), for fast access
  // Map from vertex to id
  val _vertexIds = HashMap[Vertex[VD,ED], Int]()

  var _vertices : Array[Vertex[VD,ED]] = null
  var _edges : Array[Edge[VD,ED]] = null

  var _vertexEdges : Array[DenseVector[Edge[VD,ED]]] = null
  var _neighbors : Array[DenseVector[Vertex[VD,ED]]] = null
  var _neighborsSelf : Array[DenseVector[Vertex[VD,ED]]] = null

  var _frozen = false
}