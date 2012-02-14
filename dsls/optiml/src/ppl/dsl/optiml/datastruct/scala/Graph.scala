package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._
import collection.mutable.{HashMap}

trait MessageData // placeholder

class Graph {
  // TODO: we are storing a lot of data here. investigate reducing the footprint vs. performance.
  var _edgeToVertices = HashMap[Edge, (Vertex, Vertex)]()
  // var verticesToEdges = HashMap[(V, V), E]()

  // this is used only during construction (before frozen), for fast sorting
  var _adjacencies = HashMap[Vertex, List[(Edge, Vertex)]]()

  // this is used only after construction (after frozen), for fast access
  // Map from vertex to id
  val _vertexIds = HashMap[Vertex, Int]()

  var _vertices : Array[Vertex] = null
  var _edges : Array[Edge] = null

  var _vertexEdges : Array[DenseVector[Edge]] = null
  var _neighbors : Array[DenseVector[Vertex]] = null
  var _neighborsSelf : Array[DenseVector[Vertex]] = null

  var _frozen = false
}