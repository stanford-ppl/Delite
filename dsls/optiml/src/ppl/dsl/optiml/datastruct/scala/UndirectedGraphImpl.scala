package ppl.dsl.optiml.datastruct.scala

import collection.mutable.{HashMap, ArrayBuffer, Map}

class UndirectedGraphImpl[V <: Vertex,E <: Edge]()(implicit mV: ClassManifest[V], mE: ClassManifest[E]) extends Graph[V,E] {
  // TODO: we are storing a lot of data here. investigate reducing the footprint vs. performance.
  protected var edgeToVertices = Map[E, (V, V)]()
  protected var verticesToEdges = Map[(V, V), E]()

  // Map from vertex to id
  protected val vertexIds = HashMap[V, Int]()

  protected var _vertices = Vector[V]()
  protected var _edges = Vector[E]()

  // this is used only during construction (before frozen), for fast sorting
  protected var adjacencies = Vector[List[(Int, Int)]]()

  // this is used only after construction (after frozen), for fast access
  protected var vertexEdges = Vector[Edges[E]]()
  protected var neighbors = Vector[Vertices[V]]()  

  var _frozen = false

  def vertices = {
    new VerticesImpl(_vertices.toArray)
  }

  def edges = {
    new EdgesImpl(_edges.toArray)
  }

  def adjacent(a: V, b: V) = verticesToEdges.contains((a, b))

  def containsEdge(e: E) = edgeToVertices.contains(e)

  def containsVertex(v: V) = {
    if (!_frozen) adjacencies.contains(v)
    else vertexInfo.contains(v)
  }

  // only available before finalization
  def addVertex(v: V) = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    if (!vertexIds.contains(v)) {
      val id = _vertices.length
      vertexIds(v) = id
      _vertices += v
      adjacencies += List[(Int,Int)]()
    }
  }
  
  def addEdge(e: E, a: V, b: V) = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    assert(vertexIds.contains(a))
    assert(vertexIds.contains(b))

    val eId = _edges.length
    _edges += e

    val aId = vertexIds(a)
    val bId = vertexIds(b)

    adjacencies(aId) ::= ((eId, bId))
    adjacencies(bId) ::= ((eId, aId))
  }

  def removeEdge(a: V, b: V): Unit = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    assert(vertexIds.contains(a))
    assert(vertexIds.contains(b))

    val aId = vertexIds(a)
    val bId = vertexIds(b)
  }

  // finalizes the graph structure; we may want to rename this to clarify the semantics.
  def freeze() : Unit = {
    val sorted = adjacencies map {_.sortBy{case(e, v) => v}}
    vertexEdges = sorted map {new EdgesImpl((_ map {_._1}).toArray)}
    neighbors = sorted map {new VerticesImpl((_ map {_._2}).toArray)}

    adjacencies = null

    _frozen = true
  }

  // only available after finalization
  def neighborsOf(v: V) = {
    if (!_frozen) throw new RuntimeException("Graph has not been finalized")

    if (!vertexIds.contains(v)) {
      new VerticesImpl[V](0)
    }
    else {
      val id = vertexIds(v)
      neighbors(id)
    }
  }

  def edgesOf(v: V) = {
    if (!_frozen) throw new RuntimeException("Graph has not been finalized")

    if (!vertexIds.contains(v)) {
      new EdgesImpl[E](0)
    }
    else {
      val id = vertexIds(v)
      vertexEdges(id)
    }
  }

}