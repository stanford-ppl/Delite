package ppl.dsl.optiml.datastruct.scala

import collection.mutable.{HashMap, ArrayBuffer, Map}

class UndirectedGraphImpl[V <: Vertex,E <: Edge]()(implicit mV: ClassManifest[V], mE: ClassManifest[E]) extends Graph[V,E] {
  // TODO: we are storing a lot of data here. investigate reducing the footprint vs. performance.
  protected var edgeToVertices = Map[E, (V, V)]()
  //protected var verticesToEdges = Map[(V, V), E]()

  // this is used only during construction (before frozen), for fast sorting
  protected var adjacencies = HashMap[V, List[(E, V)]]()

  // this is used only after construction (after frozen), for fast access
  // Map from vertex to id
  protected val vertexIds = HashMap[V, Int]()

  protected var _vertices = Vector[V]()
  protected var _edges = Vector[E]()
  
  protected var vertexEdges = Vector[Edges[E]]()
  protected var neighbors = Vector[Vertices[V]]()  

  var _frozen = false

  def vertices = {
    if(_frozen) {
      new VerticesImpl(_vertices.toArray)
    }
    else {
      new VerticesImpl(adjacencies.keySet.toArray)
    }
  }

  def edges = {
    if(_frozen) {
      new EdgesImpl(_edges.toArray)
    }
    else {
      new EdgesImpl(edgeToVertices.keySet.toArray)
    }
  }

  //def adjacent(a: V, b: V) = verticesToEdges.contains((a, b))

  def containsEdge(e: E) = edgeToVertices.contains(e)

  def containsVertex(v: V) = {
    if (!_frozen) adjacencies.contains(v)
    else vertexInfo.contains(v)
  }

  // only available before finalization
  def addVertex(v: V) = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    if (!adjacencies.contains(v)) {
      adjacencies(v) = List()
    }
  }
  
  def addEdge(e: E, a: V, b: V) = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    if (!edgeToVertices.contains(e)) {
      //verticesToEdges((a, b)) = e
      adjacencies(a) ::= ((e, b))
      adjacencies(b) ::= ((e, a))
      edgeToVertices(e) = ((a,b))
    }
  }

  /*def removeEdge(a: V, b: V): Unit = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    if (adjacencies.contains((a, b))) {
      val e = verticesToEdges((a, b))

      verticesToEdges.remove(edgeToVertices(e))
      adjacencies(a) -= ((e, b))
      adjacencies(b) -= ((e, a))
      edgeToVertices.remove(e)
    }
  } */

  // finalizes the graph structure; we may want to rename this to clarify the semantics.
  def freeze() : Unit = {
    for(v <- adjacencies.keySet) {
      vertexIds(v) = _vertices.length
      _vertices += v
    }

    val sorted = _vertices map {adjacencies(_).sortBy{case(e, v) => vertexIds(v)}}
    vertexEdges = sorted map {new EdgesImpl((_ map {_._1}).toArray)}
    neighbors = sorted map {new VerticesImpl((_ map {_._2}).toArray)}

    adjacencies = null
    _frozen = true
  }

  def frozen = {
    _frozen
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