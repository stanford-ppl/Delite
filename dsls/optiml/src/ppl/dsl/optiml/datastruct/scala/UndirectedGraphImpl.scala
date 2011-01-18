package ppl.dsl.optiml.datastruct.scala

import collection.mutable.{ArrayBuffer,Map}

class UndirectedGraphImpl[V <: Vertex,E <: Edge]()(implicit mV: ClassManifest[V], mE: ClassManifest[E]) extends Graph[V,E] {
  // TODO: we are storing a lot of data here. investigate reducing the footprint vs. performance.
  protected var edgeToVertices = Map[E, (V, V)]()
  protected var verticesToEdges = Map[(V, V), E]()

  // this is used only during construction (before frozen), for fast sorting
  protected var vertexEdgeList = Map[V, List[(E, V)]]()
  // this is used only after construction (after frozen), for fast access
  protected var vertexInfo = Map[V, (Edges[E],Vertices[V])]()

  var _sorted = false
  var _frozen = false

  def sorted = _sorted

  def vertices = {
    if (!_frozen) new VerticesImpl(vertexEdgeList.keySet.toArray)
    else new VerticesImpl(vertexInfo.keySet.toArray)
  }

  def edges = new EdgesImpl(edgeToVertices.keySet.toArray)

  def adjacent(a: V, b: V) = verticesToEdges.contains((a, b))

  def containsEdge(e: E) = edgeToVertices.contains(e)

  def containsVertex(v: V) = {
    if (!_frozen) vertexEdgeList.contains(v)
    else vertexInfo.contains(v)
  }

  // only available before finalization
  def addVertex(v: V) = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    if (!vertexEdgeList.contains(v)) {
      vertexEdgeList(v) = List()
    }
  }
  
  def addEdge(e: E, a: V, b: V) = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    if (!edgeToVertices.contains(e)) {
      verticesToEdges((a, b)) = e
      vertexEdgeList(a) ::= ((e, b))
      vertexEdgeList(b) ::= ((e, a))
      edgeToVertices(e) = ((a,b))
    }
  }

  def removeEdge(a: V, b: V): Unit = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    if (verticesToEdges.contains((a, b))) {
      val e = verticesToEdges((a, b))
  
      verticesToEdges.remove(edgeToVertices(e))
      vertexEdgeList(a) -= ((e, b))
      vertexEdgeList(b) -= ((e, a))
      edgeToVertices.remove(e)
    }
  }

  // finalizes the graph structure; we may want to rename this to clarify the semantics.
  def sort() : Unit = {
    for((vertex, edgeList) <- vertexEdgeList) {
      vertexEdgeList(vertex) = edgeList.sortBy{case(e, v) => System.identityHashCode(v)}
    }

    for (v <- vertexEdgeList.keys) {
      vertexInfo(v) = (new EdgesImpl((vertexEdgeList(v) map (_._1)).toArray),new VerticesImpl((vertexEdgeList(v) map (_._2)).toArray))
    }

    vertexEdgeList = null

    _sorted = true
    _frozen = true
  }

  // only available after finalization
  def neighborsOf(v: V) = {
    if (!_frozen) throw new RuntimeException("Graph has not been finalized")

    if (!vertexInfo.contains(v)) {
      new VerticesImpl[V](0)
    }
    else {
      vertexInfo(v)._2
    }
  }

  def edgesOf(v: V) = {
    if (!_frozen) throw new RuntimeException("Graph has not been finalized")

    if (!vertexInfo.contains(v)) {
      new EdgesImpl[E](0)
    }
    else {
      vertexInfo(v)._1
    }
  }

}