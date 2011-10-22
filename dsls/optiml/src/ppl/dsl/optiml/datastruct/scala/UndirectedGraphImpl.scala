package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._
import collection.mutable.{HashMap, Map}

class UndirectedGraphImpl[V <: Vertex,E <: Edge]()(implicit mV: ClassManifest[V], mE: ClassManifest[E]) extends Graph[V,E] {
  // TODO: we are storing a lot of data here. investigate reducing the footprint vs. performance.
  protected var edgeToVertices = Map[E, (V, V)]()
  //protected var verticesToEdges = Map[(V, V), E]()

  // this is used only during construction (before frozen), for fast sorting
  protected var adjacencies = HashMap[V, List[(E, V)]]()

  // this is used only after construction (after frozen), for fast access
  // Map from vertex to id
  protected val vertexIds = HashMap[V, Int]()

  protected var _vertices : Array[V] = null
  protected var _edges : Array[E] = null
  
  protected var vertexEdges : Array[Edges[E]] = null
  protected var neighbors : Array[Vertices[V]] = null
  protected var neighborsSelf : Array[Vertices[V]] = null

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
      new EdgesImpl(_edges)
    }
    else {
      new EdgesImpl(edgeToVertices.keySet.toArray)
    }
  }

  //def adjacent(a: V, b: V) = verticesToEdges.contains((a, b))

  def containsEdge(e: E) = edgeToVertices.contains(e)

  def containsVertex(v: V) = {
    if (!_frozen) adjacencies.contains(v)
    else adjacencies.contains(v)
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
    _edges = edgeToVertices.keySet.toArray
    _vertices = adjacencies.keySet.toArray

    for(id <- 0 until _vertices.length) {
      vertexIds(_vertices(id)) = id
    }

    val sorted = _vertices map {adjacencies(_).sortBy{case(e, v) => vertexIds(v)}}
    vertexEdges = sorted map {(l: List[(E,V)]) => new EdgesImpl((l map {_._1}).toArray)}
    neighbors = sorted map {(l: List[(E,V)]) => new VerticesImpl((l map {_._2}).toArray)}

    neighborsSelf = _vertices map {(v: V) => val ns = v :: (adjacencies(v) map {_._2})
    new VerticesImpl(ns.sortBy{(v: V) => vertexIds(v)}.toArray)}
    
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
  
  // only available after finalization
  def neighborsSelfOf(v: V) = {
    if (!_frozen) throw new RuntimeException("Graph has not been finalized")

    if (!vertexIds.contains(v)) {
      new VerticesImpl[V](0)
    }
    else {
      val id = vertexIds(v)
      neighborsSelf(id)
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