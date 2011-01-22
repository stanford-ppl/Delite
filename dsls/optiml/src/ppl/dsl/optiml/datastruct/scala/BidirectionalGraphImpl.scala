package ppl.dsl.optiml.datastruct.scala

import scala.collection.mutable.HashMap

class BidirectionalGraphImpl[V <: Vertex,E <: Edge]()(implicit mV: ClassManifest[V], mE: ClassManifest[E]) extends Graph[V,E] {
  // TODO: we are storing a lot of data here. investigate reducing the footprint vs. performance.
  //protected var verticesToEdges = Map[(V, V), E]()

  // Map from vertex to id
  protected val vertexIds = HashMap[V, Int]()

  protected var _vertices = Vector[V]()
  protected var _edges = Vector[E]()

  // this is used only during construction (before frozen), for fast structure modifications
  protected var inList = Vector[List[(Int, Int)]]()
  protected var outList = Vector[List[(Int, Int)]]()

  // For fast access after frozen
  protected var inEdges = Vector[Edges[E]]()
  protected var outEdges = Vector[Edges[E]]()
  protected var inOutEdges = Vector[InOutEdges[E]]()
  protected var neighbors = Vector[Vector[V]]()

  var _frozen = false

  def vertices = {
    new VerticesImpl(_vertices.toArray)
  }

  def edges = {
    new EdgesImpl(_edges.toArray)
  }

  def containsVertex(v: V) = {
    vertexIds.contains(v)
  }

  // only available before finalization
  def addVertex(v: V) = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    if (!vertexIds.contains(v)) {
      val id = _vertices.length
      vertexIds(v) = id
      _vertices += v
      inList += List[(Int,Int)]()
      outList += List[(Int,Int)]()
    }
  }
  
  def addEdge(e1: E, e2: E, a: V, b: V) = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    assert(vertexIds.contains(a))
    assert(vertexIds.contains(b))

    val eId1 = _edges.length
    _edges += e1

    val eId2 = eId1 + 1
    _edges += e2

    val aId = vertexIds(a)
    val bId = vertexIds(b)

    inList(aId) ::= ((eId2, bId))
    outList(aId) ::= ((eId1, bId))

    inList(bId) ::= ((eId2, aId))
    outList(bId) ::= ((eId1, aId))
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
    for(i <- 0 until inList.length) {
      inVector = inList(i) map {edges(_._1)}
      inEdges(v) = new EdgesImpl(in)

      outVector = outList(i) map {edges(_._1)}
      outEdges(v) = new EdgesImpl(in)
      
      inOutEdges = new InOutEdgesImpl((inVector zip outVector {case (in, out) => (in, out)}).toArray)
      neighbors(i) = new VerticesImpl((inList(i) map {vertices(_._2)}).toArray)
    }

    vertexEdgeList = null

    _frozen = true
  }

  // only available after finalization
  def neighborsOf(v: V) = {
    if (!_frozen) throw new RuntimeException("Graph has not been finalized")

    val id = vertexIds.getOrElse(v, -1)

    if (id == -1) {
      new VerticesImpl[V](0)
    }
    else {
      val start = neighborIds(id)
      val end = neighborIds(id + 1)
     //new VerticesImpl[V]( neighbors.slice(start, end) map {vertices(_)} )
    }
  }

  def edgesOf(v: V) = {
    if (!_frozen) throw new RuntimeException("Graph has not been finalized")

    val id = vertexIds.getOrElse(v, -1)

    if (id == -1) {
      new EdgesImpl[V](0)
    }
    else {
      val start = neighborIds(id)
      val end = neighborIds(id + 1)
      new VerticesImpl[V](neighborIds.slice(start:end) map{edges(_)} )
    }
  }

}