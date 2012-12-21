package ppl.dsl.optigraph.datastruct.scala

import collection.mutable.{HashMap}
import scala.util.Random
import java.io._

/**
 * Directed/undirected multi-graph
 */

class Graph(val isDirected: Boolean)  {
  /** Immutable graph structures */
  // graph nodes
  var _nodes : Array[Node] = new Array[Node](0)
  // graph edges
  var _edges : Array[Edge] = new Array[Edge](0)
  // out edges associated with each node
  var nodeOutEdges : Array[GIterable[Edge]] = new Array[GIterable[Edge]](0)
  // in edges associated with each node
  var nodeInEdges : Array[GIterable[Edge]] = new Array[GIterable[Edge]](0)
  // out neighbors of each node
  var nodeOutNeighbors : Array[GIterable[Node]] = new Array[GIterable[Node]](0)
  // in neighbors of each node
  var nodeInNeighbors : Array[GIterable[Node]] = new Array[GIterable[Node]](0)

  /*

  /** Basic graph lookup operations (mutable/immutable graphs) */
  def nodes = {
    new GIterable[Node](_nodes)
  }

  def edges = {
    new GIterable[Edge](_edges)
  }

  def numEdges: Int = {
    _edges.length
  }
  def numNodes: Int = {
    _nodes.length
  }

  /** Graph analysis operations (immutable graphs only) */

  def getNode(nId: Int): Node = {
    _nodes(nId)
  }

  def getEdge(eId: Int): Edge = {
    _edges(eId)
  }

  // "flip" the edge directions, to be used during traversals
  // (note: does not currently modify the actual Edge objects To/From fields)
  // TODO: ensure this is used during traversals/iterations only or update the Edges
  // to reflect a true flip
  def reverse: Graph = {
    val rg = new Graph(isDirected)

    rg._nodes = this._nodes
    rg._edges = this._edges
    rg.nodeInEdges = this.nodeOutEdges
    rg.nodeOutEdges = this.nodeInEdges
    rg.nodeInNeighbors = this.nodeOutNeighbors
    rg.nodeOutNeighbors = this.nodeInNeighbors

    rg
  }

  // only available after construction finalization
  def outNeighbors(n: Node) = {
    nodeOutNeighbors(n.id)
  }

  def inNeighbors(n: Node) = {
    //if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    //if(isDirected) {
      nodeInNeighbors(n.id)
    //} else {
      //nodeOutNeighbors(n.id)
    //}
  }

  def outEdges(n: Node) = {
    nodeOutEdges(n.id)
  }

  def inEdges(n: Node) = {
    if(isDirected) {
      nodeInEdges(n.id)
    } else {
      nodeOutEdges(n.id)
    }
  }

  def inDegree(n: Node) = {
    if(isDirected) {
      nodeInEdges(n.id)._size //TODO use giterable_raw_size(g)
    } else {
      nodeOutEdges(n.id)._size //TODO use giterable_raw_size(g)
    }
  }

  def outDegree(n: Node) = {
    nodeOutEdges(n.id)._size //TODO use giterable_raw_size(g)
  }

  def upNeighbors(n: Node, visited: Array[Int]): GIterable[Node] = {
    if(visited == null) {
      throw new RuntimeException("Operation available during BFS traversal only")
    }

    val inNbrs = inNeighbors(n)
    val upNbrs = collection.mutable.HashSet[Node]()
    var i = 0
    while (i < inNbrs._size) { //TODO use giterable_raw_size(g) and giterable_raw_apply(g, i)
      if ((visited(inNbrs._data(inNbrs._offset + i).id) < visited(n.id)) && (visited(inNbrs._data(inNbrs._offset + i).id) != 0)) {
        upNbrs.add(inNbrs._data(inNbrs._offset + i))
      }
      i += 1
    }

    new GIterable[Node](upNbrs.toArray)
  }

  def downNeighbors(n: Node, visited: Array[Int]): GIterable[Node] = {
    if(visited == null) {
      throw new RuntimeException("Operation available during BFS traversal only")
    }

    val outNbrs = outNeighbors(n)
    val downNbrs = collection.mutable.HashSet[Node]()
    var i = 0
    while (i < outNbrs._size) { //TODO use giterable_raw_size(g) and giterable_raw_apply(g, i)
      if (visited(outNbrs._data(outNbrs._offset + i).id) > visited(n.id) || visited(outNbrs._data(outNbrs._offset + i).id) == 0) {
        downNbrs.add(outNbrs._data(outNbrs._offset + i))
      }
      i += 1
    }

    new GIterable[Node](downNbrs.toArray)
  }

  def upEdges(n: Node, visited: Array[Int]): GIterable[Edge] = {
    if(visited == null) {
      throw new RuntimeException("Operation available during BFS traversal only")
    }

    val inEdgs = inEdges(n)
    val upEdges = new MutableList[Edge]()
    var i = 0
    while (i < inEdgs._size) { //TODO use giterable_raw_size(g) and giterable_raw_apply(g, i)
      if ((visited(inEdgs._data(inEdgs._offset + i).from.id) < visited(n.id)) && (visited(inEdgs._data(inEdgs._offset + i).from.id) != 0)) {
        upEdges += inEdgs._data(inEdgs._offset + i)
      }
      i += 1
    }
    new GIterable[Edge](upEdges.toArray)
  }

  def downEdges(n: Node, visited: Array[Int]): GIterable[Edge] = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")

    if(visited == null) {
      throw new RuntimeException("Operation available during BFS traversal only")
    }

    val outEdgs = outEdges(n)
    val downEdges = new MutableList[Edge]()
    var i = 0
    while (i < outEdgs._size) { //TODO use giterable_raw_size(g) and giterable_raw_apply(g, i)
      if (visited(outEdgs._data(outEdgs._offset + i).to.id) > visited(n.id) || visited(outEdgs._data(outEdgs._offset + i).to.id) == 0) {
        downEdges += outEdgs._data(outEdgs._offset + i)
      }
      i += 1
    }

    new GIterable[Edge](downEdges.toArray)
  }
}

object Graph {
  // Random graph generators
  def uniformRandomGraph(isDirected: Boolean, n: Int, m: Int, seed: Long): Graph = {
    val rand = new Random(seed);
    val G = new Graph(isDirected)
    val gNodes = new Array[Node](n)
    var i = 0
    while(i < n) {
      gNodes(i) = G.addNode
      i += 1
    }
    i = 0
    while(i < m) {
      val fromId = rand.nextInt(n)
      val toId = rand.nextInt(n);
      G.addEdge(gNodes(fromId), gNodes(toId))
      i += 1
    }
    G.snapshot()
  }

  def loadGraph_(fileName: String): Graph = {
    val fis = new FileInputStream(fileName)
    val dis = new DataInputStream(fis)
    // skip first 12 bytes (assume the node/edge ids are 32-bit)
    dis.readInt()
    dis.readInt()
    dis.readInt()

    val G = new Graph(true)
    // graph size
    val n = java.lang.Integer.reverseBytes(dis.readInt())
    val m = java.lang.Integer.reverseBytes(dis.readInt())

    // graph adjacency
    G.offsets = new Array[Int](n+1)
    val nbrs_ids = new Array[Int](m)
    val gNodes = new Array[Node](n)

    var i = 0
    while(i < n) {
      G.offsets(i) = java.lang.Integer.reverseBytes(dis.readInt())
      gNodes(i) = G.addNode
      i += 1
    }
    G.offsets(n) = java.lang.Integer.reverseBytes(dis.readInt())

    i = 0
    while(i < m) {
      nbrs_ids(i) = java.lang.Integer.reverseBytes(dis.readInt())
      i += 1
    }

    i = 0
    while(i < n) {
      val fromId = i
      var j = G.offsets(i)
      while(j < G.offsets(i+1)) {
        val toId = nbrs_ids(j)
    	G.addEdge(gNodes(fromId), gNodes(toId))
    	j += 1
      }
      i += 1
    }
    G.snapshot()
  }

   def loadGraph(fileName: String): Graph = {
    val fis = new FileInputStream(fileName)
    val dis = new DataInputStream(fis)
    // skip first 12 bytes (assume the node/edge ids are 32-bit)
    dis.readInt()
    dis.readInt()
    dis.readInt()

    val G = new Graph(true)
    G.immutable = true
    // graph size
    val n = java.lang.Integer.reverseBytes(dis.readInt())
    val m = java.lang.Integer.reverseBytes(dis.readInt())

    // graph adjacency
    G._nodes = new Array[Node](n)
    G._edges = new Array[Edge](m)
    G.offsets = new Array[Int](n+1)
    G.nbrs = new Array[Node](m)
    G.r_offsets = new Array[Int](n+1)
    G.r_nbrs = new Array[Node](m)

    var i = 0
    while(i < n) {
      G._nodes(i) = new Node(G)
      G._nodes(i).id = i
      G.offsets(i) = java.lang.Integer.reverseBytes(dis.readInt())
      //println(" i " + i + " off " + G.offsets(i))
      i += 1
    }
    G.offsets(n) = java.lang.Integer.reverseBytes(dis.readInt())

    val nbrs_ids = new Array[Int](m)
    i = 0
    while(i < m) {
      nbrs_ids(i) = java.lang.Integer.reverseBytes(dis.readInt())
      //println(" i " + i + " nbrs " + nbrs_ids(i))
      i += 1
    }

    i = 0
    val r_nbrs_pos = new Array[Int](m)
    while(i < n) {
      val fromId = i
      var j = G.offsets(i)
      while(j < G.offsets(i+1)) {
        val toId = nbrs_ids(j)
        G.nbrs(j) = G._nodes(toId)
    	G._edges(j) = new Edge(G, G._nodes(fromId), G._nodes(toId))
        G._edges(j).id = j

        // count how many in-coming edges to this node
        r_nbrs_pos(j) = G.r_offsets(toId)
        G.r_offsets(toId) += 1
    	j += 1
      }
      i += 1
    }

    // compute offsets
    var partial_sum = 0
    i = 0
    while(i < n) {
      val count = G.r_offsets(i)
      G.r_offsets(i) = partial_sum
      partial_sum += count
      i += 1
    }
    G.r_offsets(n) = partial_sum

    // compute reverse neighbors
    i = 0
    while(i < n) {
      val fromId = i
      var j = G.offsets(i)
      while(j < G.offsets(i+1)) {
        val toId = nbrs_ids(j)
        G.r_nbrs(G.r_offsets(toId) + r_nbrs_pos(j)) = G._nodes(fromId)
        //println(" toId " + toId + " j " + j + " off " +G.r_offsets(toId) + " pos " + r_nbrs_pos(j) + " r_nbrs " + G.r_nbrs(G.r_offsets(toId) + r_nbrs_pos(j)))
    	j += 1
      }
      i += 1
    }


    G.nodeInNeighbors = new Array[GIterable[Node]](n)
    G.nodeInEdges = new Array[GIterable[Edge]](n)
    G.nodeOutNeighbors = new Array[GIterable[Node]](n)
    G.nodeOutEdges = new Array[GIterable[Edge]](n)

    i = 0
    while(i < n) {
      G.nodeOutNeighbors(i) = new GIterable[Node](G.nbrs, G.offsets(i), G.offsets(i+1)-G.offsets(i))
      G.nodeInNeighbors(i) = new GIterable[Node](G.r_nbrs, G.r_offsets(i), G.r_offsets(i+1)-G.r_offsets(i))
      i += 1
    }

    i = 0
    while(i < n) {
      G.nodeOutEdges(i) = new GIterable[Edge](G._edges, G.offsets(i), G.offsets(i+1)-G.offsets(i))
      i += 1
    }
    G
  }
  */
}