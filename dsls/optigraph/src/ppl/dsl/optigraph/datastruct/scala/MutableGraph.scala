package ppl.dsl.optigraph.datastruct.scala

import collection.mutable.{HashMap}
import collection.immutable.{List}
import java.io._

/**
 * Directed/undirected mutable multi-graph
 */

class MutableGraph(val isDirected: Boolean)  {

  /** Mutable graph structures */
  var adjMap = HashMap[MutableNode, List[(MutableEdge,MutableNode)]]()
  var adjMapReversed = HashMap[MutableNode, List[(MutableEdge,MutableNode)]]()
  var edgeList = List[MutableEdge]()
  /*

  /** Basic graph lookup operations (mutable/immutable graphs) */
  def nodes = {
    new GIterable[Node](adjMap.keySet.toArray)
  }

  def edges = {
    new GIterable[Edge](edgeList.toArray)
  }

  def numEdges: Int = {
    edgeList.size
  }
  def numNodes: Int = {
    adjMap.keySet.size
  }

  /** Graph construction operations */
  def addNode: Node = {
    val n:Node = new Node(this)
    adjMap(n) = new MutableList[(Edge, Node)]()

    if(isDirected) {
      adjMapReversed(n) = new MutableList[(Edge, Node)]()
    }
    n
  }

  // note: multiple edges between same nodes are allowed
  def addEdge(from: Node, to: Node): Edge = {
    val e:Edge = new Edge(this, from, to)

    edgeList += e
    adjMap(e.from) += Pair(e, e.to)
    if(isDirected) {
      adjMapReversed(e.to) += Pair(e, e.from)
    }
    e
  }

  // returns an immutable snapshot of the current graph
  def snapshot() : Graph = {
    val immutableSnapshot = new Graph(isDirected)
    immutableSnapshot.immutable = true
    // make a structural copy of the graph
    // preserving the appropriate node relations
    immutableSnapshot._nodes = new Array[Node](numNodes)
    immutableSnapshot._edges = new Array[Edge](numEdges)
    val nodesToCopy = this.adjMap.keySet.toArray
    val edgesToCopy = this.edgeList.toArray
    // assign a correspondence with copied nodes and edges
    val nodeToCopyMap = HashMap[Node, Node]()
    val edgeToCopyMap = HashMap[Edge, Edge]()
    var i = 0
    while(i < numNodes) {
      immutableSnapshot._nodes(i) = new Node(immutableSnapshot)
      immutableSnapshot._nodes(i).id = i
      nodeToCopyMap(nodesToCopy(i)) = immutableSnapshot._nodes(i)
      i += 1
    }
    i = 0
    while(i < numEdges) {
      val e = edgesToCopy(i)
      immutableSnapshot._edges(i) = new Edge(immutableSnapshot, nodeToCopyMap(e.from), nodeToCopyMap(e.to))
      immutableSnapshot._edges(i).id = i
      edgeToCopyMap(e) = immutableSnapshot._edges(i)
      i += 1
    }

    val sorted = nodesToCopy map {this.adjMap(_).sortBy{case (e,n) => nodeToCopyMap(n).id}}
    immutableSnapshot.nodeOutEdges = sorted map {(l: MutableList[(Edge,Node)]) => new GIterable[Edge]((l map {(t: (Edge, Node)) => edgeToCopyMap(t._1)}).toArray)}
    immutableSnapshot.nodeOutNeighbors = sorted map {(l: MutableList[(Edge,Node)]) => new GIterable[Node]((l map {(t: (Edge, Node)) => {nodeToCopyMap(t._2)}}).toArray)}

    if(isDirected) {
      val sortedReversed = nodesToCopy map {this.adjMapReversed(_).sortBy{case (e,n) => nodeToCopyMap(n).id}}
      immutableSnapshot.nodeInEdges = sortedReversed map {(l: MutableList[(Edge,Node)]) => new GIterable[Edge]((l map {(t: (Edge, Node)) => edgeToCopyMap(t._1)}).toArray)}
      immutableSnapshot.nodeInNeighbors = sortedReversed map {(l: MutableList[(Edge,Node)]) => new GIterable[Node]((l map {(t: (Edge, Node)) => nodeToCopyMap(t._2)}).toArray)}
    }

    immutableSnapshot
  }
  */
}