package ppl.apps.interop

import collection.mutable.{HashMap, Map, MutableList}
import java.io._
import Types._

/**
 * Directed/undirected multi-graph
 */

class Graph(val isDirected: Boolean)  {
  /** Flag indicating whether the graph can be mutated or not */
  var immutable = false

  /** Immutable graph structures */
  // graph nodes
  protected var _nodes : Array[Node] = null
  // out neighbors of each node
  protected var nodeOutNeighbors : Array[GIterable[Node]] = null
  // in neighbors of each node
  protected var nodeInNeighbors : Array[GIterable[Node]] = null

  /** Basic graph lookup operations (mutable/immutable graphs) */
  def nodes = new GIterable[Node](_nodes)
  def numNodes: Int = _nodes.length

  // only available after construction finalization
  def outNeighbors(n: Node) = nodeOutNeighbors(n)
  def OutNbrs(n: Node) = outNeighbors(n)
  def HasOutNbr(from: Node, to: Node): Boolean = outNeighbors(from).contains(to)
  def inNeighbors(n: Node) = nodeInNeighbors(n)
  def InNbrs(n: Node) = inNeighbors(n)

}

object Graph {

  def loadGraph(fileName: String): Graph = {
    val g = new Graph(true)
    val nodemap = new HashMap[Int, Node]()
    var xfs = new BufferedReader(new FileReader(fileName))
    var line = xfs.readLine()
    var nodeIdx = 0
    while (line != null) {
      if(line.charAt(0) != '#') {
        val edges = line.split("\\t").map(_.toInt)
        if(!nodemap.contains(edges(0))) {
          nodemap(edges(0)) = nodeIdx
          nodeIdx += 1
        }
        if(!nodemap.contains(edges(1))) {
          nodemap(edges(1)) = nodeIdx
          nodeIdx += 1
        }
      }
      line = xfs.readLine()
    }

    xfs = new BufferedReader(new FileReader(fileName))
    line = xfs.readLine()
    val nodes = new Array[Node](nodemap.size)
    val outneighbors = new Array[GIterable[Node]](nodemap.size)
    val inneighbors = new Array[GIterable[Node]](nodemap.size)
    for(i <- 0 until nodemap.size) {
      nodes(i) = i
      outneighbors(i) = new GIterable[Node]()
      inneighbors(i) = new GIterable[Node]()
    }
    while (line != null) {
      if(line.charAt(0) != '#') {
        val edges = line.split("\\t").map(_.toInt)
        outneighbors(nodemap(edges(0))).insert(nodemap(edges(1)))
        inneighbors(nodemap(edges(1))).insert(nodemap(edges(0)))
      }
      line = xfs.readLine()
    }
    g._nodes = nodes
    g.nodeOutNeighbors = outneighbors
    g.nodeInNeighbors = inneighbors
    g
  }

  def loadGraph(fromArray: Array[(Int,Int)]): Graph = {
    val g = new Graph(true)
    val nodemap = new HashMap[Int, Node]()
    var nodeIdx = 0
    for(i <- 0 until fromArray.length) {
      if(!nodemap.contains(fromArray(i)._1)) {
        nodemap(fromArray(i)._1) = nodeIdx
        nodeIdx += 1
      }
      if(!nodemap.contains(fromArray(i)._2)) {
        nodemap(fromArray(i)._2) = nodeIdx
        nodeIdx += 1
      }
    }   
    
    val nodes = new Array[Node](nodemap.size)
    val outneighbors = new Array[GIterable[Node]](nodemap.size)
    val inneighbors = new Array[GIterable[Node]](nodemap.size)
    for(i <- 0 until nodemap.size) {
      nodes(i) = i
      outneighbors(i) = new GIterable[Node]()
      inneighbors(i) = new GIterable[Node]()
    }
    for(i <- 0 until fromArray.length) {
      outneighbors(nodemap(fromArray(i)._1)).insert(nodemap(fromArray(i)._2))
      inneighbors(nodemap(fromArray(i)._2)).insert(nodemap(fromArray(i)._1))
    }
    g._nodes = nodes
    g.nodeOutNeighbors = outneighbors
    g.nodeInNeighbors = inneighbors
    g
  }
}
