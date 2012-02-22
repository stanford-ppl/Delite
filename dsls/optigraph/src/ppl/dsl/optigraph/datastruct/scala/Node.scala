package ppl.dsl.optigraph.datastruct.scala

/**
 * Graph node
 * A node is always bound to some graph instance
 */
class Node(val g: Graph) {

  /** Node id */
  var id: Int = 0
  /** Returns the nodes that this node has edges to */
  def outNeighbors: GIterable[Node] = { g.outNeighbors(this) }
  /** Returns the nodes that have edges to this node */
  def inNeighbors: GIterable[Node] = { g.inNeighbors(this) }
  /** During BFS: returns the in-neighbors that are closer to the BFS root node
   * than the current node, in hop distance */
  def upNeighbors(visited: Array[Int]): GIterable[Node] = { g.upNeighbors(this, visited) }
   /** During BFS: returns the out-neighbors that are farther to the BFS root node
   * than the current node, in hop distance */
  def downNeighbors(visited: Array[Int]): GIterable[Node] = { g.downNeighbors(this, visited) }
  /** Returns the outgoing edges of this node */
  def outEdges: GIterable[Edge] = { g.outEdges(this) }
  /** Returns the incoming edges of this node */
  def inEdges: GIterable[Edge] = { g.inEdges(this) }
  /** During BFS: edges from upNeighbors */
  def upEdges(visited: Array[Int]): GIterable[Edge] = { g.upEdges(this, visited) }
  /** During BFS: edges to downNeighbors */
  def downEdges(visited: Array[Int]): GIterable[Edge] = { g.downEdges(this, visited) }
  
  def numOutNeighbors: Int = { g.outNeighbors(this).length }
  def numInNeighbors: Int = { g.inNeighbors(this).length }
  def outDegree: Int = { g.outDegree(this) }
  def inDegree: Int = { g.inDegree(this) }
}