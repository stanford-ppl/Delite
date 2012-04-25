package ppl.dsl.optigraph.datastruct.scala

/**
 * Graph edge
 * An edge is always bound to some graph instance
 */
class Edge(val g: Graph, val from: Node, val to: Node) {
  var id: Int = 0
}