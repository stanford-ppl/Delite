package ppl.dsl.optigraph

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * OptiGraph (front-end) data types 
*/

/* Graph types */

trait Graph {
  // directed
  type DGraph = Graph
  // undirected
  type UGraph = Graph
}

trait Node
trait Edge

/* Properties (for immutable graphs only) */

/** 
 * Used to associate data with the nodes in the graph
 * Note: properties can be associated only with immutable graph instances
 */
trait NodeProperty[T] extends DeliteCollection[T] {
  type NP[T] = NodeProperty[T]
}
/** 
 * An EdgeProperty object is used to associate data with graph edges
 * Note: properties can be associated only with immutable graph instances
 */
trait EdgeProperty[T] extends DeliteCollection[T] {
  type EP[T] = EdgeProperty[T]
}

/* Collection types */

/** Unordered collection of unique elements */
trait GSet[T] {
  type NodeSet = GSet[Node]
  type NS = GSet[Node]
  type EdgeSet = GSet[Edge]
  type ES = GSet[Edge]
}
/** Ordered collection of unique elements */
trait GOrder[T] {
  type NodeOrder = GOrder[Node]
  type NO = GOrder[Node]
  type EdgeOrder = GOrder[Edge]
  type EO = GOrder[Edge]
}
/** Ordered collection of elements (with duplicates possible) */
trait GSeq[T] {
  type NodeSeq = GSeq[Node]
  type NS = GSeq[Node]
  type EdgeSeq = GSeq[Edge]
  type ES = GSeq[Edge]
}

/* Other */

/** Iterable/reduceable collection of graph items (nodes or edges) */
trait GIterable[T] extends DeliteCollection[T]

/** Can be used in reduction assignments (in a parallel context) */
trait Reduceable[T]

/** Allows assignment deferment */
trait Deferrable[T]


//class GraphExpWrapper {
  //type NodeExp
  //val x: Rep[GraphExp]
  //class Node
  //class Edge
//}