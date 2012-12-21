package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, ExceptionOps, HashMapOps, ListOps, SetOps}
import scala.collection.mutable.{Set}
import scala.collection.immutable.{List}
import ppl.dsl.optigraph.{Edge, GIterable, MutableGraph, Graph, Node}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}
import ppl.delite.framework.datastructures.DeliteArray

trait GraphImplOps { this: OptiGraph =>
  def graph_nodes_impl(g: Rep[Graph]): Rep[GIterable[Node]]
  def graph_edges_impl(g: Rep[Graph]): Rep[GIterable[Edge]]
  def graph_numnodes_impl(g: Rep[Graph]): Rep[Int]
  def graph_numedges_impl(g: Rep[Graph]): Rep[Int]

  def graph_getnode_impl(g: Rep[Graph], nid: Rep[Int]): Rep[Node]
  def graph_getedge_impl(g: Rep[Graph], eid: Rep[Int]): Rep[Edge]

  def graph_flip_impl(g: Rep[Graph]): Rep[Graph]

  def graph_outneighbors_impl(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Node]]
  def graph_inneighbors_impl(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Node]]

  def graph_outedges_impl(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Edge]]
  def graph_inedges_impl(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Edge]]

  def graph_outdegree_impl(g: Rep[Graph], n: Rep[Node]): Rep[Int]
  def graph_indegree_impl(g: Rep[Graph], n: Rep[Node]): Rep[Int]

  def graph_upneighbors_impl(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]]
  def graph_downneighbors_impl(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]]

  def graph_upedges_impl(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]]
  def graph_downedges_impl(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]]
}

trait GraphImplOpsStandard extends GraphImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def graph_nodes_impl(g: Rep[Graph]): Rep[GIterable[Node]] = {
    val d = graph_raw_nodes(g)
    GIterable[Node](d).unsafeImmutable
  }

  def graph_edges_impl(g: Rep[Graph]): Rep[GIterable[Edge]] = {
    val d = graph_raw_edges(g)
    GIterable[Edge](d).unsafeImmutable
  }

  def graph_numnodes_impl(g: Rep[Graph]): Rep[Int] = {
    graph_raw_nodes(g).length
  }

  def graph_numedges_impl(g: Rep[Graph]): Rep[Int] = {
    graph_raw_edges(g).length
  }

  def graph_getnode_impl(g: Rep[Graph], nid: Rep[Int]): Rep[Node] = {
    val sz = graph_num_nodes(g)
    if (nid >= sz || nid < 0)
      fatal("Graph getNode: Node ID " + nid + " out of bounds (numNodes=" + sz + ")")
    graph_raw_nodes(g).apply(nid)
  }

  def graph_getedge_impl(g: Rep[Graph], eid: Rep[Int]): Rep[Edge] = {
    val sz = graph_num_edges(g)
    if (eid >= sz || eid < 0)
      fatal("Graph getEdge: Edge ID " + eid + " out of bounds (numEdges=" + sz + ")")
    graph_raw_edges(g).apply(eid)
  }

  def graph_flip_impl(g: Rep[Graph]): Rep[Graph] = {
    //TODO actually update the edge objects To/From fields
    val rg: Rep[Graph] = null.asInstanceOf[Rep[Graph]]
    if (graph_is_directed(g)) {
      rg = DGraph()
    } else {
      rg = UGraph()
    }

    graph_set_raw_nodes(rg, graph_raw_nodes(g))
    graph_set_raw_edges(rg, graph_raw_edges(g))

    graph_set_raw_node_in_edges(rg, graph_raw_node_out_edges(g))
    graph_set_raw_node_out_edges(rg, graph_raw_node_in_edges(g))

    graph_set_raw_node_in_neighbors(rg, graph_raw_node_out_neighbors(g))
    graph_set_raw_node_out_neighbors(rg, graph_raw_node_in_neighbors(g))

    rg
  }

  def graph_outneighbors_impl(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Node]] = {
    val sz = graph_num_nodes(g)
    val nid = n.Id
    if (nid >= sz || nid < 0)
      fatal("Graph outNeighbors: Node ID " + nid + " out of bounds (numNodes=" + sz + ")")
    graph_raw_node_out_neighbors(g).apply(nid)
  }

  def graph_inneighbors_impl(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Node]] = {
    val sz = graph_num_nodes(g)
    val nid = n.Id
    if (nid >= sz || nid < 0)
      fatal("Graph inNeighbors: Node ID " + nid + " out of bounds (numNodes=" + sz + ")")
    graph_raw_node_in_neighbors(g).apply(nid)
  }

  def graph_outedges_impl(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Edge]] = {
    val sz = graph_num_nodes(g)
    val nid = n.Id
    if (nid >= sz || nid < 0)
      fatal("Graph outEdges: Node ID " + nid + " out of bounds (numNodes=" + sz + ")")
    graph_raw_node_out_edges(g).apply(nid)
  }

  def graph_inedges_impl(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Edge]] = {
    val sz = graph_num_nodes(g)
    val nid = n.Id
    if (nid >= sz || nid < 0)
      fatal("Graph inEdges: Node ID " + nid + " out of bounds (numNodes=" + sz + ")")
    graph_raw_node_in_edges(g).apply(nid)
  }

  def graph_outdegree_impl(g: Rep[Graph], n: Rep[Node]): Rep[Int] = {
    giterable_raw_size(graph_outedges(g, n))
  }

  def graph_indegree_impl(g: Rep[Graph], n: Rep[Node]): Rep[Int] = {
    giterable_raw_size(graph_inedges(g, n))
  }

  def graph_upneighbors_impl(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]] = {
    val inNbrs = graph_inneighbors(g, n)
    val upNbrs = Set[Node]()
    var i = 0
    val sz = giterable_raw_size(inNbrs)
    while (i < sz) {
      val current_node = giterable_raw_apply(inNbrs, i)
      val current_node_id = current_node.Id
      if (visited.apply(current_node_id) < visited.apply(n.Id) && visited.apply(current_node_id) != 0) {
        upNbrs.add(current_node)
      }
      i += 1
    }
    GIterable[Node](upNbrs.toArray.asInstanceOf[Rep[DeliteArray[Node]]])
  }

  def graph_downneighbors_impl(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]] = {
    val outNbrs = graph_outneighbors(g, n)
    val downNbrs = Set[Node]()
    var i = 0
    val sz = giterable_raw_size(outNbrs)
    while (i < sz) {
      val current_node = giterable_raw_apply(outNbrs, i)
      val current_node_id = current_node.Id
      if (visited.apply(current_node_id) > visited.apply(n.Id) || visited.apply(current_node_id) == 0) {
        downNbrs.add(current_node)
      }
      i += 1
    }
    GIterable[Node](downNbrs.toArray.asInstanceOf[Rep[DeliteArray[Node]]])
  }

  def graph_upedges_impl(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]] = {
    val inEdges = graph_inedges(g, n)
    val upEdges = List[Edge]() //: Rep[List[Edge]] = null.asInstanceOf[Rep[List[Edge]]]
    var i = 0
    val sz = giterable_raw_size(inEdges)
    while (i < sz) {
      val current_edge = giterable_raw_apply(inEdges, i)
      val current_edge_id = current_edge.From.Id
      if (visited.apply(current_edge_id) < visited(n.Id) && visited.apply(current_edge_id) != 0) {
        upEdges = current_edge :: upEdges
      }
      i += 1
    }
    GIterable[Edge](upEdges.toArray.asInstanceOf[Rep[DeliteArray[Edge]]])
  }

  def graph_downedges_impl(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]] = {
    val outEdges = graph_outedges(g, n)
    val downEdges = List[Edge]() //: Rep[List[Edge]] = null.asInstanceOf[Rep[List[Edge]]]
    var i = 0
    val sz = giterable_raw_size(outEdges)
    while (i < sz) {
      val current_edge = giterable_raw_apply(outEdges, i)
      val current_edge_id = current_edge.To.Id
      if (visited.apply(current_edge_id) > visited(n.Id) || visited.apply(current_edge_id) == 0) {
        downEdges = current_edge :: downEdges
      }
      i += 1
    }
    GIterable[Edge](downEdges.toArray.asInstanceOf[Rep[DeliteArray[Edge]]])
  }
}