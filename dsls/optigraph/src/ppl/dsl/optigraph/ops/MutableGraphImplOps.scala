package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{ArrayOps, BaseExp, Base, HashMapOps, SetOps}
import scala.collection.mutable.{HashMap, MutableList}
import ppl.dsl.optigraph.{Edge, GIterable, MutableEdge, MutableGraph, MutableNode, Graph, Node}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}
import ppl.delite.framework.datastructures.DeliteArray

trait MutableGraphImplOps { this: OptiGraph =>
  def mutable_graph_nodes_impl(mg: Rep[MutableGraph]): Rep[GIterable[MutableNode]]
  def mutable_graph_edges_impl(mg: Rep[MutableGraph]): Rep[GIterable[MutableEdge]]
  def mutable_graph_numnodes_impl(mg: Rep[MutableGraph]): Rep[Int]
  def mutable_graph_numedges_impl(mg: Rep[MutableGraph]): Rep[Int]
  def mutable_graph_addnode_impl(mg: Rep[MutableGraph]): Rep[MutableNode]
  def mutable_graph_addedge_impl(mg: Rep[MutableGraph], from: Rep[MutableNode], to: Rep[MutableNode]): Rep[MutableEdge]
  def mutable_graph_snapshot_impl(mg: Rep[MutableGraph]): Rep[Graph]
}

trait MutableGraphImplOpsStandard extends MutableGraphImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def mutable_graph_nodes_impl(mg: Rep[MutableGraph]): Rep[GIterable[MutableNode]] = {
    val d = mutable_graph_adj_map(mg)
    GIterable[MutableNode](d.keySet.toArray.asInstanceOf[Rep[DeliteArray[MutableNode]]])
  }

  def mutable_graph_edges_impl(mg: Rep[MutableGraph]): Rep[GIterable[MutableEdge]] = {
    val d = mutable_graph_edge_list(mg)
    GIterable[MutableEdge](d.toArray.asInstanceOf[Rep[DeliteArray[MutableEdge]]])
  }

  def mutable_graph_numnodes_impl(mg: Rep[MutableGraph]): Rep[Int] = {
    val d = mutable_graph_adj_map(mg)
    d.keySet.size
  }

  def mutable_graph_numedges_impl(mg: Rep[MutableGraph]): Rep[Int] = {
    // no "size" method for Lists in LMS :(
    val d = mutable_graph_edge_list(mg).toArray.asInstanceOf[Rep[DeliteArray[MutableEdge]]]
    d.length
  }

  def mutable_graph_addnode_impl(mg: Rep[MutableGraph]): Rep[MutableNode] = {
    val n = MutableNode(mg)
    val d = mutable_graph_adj_map(mg)
    hashmap_unsafe_update(d, n, List[(MutableEdge,MutableNode)]())
    if (mutable_graph_is_directed(mg)) {
      val dr = mutable_graph_adj_map_reversed(mg)
      hashmap_unsafe_update(dr, n, List[(MutableEdge,MutableNode)]())
    }
    n
  }

  def mutable_graph_addedge_impl(mg: Rep[MutableGraph], from: Rep[MutableNode], to: Rep[MutableNode]): Rep[MutableEdge] = {
    val e : Rep[MutableEdge] = MutableEdge(mg, from, to)
    // Add this edge to the edge list by cons-ing the new edge
    // with the existing edge list
    val el = mutable_graph_edge_list(mg)
    val x = e :: el
    mutable_graph_set_edge_list(mg, x)

    val d = mutable_graph_adj_map(mg)
    val old_adj_for_node = d.apply(e.From)
    hashmap_unsafe_update(d, e.From, (e, e.To) :: old_adj_for_node)

    if (mutable_graph_is_directed(mg)) {
      val dr = mutable_graph_adj_map_reversed(mg)
      val old_adj_for_node_reversed = dr.apply(e.To)
      hashmap_unsafe_update(dr, e.To, (e, e.From) :: old_adj_for_node_reversed)
    }

    e
  }

  def mutable_graph_snapshot_impl(mg: Rep[MutableGraph]): Rep[Graph] = {
    //Create a new graph instance, retaining the directedness
    val snapshot = Graph(mutable_graph_is_directed(mg))

    //Make a structural copy of the graph, preserving appropriate node relations
    graph_set_raw_nodes(snapshot, DeliteArray[Node](mg.NumNodes).unsafeImmutable)
    graph_set_raw_edges(snapshot, DeliteArray[Edge](mg.NumEdges).unsafeImmutable)
    val nodesToCopy = mutable_graph_adj_map(mg.unsafeImmutable).keySet.toArray
    val edgesToCopy = mutable_graph_edge_list(mg.unsafeImmutable).toArray
    //Assign a correspondence with copied nodes and edges
    val nodeToCopyMap = HashMap[MutableNode, Node]()
    val edgeToCopyMap = HashMap[MutableEdge, Edge]()

    for (i <- 0 until mg.NumNodes) {
      val new_node = Node(snapshot.unsafeImmutable)
      node_set_id(new_node, i)
      darray_unsafe_update(graph_raw_nodes(snapshot), i, new_node)
      hashmap_unsafe_update(nodeToCopyMap, nodesToCopy(i), new_node)
    }

    for (i <- 0 until mg.NumEdges) {
      val e = edgesToCopy.apply(i) //MutableEdge
      val new_edge = Edge(snapshot.unsafeImmutable, nodeToCopyMap(e.From).unsafeImmutable, nodeToCopyMap(e.To).unsafeImmutable)
      edge_set_id(new_edge, i.unsafeImmutable)
      darray_unsafe_update(graph_raw_edges(snapshot), i, new_edge)
      hashmap_unsafe_update(edgeToCopyMap, e, new_edge)
    }

    // Mother of God, there has to be a better way. Maybe if we kept track of manifests?
    val sorted = nodesToCopy map {mutable_graph_adj_map(mg.unsafeImmutable).apply(_).sortBy{case t:Rep[(MutableEdge,MutableNode)] => nodeToCopyMap(t._2.asInstanceOf[Rep[MutableNode]]).Id}}
    val outedges = sorted map {(l: Rep[List[(MutableEdge,MutableNode)]]) => (GIterable[Edge]( (l.map({(t: Rep[(MutableEdge,MutableNode)]) => edgeToCopyMap(t._1.asInstanceOf[Rep[MutableEdge]]).unsafeImmutable})).toArray.asInstanceOf[Rep[DeliteArray[Edge]]] )).unsafeImmutable}
    graph_set_raw_node_out_edges(snapshot, outedges.asInstanceOf[Rep[DeliteArray[GIterable[Edge]]]])
    val outneighbors = sorted map {(l: Rep[List[(MutableEdge,MutableNode)]]) => (GIterable[Node]((l map {(t: Rep[(MutableEdge,MutableNode)]) => nodeToCopyMap(t._2.asInstanceOf[Rep[MutableNode]]).unsafeImmutable}).toArray.asInstanceOf[Rep[DeliteArray[Node]]])).unsafeImmutable}
    graph_set_raw_node_out_neighbors(snapshot, outneighbors.asInstanceOf[Rep[DeliteArray[GIterable[Node]]]])

    if (mutable_graph_is_directed(mg)) {
      val sortedReversed = nodesToCopy map {mutable_graph_adj_map_reversed(mg.unsafeImmutable).apply(_).sortBy{case t:Rep[(MutableEdge,MutableNode)] => nodeToCopyMap(t._2.asInstanceOf[Rep[MutableNode]]).Id}}
      val inedges = sortedReversed map {(l: Rep[List[(MutableEdge,MutableNode)]]) => (GIterable[Edge]((l map {(t: Rep[(MutableEdge,MutableNode)]) => edgeToCopyMap(t._1.asInstanceOf[Rep[MutableEdge]]).unsafeImmutable}).toArray.asInstanceOf[Rep[DeliteArray[Edge]]])).unsafeImmutable}
      graph_set_raw_node_in_edges(snapshot, inedges.asInstanceOf[Rep[DeliteArray[GIterable[Edge]]]])
      val inneighbors = sortedReversed map {(l: Rep[List[(MutableEdge,MutableNode)]]) => (GIterable[Node]((l map {(t: Rep[(MutableEdge,MutableNode)]) => nodeToCopyMap(t._2.asInstanceOf[Rep[MutableNode]]).unsafeImmutable}).toArray.asInstanceOf[Rep[DeliteArray[Node]]])).unsafeImmutable}
      graph_set_raw_node_in_neighbors(snapshot, inneighbors.asInstanceOf[Rep[DeliteArray[GIterable[Node]]]])
    }

    snapshot
  }

}