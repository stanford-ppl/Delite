package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, BooleanOps, ExceptionOps}
import ppl.dsl.optigraph.{Edge, GIterable, Graph, Node}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}
import ppl.delite.framework.datastructures.DeliteArray

trait NodeImplOps { this: OptiGraph =>
  def node_outneighbors_impl(n: Rep[Node]): Rep[GIterable[Node]]
  def node_inneighbors_impl(n: Rep[Node]): Rep[GIterable[Node]]
  def node_upneighbors_impl(n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]]
  def node_downneighbors_impl(n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]]
  def node_outedges_impl(n: Rep[Node]): Rep[GIterable[Edge]]
  def node_inedges_impl(n: Rep[Node]): Rep[GIterable[Edge]]
  def node_upedges_impl(n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]]
  def node_downedges_impl(n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]]
  def node_numoutneighbors_impl(n: Rep[Node]): Rep[Int]
  def node_numinneighbors_impl(n: Rep[Node]): Rep[Int]
  def node_outdegree_impl(n: Rep[Node]): Rep[Int]
  def node_indegree_impl(n: Rep[Node]): Rep[Int]
}

trait NodeImplOpsStandard extends NodeImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def node_outneighbors_impl(n: Rep[Node]): Rep[GIterable[Node]] = {
    graph_outneighbors(node_graph(n), n)
  }

  def node_inneighbors_impl(n: Rep[Node]): Rep[GIterable[Node]] = {
    graph_inneighbors(node_graph(n), n)
  }

  def node_upneighbors_impl(n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]] = {
    graph_upneighbors(node_graph(n), n, visited)
  }

  def node_downneighbors_impl(n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]] = {
    graph_downneighbors(node_graph(n), n, visited)
  }

  def node_outedges_impl(n: Rep[Node]): Rep[GIterable[Edge]] = {
    graph_outedges(node_graph(n), n)
  }

  def node_inedges_impl(n: Rep[Node]): Rep[GIterable[Edge]] = {
    graph_inedges(node_graph(n), n)
  }

  def node_upedges_impl(n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]] = {
    graph_upedges(node_graph(n), n, visited)
  }

  def node_downedges_impl(n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]] = {
    graph_downedges(node_graph(n), n, visited)
  }

  def node_numoutneighbors_impl(n: Rep[Node]): Rep[Int] = {
    giterable_raw_size(node_out_neighbors(n))
  }

  def node_numinneighbors_impl(n: Rep[Node]): Rep[Int] = {
    giterable_raw_size(node_in_neighbors(n))
  }

  def node_outdegree_impl(n: Rep[Node]): Rep[Int] = {
    graph_outdegree(node_graph(n), n)
  }

  def node_indegree_impl(n: Rep[Node]): Rep[Int] = {
    graph_indegree(node_graph(n), n)
  }
}
