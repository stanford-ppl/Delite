package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, BooleanOps}
import ppl.dsl.optigraph.{GIterable, GSet, Graph, Node}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}

trait GraphImplOps { this: OptiGraph =>
  def graph_load_impl(fileName: Rep[String]): Rep[Graph]
  def graph_nodes_impl(g: Rep[Graph]): Rep[GIterable[Node]]
}

trait GraphImplOpsStandard extends GraphImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def graph_load_impl(fileName: Rep[String]): Rep[Graph] = {
    val g = graph_new()
    val nodemap = HashMap[Int, Node]()
    var xfs = BufferedReader(FileReader(fileName))
    var line = xfs.readLine()
    var nodeIdx = 0
    while (line != null) {
      if(line.charAt(0) != '#') {
        val edges = line.split("\\t").map(_.toInt)
        if(!nodemap.contains(edges(0))) {
          nodemap(edges(0)) = node_new(nodeIdx)
          nodeIdx += 1
        }
        if(!nodemap.contains(edges(1))) {
          nodemap(edges(1)) = node_new(nodeIdx)
          nodeIdx += 1
        }
      }
      line = xfs.readLine()
    }

    xfs = BufferedReader(FileReader(fileName))
    line = xfs.readLine()
    val nodes = DeliteArray[Node](nodemap.size)
    val outneighbors = DeliteArray[GIterable[Node]](nodemap.size)
    val inneighbors = DeliteArray[GIterable[Node]](nodemap.size)
    for(i <- 0 until nodemap.size) {
      nodes(i) = nodemap(i)
      outneighbors(i) = new_empty_iterable[Node]()
      inneighbors(i) = new_empty_iterable[Node]()
    }
    while (line != null) {
      if(line.charAt(0) != '#') {
        val edges = line.split("\\t").map(_.toInt)
        delitearray_giterable_append(outneighbors, nodemap(edges(0)).Id, nodemap(edges(1)))
        delitearray_giterable_append(inneighbors, nodemap(edges(1)).Id, nodemap(edges(0)))
      }
      line = xfs.readLine()
    }
    graph_set_raw_nodes(g,nodes)
    graph_set_raw_outneighbors(g,outneighbors)
    graph_set_raw_inneighbors(g,inneighbors)
    g
  }

  def graph_nodes_impl(g: Rep[Graph]): Rep[GIterable[Node]] = {
    val node_array = graph_raw_nodes(g)
    new_iterable(node_array, unit(0), node_array.length)
  }
}
