package ppl.dsl.optiml.graph

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import scala.collection.mutable.HashMap
import ppl.dsl.optiml._

trait GraphImplOps { this: OptiML =>
  def graph_vertices_impl(g: Rep[Graph]): Rep[DenseVector[Vertex]]
  def graph_edges_impl(g: Rep[Graph]): Rep[DenseVector[Edge]]
  //def graph_adjacent_impl(g: Rep[Graph], a: Rep[Vertex], b: Rep[Vertex]): Rep[Boolean]
  def graph_neighborsof_impl(g: Rep[Graph], a: Rep[Vertex]): Rep[DenseVector[Vertex]]
  def graph_neighborsselfof_impl(g: Rep[Graph], a: Rep[Vertex]): Rep[DenseVector[Vertex]]
  def graph_edgesof_impl(g: Rep[Graph], a: Rep[Vertex]): Rep[DenseVector[Edge]]
  def graph_containsedge_impl(g: Rep[Graph], a: Rep[Edge]): Rep[Boolean]
  def graph_containsvertex_impl(g: Rep[Graph], a: Rep[Vertex]): Rep[Boolean]
  def graph_addvertex_impl(g: Rep[Graph], a: Rep[Vertex]): Rep[Unit]
  def graph_addedge_impl(g: Rep[Graph], e: Rep[Edge], a: Rep[Vertex], b: Rep[Vertex]): Rep[Unit]
  //def graph_removeedge_impl(g: Rep[Graph], a: Rep[Vertex], b: Rep[Vertex]): Rep[Unit]
  def graph_freeze_impl(g: Rep[Graph]): Rep[Unit]
  def graph_cleartasks_impl(g: Rep[Graph], i: Rep[Int]): Rep[Unit]  
}

trait GraphImplOpsStandard extends GraphImplOps {
  this: OptiMLCompiler with OptiMLLift =>


  //////////////////////////
  // kernel implementations
  
  def graph_vertices_impl(g: Rep[Graph]): Rep[DenseVector[Vertex]] = {
    if (g.frozen) {
      densevector_obj_fromseq[Vertex](graph_get_vertices(g).toSeq)
    }
    else {
      densevector_obj_fromseq[Vertex](graph_get_adjacencies(g).keySet.toSeq)
    }
  }

  def graph_edges_impl(g: Rep[Graph]): Rep[DenseVector[Edge]] = {
    if (g.frozen) {
      densevector_obj_fromseq[Edge](graph_get_edges(g).toSeq)
    }
    else {
      densevector_obj_fromseq[Edge](graph_get_edgetovertices(g).keySet.toSeq)
    }
  }

  //def graph_adjacent_impl(g: Rep[Graph], a: Rep[Vertex], b: Rep[Vertex]): Rep[Boolean] = verticesToEdges.contains((a, b))

  def graph_containsedge_impl(g: Rep[Graph], e: Rep[Edge]) = graph_get_edgetovertices(g).contains(e)

  def graph_containsvertex_impl(g: Rep[Graph], v: Rep[Vertex]) = graph_get_adjacencies(g).contains(v)
  
  // only available before finalization
  def graph_addvertex_impl(g: Rep[Graph], v: Rep[Vertex]): Rep[Unit] = {
    if (g.frozen) fatal("error adding vertex to graph: graph is frozen")

    val adjacencies = graph_get_adjacencies(g)
    if (!adjacencies.contains(v)) {
      adjacencies(v) = List()
    }
  }
  
  def graph_addedge_impl(g: Rep[Graph], e: Rep[Edge], a: Rep[Vertex], b: Rep[Vertex]): Rep[Unit] = {
    if (g.frozen) fatal("error adding edge to graph: graph is frozen")

    val adjacencies = graph_get_adjacencies(g)
    val edgeToVertices = graph_get_edgetovertices(g)    
    if (!edgeToVertices.contains(e)) {
      //verticesToEdges((a, b)) = e
      adjacencies(a) = ((e,b)) :: adjacencies(a)
      adjacencies(b) = ((e,a)) :: adjacencies(b)
      edgeToVertices(e) = ((a,b))
    }
  }

  /*def graph_removeedge_impl(g: Rep[Graph], a: Rep[Vertex], b: Rep[Vertex]): Rep[Unit] = {
    if (_frozen) throw new RuntimeException("Graph is frozen")

    if (adjacencies.contains((a, b))) {
      val e = verticesToEdges((a, b))

      verticesToEdges.remove(edgeToVertices(e))
      adjacencies(a) -= ((e, b))
      adjacencies(b) -= ((e, a))
      edgeToVertices.remove(e)
    }
  } */

  // finalizes the graph structure; we may want to rename this to clarify the semantics.
  def graph_freeze_impl(g: Rep[Graph]): Rep[Unit] = {
    val edgeToVertices = graph_get_edgetovertices(g)
    val adjacencies = graph_get_adjacencies(g)
    val vertexIds = graph_get_vertexids(g)
    
    graph_set_edges(g, edgeToVertices.keySet.toArray)
    graph_set_vertices(g, adjacencies.keySet.toArray)

    val vertices = graph_get_vertices(g)
    
    for(id <- 0 until vertices.length) {      
      vertexIds(vertices(id)) = id
    }

    val sorted = vertices map {adjacencies(_).sortBy{t => vertexIds(t._2)}}
    graph_set_vertexedges(g, sorted map {l => densevector_obj_fromseq[Edge]((l map {_._1}).toSeq)})
    graph_set_neighbors(g, sorted map {l => densevector_obj_fromseq[Vertex]((l map {_._2}).toSeq)})
    graph_set_neighborsself(g, vertices map {v => val ns = v :: (adjacencies(v) map {_._2}); densevector_obj_fromseq[Vertex](ns.sortBy{v => vertexIds(v)}.toSeq)})
    graph_set_adjacencies(g,unit(null))
    graph_set_frozen(g,true)
  }
  
  def graph_cleartasks_impl(g: Rep[Graph], i: Rep[Int]): Rep[Unit] = {
    g.vertices(i).clearTasks
  }

  // only available after finalization
  def graph_neighborsof_impl(g: Rep[Graph], v: Rep[Vertex]): Rep[DenseVector[Vertex]] = {
    if (!g.frozen) fatal("error accessing graph neighbors: graph has not been finalized")

    val vertexIds = graph_get_vertexids(g)
    if (!vertexIds.contains(v)) {
      DenseVector[Vertex](0, true)
    }
    else {
      val id = vertexIds(v)
      graph_get_neighbors(g).apply(id)
    }
  }
  
  // only available after finalization
  def graph_neighborsselfof_impl(g: Rep[Graph], v: Rep[Vertex]): Rep[DenseVector[Vertex]] = {
    if (!g.frozen) fatal("error accessing graph neighbors: graph has not been finalized")

    val vertexIds = graph_get_vertexids(g)
    if (!vertexIds.contains(v)) {
      DenseVector[Vertex](0, true)
    }
    else {
      val id = vertexIds(v)
      graph_get_neighborsself(g).apply(id)
    }
  }

  def graph_edgesof_impl(g: Rep[Graph], v: Rep[Vertex]): Rep[DenseVector[Edge]] = {
    if (!g.frozen) fatal("error accessing graph edges: graph has not been finalized")

    val vertexIds = graph_get_vertexids(g)
    if (!vertexIds.contains(v)) {
      DenseVector[Edge](0, true)
    }
    else {
      val id = vertexIds(v)
      graph_get_vertexedges(g).apply(id)
    }
  }
  

}