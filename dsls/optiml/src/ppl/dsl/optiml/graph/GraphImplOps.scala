package ppl.dsl.optiml.graph

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import scala.collection.mutable.HashMap
import ppl.dsl.optiml._

trait GraphImplOps { this: OptiML =>
  def graph_vertices_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def graph_edges_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[DenseVector[Edge[VD,ED]]]
  //def graph_adjacent_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Boolean]
  def graph_neighborsof_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def graph_neighborsselfof_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def graph_edgesof_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[DenseVector[Edge[VD,ED]]]
  def graph_containsedge_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Edge[VD,ED]]): Rep[Boolean]
  def graph_containsvertex_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[Boolean]
  def graph_addvertex_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[Unit]
  def graph_addedge_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], e: Rep[Edge[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Unit]
  //def graph_removeedge_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Unit]
  def graph_freeze_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[Unit]
  def graph_cleartasks_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], i: Rep[Int]): Rep[Unit]  
}

trait GraphImplOpsStandard extends GraphImplOps {
  this: OptiMLCompiler with OptiMLLift =>


  //////////////////////////
  // kernel implementations
  
  def graph_vertices_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]] = {
    if (g.frozen) {
      densevector_obj_fromseq[Vertex[VD,ED]](graph_get_vertices(g.unsafeImmutable).toSeq)
    }
    else {
      densevector_obj_fromseq[Vertex[VD,ED]](graph_get_adjacencies(g.unsafeImmutable).keySet.toSeq)
    }
  }

  def graph_edges_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[DenseVector[Edge[VD,ED]]] = {
    if (g.frozen) {
      densevector_obj_fromseq[Edge[VD,ED]](graph_get_edges(g.unsafeImmutable).toSeq)
    }
    else {
      densevector_obj_fromseq[Edge[VD,ED]](graph_get_edgetovertices(g.unsafeImmutable).keySet.toSeq)
    }
  }

  //def graph_adjacent_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Boolean] = verticesToEdges.contains((a, b))

  def graph_containsedge_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], e: Rep[Edge[VD,ED]]) = graph_get_edgetovertices(g).contains(e)

  def graph_containsvertex_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], v: Rep[Vertex[VD,ED]]) = graph_get_adjacencies(g).contains(v)
  
  // only available before finalization
  def graph_addvertex_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[Unit] = {
    if (g.frozen) fatal("error adding vertex to graph: graph is frozen")

    val adjacencies = graph_get_adjacencies(g.unsafeImmutable)
    if (!adjacencies.contains(v)) {      
      hashmap_unsafe_update(adjacencies, v, List[(Edge[VD,ED], Vertex[VD,ED])]())
      // adjacencies(v) = List[(Edge[VD,ED], Vertex[VD,ED])]()
    }
  }
  
  def graph_addedge_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], e: Rep[Edge[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Unit] = {
    if (g.frozen) fatal("error adding edge to graph: graph is frozen")

    val adjacencies = graph_get_adjacencies(g.unsafeImmutable)
    val edgeToVertices = graph_get_edgetovertices(g.unsafeImmutable)    
    if (!edgeToVertices.contains(e)) {
      //verticesToEdges((a, b)) = e
      hashmap_unsafe_update(adjacencies, a, ((e,b)) :: adjacencies(a))
      hashmap_unsafe_update(adjacencies, b, ((e,a)) :: adjacencies(b))
      hashmap_unsafe_update(edgeToVertices, e, ((a,b)))
      // adjacencies(a) = ((e,b)) :: adjacencies(a)
      // adjacencies(b) = ((e,a)) :: adjacencies(b)
      // edgeToVertices(e) = ((a,b))
    }
  }

  /*def graph_removeedge_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Unit] = {
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
  def graph_freeze_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[Unit] = {
    val edgeToVertices = graph_get_edgetovertices(g.unsafeImmutable)
    val adjacencies = graph_get_adjacencies(g.unsafeImmutable)
    val vertexIds = graph_get_vertexids(g.unsafeImmutable)
    
    graph_set_edges(g, edgeToVertices.keySet.toArray)
    graph_set_vertices(g, adjacencies.keySet.toArray)

    val vertices = graph_get_vertices(g.unsafeImmutable)
    
    for(id <- 0 until vertices.length) {      
      hashmap_unsafe_update(vertexIds, vertices(id), id)
      // vertexIds(vertices(id)) = id
    }

    val sorted = vertices map {adjacencies(_).sortBy{t => vertexIds(t._2)}}
    graph_set_vertexedges(g, sorted map {l => densevector_obj_fromseq[Edge[VD,ED]]((l map {_._1}).toSeq)})
    graph_set_neighbors(g, sorted map {l => densevector_obj_fromseq[Vertex[VD,ED]]((l map {_._2}).toSeq)})
    graph_set_neighborsself(g, vertices map {v => val ns = v :: (adjacencies(v) map {_._2}); densevector_obj_fromseq[Vertex[VD,ED]](ns.sortBy{v => vertexIds(v)}.toSeq)})
    graph_set_adjacencies(g,unit(null))
    graph_set_frozen(g,true)
  }
  
  def graph_cleartasks_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], i: Rep[Int]): Rep[Unit] = {
    g.vertices(i).clearTasks
  }

  // only available after finalization
  def graph_neighborsof_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]] = {
    if (!g.frozen) fatal("error accessing graph neighbors: graph has not been finalized")

    val vertexIds = graph_get_vertexids(g)
    if (!vertexIds.contains(v)) {
      (DenseVector[Vertex[VD,ED]](0, true)).unsafeImmutable
    }
    else {
      val id = vertexIds(v)
      graph_get_neighbors(g.unsafeImmutable).apply(id)
    }
  }
  
  // only available after finalization
  def graph_neighborsselfof_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]] = {
    if (!g.frozen) fatal("error accessing graph neighbors: graph has not been finalized")

    val vertexIds = graph_get_vertexids(g)
    if (!vertexIds.contains(v)) {
      (DenseVector[Vertex[VD,ED]](0, true)).unsafeImmutable
    }
    else {
      val id = vertexIds(v)
      graph_get_neighborsself(g.unsafeImmutable).apply(id)
    }
  }

  def graph_edgesof_impl[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Edge[VD,ED]]] = {
    if (!g.frozen) fatal("error accessing graph edges: graph has not been finalized")

    val vertexIds = graph_get_vertexids(g)
    if (!vertexIds.contains(v)) {
      (DenseVector[Edge[VD,ED]](0, true)).unsafeImmutable
    }
    else {
      val id = vertexIds(v)
      graph_get_vertexedges(g.unsafeImmutable).apply(id)
    }
  }
  

}