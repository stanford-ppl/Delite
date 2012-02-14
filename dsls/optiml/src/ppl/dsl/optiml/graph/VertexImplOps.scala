package ppl.dsl.optiml.graph

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml._

trait VertexImplOps { this: OptiML =>
  def vertex_edges_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Edge[VD,ED]]]
  def vertex_neighbors_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def vertex_neighborsself_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def vertex_tasks_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def vertex_addtask_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]], t: Rep[Vertex[VD,ED]]): Rep[Unit]
  def vertex_cleartasks_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[Unit]
  def vertex_target_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]], e: Rep[Edge[VD,ED]]): Rep[Vertex[VD,ED]] 
}

trait VertexImplOpsStandard extends VertexImplOps {
  this: OptiMLCompiler with OptiMLLift =>


  //////////////////////////
  // kernel implementations
  
  def vertex_edges_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Edge[VD,ED]]] = v.graph.edgesOf(v)
  def vertex_neighbors_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]] = v.graph.neighborsOf(v)
  def vertex_neighborsself_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]] = v.graph.neighborsSelfOf(v)
  def vertex_addtask_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]], t: Rep[Vertex[VD,ED]]): Rep[Unit] = vertex_tasks(v) += t  
  def vertex_tasks_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]] = densevector_obj_fromseq[Vertex[VD,ED]](vertex_get_tasks(v).toSeq)
  def vertex_cleartasks_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[Unit] = vertex_tasks(v).clear()
  def vertex_target_impl[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]], e: Rep[Edge[VD,ED]]): Rep[Vertex[VD,ED]] = e.target(v)
}