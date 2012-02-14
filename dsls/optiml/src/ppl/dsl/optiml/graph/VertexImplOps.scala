package ppl.dsl.optiml.graph

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml._

trait VertexImplOps { this: OptiML =>
  def vertex_edges_impl(v: Rep[Vertex]): Rep[DenseVector[Edge]]
  def vertex_neighbors_impl(v: Rep[Vertex]): Rep[DenseVector[Vertex]]
  def vertex_neighborsself_impl(v: Rep[Vertex]): Rep[DenseVector[Vertex]]
  def vertex_tasks_impl(v: Rep[Vertex]): Rep[DenseVector[Vertex]]
  def vertex_addtask_impl(v: Rep[Vertex], t: Rep[Vertex]): Rep[Unit]
  def vertex_cleartasks_impl(v: Rep[Vertex]): Rep[Unit]
  def vertex_target_impl(v: Rep[Vertex], e: Rep[Edge]): Rep[Vertex] 
}

trait VertexImplOpsStandard extends VertexImplOps {
  this: OptiMLCompiler with OptiMLLift =>


  //////////////////////////
  // kernel implementations
  
  def vertex_edges_impl(v: Rep[Vertex]): Rep[DenseVector[Edge]] = v.graph.edgesOf(v)
  def vertex_neighbors_impl(v: Rep[Vertex]): Rep[DenseVector[Vertex]] = v.graph.neighborsOf(v)
  def vertex_neighborsself_impl(v: Rep[Vertex]): Rep[DenseVector[Vertex]] = v.graph.neighborsSelfOf(v)
  def vertex_addtask_impl(v: Rep[Vertex], t: Rep[Vertex]): Rep[Unit] = vertex_tasks(v) += t  
  def vertex_tasks_impl(v: Rep[Vertex]): Rep[DenseVector[Vertex]] = densevector_obj_fromseq[Vertex](vertex_get_tasks(v).toSeq)
  def vertex_cleartasks_impl(v: Rep[Vertex]): Rep[Unit] = vertex_tasks(v).clear()
  def vertex_target_impl(v: Rep[Vertex], e: Rep[Edge]): Rep[Vertex]  = e.target(v)
}