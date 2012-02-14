package ppl.dsl.optiml.graph

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml._

trait EdgeImplOps { this: OptiML =>
  def edge_in_impl(e: Rep[Edge], v: Rep[Vertex]): Rep[MessageData]
  def edge_out_impl(e: Rep[Edge], v: Rep[Vertex]): Rep[MessageData]
  def edge_target_impl(e: Rep[Edge], v: Rep[Vertex]): Rep[Vertex]  
}

trait EdgeImplOpsStandard extends EdgeImplOps {
  this: OptiMLCompiler =>

  //////////////////////////
  // kernel implementations
  
  def edge_in_impl(e: Rep[Edge], v: Rep[Vertex]) = if (v == e.v1) e.inData else e.outData
  def edge_out_impl(e: Rep[Edge], v: Rep[Vertex]) = if(v == e.v1) e.outData else e.inData
  def edge_target_impl(e: Rep[Edge], source: Rep[Vertex]) = if (source == e.v1) e.v2 else e.v1
}
