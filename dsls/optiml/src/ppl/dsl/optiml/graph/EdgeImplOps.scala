package ppl.dsl.optiml.graph

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml._

trait EdgeImplOps { this: OptiML =>
  def edge_in_impl[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[ED]
  def edge_out_impl[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[ED]
  def edge_target_impl[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[Vertex[VD,ED]]  
}

trait EdgeImplOpsStandard extends EdgeImplOps {
  this: OptiMLCompiler =>

  //////////////////////////
  // kernel implementations
  
  def edge_in_impl[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]], v: Rep[Vertex[VD,ED]]) = if (v == e.v1) e.inData else e.outData
  def edge_out_impl[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]], v: Rep[Vertex[VD,ED]]) = if(v == e.v1) e.outData else e.inData
  def edge_target_impl[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]], source: Rep[Vertex[VD,ED]]) = if (source == e.v1) e.v2 else e.v1
}
