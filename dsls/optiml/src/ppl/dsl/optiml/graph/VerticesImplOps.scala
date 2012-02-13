package ppl.dsl.optiml.graph

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml._

trait VerticesImplOps { 
  this: OptiML =>
  
  def vertices_tolist_impl(v: Rep[DenseVector[Vertex]]): Rep[List[Vertex]]
}

trait VerticesImplOpsStandard extends VerticesImplOps {
  this: OptiMLCompiler with OptiMLLift =>


  //////////////////////////
  // kernel implementations

  def vertices_tolist_impl(v: Rep[DenseVector[Vertex]]) = {
    val data = densevector_raw_data(v)
    list_fromseq(data.toSeq)
  }
  
}