package ppl.dsl.optila.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila._

trait SparseVectorViewImplOps {
  this: OptiLACompiler with OptiLALift =>

  def sparse_vectorview_apply_impl[A:Manifest](x: Rep[SparseVectorView[A]], n: Rep[Int]): Rep[A] 
}