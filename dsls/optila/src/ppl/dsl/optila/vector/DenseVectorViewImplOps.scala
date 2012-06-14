package ppl.dsl.optila.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila.{DenseVectorView}
import ppl.dsl.optila.{OptiLALift, OptiLACompiler, OptiLA}

trait DenseVectorViewImplOps { this: OptiLA =>
  def dense_vectorview_update_impl[A:Manifest](v: Rep[DenseVectorView[A]], a: Rep[Array[A]]): Rep[Unit]
}

trait DenseVectorViewImplOpsStandard extends DenseVectorViewImplOps {
  this: OptiLACompiler with OptiLALift =>

  def dense_vectorview_update_impl[A:Manifest](v: Rep[DenseVectorView[A]], a: Rep[Array[A]]) = {
    var i = unit(0)
    while (i < a.length) {
      v(i) = a(i)    
      i += 1
    }    
  }
}