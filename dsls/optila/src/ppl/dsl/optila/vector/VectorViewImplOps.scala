package ppl.dsl.optila.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila.{VectorView}
import ppl.dsl.optila.{OptiLALift, OptiLACompiler, OptiLA}

trait VectorViewImplOps { this: OptiLA =>
  def vectorview_update_impl[A:Manifest](v: Rep[VectorView[A]], a: Rep[Array[A]]): Rep[Unit]
}

trait VectorViewImplOpsStandard extends VectorViewImplOps {
  this: OptiLACompiler with OptiLALift =>

  def vectorview_update_impl[A:Manifest](v: Rep[VectorView[A]], a: Rep[Array[A]]) = {
    var i = unit(0)
    while (i < a.length) {
      v(i) = a(i)    
      i += 1
    }    
  }
}