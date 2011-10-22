package ppl.dsl.optiml.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml.OptiML
import ppl.dsl.optiml.{Vector,IndexVectorDense}

trait IndexVectorImplOps { this: Base =>
  def index_vector_obj_fromvec_impl(xs: Interface[Vector[Int]]): Rep[IndexVectorDense]
}

trait IndexVectorImplOpsStandard extends IndexVectorImplOps {
  this: OptiML =>

  def index_vector_obj_fromvec_impl(xs: Interface[Vector[Int]]) = {
    val out = IndexVector(0) ++ xs
    out.unsafeImmutable.asInstanceOf[Rep[IndexVectorDense]]
  }

}