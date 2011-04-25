package ppl.dsl.optiml.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml.OptiML
import ppl.dsl.optiml.datastruct.scala.{Vector,IndexVector}

trait IndexVectorImplOps { this: Base =>
  def index_vector_obj_fromvec_impl(xs: Rep[Vector[Int]]): Rep[IndexVector]
}

trait IndexVectorImplOpsStandard extends IndexVectorImplOps {
  this: OptiML =>

  def index_vector_obj_fromvec_impl(xs: Rep[Vector[Int]]) = {
    val out = IndexVector(0)
    out ++= xs
    out
  }

}