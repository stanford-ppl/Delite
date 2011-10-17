package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{Vector,Matrix,IndexVector}
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml.{OptiMLLift, OptiMLCompiler, OptiML}

trait VectorImplOps { this: OptiML =>

  def vector_find_override_impl[A:Manifest](v: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean]): Rep[IndexVector]
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: OptiMLCompiler with OptiMLLift =>

  //////////////////////////
  // kernel implementations

  def vector_find_override_impl[A:Manifest](v: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean]) = {
    val indices = IndexVector(0)
    for (i <- 0 until v.length) {
      if (pred(v(i))) indices += i
    }
    indices.unsafeImmutable.asInstanceOf[Rep[IndexVector]]
  }
}