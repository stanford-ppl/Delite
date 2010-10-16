package ppl.dsl.optiml

import scala.virtualization.lms.ppl._
import scala.virtualization.lms.common.Base
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.embedded.scala.ScalaOpsPkg3

trait VectorViewImplOps { this: DeliteApplication =>

  def vectorview_new_impl[A:Manifest](x: Rep[Array[A]], offset: Rep[Int], str: Rep[Int], len: Rep[Int], is_row: Rep[Boolean]) : Rep[VectorView[A]]
}

trait VectorViewImplOpsStandard extends VectorViewImplOps {
  this: DeliteApplication with ScalaOpsPkg3 with VectorViewOps with MatrixOps =>

  private val base = "ppl.dsl.optiml.embedding"

  //////////////////////////
  // kernel implementations

  def vectorview_new_impl[A](x: Rep[Array[A]], offset: Rep[Int], str: Rep[Int], len: Rep[Int], is_row: Rep[Boolean])(implicit mA: Manifest[A]) = {
    External[VectorView[A]]("new " + base + ".VectorViewImpl[" + mA + "](%s,%s, %s, %s, %s)", List(x, offset, str, len, is_row))
  }

}