package ppl.dsl.optiml

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}

trait VectorViewImplOps { this: Base =>

  def vectorview_new_impl[A:Manifest](x: Rep[Array[A]], offset: Rep[Int], str: Rep[Int], len: Rep[Int], is_row: Rep[Boolean]) : Rep[VectorView[A]]
}

trait VectorViewImplOpsStandard extends VectorViewImplOps {
  this: BaseExp with ScalaOpsPkg with VectorViewOps with MatrixOps =>

  private val base = "ppl.dsl.optiml.embedding"

  //////////////////////////
  // kernel implementations

  def vectorview_new_impl[A](x: Rep[Array[A]], offset: Rep[Int], str: Rep[Int], len: Rep[Int], is_row: Rep[Boolean])(implicit mA: Manifest[A]) = {
    External[VectorView[A]]("new " + base + ".VectorViewImpl[" + mA + "](%s,%s, %s, %s, %s)", List(x, offset, str, len, is_row))
  }

}