package ppl.dsl.optiml

import scala.virtualization.lms.ppl._
import scala.virtualization.lms.common.Base

trait VectorViewImplOps extends Base {

  def vectorview_new_impl[A:Manifest] : Rep[Tuple5[Array[A],Int,Int,Int,Boolean]] => Rep[VectorView[A]]

}

trait VectorViewImplOpsStandard extends VectorViewImplOps with MatrixImplOps with VectorViewOpsRepExp with MatrixOpsRepExp
  with EmbeddingPkgExp with ScalaOpsPkgExp {

  private val base = "ppl.dsl.optiml.embedding"

  //////////////////////////
  // kernel implementations

  def vectorview_new_impl[A](implicit mA: Manifest[A]) = t => {
    External[VectorView[A]]("new " + base + ".VectorViewImpl[" + mA + "](%s,%s, %s, %s, %s)", List(t._1, t._2, t._3, t._4, t._5))
  }

}