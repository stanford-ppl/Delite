package ppl.dsl.optila.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila._

trait SparseMatrixImplOps { this: OptiLA =>
  def sparsematrix_vview_impl[A:Manifest](x: Rep[SparseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]): Rep[SparseVectorView[A]]
  def sparsematrix_apply_impl[A:Manifest](x: Rep[SparseMatrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A]
  def sparsematrix_update_impl[A:Manifest](m: Rep[SparseMatrix[A]], i: Rep[Int], j: Rep[Int], y: Rep[A]): Rep[Unit]  
  // def sparsematrix_inverse_impl[A:Manifest](m: Rep[SparseMatrix[A]])(implicit conv: Rep[A] => Rep[Double]): Rep[SparseMatrix[Double]]
}
