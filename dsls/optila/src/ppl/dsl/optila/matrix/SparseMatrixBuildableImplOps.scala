package ppl.dsl.optila.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila._

trait SparseMatrixBuildableImplOps { this: OptiLA =>
  def sparsematrix_buildable_apply_impl[A:Manifest](x: Rep[SparseMatrixBuildable[A]], i: Rep[Int], j: Rep[Int]): Rep[A]
  def sparsematrix_buildable_update_impl[A:Manifest](x: Rep[SparseMatrixBuildable[A]], row: Rep[Int], col: Rep[Int], y: Rep[A]): Rep[Unit]
  def sparsematrix_buildable_append_impl[A:Manifest](m: Rep[SparseMatrixBuildable[A]], i: Rep[Int], j: Rep[Int], y: Rep[A], alwaysWrite: Boolean = false): Rep[Unit]
  def sparsematrix_buildable_insertrow_impl[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit]
  def sparsematrix_buildable_insertallrows_impl[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Matrix[A]]): Rep[Unit]
  def sparsematrix_buildable_insertcol_impl[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Vector[A]]): Rep[Unit]
  def sparsematrix_buildable_insertallcols_impl[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], y: Interface[Matrix[A]]): Rep[Unit]
  def sparsematrix_buildable_removerows_impl[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
  def sparsematrix_buildable_removecols_impl[A:Manifest](x: Rep[SparseMatrixBuildable[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
}
