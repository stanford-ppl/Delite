package ppl.dsl.optiml.embedded

import scala.virtualization.lms.common.{Base, FunctionsExp}
import scala.virtualization.lms.ppl.{EmbeddingPkgExp, ScalaOpsPkgExp}

trait MatrixImplOps extends Base {
  def matrix_new_impl[A:Manifest] : Rep[Tuple2[Int,Int]] => Rep[Matrix[A]]
}

trait MatrixImplOpsStandard extends MatrixImplOps
  with EmbeddingPkgExp with ScalaOpsPkgExp {
  
  private val base = "ppl.dsl.optiml.embedding"
  
  protected[optiml] def newMatrix[A](numRows: Exp[Int], numCols: Exp[Int])(implicit mA: Manifest[A]) = {
    External[Matrix[A]]("new " + base + ".MatrixImpl[" + mA + "](%s,%s)", List(numRows, numCols))
  }

  def matrix_new_impl[A:Manifest] = t => newMatrix(t._1, t._2)

}