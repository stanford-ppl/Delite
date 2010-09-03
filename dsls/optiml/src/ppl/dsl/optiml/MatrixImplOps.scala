package ppl.dsl.optiml

import scala.virtualization.lms.common.{Base, FunctionsExp}
import scala.virtualization.lms.ppl.{EmbeddingPkgExp, ScalaOpsPkgExp}

trait MatrixImplOps extends Base {
  def matrix_new_impl[A:Manifest] : Rep[Tuple2[Int,Int]] => Rep[Matrix[A]]

  def matrix_plus_impl[A:Manifest:Numeric] : Rep[Tuple2[Matrix[A],Matrix[A]]] => Rep[Matrix[A]]
  def matrix_plusequals_impl[A] : Rep[Tuple2[Matrix[A],Vector[A]]] => Rep[Matrix[A]]
  def matrix_pprint_impl[A] : Rep[Matrix[A]] => Rep[Unit]
}

trait MatrixImplOpsStandard extends MatrixImplOps with VectorImplOps with MatrixOpsRepExp with VectorOpsRepExp
  with EmbeddingPkgExp with ScalaOpsPkgExp {
  
  private val base = "ppl.dsl.optiml"

  ///////////////
  // helpers

  private def newMatrix[A](numRows: Exp[Int], numCols: Exp[Int])(implicit mA: Manifest[A]) = {
    External[Matrix[A]]("new " + base + ".MatrixImpl[" + mA + "](%s,%s)", List(numRows, numCols))
  }

  private def map[A,B:Manifest](m: Exp[Matrix[A]], f: Exp[A] => Exp[B]) = {
    val out = Matrix[B](m.numRows, m.numCols)
    for (i <- 0 until m.numRows){
      for (j <- 0 until m.numCols){
        out(i,j) = f(m(i,j))
      }
    }
    out
  }

  private def zipWith[A,B:Manifest](m1: Exp[Matrix[A]], m2: Exp[Matrix[A]], f: (Exp[A],Exp[A]) => Exp[B]) = {
    val out = Matrix[B](m1.numRows, m2.numCols)
    for (i <- 0 until m1.numRows){
      for (j <- 0 until m1.numCols){
        out(i,j) = f(m1(i,j), m2(i,j))
      }
    }
    out
  }

  ///////////////
  // kernels

  def matrix_new_impl[A:Manifest] = t => newMatrix(t._1, t._2)

  def matrix_plusequals_impl[A] = t => t._1.insertRow(t._1.numRows, t._2)
  def matrix_plus_impl[A:Manifest:Numeric]  = t => zipWith[A,A](t._1, t._2, (a,b) => a+b)

  def matrix_pprint_impl[A] = m => {
    for (i <- 0 until m.numRows){
      print("[ ")
      for (j <- 0 until m.numCols){
        print(m(i,j))
        print(" ")
      }
      print("]\\n")
    }
  }


}