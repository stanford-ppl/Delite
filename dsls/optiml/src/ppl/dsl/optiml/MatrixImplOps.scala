package ppl.dsl.optiml

import scala.virtualization.lms.common.Base
import scala.virtualization.lms.ppl.{EmbeddingPkg, ScalaOpsPkg}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.embedded.scala.ScalaOpsPkg3

trait MatrixImplOps { this: DeliteApplication =>
  def matrix_new_impl[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[A]]

  def matrix_plus_impl[A:Manifest:Numeric](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) : Rep[Matrix[A]]
  def matrix_plusequals_impl[A](m: Rep[Matrix[A]], v: Rep[Vector[A]]) : Rep[Matrix[A]]
  def matrix_pprint_impl[A](m: Rep[Matrix[A]]) : Rep[Unit]
}

trait MatrixImplOpsStandard extends MatrixImplOps {
  this: DeliteApplication with MatrixOps with VectorOps with ScalaOpsPkg3 =>
  
  private val base = "ppl.dsl.optiml"

  ///////////////
  // helpers

  private def map[A,B:Manifest](m: Rep[Matrix[A]], f: Rep[A] => Rep[B]) = {
    val out = Matrix[B](m.numRows, m.numCols)
    for (i <- 0 until m.numRows){
      for (j <- 0 until m.numCols){
        out(i,j) = f(m(i,j))
      }
    }
    out
  }

  private def zipWith[A,B:Manifest](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], f: (Rep[A],Rep[A]) => Rep[B]) = {
    val out = Matrix[B](m1.numRows, m1.numCols)
    for (i <- 0 until m1.numRows){
      for (j <- 0 until m1.numCols){
        out(i,j) = f(m1(i,j), m2(i,j))
      }
    }
    out
  }

  ///////////////
  // kernels

  def matrix_new_impl[A](numRows: Rep[Int], numCols: Rep[Int])(implicit mA: Manifest[A]) =
    External[Matrix[A]]("new " + base + ".MatrixImpl[" + mA + "](%s,%s)", List(numRows, numCols))

  def matrix_plusequals_impl[A](m: Rep[Matrix[A]], v: Rep[Vector[A]]) = m.insertRow(m.numRows, v)

  def matrix_plus_impl[A:Manifest:Numeric](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]])  = zipWith[A,A](m1, m2, (a,b) => a+b)

  def matrix_pprint_impl[A](m: Rep[Matrix[A]]) = {
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