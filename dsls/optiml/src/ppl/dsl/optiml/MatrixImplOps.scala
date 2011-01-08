package ppl.dsl.optiml

import datastruct.scala.{Vector,Matrix}
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}

trait MatrixImplOps { this: OptiML =>
  def matrix_plus_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) : Rep[Matrix[A]]
  def matrix_plusequals_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) : Rep[Matrix[A]]
  def matrix_pprint_impl[A:Manifest](m: Rep[Matrix[A]]) : Rep[Unit]
}

trait MatrixImplOpsStandard extends MatrixImplOps {
  this: OptiML =>
  
  private val base = "ppl.dsl.optiml"

  ///////////////
  // helpers

  private def map[A:Manifest,B:Manifest](m: Rep[Matrix[A]], f: Rep[A] => Rep[B]) = {
    val out = Matrix[B](m.numRows, m.numCols)
    for (i <- 0 until m.numRows){
      for (j <- 0 until m.numCols){
        out(i,j) = f(m(i,j))
      }
    }
    out
  }

  private def zipWith[A:Manifest,B:Manifest](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], f: (Rep[A],Rep[A]) => Rep[B]) = {
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

  def matrix_plusequals_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = {
    val out = m1
    for (i <- 0 until out.numRows) {
      for (j <- 0 until out.numCols) {
        out(i,j) = m1(i,j)+m2(i,j)
      }
    }
    out
  }

  def matrix_plus_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]])  = zipWith[A,A](m1, m2, (a,b) => a+b)

  def matrix_pprint_impl[A:Manifest](m: Rep[Matrix[A]]) = {
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