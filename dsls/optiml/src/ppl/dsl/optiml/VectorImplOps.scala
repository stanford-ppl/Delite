package ppl.dsl.optiml

import datastruct.scala.{Vector,Matrix}
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}

trait VectorImplOps { this: OptiML =>

  def vector_obj_fromseq_impl[A:Manifest](xs: Rep[Seq[A]]): Rep[Vector[A]]
  def vector_obj_ones_impl(length: Rep[Int]) : Rep[Vector[Double]]
  def vector_obj_zeros_impl(length: Rep[Int]) : Rep[Vector[Double]]
  def vector_obj_uniform_impl(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean]): Rep[Vector[Double]]

  def vector_outer_impl[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) : Rep[Matrix[A]]
  def vector_pprint_impl[A:Manifest](v: Rep[Vector[A]]) : Rep[Unit]
  def vector_trans_impl[A](v: Rep[Vector[A]])(implicit mA: Manifest[A], vA: Manifest[Vector[A]]) : Rep[Vector[A]]
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: OptiML =>

  private val base = "ppl.dsl.optiml"


  //////////////////////////
  // kernel implementations

  // TODO: support for asserts and other debugging tools

  def vector_obj_fromseq_impl[A:Manifest](xs: Rep[Seq[A]]) = {
    val v = Vector[A](xs.length, true)
    for (i <- 0 until xs.length) {
      v(i) = xs(i)
    }
    v
  }

  def vector_obj_ones_impl(length: Rep[Int]) = Vector[Double](length, true) map { e => 1. }

  def vector_obj_zeros_impl(length: Rep[Int]) = Vector[Double](length, true)

  def vector_obj_zerosf_impl(length: Rep[Int]) = Vector[Float](length, true)

  def vector_obj_rand_impl(length: Rep[Int]) = Vector[Double](length, true) map { e => random[Double] }

  def vector_obj_uniform_impl(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean]) = {
    val length = Math.ceil((end-start)/step_size).asInstanceOfL[Int]
    val out = Vector[Double](length, true)
    for (i <- 0 until length) {
      // TODO: i*step_size (int*double returning double) doesn't work yet (needs to chain 2 implicits: intToDouble, repArithToArithOps)
      out(i) = step_size*i + start
    }
    out
  }

  def vector_outer_impl[A:Manifest:Arith](collA: Rep[Vector[A]], collB: Rep[Vector[A]]) = {
    val out = Matrix[A](collA.length, collA.length)
    for (i <- 0 until collA.length ){
      for (j <- 0 until collB.length ){
        out(i,j) = collA(i)*collB(j)
      }
    }
    out
  }

  def vector_pprint_impl[A:Manifest](v: Rep[Vector[A]]) = {
    if (v.isRow){
      print("[ ")
      for (i <- 0 until v.length){
        print(v(i)); print(" ");
      }
      print("]\\n")
    }
    else{
      for (i <- 0 until v.length){
        print("[")
        print(v(i))
        print(" ]\\n")
      }
    }
  }

  def vector_trans_impl[A](v: Rep[Vector[A]])(implicit mA: Manifest[A], vA: Manifest[Vector[A]]) = {
    val out = Vector[A](v.length, !v.isRow)
    for (i <- 0 until v.length){
      out(i) = v(i)
    }
    out  
  }
}

trait VectorImplOpsBLAS extends VectorImplOpsStandard { this: OptiML =>
  
  //override def vector_obj_plus_impl = External(..)
}