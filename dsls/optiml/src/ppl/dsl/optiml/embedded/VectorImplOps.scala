package ppl.dsl.optiml.embedded

import scala.virtualization.lms.internal.Utils
import scala.virtualization.lms.ppl._
import scala.virtualization.lms.common.{Base, Functions, FunctionsExp, BaseExp}

trait VectorImplOps extends Base {

  def vector_obj_zeros_impl : Rep[Int] => Rep[Vector[Double]]

  def vector_plus_impl[A:Manifest:Numeric] : Rep[Tuple2[Vector[A],Vector[A]]] => Rep[Vector[A]]
  def vector_minus_impl[A:Manifest:Numeric] : Rep[Tuple2[Vector[A],Vector[A]]] => Rep[Vector[A]]
  def vector_divide_impl[A:Manifest:Fractional] : Rep[Tuple2[Vector[A],A]] => Rep[Vector[A]]
  def vector_outer_impl[A:Manifest:Numeric] : Rep[Tuple2[Vector[A],Vector[A]]] => Rep[Matrix[A]]
  def vector_pprint_impl[A] : Rep[Vector[A]] => Rep[Unit]

  def vector_trans_impl[A](implicit mA: Manifest[A], vA: Manifest[Vector[A]]) : Rep[Vector[A]] => Rep[Vector[A]]
  def vector_toboolean_impl[A] : Rep[Tuple2[Vector[A], A => Boolean]] => Rep[Vector[Boolean]]

  def vector_new_impl[A:Manifest] : Rep[Tuple2[Int,Boolean]] => Rep[Vector[A]]

}

trait VectorImplOpsStandard extends VectorImplOps with MatrixImplOps with VectorOpsRepExp with MatrixOpsRepExp 
  with EmbeddingPkgExp with ScalaOpsPkgExp {
  //this: Functions with VectorOps with RangeOps with TupleOps with BooleanOps =>

  private val base = "ppl.dsl.optiml.embedding"

  ///////////////
  // helpers

  protected[optiml] def newVector[A](length: Exp[Int], is_row: Exp[Boolean])(implicit mA: Manifest[A]) = {
    External[Vector[A]]("new " + base + ".VectorImpl[" + mA + "](%s,%s)", List(length, is_row))
  }

  private def map[A,B:Manifest](v: Exp[Vector[A]], f: Exp[A] => Exp[B]) = {
    // bad! update happens on the External directly, instead of on a NewVector node
    //val out = newVector[B](v.length, v.is_row)
    val out = vector_new[B](v.length, v.is_row)
    for (i <- 0 until v.length){
      out(i) = f(v(i))
    }
    out
  }

  // TODO: fix: two versions of map that do the same thing with different representations of a function?
  // we could transform the above to this, at the cost of an unnecessary wrapping in a Lambda object
  private def mapl[A,B:Manifest](v: Exp[Vector[A]], f: Exp[A => B]) = {
    val out = vector_new[B](v.length, v.is_row)
    for (i <- 0 until v.length){
      out(i) = f(v(i))
    }
    out
  }

  private def zipWith[A,B:Manifest](v1: Exp[Vector[A]], v2: Exp[Vector[A]], f: (Exp[A],Exp[A]) => Exp[B]) = {
    val out = vector_new[B](v1.length, v1.is_row)
    for (i <- 0 until v1.length){
      out(i) = f(v1(i), v2(i))
    }
    out
  }


  //////////////////////////
  // kernel implementations

  // TODO: everything is a for loop because while is not lifted yet
  // TODO: support for asserts and other debugging tools

  // interestingly, vector_obj_zeros_impl generates more efficient code, because it does not have to wrap its arguments
  // in a tuple like vector_new_impl
  def vector_obj_zeros_impl = length => newVector[Double](length, true)

  def vector_plus_impl[A:Manifest:Numeric]  = t => zipWith[A,A](t._1, t._2, (a,b) => a+b)
  def vector_minus_impl[A:Manifest:Numeric] = t => zipWith[A,A](t._1, t._2, (a,b) => a-b)
  def vector_divide_impl[A:Manifest:Fractional] = t => map[A,A](t._1, e => e / t._2)

  def vector_outer_impl[A:Manifest:Numeric] = t => {
    val collA = t._1
    val collB = t._2

    val out = matrix_new[A](collA.length, collA.length)
    for (i <- 0 until collA.length ){
      for (j <- 0 until collB.length ){
        out(i,j) = collA(i)*collB(i)
      }
    }
    out
  }

  def vector_pprint_impl[A] = v => {
    if (v.is_row){
      print("[ ")
      for (i <- 0 until v.length){
        print(v(i)); print(" ");
      }
      print("]\n")
    }
    else{
      for (i <- 0 until v.length){
        print("[")
        print(v(i))
        print(" ]\n")
      }
    }
  }

  def vector_trans_impl[A](implicit mA: Manifest[A], vA: Manifest[Vector[A]]) = v => {
    val out = vector_new[A](v.length, !v.is_row)
    for (i <- 0 until v.length){
      out(i) = v(i)
    }
    out  
  }

  def vector_toboolean_impl[A] = t => mapl[A,Boolean](t._1, t._2)

  def vector_new_impl[A:Manifest] = t => newVector(t._1, t._2)

}

trait VectorImplOpsBLAS extends VectorImplOpsStandard {
  //this: Functions with VectorOps with RangeOps with TupleOps with BooleanOps =>
  
  //override def vector_obj_plus_impl = External(..)
}