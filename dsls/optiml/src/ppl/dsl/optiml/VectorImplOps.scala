package ppl.dsl.optiml

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}

trait VectorImplOps { this: Base =>

  def vector_obj_zeros_impl(length: Rep[Int]) : Rep[Vector[Double]]

  def vector_plus_impl[A:Manifest:Numeric](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) : Rep[Vector[A]]
  def vector_minus_impl[A:Manifest:Numeric](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) : Rep[Vector[A]]
  def vector_divide_impl[A:Manifest:Fractional](v1: Rep[Vector[A]], y: Rep[A]) : Rep[Vector[A]]
  def vector_outer_impl[A:Manifest:Numeric](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) : Rep[Matrix[A]]
  def vector_pprint_impl[A:Manifest](v: Rep[Vector[A]]) : Rep[Unit]

  def vector_trans_impl[A](v: Rep[Vector[A]])(implicit mA: Manifest[A], vA: Manifest[Vector[A]]) : Rep[Vector[A]]
  def vector_toboolean_impl[A:Manifest](v: Rep[Vector[A]], conv: Rep[A] => Rep[Boolean]) : Rep[Vector[Boolean]]

  def vector_new_impl[A:Manifest](length: Rep[Int], is_row: Rep[Boolean]) : Rep[Vector[A]]

  def vector_obj_range_impl(start: Rep[Int], end: Rep[Int], stride: Rep[Int], is_row: Rep[Boolean]) : Rep[Vector[Int]]
  def vector_map_impl[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[B]) : Rep[Vector[B]]
  def vector_sum_impl[A](x: Rep[Vector[A]])(implicit mA: Manifest[A], ops: ArithOps[Rep[A]]) : Rep[A]
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: BaseExp with ScalaOpsPkg with VectorOps with MatrixOps =>
  
  private val base = "ppl.dsl.optiml"

  ///////////////
  // helpers

  private def map[A:Manifest,B:Manifest](v: Rep[Vector[A]], f: Rep[A] => Rep[B]) = {
    val out = Vector[B](v.length, v.is_row)
    for (i <- 0 until v.length){
      out(i) = f(v(i))
    }
    out
  }

  private def zipWith[A:Manifest,B:Manifest](v1: Rep[Vector[A]], v2: Rep[Vector[A]], f: (Rep[A],Rep[A]) => Rep[B]) = {
    val out = Vector[B](v1.length, v1.is_row)
    for (i <- 0 until v1.length){
      out(i) = f(v1(i), v2(i))
    }
    out
  }


  //////////////////////////
  // kernel implementations

  // TODO: support for asserts and other debugging tools

  def vector_obj_zeros_impl(length: Rep[Int]) = Vector[Double](length, true)

  def vector_plus_impl[A:Manifest:Numeric](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = zipWith[A,A](v1, v2, (a,b) => a+b)

  def vector_minus_impl[A:Manifest:Numeric](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = zipWith[A,A](v1, v2, (a,b) => a-b)

  def vector_divide_impl[A:Manifest:Fractional](v1: Rep[Vector[A]], y: Rep[A]) = map[A,A](v1, e => e / y)

  def vector_outer_impl[A:Manifest:Numeric](collA: Rep[Vector[A]], collB: Rep[Vector[A]]) = {
    val out = Matrix[A](collA.length, collA.length)
    for (i <- 0 until collA.length ){
      for (j <- 0 until collB.length ){
        out(i,j) = collA(i)*collB(j)
      }
    }
    out
  }

  def vector_pprint_impl[A:Manifest](v: Rep[Vector[A]]) = {
    if (v.is_row){
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
    val out = Vector[A](v.length, !v.is_row)
    for (i <- 0 until v.length){
      out(i) = v(i)
    }
    out  
  }

  def vector_toboolean_impl[A:Manifest](v: Rep[Vector[A]], conv: Rep[A] => Rep[Boolean]) = map[A,Boolean](v, conv)

  def vector_new_impl[A](length: Rep[Int], is_row: Rep[Boolean])(implicit mA: Manifest[A])
    = External[Vector[A]]("new " + base + ".VectorImpl[" + implMap(mA) + "](%s,%s)", List(length, is_row))

  def vector_obj_range_impl(start: Rep[Int], end: Rep[Int], stride: Rep[Int], is_row: Rep[Boolean])
    = External[Vector[Int]]("new " + base + ".RangeVectorImpl(%s,%s,%s,%s)", List(start, end, stride, is_row))

  def vector_map_impl[A:Manifest,B:Manifest](v: Rep[Vector[A]], f: Rep[A] => Rep[B]) = map(v, f)

  def vector_sum_impl[A](v: Rep[Vector[A]])(implicit mA: Manifest[A], ops: ArithOps[Rep[A]]) : Rep[A] = {
    var acc = v(0)
    for (i <- 1 until v.length) {
      ops.+=(acc, v(i))
    }
    acc
  }

  /**
   * HACK! The namespace problem between data structures and user types needs to be solved more generally.
   */
  def implMap[A](mA: Manifest[A]) : String = mA.toString match {
    case "ppl.dsl.optiml.Matrix[Double]" => "ppl.dsl.optiml.MatrixImpl[Double]"
    case _ => mA.toString
  }
}

trait VectorImplOpsBLAS extends VectorImplOpsStandard { this: BaseExp with ScalaOpsPkg with VectorOps with MatrixOps =>
  //this: Functions with VectorOps with RangeOps with TupleOps with BooleanOps =>
  
  //override def vector_obj_plus_impl = External(..)
}