package ppl.dsl.optiml.embedded

import scala.virtualization.lms.common.Base


trait VectorImplOpsInlined extends Base {

  /////////////////////////////////
  // returning expanded lambdas

//  def vector_obj_apply_impl[A](implicit mA: Manifest[A]) : Rep[Tuple2[Boolean,Int] => Vector[A]]
//  def vector_obj_zeros_impl : Rep[Int => Vector[Double]]
//
//  def vector_trans_impl[A](implicit mA: Manifest[A], vA: Manifest[Vector[A]]) : Rep[Vector[A] => Vector[A]]
//  def vector_toboolean_impl[A] : Exp[Vector[A] => Vector[Boolean]]
//  def vector_plus_impl[A] : Exp[(Vector[A], Vector[A]) => Vector[A]]
//  def vector_map_impl[A,B] : Exp[(Vector[A], A=>B) => Vector[B]]
//
//  // why does this work with Tuple2, but apparently not with (Boolean,Int)?
//  def vector_new_impl[A](implicit mA: Manifest[A]) : Rep[Tuple2[Boolean,Int] => Vector[A]]

}

trait VectorImplOpsInlinedExp extends VectorImplOpsInlined {

  //////////////////////////////////
  // returning expanded lambdas

//  def vector_new_impl[A](implicit mA: Manifest[A]) =
//    doLambda[Tuple2[Boolean,Int], Vector[A]]{ t =>
//      External[Vector[A]]("new " + base + ".VectorImpl[" + mA + "](%s,%s)", List(t._1, t._2))
//    }
//
//  lazy val vector_obj_zeros_impl = doLambda[Int, Vector[Double]]{length =>
//    vector_new[Double](true, length)
//  }
//
//  def vector_trans_impl[A](implicit mA: Manifest[A], vA: Manifest[Vector[A]]) = doLambda[Vector[A], Vector[A]]{ v =>
//    val out = vector_new[A](!v.is_row, v.length) //new VectorImpl[A](v.length, !v.is_row)
//    for (i <- 0 until v.length){
//      out(i) = v(i)
//    }
//    out
//  }

//  def vector_toboolean_impl[A](v: Vector[A])(implicit conv: A => Boolean) : Vector[Boolean] = {
//    val out = newVector[Boolean](v.length, v.is_row)
//    for (i <- 0 until v.length){
//      out(i) = conv(v(i))
//    }
//    out
//  }

//    def vector_plus_impl[A] = vector_map_impl[A,A]( (a,b) => a + b )
//    def vector_map_impl[A,B](f: A => B) = doLambda[Vector[A], Vector[B]]{ v =>
//      val out = new VectorImpl[B](v.length, v.is_row)
//      for (i <- 0 until v.length){
//        out(i) = f(v(i))
//      }
//    }
}