package ppl.dsl.optisdr.capabilities

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import java.io.PrintWriter
import scala.virtualization.lms.internal.{CLikeCodegen}
import ppl.dsl.optisdr._
import ppl.dsl.optila.DenseVector
import scala.reflect.SourceContext

/*
 * SDR specific arithmetic operations definitions for OptiSDR supported types.
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

/* Type class for basic math, but less restrictive than Numeric. */

trait SDRArithInternal[Rep[X],T] {
  // def +(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  // endian swap
  // reciprocal for q15?
  def conj(a: Rep[T])(implicit ctx: SourceContext) : Rep[T]
}

trait SDRArithOps extends Variables with OverloadHack {
  this: OptiSDR =>
  
  type SDRArith[X] = SDRArithInternal[Rep,X]

  /**
   * Interface, enables using Ariths with operator notation. Note that the inclusion of these
   * causes the NumericOps and FractionalOps implicit conversions to be ambiguous, so OptiLA
   * programs cannot include them.
   */
  implicit def sdrArithToSDRArithOps[T:SDRArith:Manifest](n: T) = new SDRArithOpsCls(unit(n))
  implicit def repSDRArithToSDRArithOps[T:SDRArith:Manifest](n: Rep[T]) = new SDRArithOpsCls(n)
  implicit def varSDRArithToSDRArithOps[T:SDRArith:Manifest](n: Var[T]) = new SDRArithOpsCls(readVar(n))

  class SDRArithOpsCls[T](lhs: Rep[T])(implicit mT: Manifest[T], arith: SDRArith[T]){
    def conj(implicit ctx: SourceContext): Rep[T] = arith.conj(lhs)
  }
  
  implicit val intSDRArith : SDRArith[Int] = new SDRArith[Int] {
    def conj(a: Rep[Int])(implicit ctx: SourceContext) = a
  }
  
  implicit val uintSDRArith : SDRArith[UInt] = new SDRArith[UInt] {
    def conj(a: Rep[UInt])(implicit ctx: SourceContext) = a
  }
  
  implicit val realSDRArith : SDRArith[Real] = new SDRArith[Real] {
    def conj(a: Rep[Real])(implicit ctx: SourceContext) = a
  }
  
  implicit val complexSDRArith : SDRArith[Complex] = new SDRArith[Complex] {
    def conj(a: Rep[Complex])(implicit ctx: SourceContext) = a.conj
  }
  
  implicit def denseVectorSDRArith[T:SDRArith:Manifest]: SDRArith[DenseVector[T]] = new SDRArith[DenseVector[T]] {
    def conj(a: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToSDRVectorOps(a).conj
  }
  
  implicit def tuple2SDRArith[A:Manifest:SDRArith,B:Manifest:SDRArith] : SDRArith[Tuple2[A,B]] = new SDRArith[Tuple2[A,B]] {
    def conj(a: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) = Tuple2(a._1.conj, a._2.conj)
  }
  
  implicit def tuple3SDRArith[A:Manifest:SDRArith,B:Manifest:SDRArith,C:Manifest:SDRArith] : SDRArith[Tuple3[A,B,C]] = new SDRArith[Tuple3[A,B,C]] {
    def conj(a: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) = Tuple3(a._1.conj, a._2.conj, a._3.conj)
  }
  
  implicit def tuple4SDRArith[A:Manifest:SDRArith,B:Manifest:SDRArith,C:Manifest:SDRArith,D:Manifest:SDRArith] : SDRArith[Tuple4[A,B,C,D]] = new SDRArith[Tuple4[A,B,C,D]] {
    def conj(a: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) = Tuple4(a._1.conj, a._2.conj, a._3.conj, a._4.conj)
  }
}

trait SDRArithOpsExp extends SDRArithOps with VariablesExp {
  this: OptiSDRExp => 
}