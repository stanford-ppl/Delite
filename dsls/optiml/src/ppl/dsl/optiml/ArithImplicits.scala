package ppl.dsl.optiml

import scala.virtualization.lms.common.{ScalaOpsPkg, Base}
import scala.virtualization.lms.util.OverloadHack

/* ArithOps definitions for OptiML supported types.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: Dec 2, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait ArithImplicits extends Base with OverloadHack {
  this: ScalaOpsPkg with LanguageOps with VectorOps with MatrixOps =>

  type ArithOps[X] = ArithOpsInternal[Rep,X]

  /**
   * Vector
   */

  implicit def vectorArithOps[T:Numeric:Manifest] : ArithOps[Vector[T]] = new ArithOps[Vector[T]] {
    def +=(a: Rep[Vector[T]], b: Rep[Vector[Nothing]])(implicit o: Overloaded1) = a // if we know statically
    def +=(a: Rep[Vector[T]], b: Rep[Vector[T]]) = if (!b.isInstanceOf[NilVector[T]]) a += b else a
    def +(a: Rep[Vector[T]], b: Rep[Vector[T]]) = a + b
}


  /**
   * Matrix
   */

  implicit def matrixArithOps[T:Numeric:Manifest] : ArithOps[Matrix[T]] = new ArithOps[Matrix[T]] {
    def +=(a: Rep[Matrix[T]], b: Rep[Matrix[T]]) = a += b
    def +(a: Rep[Matrix[T]], b: Rep[Matrix[T]]) = a + b
    /*
    def -(a: Matrix[T], b: Matrix[T]) = a - b
    def *(a: Matrix[T], b: Matrix[T]) = a dot b
    def /(a: Matrix[T], b: Matrix[T]) = a / b
    def zero = throw new UnsupportedOperationException() //TODO: figure out the size
    def unary_-(a: Matrix[T]) = -a
    def abs(a: Matrix[T]) = a.abs
    def exp(a: Matrix[T]) = a.exp
    def >(a: Matrix[T], b: Matrix[T]) = a > b
    def <(a: Matrix[T], b: Matrix[T]) = a < b
    */
  }


  /**
   *  Tuple
   */

  //implicit def tuple4ArithOps[A,B,C,D](implicit rA: A => Rep[A], rB: B => Rep[B], rC: C => Rep[C], rD: D => Rep[D], opsA: ArithOps[A], mA: Manifest[A], opsB: ArithOps[B], mB: Manifest[B],
  implicit def tuple4ArithOps[A,B,C,D](implicit opsA: ArithOps[A], mA: Manifest[A], opsB: ArithOps[B], mB: Manifest[B],
                                                opsC: ArithOps[C], mC: Manifest[C], opsD: ArithOps[D], mD: Manifest[D])
                                       : ArithOps[Tuple4[A,B,C,D]] =  new ArithOps[Tuple4[A,B,C,D]] {
    def +=(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]]) =
      Tuple4(opsA.+(a._1,b._1), opsB.+(a._2,b._2), opsC.+(a._3,b._3), opsD.+(a._4,b._4))
    def +(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]]) =
      Tuple4(opsA.+(a._1,b._1), opsB.+(a._2,b._2), opsC.+(a._3,b._3), opsD.+(a._4,b._4))
  }

  /**
   * Primitives
   */

  implicit def primitiveArithOps[T:Numeric:Manifest] : ArithOps[T] = new ArithOps[T] {
    def +=(a: Rep[T], b: Rep[T]) = a+b
    def +(a: Rep[T], b: Rep[T]) = a+b
  }
}