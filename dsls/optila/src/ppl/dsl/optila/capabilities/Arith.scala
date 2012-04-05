package ppl.dsl.optila.capabilities

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import java.io.PrintWriter
import scala.virtualization.lms.internal.{CLikeCodegen}
import ppl.dsl.optila.{DenseVector,Vector,DenseMatrix,Matrix}
import ppl.dsl.optila.{OptiLAExp, OptiLA}
import scala.reflect.SourceContext

/*
 * Arith definitions for OptiLA supported types.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: Dec 2, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

/* Type class for basic math, but less restrictive than Numeric. */

trait ArithInternal[Rep[X],T] {
  def +=(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def +(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def -(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def *(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def /(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def abs(a: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def exp(a: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def empty(implicit ctx: SourceContext): Rep[T]
  def zero(a: Rep[T])(implicit ctx: SourceContext): Rep[T]
  /*
  def unary_-(a: Rep[T]) : Rep[T]
  */
}

trait ArithOps extends Variables with OverloadHack {
  this: OptiLA =>
  
  type Arith[X] = ArithInternal[Rep,X]

  /**
   * Interface, enables using Ariths with operator notation. Note that the inclusion of these
   * causes the NumericOps and FractionalOps implicit conversions to be ambiguous, so OptiLA
   * programs cannot include them.
   */
  implicit def arithToArithOps[T:Arith:Manifest](n: T) = new ArithOpsCls(unit(n))
  implicit def repArithToArithOps[T:Arith:Manifest](n: Rep[T]) = new ArithOpsCls(n)
  implicit def varArithToArithOps[T:Arith:Manifest](n: Var[T]) = new ArithOpsCls(readVar(n))

  class ArithOpsCls[T](lhs: Rep[T])(implicit mT: Manifest[T], arith: Arith[T]){
    def +=(rhs: Rep[T])(implicit ctx: SourceContext): Rep[T] = arith.+=(lhs,rhs)
    def +=[B](rhs: B)(implicit c: B => Rep[T], ctx: SourceContext): Rep[T] = arith.+=(lhs,c(rhs))    
    def +(rhs: Rep[T])(implicit ctx: SourceContext): Rep[T] = arith.+(lhs,rhs)
    def -(rhs: Rep[T])(implicit ctx: SourceContext): Rep[T] = arith.-(lhs,rhs)
    def *(rhs: Rep[T])(implicit ctx: SourceContext): Rep[T] = arith.*(lhs,rhs)
    def /(rhs: Rep[T])(implicit ctx: SourceContext): Rep[T] = arith./(lhs,rhs)

    def abs(implicit ctx: SourceContext): Rep[T] = arith.abs(lhs)
    def exp(implicit ctx: SourceContext): Rep[T] = arith.exp(lhs)
    def empty(implicit ctx: SourceContext): Rep[T] = arith.empty
    def zero(implicit ctx: SourceContext): Rep[T] = arith.zero(lhs)
  }


  /**
   * These infix methods support promoting one side of the binary op to a compatible type (precision widening).
   *  
   * This slightly asymmetric combination is the cleanest non-ambiguous way I've found to support most
   * left- and right-hand side arithmetic conversions.  Still, the overloading and implicit resolution here is
   * not fine-grained enough to cover all combinations of literal and Rep[] value promotions, so we add
   * special-cases to cover other OptiML primitive widenings. 
   * 
   * See ArithmeticConversionSuite.scala for a list of test cases.
   */     

  // kind of works, but doesn't cover all cases (if we start with a Rep[L] and need to convert that to R, the wrong signature is still preferred)
  def infix_-[L:Arith:Manifest,R](lhs: Rep[L], rhs: R)(implicit c: R => Rep[L], ctx: SourceContext): Rep[L] = implicitly[Arith[L]].-(lhs,c(rhs))
  def infix_-[L:Manifest,R:Arith:Manifest](lhs: Rep[L], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], ctx: SourceContext): Rep[R] = implicitly[Arith[R]].-(c(lhs),rhs)  

  // special cases to fill the holes - note that some redundancy is required here to prevent auto primitive widening
  def infix_-(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = implicitly[Arith[Int]].-(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].-(unit(lhs),rhs)  
  def infix_-(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].-(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].-(unit(lhs.toDouble), rhs)
  def infix_-(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].-(unit(lhs),rhs)  
  def infix_-(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].-(unit(lhs),rhs)
  def infix_-(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = implicitly[Arith[Int]].-(lhs, unit(rhs))  
  def infix_-(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].-(lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].-(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].-(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].-(lhs, unit(rhs))

  def infix_+[L:Arith:Manifest,R](lhs: Rep[L], rhs: R)(implicit c: R => Rep[L], ctx: SourceContext): Rep[L] = implicitly[Arith[L]].+(lhs,c(rhs))
  def infix_+[L:Manifest,R:Arith:Manifest](lhs: Rep[L], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], ctx: SourceContext): Rep[R] = implicitly[Arith[R]].+(c(lhs),rhs)  
  
  def infix_+(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = implicitly[Arith[Int]].+(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].+(unit(lhs),rhs)  
  def infix_+(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].+(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].+(unit(lhs.toDouble), rhs)
  def infix_+(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].+(unit(lhs),rhs)  
  def infix_+(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].+(unit(lhs),rhs)
  def infix_+(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = implicitly[Arith[Int]].+(lhs, unit(rhs))  
  def infix_+(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].+(lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].+(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].+(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].+(lhs, unit(rhs))

  def infix_*[L:Arith:Manifest,R](lhs: Rep[L], rhs: R)(implicit c: R => Rep[L], ctx: SourceContext): Rep[L] = implicitly[Arith[L]].*(lhs,c(rhs))
  def infix_*[L:Manifest,R:Arith:Manifest](lhs: Rep[L], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], ctx: SourceContext): Rep[R] = implicitly[Arith[R]].*(c(lhs),rhs)  
  
  def infix_*(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = implicitly[Arith[Int]].*(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].*(unit(lhs),rhs)  
  def infix_*(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].*(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].*(unit(lhs.toDouble), rhs)
  def infix_*(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].*(unit(lhs),rhs)  
  def infix_*(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].*(unit(lhs),rhs)
  def infix_*(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = implicitly[Arith[Int]].*(lhs, unit(rhs))  
  def infix_*(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].*(lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].*(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]].*(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]].*(lhs, unit(rhs))

  def infix_/[L:Arith:Manifest,R](lhs: Rep[L], rhs: R)(implicit c: R => Rep[L], ctx: SourceContext): Rep[L] = implicitly[Arith[L]]./(lhs,c(rhs))
  def infix_/[L:Manifest,R:Arith:Manifest](lhs: Rep[L], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], ctx: SourceContext): Rep[R] = implicitly[Arith[R]]./(c(lhs),rhs)  
  
  def infix_/(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = implicitly[Arith[Int]]./(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]]./(unit(lhs),rhs)  
  def infix_/(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]]./(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]]./(unit(lhs.toDouble), rhs)
  def infix_/(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]]./(unit(lhs),rhs)  
  def infix_/(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]]./(unit(lhs),rhs)
  def infix_/(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = implicitly[Arith[Int]]./(lhs, unit(rhs))  
  def infix_/(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]]./(lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]]./(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = implicitly[Arith[Float]]./(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = implicitly[Arith[Double]]./(lhs, unit(rhs))
       
  /**
   * Vector
   */

  implicit def denseVectorArith[T:Arith:Manifest]: Arith[DenseVector[T]] = new Arith[DenseVector[T]] {
    // these are used in sum; dynamic checks are required due to conditionals
    // def +=(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) = if (!b.IsInstanceOf[ZeroVector[T]]) a += b else a
    // def +(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) = if (a.IsInstanceOf[ZeroVector[T]]) b
    //                                               else if (b.IsInstanceOf[ZeroVector[T]]) a
    //                                               else a+b

    def +=(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToDenseVecOps(a).+=(b) 
    def +(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToDenseVecOps(a).+(b)
    def -(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToDenseVecOps(a).-(b)
    def *(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToDenseVecOps(a).*(b)
    def /(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToDenseVecOps(a)./(b)
    def abs(a: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToDenseVecOps(a).abs
    def exp(a: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToDenseVecOps(a).exp
    
    /**
     * zero for Vector[T] is a little tricky. It is used in nested Vector/Matrix operations, e.g.
     * a reduction on a Vector[Vector[T]]. For a variable dimension nested vector, the empty vector is the only
     * right answer. For a fixed dimension nested Vector, such as [[1,2,3],[4,5,6]], you'd ideally want the 
     * k-dimension zero vector, e.g. [0,0,0] in this example. However, this is the dimension
     * of v(0).dim, not v.dim, and cannot be statically enforced with our types, and furthermore would need to
     * correctly handled multiple levels of nesting. This situation is resolved by the DeliteOpReduce contract to
     * never use zero except in the case of the empty collection.
     *  
     * For non-nested cases, i.e. conditional maps or reduces, we want the zero-valued k-dimensional value,
     * but we don't always know k before running the function... (see sumIf in kmeans)
     */
    def empty(implicit ctx: SourceContext) = EmptyVector[T]
    def zero(a: Rep[DenseVector[T]])(implicit ctx: SourceContext) = ZeroVector[T](a.length)
  }


  /**
   * Matrix
   */

  implicit def denseMatrixArith[T:Arith:Manifest]: Arith[DenseMatrix[T]] = new Arith[DenseMatrix[T]] {
    def +=(a: Rep[DenseMatrix[T]], b: Rep[DenseMatrix[T]])(implicit ctx: SourceContext) = repToDenseMatOps(a).+=(b)
    def +(a: Rep[DenseMatrix[T]], b: Rep[DenseMatrix[T]])(implicit ctx: SourceContext) = repToDenseMatOps(a).+(b)
    def -(a: Rep[DenseMatrix[T]], b: Rep[DenseMatrix[T]])(implicit ctx: SourceContext) = repToDenseMatOps(a).-(b)
    def *(a: Rep[DenseMatrix[T]], b: Rep[DenseMatrix[T]])(implicit ctx: SourceContext) = repToDenseMatOps(a).*(b)
    def /(a: Rep[DenseMatrix[T]], b: Rep[DenseMatrix[T]])(implicit ctx: SourceContext) = repToDenseMatOps(a)./(b)
    def abs(a: Rep[DenseMatrix[T]])(implicit ctx: SourceContext) = repToDenseMatOps(a).abs
    def exp(a: Rep[DenseMatrix[T]])(implicit ctx: SourceContext) = repToDenseMatOps(a).exp
    def empty(implicit ctx: SourceContext) = DenseMatrix[T](unit(0),unit(0)) // EmptyDenseMatrix? 
    def zero(a: Rep[DenseMatrix[T]])(implicit ctx: SourceContext) = DenseMatrix[T](a.numRows, a.numCols)
    /*
    def unary_-(a: Rep[DenseMatrix[T]]) = -a
    */
  }


  /**
   *  Tuple
   */

  implicit def tuple2Arith[A:Manifest:Arith,B:Manifest:Arith] : Arith[Tuple2[A,B]] =
    new Arith[Tuple2[A,B]] {
      def +=(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) =
        Tuple2(a._1 += b._1, a._2 += b._2)

      def +(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) =
        Tuple2(a._1+b._1, a._2+b._2)

      def -(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) =
        Tuple2(a._1-b._1, a._2-b._2)

      def *(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) =
        Tuple2(a._1*b._1, a._2*b._2)

      def /(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) =
        Tuple2(a._1/b._1, a._2/b._2)

      def abs(a: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) =
        Tuple2(a._1.abs, a._2.abs)

      def exp(a: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) =
        Tuple2(a._1.exp, a._2.exp)
      
      def empty(implicit ctx: SourceContext) =
        Tuple2(implicitly[Arith[A]].empty, implicitly[Arith[B]].empty)
      
      def zero(a: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) =
        Tuple2(a._1.zero, a._2.zero)
    }
  
  implicit def tuple3Arith[A:Manifest:Arith,B:Manifest:Arith,C:Manifest:Arith,D:Manifest:Arith] : Arith[Tuple3[A,B,C]] =
    new Arith[Tuple3[A,B,C]] {
      def +=(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) =
        Tuple3(a._1 += b._1, a._2 += b._2, a._3 += b._3)

      def +(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) =
        Tuple3(a._1+b._1, a._2+b._2, a._3+b._3)

      def -(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) =
        Tuple3(a._1-b._1, a._2-b._2, a._3-b._3)

      def *(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) =
        Tuple3(a._1*b._1, a._2*b._2, a._3*b._3)

      def /(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) =
        Tuple3(a._1/b._1, a._2/b._2, a._3/b._3)

      def abs(a: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) =
        Tuple3(a._1.abs, a._2.abs, a._3.abs)

      def exp(a: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) =
        Tuple3(a._1.exp, a._2.exp, a._3.exp)
      
      def empty(implicit ctx: SourceContext) =
        Tuple3(implicitly[Arith[A]].empty, implicitly[Arith[B]].empty, implicitly[Arith[C]].empty)

      def zero(a: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) =
        Tuple3(a._1.zero, a._2.zero, a._3.zero)
    }

  //implicit def tuple4Arith[A,B,C,D](implicit rA: A => Rep[A], rB: B => Rep[B], rC: C => Rep[C], rD: D => Rep[D], opsA: Arith[A], mA: Manifest[A], opsB: Arith[B], mB: Manifest[B],
  implicit def tuple4Arith[A:Manifest:Arith,B:Manifest:Arith,C:Manifest:Arith,D:Manifest:Arith] : Arith[Tuple4[A,B,C,D]] =
    new Arith[Tuple4[A,B,C,D]] {
      def +=(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) =
        Tuple4(a._1 += b._1, a._2 += b._2, a._3 += b._3, a._4 += b._4)

      def +(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) =
        Tuple4(a._1+b._1, a._2+b._2, a._3+b._3, a._4+b._4)

      def -(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) =
        Tuple4(a._1-b._1, a._2-b._2, a._3-b._3, a._4-b._4)

      def *(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) =
        Tuple4(a._1*b._1, a._2*b._2, a._3*b._3, a._4*b._4)

      def /(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) =
        Tuple4(a._1/b._1, a._2/b._2, a._3/b._3, a._4/b._4)

      def abs(a: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) =
        Tuple4(a._1.abs, a._2.abs, a._3.abs, a._4.abs)

      def exp(a: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) =
        Tuple4(a._1.exp, a._2.exp, a._3.exp, a._4.exp)
      
      def empty(implicit ctx: SourceContext) =
        Tuple4(implicitly[Arith[A]].empty, implicitly[Arith[B]].empty, implicitly[Arith[C]].empty, implicitly[Arith[D]].empty)

      def zero(a: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) =
        Tuple4(a._1.zero, a._2.zero, a._3.zero, a._4.zero)    
    }


  /**
   * Primitives
   *
   * unfortunately, to use ArithOps, we have to redefine all of the operations we want to
   * to support from NumericOps and FractionalOps, since their implicits are ambiguous with ours.
   */

  implicit val doubleArith : Arith[Double] = new Arith[Double] {
    def +=(a: Rep[Double], b: Rep[Double])(implicit ctx: SourceContext) = arith_plus(a,b)
    def +(a: Rep[Double], b: Rep[Double])(implicit ctx: SourceContext) = arith_plus(a,b)
    def -(a: Rep[Double], b: Rep[Double])(implicit ctx: SourceContext) = arith_minus(a,b)
    def *(a: Rep[Double], b: Rep[Double])(implicit ctx: SourceContext) = arith_times(a,b)
    def /(a: Rep[Double], b: Rep[Double])(implicit ctx: SourceContext) = arith_fractional_divide(a,b)
    def abs(a: Rep[Double])(implicit ctx: SourceContext) = arith_abs(a)
    def exp(a: Rep[Double])(implicit ctx: SourceContext) = arith_exp(a)
    def empty(implicit ctx: SourceContext) = unit(0.0)
    def zero(a: Rep[Double])(implicit ctx: SourceContext) = empty
    //def unary_-(a: Rep[Double]) = -a
  }

  implicit val floatArith : Arith[Float] = new Arith[Float] {
    def +=(a: Rep[Float], b: Rep[Float])(implicit ctx: SourceContext) = arith_plus(a,b)
    def +(a: Rep[Float], b: Rep[Float])(implicit ctx: SourceContext) = arith_plus(a,b)
    def -(a: Rep[Float], b: Rep[Float])(implicit ctx: SourceContext) = arith_minus(a,b)
    def *(a: Rep[Float], b: Rep[Float])(implicit ctx: SourceContext) = arith_times(a,b)
    def /(a: Rep[Float], b: Rep[Float])(implicit ctx: SourceContext) = arith_fractional_divide(a,b)
    def abs(a: Rep[Float])(implicit ctx: SourceContext) = arith_abs(a)
    def exp(a: Rep[Float])(implicit ctx: SourceContext) = arith_exp(a).AsInstanceOf[Float]
    def empty(implicit ctx: SourceContext) = unit(0f)
    def zero(a: Rep[Float])(implicit ctx: SourceContext) = empty
    //def unary_-(a: Rep[Float]) = -a
  }

  implicit val intArith : Arith[Int] = new Arith[Int] {
    def +=(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = arith_plus(a,b)
    def +(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = arith_plus(a,b)
    def -(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = arith_minus(a,b)
    def *(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = arith_times(a,b)
    def /(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = int_divide(a,b)
    def abs(a: Rep[Int])(implicit ctx: SourceContext) = arith_abs(a)
    def exp(a: Rep[Int])(implicit ctx: SourceContext) = arith_exp(a).AsInstanceOf[Int]
    def empty(implicit ctx: SourceContext) = unit(0)
    def zero(a: Rep[Int])(implicit ctx: SourceContext) = empty
    //def unary_-(a: Rep[Int]) = -a
  }
  
  def arith_plus[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[T]
  def arith_minus[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[T]
  def arith_times[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[T]
  def arith_fractional_divide[T:Manifest:Fractional](lhs: Rep[T], rhs: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def arith_abs[T:Manifest:Numeric](lhs: Rep[T])(implicit ctx: SourceContext): Rep[T]
  def arith_exp[T:Manifest:Numeric](lhs: Rep[T])(implicit ctx: SourceContext): Rep[Double]
}

trait ArithOpsExp extends ArithOps with VariablesExp {
  this: OptiLAExp =>

  abstract class NumericDef[A:Manifest:Numeric,R:Manifest] extends DefWithManifest[A,R] {
    val n = implicitly[Numeric[A]]
  }
  case class ArithPlus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) extends NumericDef[T,T]
  case class ArithPlusEquals[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) extends NumericDef[T,Unit]
  case class ArithMinus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) extends NumericDef[T,T]
  case class ArithTimes[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) extends NumericDef[T,T]
  case class ArithFractionalDivide[T:Manifest:Fractional](lhs: Exp[T], rhs: Exp[T]) extends DefWithManifest[T,T]{
    val f = implicitly[Fractional[T]]
  }
  case class ArithAbs[T:Manifest:Numeric](lhs: Exp[T]) extends NumericDef[T,T]
  case class ArithExp[T:Manifest:Numeric](lhs: Exp[T]) extends NumericDef[T,Double]

  def arith_plus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Exp[T] = reflectPure(ArithPlus(lhs, rhs))
  def arith_minus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Exp[T] = reflectPure(ArithMinus(lhs, rhs))
  def arith_times[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Exp[T] = reflectPure(ArithTimes(lhs, rhs))
  def arith_fractional_divide[T:Manifest:Fractional](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext): Exp[T] = reflectPure(ArithFractionalDivide(lhs, rhs))
  def arith_abs[T:Manifest:Numeric](lhs: Exp[T])(implicit ctx: SourceContext) = reflectPure(ArithAbs(lhs))
  def arith_exp[T:Manifest:Numeric](lhs: Exp[T])(implicit ctx: SourceContext) = reflectPure(ArithExp(lhs))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@ArithPlus(lhs,rhs) => reflectPure(ArithPlus(f(lhs),f(rhs))(e.mA,e.n))(mtype(manifest[A]),implicitly[SourceContext])
    case e@ArithMinus(lhs,rhs) => reflectPure(ArithMinus(f(lhs),f(rhs))(e.mA,e.n))(mtype(manifest[A]),implicitly[SourceContext])
    case e@ArithTimes(lhs,rhs) => reflectPure(ArithTimes(f(lhs),f(rhs))(e.mA,e.n))(mtype(manifest[A]),implicitly[SourceContext])
    case e@ArithFractionalDivide(lhs,rhs) => reflectPure(ArithFractionalDivide(f(lhs),f(rhs))(e.mA,e.f))(mtype(manifest[A]),implicitly[SourceContext])
    case e@ArithAbs(lhs) => reflectPure(ArithAbs(f(lhs))(e.mA,e.n))(mtype(manifest[A]),implicitly[SourceContext])
    case e@ArithExp(lhs) => reflectPure(ArithExp(f(lhs))(e.mA,e.n))(mtype(manifest[A]),implicitly[SourceContext])
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] 
  
}

trait ArithOpsExpOpt extends ArithOpsExp {
  this: OptiLAExp =>

  override def arith_plus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext) : Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => unit(implicitly[Numeric[T]].plus(x,y))
    case (Const(0 | 0.0 | 0.0f | -0.0 | -0.0f), y) => y
    case (y, Const(0 | 0.0 | 0.0f | -0.0 | -0.0f)) => y
    case _ => super.arith_plus(lhs, rhs)
  }
  override def arith_minus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext) : Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => unit(implicitly[Numeric[T]].minus(x,y))
    case (Const(0 | 0.0 | 0.0f | -0.0 | -0.0f), y) => y
    case (y, Const(0 | 0.0 | 0.0f | -0.0 | -0.0f)) => y
    case _ => super.arith_minus(lhs, rhs)
  }
  override def arith_times[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T])(implicit ctx: SourceContext) : Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => unit(implicitly[Numeric[T]].times(x,y))
    case (Const(0 | 0.0 | 0.0f | -0.0 | -0.0f), _) => lhs
    case (_, Const(0 | 0.0 | 0.0f | -0.0 | -0.0f)) => rhs
//    case (Const(1 | 1.0 | 1.0f), y) => y //TODO: careful about type promotion!
//    case (y, Const(1 | 1.0 | 1.0f)) => y
    case _ => super.arith_times(lhs, rhs)
  }
}



trait ScalaGenArithOps extends ScalaGenBase {
  val IR: ArithOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ArithPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case ArithMinus(a,b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case ArithTimes(a,b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case ArithFractionalDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
    case ArithAbs(x) => emitValDef(sym, "java.lang.Math.abs(" + quote(x) + ")")
    //case a@ArithAbs(x) => a.m.asInstanceOf[Manifest[_]] match {
    //  case Manifest.Double => emitValDef(sym, "java.lang.Double.longBitsToDouble((java.lang.Double.doubleToRawLongBits(" + quote(x) + ")<<1)>>>1)")
    //  case _ => emitValDef(sym, "Math.abs(" + quote(x) + ")")
    //}
    case ArithExp(a) => emitValDef(sym, "java.lang.Math.exp(" + quote(a) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArithOps extends CLikeCodegen {
  val IR: ArithOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case ArithPlus(a,b) =>
          emitValDef(sym, quote(a) + " + " + quote(b))
        case ArithMinus(a,b) =>
          emitValDef(sym, quote(a) + " - " + quote(b))
        case ArithTimes(a,b) =>
          emitValDef(sym, quote(a) + " * " + quote(b))
        case ArithFractionalDivide(a,b) =>
          emitValDef(sym, quote(a) + " / " + quote(b))
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArithOps extends CudaGenBase with CLikeGenArithOps
trait OpenCLGenArithOps extends OpenCLGenBase with CLikeGenArithOps
trait CGenArithOps extends CGenBase with CLikeGenArithOps

