package ppl.dsl.optila.capabilities

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import java.io.PrintWriter
import scala.virtualization.lms.internal.{CLikeCodegen}
import ppl.dsl.optila.{DenseVector,Vector,Matrix}
import ppl.dsl.optila.{OptiLAExp, OptiLA}

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
  def +=(a: Rep[T], b: Rep[T]) : Rep[T]
  def +(a: Rep[T], b: Rep[T]) : Rep[T]
  def -(a: Rep[T], b: Rep[T]) : Rep[T]
  def *(a: Rep[T], b: Rep[T]) : Rep[T]
  def /(a: Rep[T], b: Rep[T]) : Rep[T]
  def abs(a: Rep[T]) : Rep[T]
  def exp(a: Rep[T]) : Rep[T]
  def empty: Rep[T]
  def zero(a: Rep[T]): Rep[T]
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

  // to do Rep[Int] * Float, it should get converted to Rep[Float] * Float
  // TODO: this only works when invoked explicitly (won't kick in itself)
  implicit def chainRepArithToArithOps[A,B](a: Rep[A])
    (implicit mA: Manifest[A], aA: Arith[A], mB: Manifest[B], aB: Arith[B], c: Rep[A] => Rep[B]) = new ArithOpsCls(c(a))

  class ArithOpsCls[T](lhs: Rep[T])(implicit mT: Manifest[T], arith: Arith[T]){
    // TODO: if B == Rep[T] below, the ops implicit does not work unless it is called explicitly (no unambiguous resolution?)
    def +=(rhs: Rep[T]): Rep[T] = arith.+=(lhs,rhs)
    def +(rhs: Rep[T]): Rep[T] = arith.+(lhs,rhs)
    def -(rhs: Rep[T]): Rep[T] = arith.-(lhs,rhs)
    def *(rhs: Rep[T]): Rep[T] = arith.*(lhs,rhs)
    def /(rhs: Rep[T]): Rep[T] = arith./(lhs,rhs)

    def +=[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith.+=(lhs,c(rhs))
    def +[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith.+(lhs,c(rhs))
    def -[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith.-(lhs,c(rhs))
    def *[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith.*(lhs,c(rhs))
    def /[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith./(lhs,c(rhs))

    def abs: Rep[T] = arith.abs(lhs)
    def exp: Rep[T] = arith.exp(lhs)
    def empty: Rep[T] = arith.empty
    def zero: Rep[T] = arith.zero(lhs)
  }


  // TODO: why is this needed, given the definition of / above?
  def infix_/[T,B](lhs: Rep[T], rhs: B)(implicit f: Fractional[T], c: B => Rep[T], mT: Manifest[T]) = arith_fractional_divide(lhs,c(rhs))

  // why are these recursive? (perhaps because the abstract arith method has the same signature as the infix?)
//  def infix_+=[T,B](lhs: Rep[T], rhs:B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a.+=(lhs,c(rhs))
//  def infix_+[T,B](lhs: Rep[T], rhs:B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a.+(lhs,c(rhs))
//  def infix_-[T,B](lhs: Rep[T], rhs:B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a.-(lhs,c(rhs))
//  def infix_*[T,B](lhs: Rep[T], rhs:B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a.*(lhs,c(rhs))
//  def infix_/[T,B](lhs: Rep[T], rhs: B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a./(lhs,c(rhs))
//  def infix_abs[T](lhs: Rep[T])(implicit a: Arith[T], mT: Manifest[T]) = a.abs(lhs)
//  def infix_exp[T](lhs: Rep[T])(implicit a: Arith[T], mT: Manifest[T]) = a.exp(lhs)

  /**
   * Vector
   */

  implicit def denseVectorArith[T:Arith:Manifest]: Arith[DenseVector[T]] = new Arith[DenseVector[T]] {
    // these are used in sum; dynamic checks are required due to conditionals
    // def +=(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) = if (!b.isInstanceOfL[ZeroVector[T]]) a += b else a
    // def +(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) = if (a.isInstanceOfL[ZeroVector[T]]) b
    //                                               else if (b.isInstanceOfL[ZeroVector[T]]) a
    //                                               else a+b
    def +=(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) = a += b 
    def +(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) = a+b
    def -(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) = a-b
    def *(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) = a*b
    def /(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]) = a/b

    def abs(a: Rep[DenseVector[T]]) = a.abs
    def exp(a: Rep[DenseVector[T]]) = a.exp
    
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
    def empty = EmptyVector[T]
    def zero(a: Rep[DenseVector[T]]) = ZeroVector[T](a.length)
  }


  /**
   * Matrix
   */

  implicit def matrixArith[T:Arith:Manifest] : Arith[Matrix[T]] = new Arith[Matrix[T]] {
    def +=(a: Rep[Matrix[T]], b: Rep[Matrix[T]]) = a += b
    def +(a: Rep[Matrix[T]], b: Rep[Matrix[T]]) = a+b
    def -(a: Rep[Matrix[T]], b: Rep[Matrix[T]]) = a-b
    def *(a: Rep[Matrix[T]], b: Rep[Matrix[T]]) = a*b
    def /(a: Rep[Matrix[T]], b: Rep[Matrix[T]]) = a/b
    def abs(a: Rep[Matrix[T]]) = a.abs
    def exp(a: Rep[Matrix[T]]) = a.exp
    def empty = Matrix[T](0,0) // EmptyMatrix? 
    def zero(a: Rep[Matrix[T]]) = Matrix[T](a.numRows, a.numCols)
    /*
    def unary_-(a: Rep[Matrix[T]]) = -a
    */
  }


  /**
   *  Tuple
   */

  implicit def tuple2Arith[A:Manifest:Arith,B:Manifest:Arith] : Arith[Tuple2[A,B]] =
    new Arith[Tuple2[A,B]] {
      def +=(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]]) =
        Tuple2(a._1 += b._1, a._2 += b._2)

      def +(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]]) =
        Tuple2(a._1+b._1, a._2+b._2)

      def -(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]]) =
        Tuple2(a._1-b._1, a._2-b._2)

      def *(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]]) =
        Tuple2(a._1*b._1, a._2*b._2)

      def /(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]]) =
        Tuple2(a._1/b._1, a._2/b._2)

      def abs(a: Rep[Tuple2[A,B]]) =
        Tuple2(a._1.abs, a._2.abs)

      def exp(a: Rep[Tuple2[A,B]]) =
        Tuple2(a._1.exp, a._2.exp)
      
      def empty =
        Tuple2(implicitly[Arith[A]].empty, implicitly[Arith[B]].empty)
      
      def zero(a: Rep[Tuple2[A,B]]) =
        Tuple2(a._1.zero, a._2.zero)
    }
  
  implicit def tuple3Arith[A:Manifest:Arith,B:Manifest:Arith,C:Manifest:Arith,D:Manifest:Arith] : Arith[Tuple3[A,B,C]] =
    new Arith[Tuple3[A,B,C]] {
      def +=(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]]) =
        Tuple3(a._1 += b._1, a._2 += b._2, a._3 += b._3)

      def +(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]]) =
        Tuple3(a._1+b._1, a._2+b._2, a._3+b._3)

      def -(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]]) =
        Tuple3(a._1-b._1, a._2-b._2, a._3-b._3)

      def *(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]]) =
        Tuple3(a._1*b._1, a._2*b._2, a._3*b._3)

      def /(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]]) =
        Tuple3(a._1/b._1, a._2/b._2, a._3/b._3)

      def abs(a: Rep[Tuple3[A,B,C]]) =
        Tuple3(a._1.abs, a._2.abs, a._3.abs)

      def exp(a: Rep[Tuple3[A,B,C]]) =
        Tuple3(a._1.exp, a._2.exp, a._3.exp)
      
      def empty =
        Tuple3(implicitly[Arith[A]].empty, implicitly[Arith[B]].empty, implicitly[Arith[C]].empty)

      def zero(a: Rep[Tuple3[A,B,C]]) =
        Tuple3(a._1.zero, a._2.zero, a._3.zero)
    }

  //implicit def tuple4Arith[A,B,C,D](implicit rA: A => Rep[A], rB: B => Rep[B], rC: C => Rep[C], rD: D => Rep[D], opsA: Arith[A], mA: Manifest[A], opsB: Arith[B], mB: Manifest[B],
  implicit def tuple4Arith[A:Manifest:Arith,B:Manifest:Arith,C:Manifest:Arith,D:Manifest:Arith] : Arith[Tuple4[A,B,C,D]] =
    new Arith[Tuple4[A,B,C,D]] {
      def +=(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]]) =
        Tuple4(a._1 += b._1, a._2 += b._2, a._3 += b._3, a._4 += b._4)

      def +(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]]) =
        Tuple4(a._1+b._1, a._2+b._2, a._3+b._3, a._4+b._4)

      def -(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]]) =
        Tuple4(a._1-b._1, a._2-b._2, a._3-b._3, a._4-b._4)

      def *(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]]) =
        Tuple4(a._1*b._1, a._2*b._2, a._3*b._3, a._4*b._4)

      def /(a: Rep[Tuple4[A,B,C,D]], b: Rep[Tuple4[A,B,C,D]]) =
        Tuple4(a._1/b._1, a._2/b._2, a._3/b._3, a._4/b._4)

      def abs(a: Rep[Tuple4[A,B,C,D]]) =
        Tuple4(a._1.abs, a._2.abs, a._3.abs, a._4.abs)

      def exp(a: Rep[Tuple4[A,B,C,D]]) =
        Tuple4(a._1.exp, a._2.exp, a._3.exp, a._4.exp)
      
      def empty =
        Tuple4(implicitly[Arith[A]].empty, implicitly[Arith[B]].empty, implicitly[Arith[C]].empty, implicitly[Arith[D]].empty)

      def zero(a: Rep[Tuple4[A,B,C,D]]) =
        Tuple4(a._1.zero, a._2.zero, a._3.zero, a._4.zero)    
    }


  /**
   * Primitives
   *
   * unfortunately, to use ArithOps, we have to redefine all of the operations we want to
   * to support from NumericOps and FractionalOps, since their implicits are ambiguous with ours.
   */

  implicit val doubleArith : Arith[Double] = new Arith[Double] {
    def +=(a: Rep[Double], b: Rep[Double]) = arith_plus(a,b)
    def +(a: Rep[Double], b: Rep[Double]) = arith_plus(a,b)
    def -(a: Rep[Double], b: Rep[Double]) = arith_minus(a,b)
    def *(a: Rep[Double], b: Rep[Double]) = arith_times(a,b)
    def /(a: Rep[Double], b: Rep[Double]) = arith_fractional_divide(a,b)
    def abs(a: Rep[Double]) = arith_abs(a)
    def exp(a: Rep[Double]) = arith_exp(a)
    def empty = unit(0.0)
    def zero(a: Rep[Double]) = empty
    //def unary_-(a: Rep[Double]) = -a
  }

  implicit val floatArith : Arith[Float] = new Arith[Float] {
    def +=(a: Rep[Float], b: Rep[Float]) = arith_plus(a,b)
    def +(a: Rep[Float], b: Rep[Float]) = arith_plus(a,b)
    def -(a: Rep[Float], b: Rep[Float]) = arith_minus(a,b)
    def *(a: Rep[Float], b: Rep[Float]) = arith_times(a,b)
    def /(a: Rep[Float], b: Rep[Float]) = arith_fractional_divide(a,b)
    def abs(a: Rep[Float]) = arith_abs(a)
    def exp(a: Rep[Float]) = arith_exp(a).asInstanceOfL[Float]
    def empty = unit(0f)
    def zero(a: Rep[Float]) = empty
    //def unary_-(a: Rep[Float]) = -a
  }

  implicit val intArith : Arith[Int] = new Arith[Int] {
    def +=(a: Rep[Int], b: Rep[Int]) = arith_plus(a,b)
    def +(a: Rep[Int], b: Rep[Int]) = arith_plus(a,b)
    def -(a: Rep[Int], b: Rep[Int]) = arith_minus(a,b)
    def *(a: Rep[Int], b: Rep[Int]) = arith_times(a,b)
    def /(a: Rep[Int], b: Rep[Int]) = int_divide(a,b)
    def abs(a: Rep[Int]) = arith_abs(a)
    def exp(a: Rep[Int]) = arith_exp(a).asInstanceOfL[Int]
    def empty = unit(0)
    def zero(a: Rep[Int]) = empty
    //def unary_-(a: Rep[Int]) = -a
  }
  
  def arith_plus[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def arith_minus[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def arith_times[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def arith_fractional_divide[T:Manifest:Fractional](lhs: Rep[T], rhs: Rep[T]) : Rep[T]
  def arith_abs[T:Manifest:Numeric](lhs: Rep[T]): Rep[T]
  def arith_exp[T:Manifest:Numeric](lhs: Rep[T]): Rep[Double]
}

trait ArithOpsExp extends ArithOps with VariablesExp {
  this: OptiLAExp =>

  case class ArithPlus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class ArithPlusEquals[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[Unit]
  case class ArithMinus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class ArithTimes[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class ArithFractionalDivide[T:Manifest:Fractional](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class ArithAbs[T:Manifest:Numeric](lhs: Exp[T]) extends Def[T] {
    val m = manifest[T]
  }
  case class ArithExp[T:Manifest:Numeric](lhs: Exp[T]) extends Def[Double]

  def arith_plus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = reflectPure(ArithPlus(lhs, rhs))
  def arith_minus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = reflectPure(ArithMinus(lhs, rhs))
  def arith_times[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = reflectPure(ArithTimes(lhs, rhs))
  def arith_fractional_divide[T:Manifest:Fractional](lhs: Exp[T], rhs: Exp[T]): Exp[T] = reflectPure(ArithFractionalDivide(lhs, rhs))
  def arith_abs[T:Manifest:Numeric](lhs: Exp[T]) = reflectPure(ArithAbs(lhs))
  def arith_exp[T:Manifest:Numeric](lhs: Exp[T]) = reflectPure(ArithExp(lhs))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    implicit var a: Fractional[A] = null // hack!! need to store it in Def instances??
    e match {
      case ArithPlus(lhs,rhs) => arith_plus(f(lhs), f(rhs))
      case ArithMinus(lhs,rhs) => arith_minus(f(lhs), f(rhs))
      case ArithTimes(lhs,rhs) => arith_times(f(lhs), f(rhs))
      case ArithFractionalDivide(lhs,rhs) => arith_fractional_divide(f(lhs), f(rhs))
      case ArithAbs(lhs) => arith_abs(f(lhs))
      case _ => super.mirror(e,f)
    }
  }
}

trait ArithOpsExpOpt extends ArithOpsExp {
  this: OptiLAExp =>

  override def arith_plus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => unit(implicitly[Numeric[T]].plus(x,y))
    case (Const(0 | 0.0 | 0.0f | -0.0 | -0.0f), y) => y
    case (y, Const(0 | 0.0 | 0.0f | -0.0 | -0.0f)) => y
    case _ => super.arith_plus(lhs, rhs)
  }
  override def arith_minus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => unit(implicitly[Numeric[T]].minus(x,y))
    case (Const(0 | 0.0 | 0.0f | -0.0 | -0.0f), y) => y
    case (y, Const(0 | 0.0 | 0.0f | -0.0 | -0.0f)) => y
    case _ => super.arith_minus(lhs, rhs)
  }
  override def arith_times[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = (lhs,rhs) match {
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

