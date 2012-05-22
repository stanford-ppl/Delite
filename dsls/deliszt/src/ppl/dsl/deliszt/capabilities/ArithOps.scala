package ppl.dsl.deliszt.capabilities

import java.io.PrintWriter
import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{CLikeCodegen}
import scala.virtualization.lms.util.OverloadHack

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

/*
* Arith definitions for OptiML supported types.
*
* author:  Arvind Sujeeth (asujeeth@stanford.edu)
* created: Dec 2, 2010
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

trait ArithOps extends Variables with OverloadHack {
  this: DeLiszt =>

  type Arith[X] = ArithInternal[Rep, X]

  /**
   * Interface, enables using Ariths with operator notation. Note that the inclusion of these
   * causes the NumericOps and FractionalOps implicit conversions to be ambiguous, so OptiML
   * programs cannot include them.
   */
  implicit def arithToArithOps[T: Arith : Manifest](n: T) = new ArithOpsCls(unit(n))
  implicit def repArithToArithOps[T: Arith : Manifest](n: Rep[T]) = new ArithOpsCls(n)
  implicit def varArithToArithOps[T: Arith : Manifest](n: Var[T]) = new ArithOpsCls(readVar(n))
  
  // to do Rep[Int] * Float, it should get converted to Rep[Float] * Float
  // TODO: this only works when invoked explicitly (won't kick in itself)
  /*implicit*/ def chainRepArithToArithOps[A:Manifest:Arith, B:Manifest:Arith](a: Rep[A])
                                            (implicit c: Rep[A] => Rep[B]) = new ArithOpsCls(c(a))

  class ArithOpsCls[T:Manifest:Arith](lhs: Rep[T])(implicit arith: Arith[T]) {
    // TODO: if B == Rep[T] below, the ops implicit does not work unless it is called explicitly (no unambiguous resolution?)
    //def +=(rhs: Rep[T]): Rep[T] = arith.+=(lhs, rhs)
    def +(rhs: Rep[T]): Rep[T] = arith.+(lhs, rhs)
    def -(rhs: Rep[T]): Rep[T] = arith.-(lhs, rhs)
    def *(rhs: Rep[T]): Rep[T] = arith.*(lhs, rhs)
    def /(rhs: Rep[T]): Rep[T] = arith./(lhs, rhs)

    //def +=[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith.+=(lhs, c(rhs))
    def +[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith.+(lhs, c(rhs))
    def -[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith.-(lhs, c(rhs))
    def *[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith.*(lhs, c(rhs))
    def /[B](rhs: B)(implicit c: B => Rep[T]): Rep[T] = arith./(lhs, c(rhs))

    def abs: Rep[T] = arith.abs(lhs)
    def exp: Rep[T] = arith.exp(lhs)

    def unary_-(): Rep[T] = arith.unary_-(lhs)
    
    def empty: Rep[T] = arith.empty
    def zero: Rep[T] = arith.zero(lhs)
  }


  // TODO: why is this needed, given the definition of / above?
  def infix_/[T:Manifest:Fractional, B](lhs: Rep[T], rhs: B)(implicit c: B => Rep[T]) = arith_fractional_divide(lhs, c(rhs))

  // why are these recursive? (perhaps because the abstract arith method has the same signature as the infix?)
  //  def infix_+=[T,B](lhs: Rep[T], rhs:B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a.+=(lhs,c(rhs))
  //  def infix_+[T,B](lhs: Rep[T], rhs:B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a.+(lhs,c(rhs))
  //  def infix_-[T,B](lhs: Rep[T], rhs:B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a.-(lhs,c(rhs))
  //  def infix_*[T,B](lhs: Rep[T], rhs:B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a.*(lhs,c(rhs))
  //  def infix_/[T,B](lhs: Rep[T], rhs: B)(implicit a: Arith[T], c: B => Rep[T], mT: Manifest[T]) = a./(lhs,c(rhs))
  //  def infix_abs[T](lhs: Rep[T])(implicit a: Arith[T], mT: Manifest[T]) = a.abs(lhs)
  //  def infix_exp[T](lhs: Rep[T])(implicit a: Arith[T], mT: Manifest[T]) = a.exp(lhs)

  /**
   * Vec
   */

  implicit def VecArith[N <: IntM : Manifest:MVal, T: Arith : Manifest]: Arith[Vec[N, T]] = new Arith[Vec[N, T]] {
    // these are used in sum; dynamic checks are required due to conditionals
    def +(a: Rep[Vec[N, T]], b: Rep[Vec[N, T]]) = a + b
    def -(a: Rep[Vec[N, T]], b: Rep[Vec[N, T]]) = a - b
    def *(a: Rep[Vec[N, T]], b: Rep[Vec[N, T]]) = a * b
    def /(a: Rep[Vec[N, T]], b: Rep[Vec[N, T]]) = a / b
    def unary_-(a: Rep[Vec[N, T]]) = -a
    
    def abs(a: Rep[Vec[N,T]]) = a.abs
    def exp(a: Rep[Vec[N,T]]) = a.exp
    
    def empty : Rep[Vec[N,T]] = Vec[N,T]()
    def zero(a: Rep[Vec[N,T]]) = Vec[N,T]()
  }


  /**
   * Mat
   */

  implicit def MatArith[R <: IntM : Manifest:MVal, C <: IntM : Manifest:MVal, T: Arith : Manifest]: Arith[Mat[R, C, T]] = new Arith[Mat[R, C, T]] {
    def +(a: Rep[Mat[R,C,T]], b: Rep[Mat[R,C,T]]) = a + b
    def -(a: Rep[Mat[R,C,T]], b: Rep[Mat[R,C,T]]) = a - b
    def *(a: Rep[Mat[R,C,T]], b: Rep[Mat[R,C,T]]) = a *:* b
    def /(a: Rep[Mat[R,C,T]], b: Rep[Mat[R,C,T]]) = a / b
    def unary_-(a: Rep[Mat[R, C, T]]) = -a
    
    def abs(a: Rep[Mat[R,C,T]]) = a.abs
    def exp(a: Rep[Mat[R,C,T]]) = a.exp
    
    def empty : Rep[Mat[R,C,T]] = Mat[R,C,T]()
    def zero(a: Rep[Mat[R,C,T]]) = Mat[R,C,T]()
  }

  /**
   * Primitives
   *
   * unfortunately, to use ArithOps, we have to redefine all of the operations we want to
   * to support from NumericOps and FractionalOps, since their implicits are ambiguous with ours.
   */

  implicit val doubleArith: Arith[Double] = new Arith[Double] {
    //def +=(a: Rep[Double], b: Rep[Double]) = arith_plus(a, b)
    def +(a: Rep[Double], b: Rep[Double]) = arith_plus(a, b)
    def -(a: Rep[Double], b: Rep[Double]) = arith_minus(a, b)
    def *(a: Rep[Double], b: Rep[Double]) = arith_times(a, b)
    def /(a: Rep[Double], b: Rep[Double]) = arith_fractional_divide(a, b)
    def abs(a: Rep[Double]) = arith_abs(a)
    def exp(a: Rep[Double]) = arith_exp(a)
    def unary_-(a: Rep[Double]) = arith_negate(a)
    def empty = unit(0.0)
    def zero(a: Rep[Double]) = empty
  }

  implicit val floatArith: Arith[Float] = new Arith[Float] {
    //def +=(a: Rep[Float], b: Rep[Float]) = arith_plus(a, b)
    def +(a: Rep[Float], b: Rep[Float]) = arith_plus(a, b)
    def -(a: Rep[Float], b: Rep[Float]) = arith_minus(a, b)
    def *(a: Rep[Float], b: Rep[Float]) = arith_times(a, b)
    def /(a: Rep[Float], b: Rep[Float]) = arith_fractional_divide(a, b)
    def abs(a: Rep[Float]) = arith_abs(a)
    def exp(a: Rep[Float]) = arith_exp(a).AsInstanceOf[Float]
    def unary_-(a: Rep[Float]) = arith_negate(a)
    def empty = unit(0f)
    def zero(a: Rep[Float]) = empty
  }

  implicit val intArith: Arith[Int] = new Arith[Int] {
   // def +=(a: Rep[Int], b: Rep[Int]) = arith_plus(a, b)
    def +(a: Rep[Int], b: Rep[Int]) = arith_plus(a, b)
    def -(a: Rep[Int], b: Rep[Int]) = arith_minus(a, b)
    def *(a: Rep[Int], b: Rep[Int]) = arith_times(a, b)
    def /(a: Rep[Int], b: Rep[Int]) = int_divide(a, b)
    def abs(a: Rep[Int]) = arith_abs(a)
    def exp(a: Rep[Int]) = arith_exp(a).AsInstanceOf[Int]
    def unary_-(a: Rep[Int]) = arith_negate(a)
    def empty = unit(0)
    def zero(a: Rep[Int]) = empty
  }

  def arith_plus[T: Manifest : Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def arith_minus[T: Manifest : Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def arith_times[T: Manifest : Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def arith_fractional_divide[T: Manifest : Fractional](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def arith_abs[T: Manifest : Numeric](lhs: Rep[T]): Rep[T]
  def arith_exp[T: Manifest : Numeric](lhs: Rep[T]): Rep[Double]
  def arith_negate[T: Manifest : Numeric](lhs: Rep[T]): Rep[T]
}

trait ArithOpsExp extends ArithOps with VariablesExp {
  this: DeLiszt =>

  case class ArithPlus[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  //case class ArithPlusEquals[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[Unit]

  case class ArithMinus[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  case class ArithTimes[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  case class ArithNegate[T: Manifest : Numeric](lhs: Exp[T]) extends Def[T]

  case class ArithFractionalDivide[T: Manifest : Fractional](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  case class ArithAbs[T: Manifest : Numeric](lhs: Exp[T]) extends Def[T] {
    val m = manifest[T]
  }

  case class ArithExp[T: Manifest : Numeric](lhs: Exp[T]) extends Def[Double]

  def arith_plus[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = ArithPlus(lhs, rhs)
  def arith_minus[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = ArithMinus(lhs, rhs)
  def arith_times[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = ArithTimes(lhs, rhs)
  def arith_negate[T: Manifest : Numeric](lhs: Exp[T]) = ArithNegate(lhs)
  def arith_fractional_divide[T: Manifest : Fractional](lhs: Exp[T], rhs: Exp[T]): Exp[T] = ArithFractionalDivide(lhs, rhs)
  def arith_abs[T: Manifest : Numeric](lhs: Exp[T]) = ArithAbs(lhs)
  def arith_exp[T: Manifest : Numeric](lhs: Exp[T]) = ArithExp(lhs)

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    implicit var a: Fractional[A] = null // hack!! need to store it in Def instances??
    e match {
      case ArithPlus(lhs, rhs) => arith_plus(f(lhs), f(rhs))
      case ArithMinus(lhs, rhs) => arith_minus(f(lhs), f(rhs))
      case ArithTimes(lhs, rhs) => arith_times(f(lhs), f(rhs))
      case ArithNegate(lhs) => arith_negate(f(lhs))
      case ArithFractionalDivide(lhs, rhs) => arith_fractional_divide(f(lhs), f(rhs))
      case ArithAbs(lhs) => arith_abs(f(lhs))
      
      case Reflect(e@ArithPlus(lhs,rhs), u, es) => reflectMirrored(Reflect(ArithPlus(f(lhs),f(rhs)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@ArithMinus(lhs,rhs), u, es) => reflectMirrored(Reflect(ArithMinus(f(lhs),f(rhs)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@ArithTimes(lhs,rhs), u, es) => reflectMirrored(Reflect(ArithTimes(f(lhs),f(rhs)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@ArithNegate(lhs), u, es) => reflectMirrored(Reflect(ArithNegate(f(lhs)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@ArithFractionalDivide(lhs,rhs), u, es) => reflectMirrored(Reflect(ArithFractionalDivide(f(lhs),f(rhs)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@ArithAbs(lhs), u, es) => reflectMirrored(Reflect(ArithAbs(f(lhs)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case _ => super.mirror(e, f)
    }
  }
}

trait ArithOpsExpOpt extends ArithOpsExp {
  this: DeLiszt =>

  override def arith_plus[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = (lhs, rhs) match {
    case (Const(x), Const(y)) => unit(implicitly[Numeric[T]].plus(x, y))
    case _ => super.arith_plus(lhs, rhs)
  }
  override def arith_minus[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = (lhs, rhs) match {
    case (Const(x), Const(y)) => unit(implicitly[Numeric[T]].minus(x, y))
    case _ => super.arith_minus(lhs, rhs)
  }
  override def arith_times[T: Manifest : Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = (lhs, rhs) match {
    case (Const(x), Const(y)) => unit(implicitly[Numeric[T]].times(x, y))
    case _ => super.arith_times(lhs, rhs)
  }
  override def arith_negate[T: Manifest : Numeric](lhs: Exp[T]): Exp[T] = lhs match {
    case Const(x) => unit(implicitly[Numeric[T]].negate(x))
    case _ => super.arith_negate(lhs)
  }
}


trait ScalaGenArithOps extends ScalaGenBase {
  val IR: ArithOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArithPlus(a, b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case ArithMinus(a, b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case ArithTimes(a, b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case ArithNegate(a) => emitValDef(sym, "-" + quote(a))
    case ArithFractionalDivide(a, b) => emitValDef(sym, quote(a) + " / " + quote(b))
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ArithPlus(a, b) => emitValDef(sym, quote(a) + " + " + quote(b))
      case ArithMinus(a, b) => emitValDef(sym, quote(a) + " - " + quote(b))
      case ArithTimes(a, b) => emitValDef(sym, quote(a) + " * " + quote(b))
      case ArithNegate(a) => emitValDef(sym, "-" + quote(a))
      case ArithFractionalDivide(a, b) => emitValDef(sym, quote(a) + " / " + quote(b))
      case ArithAbs(x) => emitValDef(sym, "abs(" + quote(x) + ")")
      case ArithExp(a) => emitValDef(sym, "exp(" + quote(a) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenArithOps extends CudaGenBase with CLikeGenArithOps

trait CGenArithOps extends CGenBase with CLikeGenArithOps

