package ppl.dsl.optiml.embedded

/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Mar 29, 2010
 * modified: Mar 29, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import java.io.{PrintWriter}

import scala.virtualization.lms.internal.ScalaCodegen
import scala.virtualization.lms.common.{EffectExp, Base}

trait Vector[T]
trait VectorOps extends Base {
  //type InternalVector[T]
  //type Vector[T] = Rep[InternalVector[T]]

  object Vector {
    def apply[A](len: Rep[Int]) : Rep[Vector[A]] = vector_obj_apply(true, len)
    def apply[A](is_row: Rep[Boolean], len: Rep[Int]) : Rep[Vector[A]] = vector_obj_apply(is_row, len)
    def zeros(len: Rep[Int]) : Rep[Vector[Double]] = vector_obj_zeros(len)
  }

  class vecRepCls[A](x: Rep[Vector[A]]) {
    def apply(n: Rep[Int]) = vector_apply(x, n)
    def update(n: Rep[Int], y: Rep[A]) = vector_update(x,n,y)
    def length = vector_length(x)
    def toBoolean(implicit conv: Rep[A => Boolean]) = vector_toboolean(x)
    def +(y: Rep[Vector[A]]) = vector_plus(x,y)
    def -(y: Rep[Vector[A]]) = vector_minus(x,y)
    def *(y: Rep[Vector[A]]) = vector_times(x,y)
    def /(y: Rep[A]) = vector_divide(x,y)
    def outer(y: Rep[Vector[A]]) = vector_outer(x,y)
    def trans  = vector_trans(x)
    def pprint = vector_pprint(x)

    def +=(y: Rep[A]) = vector_plusequals(x,y)
  }

  implicit def repVecToRepVecOps[A](x: Rep[Vector[A]]) = new vecRepCls(x)
  implicit def vecToRepVecOps[A](x: Vector[A]) = new vecRepCls(x)

  // object defs
  def vector_obj_apply[A](is_row: Rep[Boolean], len: Rep[Int]): Rep[Vector[A]]
  def vector_obj_zeros(len: Rep[Int]): Rep[Vector[Double]]

  // class defs
  def vector_apply[A](x: Rep[Vector[A]], n: Rep[Int]): Rep[A]
  def vector_update[A](x: Rep[Vector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_length[A](x: Rep[Vector[A]]): Rep[Int]
  def vector_toboolean[A](x: Rep[Vector[A]])(implicit conv: Rep[A => Boolean]): Rep[Vector[Boolean]]
  def vector_plus[A](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus[A](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_times[A](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_divide[A](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_trans[A](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_outer[A](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Matrix[A]]
  def vector_pprint[A](x: Rep[Vector[A]]): Rep[Unit]
  def vector_plusequals[A](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
}

/* no longer available in common embedding
trait VectorOpsRepString extends VectorOps with RepString {
  def vector_obj_apply[A](is_row: String, len: String) = "Vector(" + is_row + ", " + len + ")"
  def vector_obj_zeros(len: String) = "Vector.zeros(" + len + ")"

  def vector_apply[A](x: String, n: String) = x + "(" + n + ")"
  def vector_update[A](x: String, n: String, y: String) = x + "(" + n + ") = " + y
  def vector_length[A](x: Rep[Vector[A]]) = "length(" + x + ")"
  def vector_toboolean[A](x: Rep[Vector[A]])(implicit conv: Rep[A => Boolean]) = x + ".toBoolean()"
  def vector_plus[A](x: String, y: String) = x + " + " + y
  def vector_minus[A](x: String, y: String) = x + " - " + y
  def vector_times[A](x: String, y: String) = x + " * " + y
  def vector_divide[A](x: String, y: String) = x + " / " + y
  def vector_trans[A](x: String) = "trans(" + x + ")"
  def vector_outer[A](x: String, y: String) = x + ".outer(" + y + ")"
  def vector_pprint[A](x: Rep[Vector[A]]) = "pprint(" + x + ")"
  def vector_plusequals[A](x: String, y: String) = x + "+=" + y
}
*/

trait VectorOpsRepExp extends VectorOps with EffectExp {
  case class VectorObjectApply[A](is_row: Exp[Boolean], len: Exp[Int]) extends Def[Vector[A]]
  case class VectorObjectZeros(len: Exp[Int]) extends Def[Vector[Double]]

  case class VectorApply[A](x: Exp[Vector[A]], n: Exp[Int]) extends Def[A]
  case class VectorUpdate[A](x: Rep[Vector[A]], n: Rep[Int], y: Rep[A]) extends Def[Unit]
  case class VectorLength[A](x: Exp[Vector[A]]) extends Def[Int]
  case class VectorToBoolean[A](x: Exp[Vector[A]], conv: Rep[A => Boolean]) extends Def[Vector[Boolean]]
  case class VectorPlus[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorMinus[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorTimes[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorDivide[A](x: Exp[Vector[A]], y: Exp[A]) extends Def[Vector[A]]
  case class VectorTrans[A](x: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorOuter[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) extends Def[Matrix[A]]
  case class VectorPPrint[A](x: Exp[Vector[A]]) extends Def[Unit]
  case class VectorPlusEquals[A](x: Exp[Vector[A]], y: Exp[A]) extends Def[Vector[A]]

  // operations on composites
  def vector_obj_apply[A](is_row: Exp[Boolean], len: Exp[Int]) = VectorObjectApply[A](is_row, len)
  def vector_obj_zeros(len: Exp[Int]) = VectorObjectZeros(len)

  def vector_apply[A](x: Exp[Vector[A]], n: Exp[Int]) = VectorApply(x, n)
  def vector_update[A](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) = VectorUpdate(x,n,y)
  def vector_length[A](x: Exp[Vector[A]]) = VectorLength(x)
  def vector_toboolean[A](x: Exp[Vector[A]])(implicit conv: Rep[A => Boolean]) = VectorToBoolean(x, conv)
  def vector_plus[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x, y)
  def vector_minus[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorMinus(x, y)
  def vector_times[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorTimes(x, y)
  def vector_divide[A](x: Exp[Vector[A]], y: Exp[A]) = VectorDivide(x, y)
  def vector_trans[A](x: Exp[Vector[A]]) = VectorTrans(x)
  def vector_outer[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorOuter(x, y)
  def vector_pprint[A](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x))
  def vector_plusequals[A](x: Exp[Vector[A]], y: Exp[A]) = VectorPlusEquals(x, y)
}

/**
 * Optimizations for composite TOps operations.
 */

trait VectorOpsRepExpOpt extends VectorOpsRepExp {
  override def vector_plus[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // (TB + TD) == T(B + D)
    case (Def(VectorTimes(a, b)), Def(VectorTimes(c, d))) if (a == c) => VectorTimes[A](a.asInstanceOf[Exp[Vector[A]]], VectorPlus[A](b.asInstanceOf[Exp[Vector[A]]],d.asInstanceOf[Exp[Vector[A]]]))
    // ...
    case _ => super.vector_plus(x, y)
  }

  override def vector_times[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    case _ => super.vector_times(x, y)
  }
}

/* CodeGen module */
trait ScalaCodegenVector extends ScalaCodegen { this: VectorOpsRepExp =>
  private val base = "ppl.dsl.optiml.direct.Vector"

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case VectorObjectZeros(len) => emitValDef(sym, base + ".zeros(" + quote(len) + ")")

    case VectorApply(x, n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
    case VectorLength(x)    => emitValDef(sym, "" + quote(x) + ".length")

    case VectorToBoolean(x, conv) => emitValDef(sym, "" + quote(x) + ".toBoolean(" + quote(conv) + ")")

    case VectorPlus(x, y)   => emitValDef(sym, "" + quote(x) + " + " + quote(y))
    case VectorMinus(x, y)  => emitValDef(sym, "" + quote(x) + " - " + quote(y))
    case VectorDivide(x, y) => emitValDef(sym, "" + quote(x) + " / " + quote(y))

    case VectorTrans(x)     => emitValDef(sym, "" + quote(x) + ".trans")

    case VectorOuter(x, y)  => emitValDef(sym, "" + quote(x) + ".outer(" + quote(y) + ")")

    case VectorPPrint(x) => emitValDef(sym, quote(x) + ".pprint")

    case _ => super.emitNode(sym, rhs)
  }
}
