package ppl.dsl.optiml

import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.{Base}
import scala.virtualization.lms.common.embedded.scala.DSLOpsExp
import scala.virtualization.lms.internal.ScalaGenBase

trait VectorView[T] extends Vector[T]

trait VectorViewOps extends DSLType with Base {

  def infix_start[A](v: Rep[VectorView[A]]) = vectorview_start(v)
  def infix_stride[A](v: Rep[VectorView[A]]) = vectorview_stride(v)

  // class defs
  def vectorview_start[A](x: Rep[VectorView[A]]): Rep[Int]
  def vectorview_stride[A](x: Rep[VectorView[A]]): Rep[Int]

  // impl defs
  def vectorview_new[A : Manifest](x: Rep[Array[A]], offset: Rep[Int], stride: Rep[Int], len: Rep[Int], is_row: Rep[Boolean]) : Rep[Vector[A]]
}

trait VectorViewOpsExp extends VectorViewOps with DSLOpsExp { this: VectorViewImplOps =>

  // implemented via method on real data structure
  case class VectorViewStart[A](x: Exp[VectorView[A]]) extends Def[Int]
  case class VectorViewStride[A](x: Exp[VectorView[A]]) extends Def[Int]

  case class VectorViewNew[A : Manifest](x: Rep[Array[A]], offset: Rep[Int], stride: Rep[Int], len: Exp[Int], is_row: Exp[Boolean])
    extends DSLOp(reifyEffects(vectorview_new_impl[A](x, offset, stride, len, is_row)))

  def vectorview_start[A](x: Exp[VectorView[A]]) = VectorViewStart(x)
  def vectorview_stride[A](x: Exp[VectorView[A]]) = VectorViewStride(x)

  def vectorview_new[A : Manifest](x: Rep[Array[A]], offset: Rep[Int], stride: Rep[Int], len: Exp[Int], is_row: Exp[Boolean])
    = reflectEffect(VectorViewNew[A](x, offset, stride, len, is_row))
}

trait ScalaGenVectorViewOps extends ScalaGenBase {
  val IR: VectorViewOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case VectorViewStart(x)    => emitValDef(sym, quote(x) + ".start")
    case VectorViewStride(x)     => emitValDef(sym, quote(x) + ".stride")

    case _ => super.emitNode(sym, rhs)
  }
}