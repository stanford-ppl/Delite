package ppl.dsl.optiml

import scala.virtualization.lms.internal.ScalaCodegen
import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.codegen.scala.CodeGeneratorScalaBase
import scala.virtualization.lms.common.{EffectExp, FunctionsExp, Base}
import ppl.delite.framework.embedded.scala.DSLOpsExp

trait VectorView[T] extends Vector[T]

trait VectorViewOps extends DSLType { this: DeliteApplication =>

  implicit def repVecViewToRepVecViewOps[A](x: Rep[VectorView[A]]) = new vecViewRepCls(x)
  implicit def vecViewToRepVecViewOps[A](x: VectorView[A]) = new vecViewRepCls(x)

  class vecViewRepCls[A](x: Rep[VectorView[A]]) {
    def start = vectorview_start(x)
    def stride = vectorview_stride(x)
  }

  // class defs
  def vectorview_start[A](x: Rep[VectorView[A]]): Rep[Int]
  def vectorview_stride[A](x: Rep[VectorView[A]]): Rep[Int]

  // impl defs
  def vectorview_new[A : Manifest](x: Rep[Array[A]], offset: Rep[Int], stride: Rep[Int], len: Rep[Int], is_row: Rep[Boolean]) : Rep[Vector[A]]
}

trait VectorViewOpsExp extends VectorViewOps { this: DeliteApplication with VectorViewImplOps with DSLOpsExp =>

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

trait CodeGeneratorScalaVectorView extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with VectorViewOpsExp with EffectExp
  import intermediate._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {

    // these are the ops that call through to the underlying real data structure
    case VectorViewStart(x)    => emitValDef(sym, quote(x) + ".start")
    case VectorViewStride(x)     => emitValDef(sym, quote(x) + ".stride")

    case _ => super.emitNode(sym, rhs)
  }
}