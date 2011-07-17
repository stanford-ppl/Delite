package ppl.dsl.optiml.vector

import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base, ScalaGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml.datastruct.scala.{VectorView, VectorViewImpl}

trait VectorViewOps extends DSLType with Base with OverloadHack {

  def infix_start[A](v: Rep[VectorView[A]])(implicit o: Overloaded1) = vectorview_start(v)
  def infix_stride[A](v: Rep[VectorView[A]]) = vectorview_stride(v)

  // class defs
  def vectorview_start[A](x: Rep[VectorView[A]]): Rep[Int]
  def vectorview_stride[A](x: Rep[VectorView[A]]): Rep[Int]

  // impl defs
  //def vectorview_new[A:Manifest](x: Rep[Array[A]], offset: Rep[Int], stride: Rep[Int], len: Rep[Int], isRow: Rep[Boolean]) : Rep[Vector[A]]
}

trait VectorViewOpsExp extends VectorViewOps with BaseExp { this: VectorViewImplOps with DeliteOpsExp =>

  // implemented via method on real data structure
  case class VectorViewStart[A](x: Exp[VectorView[A]]) extends Def[Int]
  case class VectorViewStride[A](x: Exp[VectorView[A]]) extends Def[Int]
//  case class VectorViewNew[A:Manifest](x: Rep[Array[A]], offset: Rep[Int], stride: Rep[Int], len: Exp[Int], isRow: Exp[Boolean])
//    extends Def[Vector[A]] {
//    val mV = manifest[VectorViewImpl[A]]
//  }

  def vectorview_start[A](x: Exp[VectorView[A]]) = reflectPure(VectorViewStart(x))
  def vectorview_stride[A](x: Exp[VectorView[A]]) = reflectPure(VectorViewStride(x))

//  def vectorview_new[A:Manifest](x: Rep[Array[A]], offset: Rep[Int], stride: Rep[Int], len: Exp[Int], isRow: Exp[Boolean])
//    = reflectEffect(VectorViewNew[A](x, offset, stride, len, isRow))
}

trait ScalaGenVectorViewOps extends ScalaGenBase {
  val IR: VectorViewOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
//    case v@VectorViewNew(x, offset, stride, len, isRow) =>
//      emitValDef(sym, "new " + remap(v.mV) + "(" + quote(x) + "," + quote(offset) + "," + quote(stride) + "," + quote(len) + "," + quote(isRow) + ")")
    case VectorViewStart(x)   => emitValDef(sym, quote(x) + ".start")
    case VectorViewStride(x)  => emitValDef(sym, quote(x) + ".stride")

    case _ => super.emitNode(sym, rhs)
  }
}