package ppl.dsl.optiml.vector

import ppl.dsl.optiml.datastruct.scala._
import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.internal.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.delite.framework.ops.DeliteOpsExp

trait LabelsOps extends DSLType with Base with OverloadHack {

  object Labels {
    def apply[A:Manifest](length: Rep[Int], isRow: Rep[Boolean]) = labels_obj_new[A](length, isRow)
    def apply[A:Manifest](xs: Rep[Vector[A]]) = labels_obj_fromVec(xs)
  }

  // object defs
  def labels_obj_new[A:Manifest](length: Rep[Int], isRow: Rep[Boolean]): Rep[Labels[A]]
  def labels_obj_fromVec[A:Manifest](xs: Rep[Vector[A]]): Rep[Labels[A]]
}

trait LabelsOpsExp extends LabelsOps with BaseExp { this: DeliteOpsExp =>

  // implemented via method on real data structure
  case class LabelsObjectNew[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) extends Def[Labels[A]] {
    val mV = manifest[LabelsImpl[A]]
  }
  case class LabelsObjectFromVec[A:Manifest](xs: Exp[Vector[A]]) extends Def[Labels[A]] {
    val mV = manifest[LabelsImpl[A]]
  }

  def labels_obj_new[A:Manifest](length: Exp[Int], isRow: Exp[Boolean]) = reflectEffect(LabelsObjectNew(length, isRow))
  def labels_obj_fromVec[A:Manifest](xs: Exp[Vector[A]]) = reflectEffect(LabelsObjectFromVec(xs))
}

trait ScalaGenLabelsOps extends ScalaGenBase {
  val IR: LabelsOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case l@LabelsObjectNew(length, isRow) => emitValDef(sym, "new " + remap(l.mV) + "(" + quote(length) + "," + quote(isRow) + ")")
    case l@LabelsObjectFromVec(xs) => emitValDef(sym, "new " + remap(l.mV) + "(" + quote(xs) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
