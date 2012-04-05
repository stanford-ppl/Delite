package ppl.dsl.optiml.application

import ppl.dsl.optiml._
import java.io.PrintWriter
import scala.virtualization.lms.common.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}


trait RectOps extends Variables with OverloadHack {

  object Rect {
    def apply(x: Rep[Int], y: Rep[Int], width: Rep[Int], height: Rep[Int]) = rect_obj_new(x, y, width, height)
  }

  implicit def repRectToRectOps(x: Rep[Rect]) = new rectOpsCls(x)
  implicit def rectToRectOps(x: Var[Rect]) = new rectOpsCls(readVar(x))

  class rectOpsCls(__x: Rep[Rect]) {
    def x = rect_x(__x)
    def y = rect_y(__x)
    def width = rect_width(__x)
    def height = rect_height(__x)
  }

  //object defs
  def rect_obj_new(x: Rep[Int], y: Rep[Int], width: Rep[Int], height: Rep[Int]): Rep[Rect]

  //class defs
  def rect_x(__x: Rep[Rect]): Rep[Int]
  def rect_y(__x: Rep[Rect]): Rep[Int]
  def rect_width(__x: Rep[Rect]): Rep[Int]
  def rect_height(__x: Rep[Rect]): Rep[Int]
}

trait RectOpsExp extends RectOps with EffectExp {
  case class RectObjectNew(x: Exp[Int], y: Exp[Int], width: Exp[Int], height: Exp[Int]) extends Def[Rect]
  case class RectX(__x: Exp[Rect]) extends Def[Int]
  case class RectY(__x: Exp[Rect]) extends Def[Int]
  case class RectWidth(__x: Exp[Rect]) extends Def[Int]
  case class RectHeight(__x: Exp[Rect]) extends Def[Int]

  def rect_obj_new(x: Exp[Int], y: Exp[Int], width: Exp[Int], height: Exp[Int]) = reflectEffect(RectObjectNew(x, y, width, height))
  def rect_x(__x: Rep[Rect]) = RectX(__x)
  def rect_y(__x: Rep[Rect]) = RectY(__x)
  def rect_width(__x: Rep[Rect]) = RectWidth(__x)
  def rect_height(__x: Rep[Rect]) = RectHeight(__x)
}

trait ScalaGenRectOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case RectObjectNew(x, y, width, height) => emitValDef(sym, "new " + remap(manifest[Rect]) + "(" + quote(x)  + "," + quote(y)  + "," + quote(width)  + "," + quote(height)  + ")")
    case RectX(x) =>  emitValDef(sym, quote(x) + ".x")
    case RectY(x) =>  emitValDef(sym, quote(x) + ".y")
    case RectWidth(x) =>  emitValDef(sym, quote(x) + ".width")
    case RectHeight(x) =>  emitValDef(sym, quote(x) + ".height")
    case _ => super.emitNode(sym, rhs)
  }
}
