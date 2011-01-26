package ppl.dsl.optiml

import datastruct.scala._
import java.io.PrintWriter
import ppl.delite.framework.{DSLType}
import scala.virtualization.lms.internal.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}

trait ApplicationOps extends RectOps with BinarizedGradientTemplateOps
trait ApplicationOpsExp extends RectOpsExp with BinarizedGradientTemplateOpsExp
trait ScalaGenApplicationOps extends ScalaGenRectOps with ScalaGenBinarizedGradientTemplateOps

trait RectOps extends DSLType with Variables with OverloadHack {

  object Rect {
    def apply(x: Rep[Int], y: Rep[Int], width: Rep[Int], height: Rep[Int]) = rect_obj_new(x, y, width, height)
  }

  implicit def repRectToRectOps(x: Rep[Rect]) = new rectOpsCls(x)
  implicit def rectToRectOps(x: Var[Rect]) = new rectOpsCls(readVar(x))

  class rectOpsCls(r: Rep[Rect]) {
    def x = rect_x(r)
    def y = rect_y(r)
    def width = rect_width(r)
    def height = rect_height(r)
  }

  // object defs
  def rect_obj_new(x: Rep[Int], y: Rep[Int], width: Rep[Int], height: Rep[Int]): Rep[Rect]

  // class defs
  def rect_x(x: Rep[Rect]): Rep[Int]
  def rect_y(x: Rep[Rect]): Rep[Int]
  def rect_width(x: Rep[Rect]): Rep[Int]
  def rect_height(x: Rep[Rect]): Rep[Int]
}

trait RectOpsExp extends RectOps with EffectExp {
  // implemented via method on real data structure
  case class RectObjectNew(x: Exp[Int], y: Exp[Int], width: Exp[Int], height: Exp[Int]) extends Def[Rect]
  case class RectX(x: Exp[Rect]) extends Def[Int]
  case class RectY(x: Exp[Rect]) extends Def[Int]
  case class RectWidth(x: Exp[Rect]) extends Def[Int]
  case class RectHeight(x: Exp[Rect]) extends Def[Int]

  def rect_obj_new(x: Exp[Int], y: Exp[Int], width: Exp[Int], height: Exp[Int]) = reflectEffect(RectObjectNew(x,y,width,height))
  def rect_x(x: Exp[Rect]) = RectX(x)
  def rect_y(x: Exp[Rect]) = RectY(x)
  def rect_width(x: Exp[Rect]) = RectWidth(x)
  def rect_height(x: Exp[Rect]) = RectHeight(x)
}

trait ScalaGenRectOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case RectObjectNew(x,y,width,height) => emitValDef(sym, "new " + remap(manifest[Rect]) + "(" + quote(x) + "," + quote(y) + "," + quote(width) + "," + quote(height) + ")")
    case RectX(x) => emitValDef(sym, quote(x) + ".x")
    case RectY(x) => emitValDef(sym, quote(x) + ".y")
    case RectWidth(x) => emitValDef(sym, quote(x) + ".width")
    case RectHeight(x) => emitValDef(sym, quote(x) + ".height")
    case _ => super.emitNode(sym, rhs)
  }
}


trait BinarizedGradientTemplateOps extends DSLType with Variables with OverloadHack {

  object BinarizedGradientTemplate {
    def apply(radius: Rep[Int], rect: Rep[Rect], mask_list: Rep[Vector[Int]], level: Rep[Int],
              binary_gradients: Rep[Vector[Int]], match_list: Rep[IndexVector], occlusions: Rep[Vector[Vector[Int]]],
              templates: Rep[Vector[BinarizedGradientTemplate]], hist: Rep[Vector[Float]]) = { 
                
                binarizedgradienttemplate_obj_new(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist)
    }
  }

  implicit def repBinarizedGradientTemplateToBinarizedGradientTemplateOps(x: Rep[BinarizedGradientTemplate]) = new binarizedGradientTemplateOpsCls(x)
  implicit def binarizedGradientTemplateToBinarizedGradientTemplateOps(x: Var[BinarizedGradientTemplate]) = new binarizedGradientTemplateOpsCls(readVar(x))

  class binarizedGradientTemplateOpsCls[A:Manifest](x: Rep[BinarizedGradientTemplate]) {
    def radius = binarizedgradienttemplate_radius(x)
    def rect = binarizedgradienttemplate_rect(x)
    def mask_list = binarizedgradienttemplate_mask_list(x)
    def level = binarizedgradienttemplate_level(x)
    def binary_gradients = binarizedgradienttemplate_binary_gradients(x)
    def match_list = binarizedgradienttemplate_match_list(x)
    def occlusions = binarizedgradienttemplate_occlusions(x)
    def templates = binarizedgradienttemplate_templates(x)
    def hist = binarizedgradienttemplate_hist(x)
  }

  // object defs
  def binarizedgradienttemplate_obj_new(radius: Rep[Int], rect: Rep[Rect], mask_list: Rep[Vector[Int]], level: Rep[Int],
              binary_gradients: Rep[Vector[Int]], match_list: Rep[IndexVector], occlusions: Rep[Vector[Vector[Int]]],
              templates: Rep[Vector[BinarizedGradientTemplate]], hist: Rep[Vector[Float]]): Rep[BinarizedGradientTemplate]

  // class defs
  def binarizedgradienttemplate_radius(x: Rep[BinarizedGradientTemplate]): Rep[Int]
  def binarizedgradienttemplate_rect(x: Rep[BinarizedGradientTemplate]): Rep[Rect]
  def binarizedgradienttemplate_mask_list(x: Rep[BinarizedGradientTemplate]): Rep[Vector[Int]]
  def binarizedgradienttemplate_level(x: Rep[BinarizedGradientTemplate]): Rep[Int]
  def binarizedgradienttemplate_binary_gradients(x: Rep[BinarizedGradientTemplate]): Rep[Vector[Int]]
  def binarizedgradienttemplate_match_list(x: Rep[BinarizedGradientTemplate]): Rep[IndexVector]
  def binarizedgradienttemplate_occlusions(x: Rep[BinarizedGradientTemplate]): Rep[Vector[Vector[Int]]]
  def binarizedgradienttemplate_templates(x: Rep[BinarizedGradientTemplate]): Rep[Vector[BinarizedGradientTemplate]]
  def binarizedgradienttemplate_hist(x: Rep[BinarizedGradientTemplate]): Rep[Vector[Float]]
}

trait BinarizedGradientTemplateOpsExp extends BinarizedGradientTemplateOps with EffectExp {

  // implemented via method on real data structure
  case class BinarizedGradientTemplateObjectNew(radius: Exp[Int], rect: Exp[Rect], mask_list: Exp[Vector[Int]], level: Exp[Int],
                binary_gradients: Exp[Vector[Int]], match_list: Exp[IndexVector], occlusions: Exp[Vector[Vector[Int]]],
                templates: Exp[Vector[BinarizedGradientTemplate]], hist: Exp[Vector[Float]])
    extends Def[BinarizedGradientTemplate]

  case class BinarizedGradientTemplateRadius(x: Exp[BinarizedGradientTemplate]) extends Def[Int]
  case class BinarizedGradientTemplateRect(x: Exp[BinarizedGradientTemplate]) extends Def[Rect]
  case class BinarizedGradientTemplateMaskList(x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Int]]
  case class BinarizedGradientTemplateLevel(x: Exp[BinarizedGradientTemplate]) extends Def[Int]
  case class BinarizedGradientTemplateBinaryGradients(x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Int]]
  case class BinarizedGradientTemplateMatchList(x: Exp[BinarizedGradientTemplate]) extends Def[IndexVector]
  case class BinarizedGradientTemplateOcclusions(x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Vector[Int]]]
  case class BinarizedGradientTemplateTemplates(x: Exp[BinarizedGradientTemplate]) extends Def[Vector[BinarizedGradientTemplate]]
  case class BinarizedGradientTemplateHist(x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Float]]

  def binarizedgradienttemplate_obj_new(radius: Exp[Int], rect: Exp[Rect], mask_list: Exp[Vector[Int]], level: Exp[Int],
                binary_gradients: Exp[Vector[Int]], match_list: Exp[IndexVector], occlusions: Exp[Vector[Vector[Int]]],
                templates: Exp[Vector[BinarizedGradientTemplate]], hist: Exp[Vector[Float]])

      = BinarizedGradientTemplateObjectNew(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist)
  
  def binarizedgradienttemplate_radius(x: Exp[BinarizedGradientTemplate]) = BinarizedGradientTemplateRadius(x)
  def binarizedgradienttemplate_rect(x: Exp[BinarizedGradientTemplate]) = BinarizedGradientTemplateRect(x)
  def binarizedgradienttemplate_mask_list(x: Exp[BinarizedGradientTemplate]) = BinarizedGradientTemplateMaskList(x)
  def binarizedgradienttemplate_level(x: Exp[BinarizedGradientTemplate]) = BinarizedGradientTemplateLevel(x)
  def binarizedgradienttemplate_binary_gradients(x: Exp[BinarizedGradientTemplate]) = BinarizedGradientTemplateBinaryGradients(x)
  def binarizedgradienttemplate_match_list(x: Exp[BinarizedGradientTemplate]) = BinarizedGradientTemplateMatchList(x)
  def binarizedgradienttemplate_occlusions(x: Exp[BinarizedGradientTemplate]) = BinarizedGradientTemplateOcclusions(x)
  def binarizedgradienttemplate_templates(x: Exp[BinarizedGradientTemplate]) = BinarizedGradientTemplateTemplates(x)
  def binarizedgradienttemplate_hist(x: Exp[BinarizedGradientTemplate]) = BinarizedGradientTemplateHist(x)
}

trait ScalaGenBinarizedGradientTemplateOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case BinarizedGradientTemplateObjectNew(radius,rect,mask_list,level,binary_gradients,match_list,occlusions,templates,hist)
      => emitValDef(sym, "new " + remap(manifest[BinarizedGradientTemplate]) + "(" + quote(radius) + "," + quote(rect) + "," + quote(mask_list) + "," + quote(level) +
                          "," + quote(binary_gradients) + "," + quote(match_list) + "," + quote(occlusions) + "," + quote(templates) + "," + quote(hist) + ")")
    case BinarizedGradientTemplateRadius(x) => emitValDef(sym, quote(x) + ".radius")
    case BinarizedGradientTemplateRect(x) => emitValDef(sym, quote(x) + ".rect")
    case BinarizedGradientTemplateMaskList(x) => emitValDef(sym, quote(x) + ".mask_list")
    case BinarizedGradientTemplateLevel(x) => emitValDef(sym, quote(x) + ".level")
    case BinarizedGradientTemplateBinaryGradients(x) => emitValDef(sym, quote(x) + ".binary_gradients")
    case BinarizedGradientTemplateMatchList(x) => emitValDef(sym, quote(x) + ".match_list")
    case BinarizedGradientTemplateOcclusions(x) => emitValDef(sym, quote(x) + ".occlusions")
    case BinarizedGradientTemplateTemplates(x) => emitValDef(sym, quote(x) + ".templates")
    case BinarizedGradientTemplateHist(x) => emitValDef(sym, quote(x) + ".hist")
    case _ => super.emitNode(sym, rhs)
  }
}

// TODO: add BiGGDetection, BinarizedGradientPyramid