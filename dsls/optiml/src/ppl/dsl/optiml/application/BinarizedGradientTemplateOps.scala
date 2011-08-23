package ppl.dsl.optiml.application

import ppl.dsl.optiml.datastruct.scala._
import java.io.PrintWriter
import ppl.delite.framework.{DSLType}
import scala.virtualization.lms.common.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}


trait BinarizedGradientTemplateOps extends DSLType with Variables with OverloadHack {

  object BinarizedGradientTemplate {
    def apply(radius: Rep[Int], rect: Rep[Rect], mask_list: Rep[Vector[Int]], level: Rep[Int], binary_gradients: Rep[Vector[Int]], match_list: Rep[IndexVector], occlusions: Rep[Vector[Vector[Int]]], templates: Rep[Vector[BinarizedGradientTemplate]], hist: Rep[Vector[Float]]) = binarizedgradienttemplate_obj_new(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist)
  }

  implicit def repBinarizedGradientTemplateToBinarizedGradientTemplateOps(x: Rep[BinarizedGradientTemplate]) = new binarizedgradienttemplateOpsCls(x)
  implicit def binarizedgradienttemplateToBinarizedGradientTemplateOps(x: Var[BinarizedGradientTemplate]) = new binarizedgradienttemplateOpsCls(readVar(x))

  class binarizedgradienttemplateOpsCls(__x: Rep[BinarizedGradientTemplate]) {
    def radius = binarizedgradienttemplate_radius(__x)
    def rect = binarizedgradienttemplate_rect(__x)
    def mask_list = binarizedgradienttemplate_mask_list(__x)
    def level = binarizedgradienttemplate_level(__x)
    def binary_gradients = binarizedgradienttemplate_binary_gradients(__x)
    def match_list = binarizedgradienttemplate_match_list(__x)
    def occlusions = binarizedgradienttemplate_occlusions(__x)
    def templates = binarizedgradienttemplate_templates(__x)
    def hist = binarizedgradienttemplate_hist(__x)
  }

  //object defs
  def binarizedgradienttemplate_obj_new(radius: Rep[Int], rect: Rep[Rect], mask_list: Rep[Vector[Int]], level: Rep[Int], binary_gradients: Rep[Vector[Int]], match_list: Rep[IndexVector], occlusions: Rep[Vector[Vector[Int]]], templates: Rep[Vector[BinarizedGradientTemplate]], hist: Rep[Vector[Float]]): Rep[BinarizedGradientTemplate]

  //class defs
  def binarizedgradienttemplate_radius(__x: Rep[BinarizedGradientTemplate]): Rep[Int]
  def binarizedgradienttemplate_rect(__x: Rep[BinarizedGradientTemplate]): Rep[Rect]
  def binarizedgradienttemplate_mask_list(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[Int]]
  def binarizedgradienttemplate_level(__x: Rep[BinarizedGradientTemplate]): Rep[Int]
  def binarizedgradienttemplate_binary_gradients(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[Int]]
  def binarizedgradienttemplate_match_list(__x: Rep[BinarizedGradientTemplate]): Rep[IndexVector]
  def binarizedgradienttemplate_occlusions(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[Vector[Int]]]
  def binarizedgradienttemplate_templates(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[BinarizedGradientTemplate]]
  def binarizedgradienttemplate_hist(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[Float]]
}

trait BinarizedGradientTemplateOpsExp extends BinarizedGradientTemplateOps with EffectExp {
  case class BinarizedGradientTemplateObjectNew(radius: Exp[Int], rect: Exp[Rect], mask_list: Exp[Vector[Int]], level: Exp[Int], binary_gradients: Exp[Vector[Int]], match_list: Exp[IndexVector], occlusions: Exp[Vector[Vector[Int]]], templates: Exp[Vector[BinarizedGradientTemplate]], hist: Exp[Vector[Float]]) extends Def[BinarizedGradientTemplate]
  case class BinarizedGradientTemplateRadius(__x: Exp[BinarizedGradientTemplate]) extends Def[Int]
  case class BinarizedGradientTemplateRect(__x: Exp[BinarizedGradientTemplate]) extends Def[Rect]
  case class BinarizedGradientTemplateMask_list(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Int]]
  case class BinarizedGradientTemplateLevel(__x: Exp[BinarizedGradientTemplate]) extends Def[Int]
  case class BinarizedGradientTemplateBinary_gradients(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Int]]
  case class BinarizedGradientTemplateMatch_list(__x: Exp[BinarizedGradientTemplate]) extends Def[IndexVector]
  case class BinarizedGradientTemplateOcclusions(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Vector[Int]]]
  case class BinarizedGradientTemplateTemplates(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[BinarizedGradientTemplate]]
  case class BinarizedGradientTemplateHist(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Float]]

  def binarizedgradienttemplate_obj_new(radius: Exp[Int], rect: Exp[Rect], mask_list: Exp[Vector[Int]], level: Exp[Int], binary_gradients: Exp[Vector[Int]], match_list: Exp[IndexVector], occlusions: Exp[Vector[Vector[Int]]], templates: Exp[Vector[BinarizedGradientTemplate]], hist: Exp[Vector[Float]]) = reflectEffect(BinarizedGradientTemplateObjectNew(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist))
  def binarizedgradienttemplate_radius(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateRadius(__x)
  def binarizedgradienttemplate_rect(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateRect(__x)
  def binarizedgradienttemplate_mask_list(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateMask_list(__x)
  def binarizedgradienttemplate_level(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateLevel(__x)
  def binarizedgradienttemplate_binary_gradients(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateBinary_gradients(__x)
  def binarizedgradienttemplate_match_list(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateMatch_list(__x)
  def binarizedgradienttemplate_occlusions(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateOcclusions(__x)
  def binarizedgradienttemplate_templates(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateTemplates(__x)
  def binarizedgradienttemplate_hist(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateHist(__x)
}

trait ScalaGenBinarizedGradientTemplateOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case BinarizedGradientTemplateObjectNew(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist) => emitValDef(sym, "new " + remap(manifest[BinarizedGradientTemplate]) + "(" + quote(radius)  + "," + quote(rect)  + "," + quote(mask_list)  + "," + quote(level)  + "," + quote(binary_gradients)  + "," + quote(match_list)  + "," + quote(occlusions)  + "," + quote(templates)  + "," + quote(hist)  + ")")
    case BinarizedGradientTemplateRadius(x) =>  emitValDef(sym, quote(x) + ".radius")
    case BinarizedGradientTemplateRect(x) =>  emitValDef(sym, quote(x) + ".rect")
    case BinarizedGradientTemplateMask_list(x) =>  emitValDef(sym, quote(x) + ".mask_list")
    case BinarizedGradientTemplateLevel(x) =>  emitValDef(sym, quote(x) + ".level")
    case BinarizedGradientTemplateBinary_gradients(x) =>  emitValDef(sym, quote(x) + ".binary_gradients")
    case BinarizedGradientTemplateMatch_list(x) =>  emitValDef(sym, quote(x) + ".match_list")
    case BinarizedGradientTemplateOcclusions(x) =>  emitValDef(sym, quote(x) + ".occlusions")
    case BinarizedGradientTemplateTemplates(x) =>  emitValDef(sym, quote(x) + ".templates")
    case BinarizedGradientTemplateHist(x) =>  emitValDef(sym, quote(x) + ".hist")
    case _ => super.emitNode(sym, rhs)
  }
}
