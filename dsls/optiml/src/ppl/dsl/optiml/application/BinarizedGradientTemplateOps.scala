package ppl.dsl.optiml.application

import ppl.dsl.optiml._
import java.io.PrintWriter
import scala.virtualization.lms.common.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}


trait BinarizedGradientTemplateOps extends Variables with OverloadHack {

  object BinarizedGradientTemplate {
    def apply(radius: Rep[Int], rect: Rep[Rect], mask_list: Rep[DenseVector[Int]], level: Rep[Int], binary_gradients: Rep[DenseVector[Int]], match_list: Rep[IndexVectorDense], occlusions: Rep[DenseVector[DenseVector[Int]]], templates: Rep[DenseVector[BinarizedGradientTemplate]], hist: Rep[DenseVector[Float]]) = binarizedgradienttemplate_obj_new(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist)
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
  def binarizedgradienttemplate_obj_new(radius: Rep[Int], rect: Rep[Rect], mask_list: Rep[DenseVector[Int]], level: Rep[Int], binary_gradients: Rep[DenseVector[Int]], match_list: Rep[IndexVectorDense], occlusions: Rep[DenseVector[DenseVector[Int]]], templates: Rep[DenseVector[BinarizedGradientTemplate]], hist: Rep[DenseVector[Float]]): Rep[BinarizedGradientTemplate]

  //class defs
  def binarizedgradienttemplate_radius(__x: Rep[BinarizedGradientTemplate]): Rep[Int]
  def binarizedgradienttemplate_rect(__x: Rep[BinarizedGradientTemplate]): Rep[Rect]
  def binarizedgradienttemplate_mask_list(__x: Rep[BinarizedGradientTemplate]): Rep[DenseVector[Int]]
  def binarizedgradienttemplate_level(__x: Rep[BinarizedGradientTemplate]): Rep[Int]
  def binarizedgradienttemplate_binary_gradients(__x: Rep[BinarizedGradientTemplate]): Rep[DenseVector[Int]]
  def binarizedgradienttemplate_match_list(__x: Rep[BinarizedGradientTemplate]): Rep[IndexVectorDense]
  def binarizedgradienttemplate_occlusions(__x: Rep[BinarizedGradientTemplate]): Rep[DenseVector[DenseVector[Int]]]
  def binarizedgradienttemplate_templates(__x: Rep[BinarizedGradientTemplate]): Rep[DenseVector[BinarizedGradientTemplate]]
  def binarizedgradienttemplate_hist(__x: Rep[BinarizedGradientTemplate]): Rep[DenseVector[Float]]
}

trait BinarizedGradientTemplateOpsExp extends BinarizedGradientTemplateOps with EffectExp {
  case class BinarizedGradientTemplateObjectNew(radius: Exp[Int], rect: Exp[Rect], mask_list: Exp[DenseVector[Int]], level: Exp[Int], binary_gradients: Exp[DenseVector[Int]], match_list: Exp[IndexVectorDense], occlusions: Exp[DenseVector[DenseVector[Int]]], templates: Exp[DenseVector[BinarizedGradientTemplate]], hist: Exp[DenseVector[Float]]) extends Def[BinarizedGradientTemplate]
  case class BinarizedGradientTemplateRadius(__x: Exp[BinarizedGradientTemplate]) extends Def[Int]
  case class BinarizedGradientTemplateRect(__x: Exp[BinarizedGradientTemplate]) extends Def[Rect]
  case class BinarizedGradientTemplateMask_list(__x: Exp[BinarizedGradientTemplate]) extends Def[DenseVector[Int]]
  case class BinarizedGradientTemplateLevel(__x: Exp[BinarizedGradientTemplate]) extends Def[Int]
  case class BinarizedGradientTemplateBinary_gradients(__x: Exp[BinarizedGradientTemplate]) extends Def[DenseVector[Int]]
  case class BinarizedGradientTemplateMatch_list(__x: Exp[BinarizedGradientTemplate]) extends Def[IndexVectorDense]
  case class BinarizedGradientTemplateOcclusions(__x: Exp[BinarizedGradientTemplate]) extends Def[DenseVector[DenseVector[Int]]]
  case class BinarizedGradientTemplateTemplates(__x: Exp[BinarizedGradientTemplate]) extends Def[DenseVector[BinarizedGradientTemplate]]
  case class BinarizedGradientTemplateHist(__x: Exp[BinarizedGradientTemplate]) extends Def[DenseVector[Float]]

  def binarizedgradienttemplate_obj_new(radius: Exp[Int], rect: Exp[Rect], mask_list: Exp[DenseVector[Int]], level: Exp[Int], binary_gradients: Exp[DenseVector[Int]], match_list: Exp[IndexVectorDense], occlusions: Exp[DenseVector[DenseVector[Int]]], templates: Exp[DenseVector[BinarizedGradientTemplate]], hist: Exp[DenseVector[Float]]) = reflectEffect(BinarizedGradientTemplateObjectNew(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist))
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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
