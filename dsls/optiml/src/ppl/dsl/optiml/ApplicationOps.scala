package ppl.dsl.optiml

import java.io.{PrintWriter}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import ppl.delite.framework.DSLType
import ppl.dsl.optiml.datastruct.scala._


/**
 * This file should be auto-generated!
 */

trait ApplicationOps extends BinarizedGradientPyramidOps with RectOps with BiGGDetectionOps with BinarizedGradientTemplateOps
  with DenoiseVertexDataOps with DenoiseEdgeDataOps
trait ApplicationOpsExp extends BinarizedGradientPyramidOpsExp with RectOpsExp with BiGGDetectionOpsExp with BinarizedGradientTemplateOpsExp
  with DenoiseVertexDataOpsExp with DenoiseEdgeDataOpsExp
trait ScalaGenApplicationOps extends ScalaGenBinarizedGradientPyramidOps with ScalaGenRectOps with ScalaGenBiGGDetectionOps with ScalaGenBinarizedGradientTemplateOps
  with ScalaGenDenoiseVertexDataOps with ScalaGenDenoiseEdgeDataOps

trait DenoiseVertexDataOps extends DSLType with Variables {
  object DenoiseVertexData {
    def apply(id: Rep[Int], b: Rep[Vector[Double]], p: Rep[Vector[Double]]) = denoise_vertex_data_obj_new(id, b, p)
  }

  implicit def repDenoiseVertexDataToDenoiseVertexDataOps(v: Rep[DenoiseVertexData]) = new denoiseVertexDataOpsCls(v)

  class denoiseVertexDataOpsCls(v: Rep[DenoiseVertexData]) {
    def id = denoise_vertex_data_id(v)
    def belief = denoise_vertex_data_belief(v)
    def setBelief(b: Rep[Vector[Double]]) = denoise_vertex_data_belief_update(v, b)
    def potential = denoise_vertex_data_potential(v)
  }

  // object defs
  def denoise_vertex_data_obj_new(id: Rep[Int], b: Rep[Vector[Double]], p: Rep[Vector[Double]]): Rep[DenoiseVertexData]

  // class defs
  def denoise_vertex_data_id(v: Rep[DenoiseVertexData]): Rep[Int]
  def denoise_vertex_data_belief(v: Rep[DenoiseVertexData]): Rep[Vector[Double]]
  def denoise_vertex_data_belief_update(v: Rep[DenoiseVertexData], b: Rep[Vector[Double]])
  def denoise_vertex_data_potential(v: Rep[DenoiseVertexData]): Rep[Vector[Double]]
}

trait DenoiseVertexDataOpsExp extends DenoiseVertexDataOps with VariablesExp with BaseFatExp {
  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class DenoiseVertexDataObjectNew(id: Exp[Int], belief: Exp[Vector[Double]], potential: Exp[Vector[Double]])
    extends Def[DenoiseVertexData] {
    val vD = manifest[DenoiseVertexDataImpl]
  }
  case class DenoiseVertexDataId(v: Exp[DenoiseVertexData]) extends Def[Int]
  case class DenoiseVertexDataBelief(v: Exp[DenoiseVertexData]) extends Def[Vector[Double]]
  case class DenoiseVertexDataBeliefUpdate(v: Exp[DenoiseVertexData], b: Exp[Vector[Double]]) extends Def[Unit]
  case class DenoiseVertexDataPotential(v: Exp[DenoiseVertexData]) extends Def[Vector[Double]]

  /////////////////////
  // object interface
  def denoise_vertex_data_obj_new(id: Exp[Int], b: Exp[Vector[Double]], p: Exp[Vector[Double]]) = reflectMutable(DenoiseVertexDataObjectNew(id, b, p))

  /////////////////////
  // class interface

  def denoise_vertex_data_id(v: Exp[DenoiseVertexData]) = toAtom(DenoiseVertexDataId(v))
  def denoise_vertex_data_belief(v: Exp[DenoiseVertexData]) = reflectMutable(DenoiseVertexDataBelief(v))
  def denoise_vertex_data_belief_update(v: Exp[DenoiseVertexData], b: Exp[Vector[Double]]) = reflectWrite(v)(DenoiseVertexDataBeliefUpdate(v, b))
  def denoise_vertex_data_potential(v: Exp[DenoiseVertexData]) = toAtom(DenoiseVertexDataPotential(v))
}

trait ScalaGenDenoiseVertexDataOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case v@DenoiseVertexDataObjectNew(id,b,p) => emitValDef(sym, "new " + remap(v.vD) + "(" + quote(id) + "," + quote(b) + "," + quote(p) + ")")
      case DenoiseVertexDataId(v) => emitValDef(sym, quote(v) + ".id")
      case DenoiseVertexDataBelief(v) => emitValDef(sym, quote(v) + ".belief")
      case DenoiseVertexDataBeliefUpdate(v,b) => emitValDef(sym, quote(v) + ".setBelief(" + quote(b) + ")")
      case DenoiseVertexDataPotential(v) => emitValDef(sym, quote(v) + ".potential")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait DenoiseEdgeDataOps extends DSLType with Variables {
  object DenoiseEdgeData {
    def apply(m: Rep[Vector[Double]], oM: Rep[Vector[Double]]) = denoise_edge_data_obj_new(m, oM)
  }

  implicit def repDenoiseEdgeDataToDenoiseEdgeDataOps(e: Rep[DenoiseEdgeData]) = new denoiseEdgeDataOpsCls(e)

  class denoiseEdgeDataOpsCls(e: Rep[DenoiseEdgeData]) {
    def message = denoise_edge_data_message(e)
    def setMessage(m: Rep[Vector[Double]]) = denoise_edge_data_message_update(e,m)
    def oldMessage = denoise_edge_data_old_message(e)
    def setOldMessage(m: Rep[Vector[Double]]) = denoise_edge_data_old_message_update(e,m)
    def cloneL = denoise_edge_data_cloneL(e)
  }

  // object defs
  def denoise_edge_data_obj_new(m: Rep[Vector[Double]], oM: Rep[Vector[Double]]): Rep[DenoiseEdgeData]

  // class defs
  def denoise_edge_data_message(e: Rep[DenoiseEdgeData]): Rep[Vector[Double]]
  def denoise_edge_data_message_update(e: Rep[DenoiseEdgeData], m: Rep[Vector[Double]])
  def denoise_edge_data_old_message(e: Rep[DenoiseEdgeData]): Rep[Vector[Double]]
  def denoise_edge_data_old_message_update(e: Rep[DenoiseEdgeData], m: Rep[Vector[Double]])
  def denoise_edge_data_cloneL(e: Rep[DenoiseEdgeData]): Rep[DenoiseEdgeData]
}

trait DenoiseEdgeDataOpsExp extends DenoiseEdgeDataOps with VariablesExp with BaseFatExp {
  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class DenoiseEdgeDataObjectNew(m: Exp[Vector[Double]], oM: Exp[Vector[Double]])
    extends Def[DenoiseEdgeData] {
    val eD = manifest[DenoiseEdgeDataImpl]
  }
  case class DenoiseEdgeDataMessage(e: Exp[DenoiseEdgeData]) extends Def[Vector[Double]]
  case class DenoiseEdgeDataMessageUpdate(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) extends Def[Unit]
  case class DenoiseEdgeDataOldMessage(e: Exp[DenoiseEdgeData]) extends Def[Vector[Double]]
  case class DenoiseEdgeDataOldMessageUpdate(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) extends Def[Unit]
  case class DenoiseEdgeDataCloneL(e: Exp[DenoiseEdgeData]) extends Def[DenoiseEdgeData]

  /////////////////////
  // object interface

  def denoise_edge_data_obj_new(m: Exp[Vector[Double]], oM: Exp[Vector[Double]]) = reflectMutable(DenoiseEdgeDataObjectNew(m, oM))

  /////////////////////
  // class interface

  def denoise_edge_data_message(e: Exp[DenoiseEdgeData]) = reflectMutable(DenoiseEdgeDataMessage(e))
  def denoise_edge_data_message_update(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) = reflectWrite(e)(DenoiseEdgeDataMessageUpdate(e, m))
  def denoise_edge_data_old_message(e: Exp[DenoiseEdgeData]) = reflectMutable(DenoiseEdgeDataOldMessage(e))
  def denoise_edge_data_old_message_update(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) = reflectWrite(e)(DenoiseEdgeDataOldMessageUpdate(e, m))
  def denoise_edge_data_cloneL(e: Exp[DenoiseEdgeData]) = reflectMutable(DenoiseEdgeDataCloneL(e))
}

trait ScalaGenDenoiseEdgeDataOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case e@DenoiseEdgeDataObjectNew(m,oM) => emitValDef(sym, "new " + remap(e.eD) + "(" + quote(m) + "," + quote(oM) + ")")
      case DenoiseEdgeDataMessage(e) => emitValDef(sym, quote(e) + ".message")
      case DenoiseEdgeDataMessageUpdate(e,m) => emitValDef(sym, quote(e) + ".setMessage(" + quote(m) + ")")
      case DenoiseEdgeDataOldMessage(e) => emitValDef(sym, quote(e) + ".oldMessage")
      case DenoiseEdgeDataOldMessageUpdate(e,m) => emitValDef(sym, quote(e) + ".setOldMessage(" + quote(m) + ")")
      case DenoiseEdgeDataCloneL(e) => emitValDef(sym, quote(e) + ".cloneL")
      case _ => super.emitNode(sym, rhs)
    }
  }
}







trait BiGGDetectionOps extends DSLType with Variables with OverloadHack {
  object BiGGDetection {
    def apply(name: Rep[String], score: Rep[Float], roi: Rep[Rect], mask: Rep[GrayscaleImage], index: Rep[Int], x: Rep[Int], y: Rep[Int], tpl: Rep[BinarizedGradientTemplate], crt_tpl: Rep[BinarizedGradientTemplate]) = biggdetection_obj_new(name, score, roi, mask, index, x, y, tpl, crt_tpl)
  }

  implicit def repBiGGDetectionToBiGGDetectionOps(x: Rep[BiGGDetection]) = new biggdetectionOpsCls(x)
  implicit def biggdetectionToBiGGDetectionOps(x: Var[BiGGDetection]) = new biggdetectionOpsCls(readVar(x))

  class biggdetectionOpsCls(__x: Rep[BiGGDetection]) {
    def name = biggdetection_name(__x)
    def score = biggdetection_score(__x)
    def roi = biggdetection_roi(__x)
    def mask = biggdetection_mask(__x)
    def index = biggdetection_index(__x)
    def x = biggdetection_x(__x)
    def y = biggdetection_y(__x)
    def tpl = biggdetection_tpl(__x)
    def crt_tpl = biggdetection_crt_tpl(__x)
  }

  //object defs
  def biggdetection_obj_new(name: Rep[String], score: Rep[Float], roi: Rep[Rect], mask: Rep[GrayscaleImage], index: Rep[Int], x: Rep[Int], y: Rep[Int], tpl: Rep[BinarizedGradientTemplate], crt_tpl: Rep[BinarizedGradientTemplate]): Rep[BiGGDetection]

  //class defs
  def biggdetection_name(__x: Rep[BiGGDetection]): Rep[String]
  def biggdetection_score(__x: Rep[BiGGDetection]): Rep[Float]
  def biggdetection_roi(__x: Rep[BiGGDetection]): Rep[Rect]
  def biggdetection_mask(__x: Rep[BiGGDetection]): Rep[GrayscaleImage]
  def biggdetection_index(__x: Rep[BiGGDetection]): Rep[Int]
  def biggdetection_x(__x: Rep[BiGGDetection]): Rep[Int]
  def biggdetection_y(__x: Rep[BiGGDetection]): Rep[Int]
  def biggdetection_tpl(__x: Rep[BiGGDetection]): Rep[BinarizedGradientTemplate]
  def biggdetection_crt_tpl(__x: Rep[BiGGDetection]): Rep[BinarizedGradientTemplate]
}

trait BiGGDetectionOpsExp extends BiGGDetectionOps with VariablesExp with BaseFatExp {
  case class BiGGDetectionObjectNew(name: Exp[String], score: Exp[Float], roi: Exp[Rect], mask: Exp[GrayscaleImage], index: Exp[Int], x: Exp[Int], y: Exp[Int], tpl: Exp[BinarizedGradientTemplate], crt_tpl: Exp[BinarizedGradientTemplate]) extends Def[BiGGDetection]
  case class BiGGDetectionName(__x: Exp[BiGGDetection]) extends Def[String]
  case class BiGGDetectionScore(__x: Exp[BiGGDetection]) extends Def[Float]
  case class BiGGDetectionRoi(__x: Exp[BiGGDetection]) extends Def[Rect]
  case class BiGGDetectionMask(__x: Exp[BiGGDetection]) extends Def[GrayscaleImage]
  case class BiGGDetectionIndex(__x: Exp[BiGGDetection]) extends Def[Int]
  case class BiGGDetectionX(__x: Exp[BiGGDetection]) extends Def[Int]
  case class BiGGDetectionY(__x: Exp[BiGGDetection]) extends Def[Int]
  case class BiGGDetectionTpl(__x: Exp[BiGGDetection]) extends Def[BinarizedGradientTemplate]
  case class BiGGDetectionCrt_tpl(__x: Exp[BiGGDetection]) extends Def[BinarizedGradientTemplate]

  def biggdetection_obj_new(name: Exp[String], score: Exp[Float], roi: Exp[Rect], mask: Exp[GrayscaleImage], index: Exp[Int], x: Exp[Int], y: Exp[Int], tpl: Exp[BinarizedGradientTemplate], crt_tpl: Exp[BinarizedGradientTemplate]) = toAtom(BiGGDetectionObjectNew(name, score, roi, mask, index, x, y, tpl, crt_tpl))
  def biggdetection_name(__x: Rep[BiGGDetection]) = toAtom(BiGGDetectionName(__x))
  def biggdetection_score(__x: Rep[BiGGDetection]) = toAtom(BiGGDetectionScore(__x))
  def biggdetection_roi(__x: Rep[BiGGDetection]) = toAtom(BiGGDetectionRoi(__x))
  def biggdetection_mask(__x: Rep[BiGGDetection]) = toAtom(BiGGDetectionMask(__x))
  def biggdetection_index(__x: Rep[BiGGDetection]) = toAtom(BiGGDetectionIndex(__x))
  def biggdetection_x(__x: Rep[BiGGDetection]) = toAtom(BiGGDetectionX(__x))
  def biggdetection_y(__x: Rep[BiGGDetection]) = toAtom(BiGGDetectionY(__x))
  def biggdetection_tpl(__x: Rep[BiGGDetection]) = toAtom(BiGGDetectionTpl(__x))
  def biggdetection_crt_tpl(__x: Rep[BiGGDetection]) = toAtom(BiGGDetectionCrt_tpl(__x))
}

trait ScalaGenBiGGDetectionOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case BiGGDetectionObjectNew(name, score, roi, mask, index, x, y, tpl, crt_tpl) => emitValDef(sym, "new " + remap(manifest[BiGGDetection]) + "(" + quote(name)  + "," + quote(score)  + "," + quote(roi)  + "," + quote(mask)  + "," + quote(index)  + "," + quote(x)  + "," + quote(y)  + "," + quote(tpl)  + "," + quote(crt_tpl)  + ")")
    case BiGGDetectionName(x) =>  emitValDef(sym, quote(x) + ".name")
    case BiGGDetectionScore(x) =>  emitValDef(sym, quote(x) + ".score")
    case BiGGDetectionRoi(x) =>  emitValDef(sym, quote(x) + ".roi")
    case BiGGDetectionMask(x) =>  emitValDef(sym, quote(x) + ".mask")
    case BiGGDetectionIndex(x) =>  emitValDef(sym, quote(x) + ".index")
    case BiGGDetectionX(x) =>  emitValDef(sym, quote(x) + ".x")
    case BiGGDetectionY(x) =>  emitValDef(sym, quote(x) + ".y")
    case BiGGDetectionTpl(x) =>  emitValDef(sym, quote(x) + ".tpl")
    case BiGGDetectionCrt_tpl(x) =>  emitValDef(sym, quote(x) + ".crt_tpl")
    case _ => super.emitNode(sym, rhs)
  }
}

trait BinarizedGradientPyramidOps extends DSLType with Variables with OverloadHack {

  object BinarizedGradientPyramid {
    def apply(pyramid: Rep[Vector[GrayscaleImage]], start_level: Rep[Int], levels: Rep[Int], fixedLevelIndex: Rep[Int]) = binarizedgradientpyramid_obj_new(pyramid, start_level, levels, fixedLevelIndex)
  }

  implicit def repBinarizedGradientPyramidToBinarizedGradientPyramidOps(x: Rep[BinarizedGradientPyramid]) = new binarizedgradientpyramidOpsCls(x)
  implicit def binarizedgradientpyramidToBinarizedGradientPyramidOps(x: Var[BinarizedGradientPyramid]) = new binarizedgradientpyramidOpsCls(readVar(x))

  class binarizedgradientpyramidOpsCls(__x: Rep[BinarizedGradientPyramid]) {
    def pyramid = binarizedgradientpyramid_pyramid(__x)
    def start_level = binarizedgradientpyramid_start_level(__x)
    def levels = binarizedgradientpyramid_levels(__x)
    def fixedLevelIndex = binarizedgradientpyramid_fixedLevelIndex(__x)
  }

  //object defs
  def binarizedgradientpyramid_obj_new(pyramid: Rep[Vector[GrayscaleImage]], start_level: Rep[Int], levels: Rep[Int], fixedLevelIndex: Rep[Int]): Rep[BinarizedGradientPyramid]

  //class defs
  def binarizedgradientpyramid_pyramid(__x: Rep[BinarizedGradientPyramid]): Rep[Vector[GrayscaleImage]]
  def binarizedgradientpyramid_start_level(__x: Rep[BinarizedGradientPyramid]): Rep[Int]
  def binarizedgradientpyramid_levels(__x: Rep[BinarizedGradientPyramid]): Rep[Int]
  def binarizedgradientpyramid_fixedLevelIndex(__x: Rep[BinarizedGradientPyramid]): Rep[Int]
}

trait BinarizedGradientPyramidOpsExp extends BinarizedGradientPyramidOps with EffectExp {
  case class BinarizedGradientPyramidObjectNew(pyramid: Exp[Vector[GrayscaleImage]], start_level: Exp[Int], levels: Exp[Int], fixedLevelIndex: Exp[Int]) extends Def[BinarizedGradientPyramid]
  case class BinarizedGradientPyramidPyramid(__x: Exp[BinarizedGradientPyramid]) extends Def[Vector[GrayscaleImage]]
  case class BinarizedGradientPyramidStart_level(__x: Exp[BinarizedGradientPyramid]) extends Def[Int]
  case class BinarizedGradientPyramidLevels(__x: Exp[BinarizedGradientPyramid]) extends Def[Int]
  case class BinarizedGradientPyramidFixedlevelindex(__x: Exp[BinarizedGradientPyramid]) extends Def[Int]

  def binarizedgradientpyramid_obj_new(pyramid: Exp[Vector[GrayscaleImage]], start_level: Exp[Int], levels: Exp[Int], fixedLevelIndex: Exp[Int]) = reflectEffect(BinarizedGradientPyramidObjectNew(pyramid, start_level, levels, fixedLevelIndex))
  def binarizedgradientpyramid_pyramid(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidPyramid(__x)
  def binarizedgradientpyramid_start_level(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidStart_level(__x)
  def binarizedgradientpyramid_levels(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidLevels(__x)
  def binarizedgradientpyramid_fixedLevelIndex(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidFixedlevelindex(__x)

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case Reflect(BinarizedGradientPyramidObjectNew(p,s,l,i), u, es) => reflectMirrored(Reflect(BinarizedGradientPyramidObjectNew(f(p),f(s),f(l),f(i)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenBinarizedGradientPyramidOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case BinarizedGradientPyramidObjectNew(pyramid, start_level, levels, fixedLevelIndex) => emitValDef(sym, "new " + remap(manifest[BinarizedGradientPyramid]) + "(" + quote(pyramid)  + "," + quote(start_level)  + "," + quote(levels)  + "," + quote(fixedLevelIndex)  + ")")
    case BinarizedGradientPyramidPyramid(x) =>  emitValDef(sym, quote(x) + ".pyramid")
    case BinarizedGradientPyramidStart_level(x) =>  emitValDef(sym, quote(x) + ".start_level")
    case BinarizedGradientPyramidLevels(x) =>  emitValDef(sym, quote(x) + ".levels")
    case BinarizedGradientPyramidFixedlevelindex(x) =>  emitValDef(sym, quote(x) + ".fixedLevelIndex")
    case _ => super.emitNode(sym, rhs)
  }
}


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


trait RectOps extends DSLType with Variables with OverloadHack {

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

