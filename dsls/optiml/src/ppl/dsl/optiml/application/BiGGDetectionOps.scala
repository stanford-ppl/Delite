package ppl.dsl.optiml.application

import ppl.dsl.optiml._
import java.io.PrintWriter
import scala.virtualization.lms.common.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}


trait BiGGDetectionOps extends Variables with OverloadHack {

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

trait BiGGDetectionOpsExp extends BiGGDetectionOps with EffectExp {
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

  def biggdetection_obj_new(name: Exp[String], score: Exp[Float], roi: Exp[Rect], mask: Exp[GrayscaleImage], index: Exp[Int], x: Exp[Int], y: Exp[Int], tpl: Exp[BinarizedGradientTemplate], crt_tpl: Exp[BinarizedGradientTemplate]) = reflectEffect(BiGGDetectionObjectNew(name, score, roi, mask, index, x, y, tpl, crt_tpl))
  def biggdetection_name(__x: Rep[BiGGDetection]) = BiGGDetectionName(__x)
  def biggdetection_score(__x: Rep[BiGGDetection]) = BiGGDetectionScore(__x)
  def biggdetection_roi(__x: Rep[BiGGDetection]) = BiGGDetectionRoi(__x)
  def biggdetection_mask(__x: Rep[BiGGDetection]) = BiGGDetectionMask(__x)
  def biggdetection_index(__x: Rep[BiGGDetection]) = BiGGDetectionIndex(__x)
  def biggdetection_x(__x: Rep[BiGGDetection]) = BiGGDetectionX(__x)
  def biggdetection_y(__x: Rep[BiGGDetection]) = BiGGDetectionY(__x)
  def biggdetection_tpl(__x: Rep[BiGGDetection]) = BiGGDetectionTpl(__x)
  def biggdetection_crt_tpl(__x: Rep[BiGGDetection]) = BiGGDetectionCrt_tpl(__x)
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
