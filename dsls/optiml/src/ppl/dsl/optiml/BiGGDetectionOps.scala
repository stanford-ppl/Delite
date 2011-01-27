package ppl.dsl.optiml

import datastruct.scala._
import java.io.PrintWriter
import ppl.delite.framework.{DSLType}
import scala.virtualization.lms.internal.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}


trait BiGGDetectionOps extends DSLType with Variables with OverloadHack {

  object BiGGDetection {
    def apply(name: Rep[String], score: Rep[Float], roi: Rep[Rect], mask: Rep[GrayscaleImage], index: Rep[Int], x: Rep[Int], y: Rep[Int], tpl: Rep[BinarizedGradientTemplate], crt_tpl: Rep[BinarizedGradientTemplate]) = biggdetection_obj_new(name, score, roi, mask, index, x, y, tpl, crt_tpl)
  }

  implicit def repBiGGDetectionToBiGGDetectionOps(x: Rep[BiGGDetection]) = new biggdetectionOpsCls(x)
  implicit def biggdetectionToBiGGDetectionOps(x: Var[BiGGDetection]) = new biggdetectionOpsCls(readVar(x))

  class biggdetectionOpsCls(_x_: Rep[BiGGDetection]) {
    def name = biggdetection_name(__x__)
    def score = biggdetection_score(__x__)
    def roi = biggdetection_roi(__x__)
    def mask = biggdetection_mask(__x__)
    def index = biggdetection_index(__x__)
    def x = biggdetection_x(__x__)
    def y = biggdetection_y(__x__)
    def tpl = biggdetection_tpl(__x__)
    def crt_tpl = biggdetection_crt_tpl(__x__)
  }

  //object defs
  def biggdetection_obj_new(name: Rep[String], score: Rep[Float], roi: Rep[Rect], mask: Rep[GrayscaleImage], index: Rep[Int], x: Rep[Int], y: Rep[Int], tpl: Rep[BinarizedGradientTemplate], crt_tpl: Rep[BinarizedGradientTemplate]): Rep[BiGGDetection]

  //class defs
  def biggdetection_name(__x__: Rep[BiGGDetection]): Rep[String]
  def biggdetection_score(__x__: Rep[BiGGDetection]): Rep[Float]
  def biggdetection_roi(__x__: Rep[BiGGDetection]): Rep[Rect]
  def biggdetection_mask(__x__: Rep[BiGGDetection]): Rep[GrayscaleImage]
  def biggdetection_index(__x__: Rep[BiGGDetection]): Rep[Int]
  def biggdetection_x(__x__: Rep[BiGGDetection]): Rep[Int]
  def biggdetection_y(__x__: Rep[BiGGDetection]): Rep[Int]
  def biggdetection_tpl(__x__: Rep[BiGGDetection]): Rep[BinarizedGradientTemplate]
  def biggdetection_crt_tpl(__x__: Rep[BiGGDetection]): Rep[BinarizedGradientTemplate]
}

trait BiGGDetectionOpsExp extends BiGGDetection with BaseExp { this: DeliteOpsExp =>
  case class BiGGDetectionObjectNew(name: Exp[String], score: Exp[Float], roi: Exp[Rect], mask: Exp[GrayscaleImage], index: Exp[Int], x: Exp[Int], y: Exp[Int], tpl: Exp[BinarizedGradientTemplate], crt_tpl: Exp[BinarizedGradientTemplate]) extends Def[BiGGDetection]
  case class BiGGDetectionName(__x__: Exp[BiGGDetection]) extends Def[String]
  case class BiGGDetectionScore(__x__: Exp[BiGGDetection]) extends Def[Float]
  case class BiGGDetectionRoi(__x__: Exp[BiGGDetection]) extends Def[Rect]
  case class BiGGDetectionMask(__x__: Exp[BiGGDetection]) extends Def[GrayscaleImage]
  case class BiGGDetectionIndex(__x__: Exp[BiGGDetection]) extends Def[Int]
  case class BiGGDetectionX(__x__: Exp[BiGGDetection]) extends Def[Int]
  case class BiGGDetectionY(__x__: Exp[BiGGDetection]) extends Def[Int]
  case class BiGGDetectionTpl(__x__: Exp[BiGGDetection]) extends Def[BinarizedGradientTemplate]
  case class BiGGDetectionCrt_tpl(__x__: Exp[BiGGDetection]) extends Def[BinarizedGradientTemplate]

  biggdetection_obj_new(name: Exp[String], score: Exp[Float], roi: Exp[Rect], mask: Exp[GrayscaleImage], index: Exp[Int], x: Exp[Int], y: Exp[Int], tpl: Exp[BinarizedGradientTemplate], crt_tpl: Exp[BinarizedGradientTemplate]) = reflectEffect(BiGGDetectionObjectNew(name, score, roi, mask, index, x, y, tpl, crt_tpl))
  def biggdetection_name(__x__: Rep[BiGGDetection]) = BiGGDetectionName(__x__)
  def biggdetection_score(__x__: Rep[BiGGDetection]) = BiGGDetectionScore(__x__)
  def biggdetection_roi(__x__: Rep[BiGGDetection]) = BiGGDetectionRoi(__x__)
  def biggdetection_mask(__x__: Rep[BiGGDetection]) = BiGGDetectionMask(__x__)
  def biggdetection_index(__x__: Rep[BiGGDetection]) = BiGGDetectionIndex(__x__)
  def biggdetection_x(__x__: Rep[BiGGDetection]) = BiGGDetectionX(__x__)
  def biggdetection_y(__x__: Rep[BiGGDetection]) = BiGGDetectionY(__x__)
  def biggdetection_tpl(__x__: Rep[BiGGDetection]) = BiGGDetectionTpl(__x__)
  def biggdetection_crt_tpl(__x__: Rep[BiGGDetection]) = BiGGDetectionCrt_tpl(__x__)
}

trait ScalaGenBiGGDetectionOps extends ScalaGenBase {
  val IR: BiGGDetectionOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
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
