package ppl.dsl.optiml.application

import ppl.dsl.optiml._
import java.io.PrintWriter
import scala.virtualization.lms.common.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}


trait BinarizedGradientPyramidOps extends Variables with OverloadHack {

  object BinarizedGradientPyramid {
    def apply(pyramid: Rep[DenseVector[GrayscaleImage]], start_level: Rep[Int], levels: Rep[Int], fixedLevelIndex: Rep[Int]) = binarizedgradientpyramid_obj_new(pyramid, start_level, levels, fixedLevelIndex)
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
  def binarizedgradientpyramid_obj_new(pyramid: Rep[DenseVector[GrayscaleImage]], start_level: Rep[Int], levels: Rep[Int], fixedLevelIndex: Rep[Int]): Rep[BinarizedGradientPyramid]

  //class defs
  def binarizedgradientpyramid_pyramid(__x: Rep[BinarizedGradientPyramid]): Rep[DenseVector[GrayscaleImage]]
  def binarizedgradientpyramid_start_level(__x: Rep[BinarizedGradientPyramid]): Rep[Int]
  def binarizedgradientpyramid_levels(__x: Rep[BinarizedGradientPyramid]): Rep[Int]
  def binarizedgradientpyramid_fixedLevelIndex(__x: Rep[BinarizedGradientPyramid]): Rep[Int]
}

trait BinarizedGradientPyramidOpsExp extends BinarizedGradientPyramidOps with EffectExp {
  case class BinarizedGradientPyramidObjectNew(pyramid: Exp[DenseVector[GrayscaleImage]], start_level: Exp[Int], levels: Exp[Int], fixedLevelIndex: Exp[Int]) extends Def[BinarizedGradientPyramid]
  case class BinarizedGradientPyramidPyramid(__x: Exp[BinarizedGradientPyramid]) extends Def[DenseVector[GrayscaleImage]]
  case class BinarizedGradientPyramidStart_level(__x: Exp[BinarizedGradientPyramid]) extends Def[Int]
  case class BinarizedGradientPyramidLevels(__x: Exp[BinarizedGradientPyramid]) extends Def[Int]
  case class BinarizedGradientPyramidFixedlevelindex(__x: Exp[BinarizedGradientPyramid]) extends Def[Int]

  def binarizedgradientpyramid_obj_new(pyramid: Exp[DenseVector[GrayscaleImage]], start_level: Exp[Int], levels: Exp[Int], fixedLevelIndex: Exp[Int]) = reflectEffect(BinarizedGradientPyramidObjectNew(pyramid, start_level, levels, fixedLevelIndex))
  def binarizedgradientpyramid_pyramid(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidPyramid(__x)
  def binarizedgradientpyramid_start_level(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidStart_level(__x)
  def binarizedgradientpyramid_levels(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidLevels(__x)
  def binarizedgradientpyramid_fixedLevelIndex(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidFixedlevelindex(__x)
}

trait ScalaGenBinarizedGradientPyramidOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case BinarizedGradientPyramidObjectNew(pyramid, start_level, levels, fixedLevelIndex) => emitValDef(sym, "new " + remap(manifest[BinarizedGradientPyramid]) + "(" + quote(pyramid)  + "," + quote(start_level)  + "," + quote(levels)  + "," + quote(fixedLevelIndex)  + ")")
    case BinarizedGradientPyramidPyramid(x) =>  emitValDef(sym, quote(x) + ".pyramid")
    case BinarizedGradientPyramidStart_level(x) =>  emitValDef(sym, quote(x) + ".start_level")
    case BinarizedGradientPyramidLevels(x) =>  emitValDef(sym, quote(x) + ".levels")
    case BinarizedGradientPyramidFixedlevelindex(x) =>  emitValDef(sym, quote(x) + ".fixedLevelIndex")
    case _ => super.emitNode(sym, rhs)
  }
}
