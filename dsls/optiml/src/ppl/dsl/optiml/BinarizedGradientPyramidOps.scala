package ppl.dsl.optiml

import datastruct.scala._
import java.io.PrintWriter
import ppl.delite.framework.{DSLType}
import scala.virtualization.lms.internal.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}


trait BinarizedGradientPyramidOps extends DSLType with Variables with OverloadHack {

  object BinarizedGradientPyramid {
    def apply(pyramid: Rep[Vector[GrayscaleImage]], start_level: Rep[Int], levels: Rep[Int], fixedLevelIndex: Rep[Int]) = binarizedgradientpyramid_obj_new(pyramid, start_level, levels, fixedLevelIndex)
  }

  implicit def repBinarizedGradientPyramidToBinarizedGradientPyramidOps(x: Rep[BinarizedGradientPyramid]) = new binarizedgradientpyramidOpsCls(x)
  implicit def binarizedgradientpyramidToBinarizedGradientPyramidOps(x: Var[BinarizedGradientPyramid]) = new binarizedgradientpyramidOpsCls(readVar(x))

  class binarizedgradientpyramidOpsCls(_x_: Rep[BinarizedGradientPyramid]) {
    def pyramid = binarizedgradientpyramid_pyramid(__x__)
    def start_level = binarizedgradientpyramid_start_level(__x__)
    def levels = binarizedgradientpyramid_levels(__x__)
    def fixedLevelIndex = binarizedgradientpyramid_fixedLevelIndex(__x__)
  }

  //object defs
  def binarizedgradientpyramid_obj_new(pyramid: Rep[Vector[GrayscaleImage]], start_level: Rep[Int], levels: Rep[Int], fixedLevelIndex: Rep[Int]): Rep[BinarizedGradientPyramid]

  //class defs
  def binarizedgradientpyramid_pyramid(__x__: Rep[BinarizedGradientPyramid]): Rep[Vector[GrayscaleImage]]
  def binarizedgradientpyramid_start_level(__x__: Rep[BinarizedGradientPyramid]): Rep[Int]
  def binarizedgradientpyramid_levels(__x__: Rep[BinarizedGradientPyramid]): Rep[Int]
  def binarizedgradientpyramid_fixedLevelIndex(__x__: Rep[BinarizedGradientPyramid]): Rep[Int]
}

trait BinarizedGradientPyramidOpsExp extends BinarizedGradientPyramid with BaseExp { this: DeliteOpsExp =>
  case class BinarizedGradientPyramidObjectNew(pyramid: Exp[Vector[GrayscaleImage]], start_level: Exp[Int], levels: Exp[Int], fixedLevelIndex: Exp[Int]) extends Def[BinarizedGradientPyramid]
  case class BinarizedGradientPyramidPyramid(__x__: Exp[BinarizedGradientPyramid]) extends Def[Vector[GrayscaleImage]]
  case class BinarizedGradientPyramidStart_level(__x__: Exp[BinarizedGradientPyramid]) extends Def[Int]
  case class BinarizedGradientPyramidLevels(__x__: Exp[BinarizedGradientPyramid]) extends Def[Int]
  case class BinarizedGradientPyramidFixedlevelindex(__x__: Exp[BinarizedGradientPyramid]) extends Def[Int]

  binarizedgradientpyramid_obj_new(pyramid: Exp[Vector[GrayscaleImage]], start_level: Exp[Int], levels: Exp[Int], fixedLevelIndex: Exp[Int]) = reflectEffect(BinarizedGradientPyramidObjectNew(pyramid, start_level, levels, fixedLevelIndex))
  def binarizedgradientpyramid_pyramid(__x__: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidPyramid(__x__)
  def binarizedgradientpyramid_start_level(__x__: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidStart_level(__x__)
  def binarizedgradientpyramid_levels(__x__: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidLevels(__x__)
  def binarizedgradientpyramid_fixedLevelIndex(__x__: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidFixedlevelindex(__x__)
}

trait ScalaGenBinarizedGradientPyramidOps extends ScalaGenBase {
  val IR: BinarizedGradientPyramidOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
  case BinarizedGradientPyramidObjectNew(pyramid, start_level, levels, fixedLevelIndex) => emitValDef(sym, "new " + remap(manifest[BinarizedGradientPyramid]) + "(" + quote(pyramid)  + "," + quote(start_level)  + "," + quote(levels)  + "," + quote(fixedLevelIndex)  + ")")
  case BinarizedGradientPyramidPyramid(x) =>  emitValDef(sym, quote(x) + ".pyramid")
  case BinarizedGradientPyramidStart_level(x) =>  emitValDef(sym, quote(x) + ".start_level")
  case BinarizedGradientPyramidLevels(x) =>  emitValDef(sym, quote(x) + ".levels")
  case BinarizedGradientPyramidFixedlevelindex(x) =>  emitValDef(sym, quote(x) + ".fixedLevelIndex")
   case _ => super.emitNode(sym, rhs)
}
