package ppl.dsl.optiml.application

import ppl.dsl.optiml._
import java.io.PrintWriter
import scala.virtualization.lms.common.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}


trait SimilarityOps extends Variables with OverloadHack {

  object Similarity {
    def apply(a: Rep[Int], b: Rep[Int], value: Rep[Double]) = similarity_obj_new(a, b, value)
  }

  implicit def repSimilarityToSimilarityOps(x: Rep[Similarity]) = new similarityOpsCls(x)
  implicit def similarityToSimilarityOps(x: Var[Similarity]) = new similarityOpsCls(readVar(x))

  class similarityOpsCls(__x: Rep[Similarity]) {
    def a = similarity_a(__x)
    def b = similarity_b(__x)
    def value = similarity_value(__x)
  }

  //object defs
  def similarity_obj_new(a: Rep[Int], b: Rep[Int], value: Rep[Double]): Rep[Similarity]

  //class defs
  def similarity_a(__x: Rep[Similarity]): Rep[Int]
  def similarity_b(__x: Rep[Similarity]): Rep[Int]
  def similarity_value(__x: Rep[Similarity]): Rep[Double]
}

trait SimilarityOpsExp extends SimilarityOps with EffectExp {
  case class SimilarityObjectNew(a: Exp[Int], b: Exp[Int], value: Exp[Double]) extends Def[Similarity]
  case class SimilarityA(__x: Exp[Similarity]) extends Def[Int]
  case class SimilarityB(__x: Exp[Similarity]) extends Def[Int]
  case class SimilarityValue(__x: Exp[Similarity]) extends Def[Double]

  def similarity_obj_new(a: Exp[Int], b: Exp[Int], value: Exp[Double]) = reflectEffect(SimilarityObjectNew(a, b, value))
  def similarity_a(__x: Rep[Similarity]) = SimilarityA(__x)
  def similarity_b(__x: Rep[Similarity]) = SimilarityB(__x)
  def similarity_value(__x: Rep[Similarity]) = SimilarityValue(__x)
}

trait ScalaGenSimilarityOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case SimilarityObjectNew(a, b, value) => emitValDef(sym, "new " + remap(manifest[Similarity]) + "(" + quote(a)  + "," + quote(b)  + "," + quote(value)  + ")")
    case SimilarityA(x) =>  emitValDef(sym, quote(x) + ".a")
    case SimilarityB(x) =>  emitValDef(sym, quote(x) + ".b")
    case SimilarityValue(x) =>  emitValDef(sym, quote(x) + ".value")
    case _ => super.emitNode(sym, rhs)
  }
}
