package ppl.dsl.optiml.application

import ppl.dsl.optiml._
import java.io.PrintWriter
import scala.virtualization.lms.common.ScalaGenBase
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{EffectExp, Variables}


trait PairwiseRatingOps extends Variables with OverloadHack {

  object PairwiseRating {
    def apply(profileA: Rep[Int], profileB: Rep[Int], scoreA: Rep[Int], scoreB: Rep[Int]) = pairwiserating_obj_new(profileA, profileB, scoreA, scoreB)
  }

  implicit def repPairwiseRatingToPairwiseRatingOps(x: Rep[PairwiseRating]) = new pairwiseratingOpsCls(x)
  implicit def pairwiseratingToPairwiseRatingOps(x: Var[PairwiseRating]) = new pairwiseratingOpsCls(readVar(x))

  class pairwiseratingOpsCls(__x: Rep[PairwiseRating]) {
    def profileA = pairwiserating_profileA(__x)
    def profileB = pairwiserating_profileB(__x)
    def scoreA = pairwiserating_scoreA(__x)
    def scoreB = pairwiserating_scoreB(__x)
  }

  //object defs
  def pairwiserating_obj_new(profileA: Rep[Int], profileB: Rep[Int], scoreA: Rep[Int], scoreB: Rep[Int]): Rep[PairwiseRating]

  //class defs
  def pairwiserating_profileA(__x: Rep[PairwiseRating]): Rep[Int]
  def pairwiserating_profileB(__x: Rep[PairwiseRating]): Rep[Int]
  def pairwiserating_scoreA(__x: Rep[PairwiseRating]): Rep[Int]
  def pairwiserating_scoreB(__x: Rep[PairwiseRating]): Rep[Int]
}

trait PairwiseRatingOpsExp extends PairwiseRatingOps with EffectExp {
  case class PairwiseRatingObjectNew(profileA: Exp[Int], profileB: Exp[Int], scoreA: Exp[Int], scoreB: Exp[Int]) extends Def[PairwiseRating]
  case class PairwiseRatingProfilea(__x: Exp[PairwiseRating]) extends Def[Int]
  case class PairwiseRatingProfileb(__x: Exp[PairwiseRating]) extends Def[Int]
  case class PairwiseRatingScorea(__x: Exp[PairwiseRating]) extends Def[Int]
  case class PairwiseRatingScoreb(__x: Exp[PairwiseRating]) extends Def[Int]

  def pairwiserating_obj_new(profileA: Exp[Int], profileB: Exp[Int], scoreA: Exp[Int], scoreB: Exp[Int]) = reflectEffect(PairwiseRatingObjectNew(profileA, profileB, scoreA, scoreB))
  def pairwiserating_profileA(__x: Rep[PairwiseRating]) = PairwiseRatingProfilea(__x)
  def pairwiserating_profileB(__x: Rep[PairwiseRating]) = PairwiseRatingProfileb(__x)
  def pairwiserating_scoreA(__x: Rep[PairwiseRating]) = PairwiseRatingScorea(__x)
  def pairwiserating_scoreB(__x: Rep[PairwiseRating]) = PairwiseRatingScoreb(__x)
}

trait ScalaGenPairwiseRatingOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case PairwiseRatingObjectNew(profileA, profileB, scoreA, scoreB) => emitValDef(sym, "new " + remap(manifest[PairwiseRating]) + "(" + quote(profileA)  + "," + quote(profileB)  + "," + quote(scoreA)  + "," + quote(scoreB)  + ")")
    case PairwiseRatingProfilea(x) =>  emitValDef(sym, quote(x) + ".profileA")
    case PairwiseRatingProfileb(x) =>  emitValDef(sym, quote(x) + ".profileB")
    case PairwiseRatingScorea(x) =>  emitValDef(sym, quote(x) + ".scoreA")
    case PairwiseRatingScoreb(x) =>  emitValDef(sym, quote(x) + ".scoreB")
    case _ => super.emitNode(sym, rhs)
  }
}
