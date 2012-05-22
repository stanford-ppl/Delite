package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{ScalaGenEffect, EffectExp, Base}
import java.io.PrintWriter
import ppl.dsl.optiql.{OptiQLExp,OptiQL}

trait OptiQLMiscOps extends Base {  this : OptiQL =>

  def tic(deps: Rep[Any]*) = optiql_profile_start(deps)
  def toc(deps: Rep[Any]*) = optiql_profile_stop(deps)

  def optiql_profile_start(deps: Seq[Rep[Any]]): Rep[Unit]
  def optiql_profile_stop(deps: Seq[Rep[Any]]): Rep[Unit]

}

trait OptiQLMiscOpsExp extends OptiQLMiscOps with EffectExp { this : OptiQLExp =>

  case class OptiQLProfileStart(deps: Exp[Seq[Any]]) extends Def[Unit]
  case class OptiQLProfileStop(deps: Exp[Seq[Any]]) extends Def[Unit]

  def optiql_profile_start(deps: Seq[Rep[Any]]): Rep[Unit] = reflectEffect(OptiQLProfileStart(Seq(deps: _*)))
  def optiql_profile_stop(deps: Seq[Rep[Any]]): Rep[Unit] =  reflectEffect(OptiQLProfileStop(Seq(deps: _*)))
}

trait ScalaGenOptiQLMiscOps extends ScalaGenEffect {
  val IR:OptiQLMiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

    case OptiQLProfileStart(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(\"app\", false)")
    case OptiQLProfileStop(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.stop(\"app\", false)")
    case _ => super.emitNode(sym,rhs)
  }

}