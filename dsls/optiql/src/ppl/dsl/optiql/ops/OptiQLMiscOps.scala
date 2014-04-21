package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{ScalaGenEffect, EffectExp, Base}
import java.io.PrintWriter
import ppl.dsl.optiql.{OptiQLExp,OptiQL}
import reflect.SourceContext

trait OptiQLMiscOps extends Base {  this : OptiQL =>

  def tic(deps: Rep[Any]*) = optiql_profile_start(deps)
  def toc(deps: Rep[Any]*) = optiql_profile_stop(deps)
  def infix_printAsTable[T:Manifest](t: Rep[Table[T]], max_rows: Rep[Int] = unit(100)): Rep[Unit] = tablePrintAsTable(t, max_rows)

  def optiql_profile_start(deps: Seq[Rep[Any]]): Rep[Unit]
  def optiql_profile_stop(deps: Seq[Rep[Any]]): Rep[Unit]
  def tablePrintAsTable[T:Manifest](t: Rep[Table[T]], max_rows: Rep[Int]): Rep[Unit]

}

trait OptiQLMiscOpsExp extends OptiQLMiscOps with EffectExp { this : OptiQLExp =>

  case class OptiQLProfileStart(deps: Exp[Seq[Any]]) extends Def[Unit]
  case class OptiQLProfileStop(deps: Exp[Seq[Any]]) extends Def[Unit]
  case class TablePrintAsTable[T](t: Rep[Table[T]], max_rows: Rep[Int]) extends Def[Unit]

  def optiql_profile_start(deps: Seq[Rep[Any]]): Rep[Unit] = reflectEffect(OptiQLProfileStart(Seq(deps: _*)))
  def optiql_profile_stop(deps: Seq[Rep[Any]]): Rep[Unit] =  reflectEffect(OptiQLProfileStop(Seq(deps: _*)))
  def tablePrintAsTable[T:Manifest](t: Exp[Table[T]], max_rows: Rep[Int]): Exp[Unit] = reflectEffect(TablePrintAsTable(t, max_rows)) //TODO: port pretty print function from plain Scala

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case Reflect(OptiQLProfileStart(x), u, es) => reflectMirrored(Reflect(OptiQLProfileStart(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(OptiQLProfileStop(x), u, es) => reflectMirrored(Reflect(OptiQLProfileStop(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(TablePrintAsTable(x,m), u, es) => reflectMirrored(Reflect(TablePrintAsTable(f(x),f(m)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenOptiQLMiscOps extends ScalaGenEffect {
  val IR:OptiQLMiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OptiQLProfileStart(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(\"app\", false)")
    case OptiQLProfileStop(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.stop(\"app\", false)")
    case TablePrintAsTable(t,max_rows) => emitValDef(sym, "generated.scala.container.Table.printAsTable("+ quote(t) + ", " + quote(max_rows) + ")")
    case _ => super.emitNode(sym,rhs)
  }

}
