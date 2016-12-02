package ppl.delite.framework.ops

import java.io.PrintWriter
import scala.virtualization.lms.common._
import org.scala_lang.virtualized.SourceContext
import org.scala_lang.virtualized.RefinedManifest
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.delite.framework.ops._
import ppl.delite.framework.Util._
import ppl.delite.framework.Config

trait RuntimeServiceOps extends Base {
  lazy val DELITE_NUM_THREADS = runtime_query_numthreads()
  lazy val DELITE_NUM_SOCKETS = runtime_query_numsockets()
  lazy val DELITE_NUM_SLAVES = runtime_query_numslaves()

  def runtime_query_numthreads()(implicit ctx: SourceContext): Rep[Int]
  def runtime_query_numsockets()(implicit ctx: SourceContext): Rep[Int]
  def runtime_query_numslaves()(implicit ctx: SourceContext): Rep[Int]
}

trait RuntimeServiceOpsExp extends RuntimeServiceOps with EffectExp {
  this: DeliteOpsExp =>

  case class RuntimeQueryNumThreads() extends Def[Int]
  case class RuntimeQueryNumSockets() extends Def[Int]
  case class RuntimeQueryNumSlaves() extends Def[Int]

  def runtime_query_numthreads()(implicit ctx: SourceContext) = reflectPure(RuntimeQueryNumThreads())
  def runtime_query_numsockets()(implicit ctx: SourceContext) = reflectPure(RuntimeQueryNumSockets())
  def runtime_query_numslaves()(implicit ctx: SourceContext) = reflectPure(RuntimeQueryNumSlaves())

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case RuntimeQueryNumThreads() => runtime_query_numthreads()(pos)
    case RuntimeQueryNumSockets() => runtime_query_numsockets()(pos)
    case RuntimeQueryNumSlaves() => runtime_query_numslaves()(pos)

    case Reflect(RuntimeQueryNumThreads(), u, es) => reflectMirrored(Reflect(RuntimeQueryNumThreads(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(RuntimeQueryNumSockets(), u, es) => reflectMirrored(Reflect(RuntimeQueryNumSockets(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(RuntimeQueryNumSlaves(), u, es) => reflectMirrored(Reflect(RuntimeQueryNumSlaves(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenRuntimeServiceOps extends ScalaGenEffect with GenericGenDeliteOps {
  val IR: RuntimeServiceOpsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case RuntimeQueryNumThreads() => emitValDef(sym, fieldAccess(resourceInfoSym,"numThreads"))
    case RuntimeQueryNumSlaves() => emitValDef(sym, fieldAccess(resourceInfoSym,"numSlaves"))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenRuntimeServiceOps extends CGenEffect {
  val IR: RuntimeServiceOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case RuntimeQueryNumThreads() => emitValDef(sym, "config->numThreads")
    case RuntimeQueryNumSockets() => emitValDef(sym, "config->numSockets")
    case _ => super.emitNode(sym, rhs)
  }
}
