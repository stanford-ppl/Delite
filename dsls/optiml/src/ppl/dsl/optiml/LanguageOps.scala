package ppl.dsl.optiml

import datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import reflect.Manifest
import scala.virtualization.lms.internal.GenericFatCodegen
import scala.virtualization.lms.common._

/* Machinery provided by OptiML itself (language features and control structures).
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * created: Nov 29, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait LanguageOps extends Base { this: ArithOps =>
  // TODO: type class should probable be Zeroable[A] or something
  //def <>[A:Manifest:Arith] = optiml_zero
  
  def sum[A:Manifest:Arith](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A]) = optiml_sum(start, end, block)

  // lightweight profiling, matlab style
  def tic() = profile_start()
  def toc() = profile_stop()

  def optiml_sum[A:Manifest:Arith](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]) : Rep[A]

  def profile_start() : Rep[Unit]
  def profile_stop() : Rep[Unit]
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: LanguageImplOps with ArithOps with VectorOpsExp with DSLOpsExp with DeliteOpsExp =>

  case class ProfileStart() extends Def[Unit]
  case class ProfileStop() extends Def[Unit]
  
  def profile_start() = reflectEffect(ProfileStart())
  def profile_stop() = reflectEffect(ProfileStop())

  case class Sum[A](start: Exp[Int], end: Exp[Int], mV: Sym[Int], map: Exp[A])(implicit mA: Manifest[A], ops: Arith[A])
    extends DeliteOpMapReduce[Int,A,Vector] {

    val in = Vector.range(start, end)
    val rV = (fresh[A],fresh[A])
    val reduce = reifyEffects(ops.+=(rV._1,rV._2))
    //val mapreduce = reifyEffects(ops.+=(acc, reifyEffects(block(mV))))
  }

  def optiml_sum[A](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A])
                   (implicit mA: Manifest[A], ops: Arith[A]) = {

    val mV = fresh[Int]
    val map = reifyEffects(block(mV))
    Sum(start, end, mV, map)
  }
}

trait BaseGenLanguageOps extends GenericFatCodegen {
  val IR: LanguageOpsExp
  import IR._

  /*
  override def syms(e: Any): List[Sym[Any]] = e match {
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case _ => super.getFreeVarNode(rhs)
  }
  */
}

trait ScalaGenLanguageOps extends ScalaGenEffect with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case ProfileStart() => emitValDef(sym, "ppl.delite.runtime.profiler.Stopwatch.start()")
      case ProfileStop() => emitValDef(sym, "ppl.delite.runtime.profiler.Stopwatch.stop()")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

/*
trait CudaGenLanguageOps extends CudaGenBase with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
     }
  }
}
*/