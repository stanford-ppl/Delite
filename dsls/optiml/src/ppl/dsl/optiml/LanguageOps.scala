package ppl.dsl.optiml

import datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common.{TupleOps, NumericOps, DSLOpsExp, Base}
import java.io.PrintWriter
import reflect.Manifest
import scala.virtualization.lms.internal.{GenericNestedCodegen, CudaGenBase, ScalaGenEffect}

/* Machinery provided by OptiML itself (language features and control structures).
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * created: Nov 29, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait LanguageOps extends Base { this: ArithImplicits =>
  // TODO: type class should probable be Zeroable[A] or something
  //def <>[A:Manifest:ArithOps] = optiml_zero
  
  def sum[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A]) = optiml_sum(start, end, block)

  // lightweight profiling, matlab style
  def tic() = profile_start()
  def toc() = profile_stop()

  def optiml_sum[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]) : Rep[A]

  def profile_start() : Rep[Unit]
  def profile_stop() : Rep[Unit]
}

trait LanguageOpsExp extends LanguageOps with TupleOps with NumericOps with VectorOps with DSLOpsExp with DeliteOpsExp
  { this: LanguageImplOps with ArithImplicits =>

  case class ProfileStart() extends Def[Unit]
  case class ProfileStop() extends Def[Unit]
  
  def profile_start() = reflectEffect(ProfileStart())
  def profile_stop() = reflectEffect(ProfileStop())

  case class Sum[A:Manifest:ArithOps](in: Exp[Vector[Int]], mV: Exp[Int], map: Exp[A], rV: Exp[(A,A)], reduce: Exp[A])
    extends DeliteOpMapReduce[Int,A,Vector]

  // implemented via kernel embedding
  //case class Sum[A:Manifest:ArithOps](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A])
    //extends DSLOp(reifyEffects(optiml_sum_impl(start, end, block)))

  def optiml_sum[A](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A])
                   (implicit mA: Manifest[A], ops: ArithOps[A]) = {
    //Sum(start, end, block)
    // TODO: how does the map know the Vector.zeros is not needed, after it is optimized away in the reducing function?
    // each call to map is going to instantiate a Vector.zeros that is never used (maybe we can get around this by being lazy..)
    // actually does not get optimized out, because at this point we run the function on rV, not on its real inputs
    // unless we use mapreduce composition
    // -- and even then i think the if statement in GDA is screwing us.. the resulting vector might or might be  a zero vector in the +=
    val in = Vector.range(start, end)
    val mV = fresh[Int]
    val map = reifyEffects(block(mV))
    //val mapreduce = reifyEffects(ops.+=(acc, reifyEffects(block(mV))))
    val rV = fresh[(A,A)]
    val reduce = reifyEffects(ops.+=(rV._1,rV._2))

    reflectEffect(Sum(in, mV, map, rV, reduce))
  }
}

/*
trait BaseGenLanguageOps extends GenericNestedCodegen {
  val IR: LanguageOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    //case Sum(start,end,block,x,y,op) if shallow => syms(start) ::: syms(end) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    //case Sum(start,end,block,x,y,op) => getFreeVarBlock(y,List(x.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }
}

trait ScalaGenLanguageOps extends ScalaGenEffect with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenLanguageOps extends CudaGenBase with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
      /*  case Sum(start,end,block,x,y,op) =>
          stream.println(addTab()+"int %s = %s;".format(quote(x),quote(start)))
          addVarLink(getBlockResult(y).asInstanceOf[Sym[_]],sym)
          emitBlock(y)
          removeVarLink(getBlockResult(y).asInstanceOf[Sym[_]],sym)
          stream.println(addTab()+"for(int %s=%s; %s<%s; %s++) {".format(quote(x),quote(start)+"+1",quote(x),quote(end),quote(x)))
          tabWidth += 1
          addVarLink(getBlockResult(op).asInstanceOf[Sym[_]],sym)
          emitBlock(op)
          removeVarLink(getBlockResult(op).asInstanceOf[Sym[_]],sym)
          tabWidth -= 1
          stream.println(addTab()+"}")
          allocOutput(sym,getBlockResult(y).asInstanceOf[Sym[_]])
        */
        case _ => super.emitNode(sym, rhs)
     }
  }
}
*/