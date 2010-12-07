package ppl.dsl.optiml

import scala.virtualization.lms.common.{DSLOpsExp, Base}
import java.io.PrintWriter
import scala.virtualization.lms.internal.{ScalaGenBase, GenericNestedCodegen, CudaGenBase}
import collection.mutable.LinkedList

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
  def sum[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A]) = optiml_sum(start, end, block)

  def optiml_sum[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]) : Rep[A]
}

trait LanguageOpsExp extends LanguageOps with DSLOpsExp { this: LanguageImplOps with ArithImplicits =>
  // implemented via kernel embedding
  /*
  case class Sum[A:Manifest:ArithOps](start: Exp[Int], end: Exp[Int], block: Rep[Int] => Rep[A])
    extends DSLOp(reifyEffects(optiml_sum_impl(start, end, block)))
    
  def optiml_sum[A:Manifest:ArithOps](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A]) : Exp[A] = {
    Sum(start, end, block)
  }
  */

  // Testing
  case class Sum[A:Manifest:ArithOps](start: Exp[Int], end: Exp[Int], block: Rep[Int] => Rep[A], x: Sym[Int], y: Exp[A], op: Exp[A]) //extends Def[A]
    extends DSLOp(reifyEffects(optiml_sum_impl(start, end, block)))

  def optiml_sum[A](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A])(implicit m:Manifest[A], ops:ArithOps[A]) : Exp[A] = {
    val x = fresh[Int]
    val y = reifyEffects(block(x))
    val sym = fresh[A]
    val op = reifyEffects(ops.+=(sym,y))
    val rhs = Sum(start, end, block, x, y, op)
    createDefinition(sym,rhs)
    sym
  }
}

trait BaseGenLanguageOps extends GenericNestedCodegen {
  val IR: LanguageOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Sum(start,end,block,x,y,op) if shallow => syms(start) ::: syms(end) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case Sum(start,end,block,x,y,op) => getFreeVarBlock(y,List(x.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }
}

trait ScalaGenLanguageOps extends ScalaGenBase with BaseGenLanguageOps {
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
        case Sum(start,end,block,x,y,op) =>
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
        
        case _ => super.emitNode(sym, rhs)
      }
    }
}