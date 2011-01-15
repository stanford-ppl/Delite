package ppl.delite.framework.ops

import scala.virtualization.lms.common.EffectExp
import java.io.PrintWriter
import scala.virtualization.lms.internal.{CudaGenEffect, CGenEffect, ScalaGenEffect, GenericNestedCodegen}

trait VariantsOpsExp extends EffectExp {
  this: DeliteOpsExp =>

  /**
   * Variants are used to represent a Delite op multiple ways in the IR.
   */
  //trait Variant[T <: DeliteOp[_]] extends T
  trait Variant[T <: DeliteOp[_]]

  // this is unsatisfying
  trait DeliteOpIndexedLoopVariant extends Variant[DeliteOpIndexedLoop] with IndexedLoopLike {
    val indexOp: Sym[Unit]
  }

  trait DeliteOpWhileLoopVariant extends Variant[DeliteOpWhileLoop] with WhileLoopLike

  /**
   * This is a re-wiring class used for variants. Tt translates an index symbol to a value symbol.
   *
   * @param value  the value symbol
   * @param conv   the reified function that converts the index symbol to a value to be bound to the provided value symbol
   */
  case class DeliteOpIndexToValue[A](value: Exp[A], conv: Exp[A]) extends Def[Unit]

}

trait BaseGenVariantsOps extends GenericNestedCodegen {
  val IR: VariantsOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case l:DeliteOpIndexedLoopVariant => syms(l.start) ::: syms(l.end) ::: super.syms(e)
    //case w:DeliteOpWhileLoopVariant => syms()
    case _ => super.syms(e)
  }
}

trait ScalaGenVariantsOps extends BaseGenVariantsOps with ScalaGenEffect {
  val IR: VariantsOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case DeliteOpIndexToValue(value, conv) =>  stream.println(quote(value) + " = " + "{ ")
                                               emitBlock(conv)
                                               stream.println(getBlockResult(conv))
                                               stream.println("}")
    case _ => super.emitNode(sym, rhs)

  }
}

trait CudaGenVariantsOps extends CudaGenEffect with BaseGenVariantsOps
trait CGenVariantsOps extends CGenEffect with BaseGenVariantsOps

