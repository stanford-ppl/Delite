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
  trait DeliteOpMapLikeWhileLoopVariant extends Variant[DeliteOpWhileLoop] with WhileLoopLike {
    // TODO: somehow index is getting moved outside of the loop -- how/why?
    val index = var_new(0)
    val alloc: Exp[Any]
  }
}

trait BaseGenVariantsOps extends GenericNestedCodegen {
  val IR: VariantsOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    //case w:DeliteOpMapLikeWhileLoopVariant if (!shallow) => syms(w.cond) ::: syms(w.body) ::: super.syms(e)
    case _ => super.syms(e)
  }
}

trait ScalaGenVariantsOps extends BaseGenVariantsOps with ScalaGenEffect {
  val IR: VariantsOpsExp
  import IR._

  /*
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)

  }
  */
}

trait CudaGenVariantsOps extends CudaGenEffect with BaseGenVariantsOps
trait CGenVariantsOps extends CGenEffect with BaseGenVariantsOps

