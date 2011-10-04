package ppl.dsl.experimental

import scala.virtualization.lms.common.EffectExp
import java.io.PrintWriter
import scala.virtualization.lms.common.{CudaGenEffect, CGenEffect, ScalaGenEffect}
import scala.virtualization.lms.internal.{GenericNestedCodegen}

trait SandboxVariantsOpsExp extends EffectExp {
  this: SandboxDeliteOpsExp =>

  /**
   * Variants are used to represent a Delite op multiple ways in the IR.
   */
  //trait Variant[T <: DeliteOp[_]] extends T
  trait Variant {
    val variant: Exp[Any]
  }

  // this is unsatisfying
  trait DeliteOpMapLikeWhileLoopVariant extends Variant {
    val alloc: Exp[Any] // same scope as variant
  }

  trait DeliteOpReduceLikeWhileLoopVariant extends Variant {
    val init: Exp[Any] // inner scope, separate from variant
    //val acc: Var[Any] // outer scope
    //val index: Var[Any] // outer scope
    val Acc: Exp[Any]
    val Index: Exp[Any]
  }
}

trait SandboxBaseGenVariantsOps extends GenericNestedCodegen {
  val IR: SandboxVariantsOpsExp
  import IR._

}

trait SandboxScalaGenVariantsOps extends SandboxBaseGenVariantsOps with ScalaGenEffect {
  val IR: SandboxVariantsOpsExp
  import IR._

  /*
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)

  }
  */
}

trait SandboxCudaGenVariantsOps extends CudaGenEffect with SandboxBaseGenVariantsOps
trait SandboxCGenVariantsOps extends CGenEffect with SandboxBaseGenVariantsOps

