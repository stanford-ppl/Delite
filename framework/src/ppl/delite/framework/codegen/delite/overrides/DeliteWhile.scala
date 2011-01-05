package ppl.delite.framework.codegen.delite.overrides

import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common.{WhileExp}
import scala.virtualization.lms.internal.{ScalaGenEffect, GenericNestedCodegen}
import java.io.PrintWriter

trait DeliteWhileExp extends WhileExp {

  this: DeliteOpsExp =>

  case class DeliteWhile(cond: Exp[Boolean], body: Exp[Unit]) extends DeliteOpWhileLoop(cond, body)

  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) {
    val c = reifyEffects(cond)
    val a = reifyEffects(body)
    reflectEffect(DeliteWhile(c, a))
  }

}

trait DeliteBaseGenWhile extends GenericNestedCodegen {
  val IR: DeliteWhileExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case DeliteWhile(c, b) if shallow => Nil
    case _ => super.syms(e)
  }

  // TODO: What about condition node?
  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case DeliteWhile(c,b) => getFreeVarBlock(c,Nil) ::: getFreeVarBlock(b,Nil)
    case _ => super.getFreeVarNode(rhs)
  }

}

trait DeliteScalaGenWhile extends ScalaGenEffect with DeliteBaseGenWhile