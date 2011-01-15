package ppl.delite.framework.codegen.delite.overrides

import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common.{WhileExp}
import java.io.PrintWriter
import scala.virtualization.lms.internal._

trait DeliteWhileExp extends WhileExp {

  this: DeliteOpsExp =>

  case class DeliteWhile(cond: Exp[Boolean], body: Exp[Unit]) extends DeliteOpWhileLoop

  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) {
    cond match {
      case Const(true) => // print warning?
      case Const(false) => return
      case _ => // pass
    }

    val c = reifyEffects(cond)
    val a = reifyEffects(body)
    // TODO: reflectEffect(new While(c, a) with DeliteOpWhile))
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

trait DeliteScalaGenWhile extends ScalaGenEffect with DeliteBaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case DeliteWhile(c,b) =>
      stream.print("val " + quote(sym) + " = while ({")
      emitBlock(c)
      stream.print(quote(getBlockResult(c)))
      stream.println("}) {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}

trait DeliteCudaGenWhile extends CudaGenEffect with DeliteBaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        case DeliteWhile(c,b) =>
            // Get free variables list
            val freeVars = getFreeVarBlock(c,Nil)
            val argListStr = if(freeVars.length == 0) freeVars.map(quote(_)).mkString(", ") else ""
            val paramListStr = if(freeVars.length == 0) freeVars.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ") else ""

            // emit function for the condition evaluation
            stream.println("__device__ __host__ %s %s(%s) {".format("bool", "cond_"+quote(sym), paramListStr))
            tabWidth += 1
            emitBlock(c)
            stream.println(addTab()+"return %s;".format(quote(getBlockResult(c))))
            stream.println("}")
            tabWidth -= 1

            // Emit while loop (only the result variable of condition)
            stream.print(addTab() + "while (")
            stream.print("cond_"+quote(sym)+"(%s)".format(argListStr))
            stream.println(") {")
            tabWidth += 1
            emitBlock(b)
            tabWidth -= 1
            //stream.println(quote(getBlockResult(b)))   //TODO: Is this needed?
            stream.println("}")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait DeliteCGenWhile extends CGenEffect with DeliteBaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case DeliteWhile(c,b) =>
        // calculate condition
        emitBlock(c)
        stream.println("bool cond_%s = %s;".format(quote(sym),quote(getBlockResult(c))))
        // Emit while loop
        stream.print("while (cond_%s) {".format(quote(sym)))
        emitBlock(b)
        stream.println("}")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
