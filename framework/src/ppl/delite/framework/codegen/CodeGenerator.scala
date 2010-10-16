package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.common.BaseExp
import _root_.scala.virtualization.lms.internal.{Effects, GenericNestedCodegen}
import _root_.scala.virtualization.lms.util.GraphUtil
import java.io.PrintWriter

trait CodeGenerator {
  val intermediate: BaseExp with Effects
  import intermediate._

  // merged codegen and nested codegen
  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) {
    rhs match {
      case Reflect(s, effects) =>  emitNode(sym, s)
      case Reify(s, effects) =>
        // just ignore -- effects are accounted for in emitBlock
      case _ => throw new Exception("don't know how to generate code for: " + rhs)
    }
  }

  // default syms that each generator can override if it needs to
  def syms2(e: Any, shallow: Boolean) : Option[List[Sym[Any]]] = None
}