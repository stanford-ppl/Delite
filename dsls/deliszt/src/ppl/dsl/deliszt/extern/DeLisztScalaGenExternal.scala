package ppl.dsl.deliszt.extern

import java.io.PrintWriter
import scala.virtualization.lms.internal._
import ppl.delite.framework.extern.codegen.scala.ScalaGenExternalBase
import ppl.dsl.deliszt.DeLisztExp

trait DeLisztScalaGenExternal extends ScalaGenExternalBase {
  val IR: DeLisztExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
    
  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    case _ => super.emitExternalLib(rhs)
  }     
}
