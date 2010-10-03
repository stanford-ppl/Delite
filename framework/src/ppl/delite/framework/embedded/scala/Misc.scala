package ppl.delite.framework.embedded.scala

import ppl.delite.framework.DSLType
import scala.virtualization.lms.ppl.ScalaOpsExp
import java.io.PrintWriter
import scala.virtualization.lms.common.BaseExp
import scala.virtualization.lms.internal.Effects
import ppl.delite.framework.codegen.CodeGenerator
import ppl.delite.framework.codegen.scala.CodeGeneratorScalaBase
import ppl.delite.framework.codegen.c.{CodeGeneratorCBase, TargetC}

trait Misc extends DSLType {
  //put my stuff here
}


//todo replace ScalaOpsExp with our own hiearchy
trait CodeGeneratorScalaMisc extends CodeGeneratorScalaBase {

  val intermediate: BaseExp with Effects with ScalaOpsExp
  import intermediate._

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Print(s) => emitValDef("", sym, "println(" + quote(s) + ")")
    case Exit(a) => emitValDef("", sym, "exit(" + quote(a) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}


//todo factor out commonality
trait CodeGeneratorCMisc extends CodeGeneratorCBase {

  val intermediate: BaseExp with Effects with ScalaOpsExp
  import intermediate._

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Print(s) => stream.println("printf(\"%s\"," + quote(s) + ");")
    case Exit(a) => stream.println("exit(" + quote(a) + ");")

    case _ => super.emitNode(sym, rhs)
  }

}
