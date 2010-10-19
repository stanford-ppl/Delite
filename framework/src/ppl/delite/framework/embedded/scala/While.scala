package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait While { this: DeliteApplication =>
  def __whileDo(cond: => Rep[Boolean], body: => Rep[Unit])
}


trait WhileExp extends While { this: DeliteApplication with FunctionsExp =>
  case class While(cond: () => Exp[Boolean], body: Exp[Unit]) extends Def[Unit]

  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) {
    val a = reifyEffects(body)
    reflectEffect(While(() => cond, a))
  }

  targets.get("Scala").getOrElse(
    throw new RuntimeException("Couldn't find Scala code generator")
  ) .generators += new CodeGeneratorScalaWhile {
    val intermediate: WhileExp.this.type = WhileExp.this
  }
}


trait CodeGeneratorScalaWhile extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with WhileExp
  import intermediate._

  override def syms2(e: Any, shallow: Boolean): Option[List[Sym[Any]]] = e match {
    case While(c, b) if shallow => Some(Nil)
    case _ => None
  }
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case While(c,b) =>
      val c_blk = reifyEffects(c())
      stream.print("while ({")
      emitBlock(c_blk, intermediate.targets.get("Scala").get)
      stream.print(quote(getBlockResult(c_blk)))
      stream.println("}) {")
      emitBlock(b, intermediate.targets.get("Scala").get)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}