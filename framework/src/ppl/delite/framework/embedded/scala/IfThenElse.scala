package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait IfThenElse { this: DeliteApplication =>
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T]
}

// TODO: it would be nice if IfThenElseExp would extend IfThenElsePureExp
// but then we would need to give it a different name.

trait IfThenElsePureExp extends IfThenElse { this: DeliteApplication => 
  case class IfThenElse[T](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]
  
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = IfThenElse(cond, thenp, elsep)
}


trait IfThenElseExp extends IfThenElse { this: DeliteApplication =>
  case class IfThenElse[T](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]
  override def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = {
    val a = reifyEffects(thenp)
    val b = reifyEffects(elsep)
    (a,b) match {
      case (Def(Reify(_,_)), _) | (_, Def(Reify(_,_))) => reflectEffect(IfThenElse(cond,a,b))
      case _ => IfThenElse(cond, thenp, elsep)
    }
  }

  targets.get("Scala").getOrElse(
    throw new RuntimeException("Couldn't find Scala code generator")
  ) .generators += new CodeGeneratorScalaIfThenElse {
    val intermediate: IfThenElseExp.this.type = IfThenElseExp.this
  }
}


trait CodeGeneratorScalaIfThenElse extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with IfThenElseExp
  import intermediate._

  override def syms2(e: Any, shallow: Boolean): Option[List[Sym[Any]]] = e match {
    case IfThenElse(c, t, e) if shallow => Some(findSyms(c, intermediate.targets.get("Scala").get, shallow)) // in shallow mode, don't count deps from nested blocks
    case _ => None
  }

  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Boolean = {
    rhs match {
      case IfThenElse(c,a,b) =>
        stream.println("val " + quote(sym) + " = if (" + quote(c) + ") {")
        emitBlock(a, intermediate.targets.get("Scala").get)
        stream.println(quote(getBlockResult(a)))
        stream.println("} else {")
        emitBlock(b, intermediate.targets.get("Scala").get)
        stream.println(quote(getBlockResult(b)))
        stream.println("}")
      case _ => return false
    }
    true
  }
}