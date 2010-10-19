package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}
import scala.virtualization.lms.util.OverloadHack

trait StringOps extends DSLType with OverloadHack { this: DeliteApplication with Variables =>
  // NOTE: if something doesn't get lifted, this won't give you a compile time error,
  //       since string concat is defined on all objects
  def infix_+(s1: String, s2: Rep[Any]) = string_plus(s1,s2)
  def infix_+(s1: Rep[Any], s2: String)(implicit o: Overloaded1) = string_plus(s1,s2)
  def infix_+(s1: String, s2: Var[Any])(implicit o: Overloaded4) = string_plus(s1,s2)
  def infix_+(s1: Var[Any], s2: String)(implicit o: Overloaded5) = string_plus(s1,s2)

  def infix_trim(s: Rep[String]) = string_trim(s)
  def infix_split(s: Rep[String], separators: Rep[String]) = string_split(s, separators)

  def string_plus(s: Rep[Any], o: Rep[Any]): Rep[String]
  def string_trim(s: Rep[String]) : Rep[String]
  def string_split(s: Rep[String], separators: Rep[String]) : Rep[Array[String]]
}

trait StringOpsExp extends StringOps { this: DeliteApplication with VariablesExp =>
  case class StringPlus(s: Exp[Any], o: Exp[Any]) extends Def[String]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String]) extends Def[Array[String]]

  def string_plus(s: Exp[Any], o: Exp[Any]): Rep[String] = StringPlus(s,o)
  def string_trim(s: Exp[String]) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String]) : Rep[Array[String]] = StringSplit(s, separators)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaString { val intermediate: StringOpsExp.this.type = StringOpsExp.this })
}

trait CodeGeneratorScalaString extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with StringOpsExp
  import intermediate._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep) => emitValDef(sym, "%s.split(%s)".format(quote(s), quote(sep)))
    case _ => super.emitNode(sym, rhs)
  }
}