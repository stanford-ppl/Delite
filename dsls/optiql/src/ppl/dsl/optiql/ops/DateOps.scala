package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.util.Date
import java.io.PrintWriter
import scala.virtualization.lms.common.{ScalaGenBase, ScalaGenEffect, BaseExp, Base}
import scala.reflect.SourceContext

trait DateOps extends Base {

  //inject interface
  implicit def dateRepToDateRepOps(d: Rep[Date]) = new DateRepOps(d)

  object Date {
    def apply(str: Rep[String]): Rep[Date] = dateObjectApply(str)
  }

  class DateRepOps(d: Rep[Date]) {
    def <=(rd: Rep[Date]): Rep[Boolean] = dateLessThan(d,rd)
  }

  def dateObjectApply(str: Rep[String]): Rep[Date]
  def dateLessThan(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]

}

trait DateOpsExp extends DateOps with BaseExp {

  //IR nodes
  case class DateObjectApply[T:Manifest](str: Rep[String]) extends Def[Date]
  case class DateLessThan[T:Manifest](ld: Rep[Date], rd: Rep[Date]) extends Def[Boolean]

  //Interface implementation
  def dateObjectApply(str: Rep[String]) = DateObjectApply(str)
  def dateLessThan(ld: Rep[Date], rd: Rep[Date]) = DateLessThan(ld, rd)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case DateLessThan(l,r) => dateLessThan(f(l), f(r));
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenDateOps extends ScalaGenBase {
  val IR: DateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DateObjectApply(str: Exp[String]) => emitValDef(sym, "generated.scala.util.Date(" + quote(str) + ")" )
    case DateLessThan(ls: Exp[Date], rd: Exp[Date]) => emitValDef(sym, quote(ls) + " <= " + quote(rd))
    case _ => super.emitNode(sym, rhs)
  }

}
