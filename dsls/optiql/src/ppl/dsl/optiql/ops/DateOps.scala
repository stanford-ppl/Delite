package ppl.dsl.optiql.ops

import java.io.PrintWriter
import scala.virtualization.lms.common.{ScalaGenBase, ScalaGenEffect, BaseExp, Base}
import reflect.SourceContext
import ppl.dsl.optiql.{OptiQLCompiler, OptiQLLift, OptiQLExp, OptiQL}


trait DateOps extends Base { this: OptiQL =>

  //inject interface
  implicit def dateRepToDateRepOps(d: Rep[Date]) = new DateRepOps(d)

  object Date {
    def apply(str: Rep[String]): Rep[Date] = dateObjectApply(str)
  }

  class DateRepOps(d: Rep[Date]) {
    def <=(rd: Rep[Date]): Rep[Boolean] = dateLessThanEqual(d,rd)
  }

  def dateObjectApply(str: Rep[String]): Rep[Date]
  def dateLessThanEqual(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]

}

trait DateOpsExp extends DateOps with BaseExp { this: OptiQLExp =>

  case class DateObjectApply(str: Rep[String], dateBlock: Block[Date]) extends DeliteOpSingleTask[Date](dateBlock)
  case class DateLessThanEqual(ld: Rep[Date], rd: Rep[Date], lteBlock: Block[Boolean]) extends DeliteOpSingleTask[Boolean](lteBlock)

  def dateObjectApply(str: Rep[String]) = DateObjectApply(str, reifyEffects(optiql_date_from_string(str)))
  
  def dateLessThanEqual(ld: Rep[Date], rd: Rep[Date]) = DateLessThanEqual(ld, rd, reifyEffects(optiql_date_lte(ld,rd)))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case DateObjectApply(str,block) => dateObjectApply(f(str))
    case DateLessThanEqual(ld,rd,block) => dateLessThanEqual(f(ld),f(rd))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait DateImplOps { this: OptiQL =>
  def optiql_date_from_string(str: Rep[String]): Rep[Date]
  def optiql_date_lte(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def optiql_date_lt(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def optiql_date_gte(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def optiql_date_gt(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def optiql_date_eq(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def optiql_date_ne(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
}

trait DateImplOpsStandard extends DateImplOps { this: OptiQLExp with OptiQLLift =>

  private def makeDate(year: Rep[Int], month: Rep[Int], day: Rep[Int]) = (year << 9) + (month << 5) + day
  private def year(date: Rep[Int]) = date >>> 9
  private def month(date: Rep[Int]) = (date >>> 5) & 0xf
  private def day(date: Rep[Int]) = date & 0x1f

  private def infix_toDate(d: Rep[Int]): Rep[Date] = d.asInstanceOf[Rep[Date]]
  private def infix_toInt(d: Rep[Date]): Rep[Int] = d.asInstanceOf[Rep[Int]]

  def optiql_date_from_string(str: Rep[String]): Rep[Date] = {
    val tokens = str.split("-")
    val year = Integer.parseInt(tokens(0)) //0 - 9999
    val month = Integer.parseInt(tokens(1)) //1 - 12 (4 bits)
    val day = Integer.parseInt(tokens(2)) //1 - 31 (5 bits)
    makeDate(year, month, day).toDate
  }

  //encoding is order preserving
  def optiql_date_lte(ld: Rep[Date], rd: Rep[Date]) = ld.toInt <= rd.toInt
  def optiql_date_lt(ld: Rep[Date], rd: Rep[Date]) = ld.toInt < rd.toInt
  def optiql_date_gte(ld: Rep[Date], rd: Rep[Date]) = ld.toInt >= rd.toInt
  def optiql_date_gt(ld: Rep[Date], rd: Rep[Date]) = ld.toInt > rd.toInt
  def optiql_date_eq(ld: Rep[Date], rd: Rep[Date]) = ld.toInt == rd.toInt
  def optiql_date_ne(ld: Rep[Date], rd: Rep[Date]) = ld.toInt != rd.toInt

}

trait ScalaGenDateOps extends ScalaGenBase {
  val IR: DateOpsExp with OptiQLExp
}
