package ppl.dsl.optiql.ops

import java.io.PrintWriter
import scala.virtualization.lms.common.{ScalaGenBase, ScalaGenEffect, BaseExp, Base}
import reflect.SourceContext
import ppl.dsl.optiql.{OptiQLCompiler, OptiQLLift, OptiQLExp, OptiQL}
import ppl.delite.framework.datastructures.DeliteStructsExp


trait DateOps extends Base { this: OptiQL =>

  //inject interface
  implicit def dateRepToDateRepOps(d: Rep[Date]) = new DateRepOps(d)

  object Date {
    def apply(str: Rep[String]): Rep[Date] = dateObjectApply(str)
  }

  class DateRepOps(d: Rep[Date]) {
    def <(rd: Rep[Date]): Rep[Boolean] = dateLessThan(d,rd)
    def <=(rd: Rep[Date]): Rep[Boolean] = dateLessThanEqual(d,rd)
    def >(rd: Rep[Date]): Rep[Boolean] = dateGreaterThan(d,rd)
    def >=(rd: Rep[Date]): Rep[Boolean] = dateGreaterThanEqual(d,rd)
    def !=(rd: Rep[Date]): Rep[Boolean] = dateNotEqual(d,rd)
  }

  def __equal(ld: Rep[Date], rd: Rep[Date]) = dateEqual(ld,rd)

  def dateObjectApply(str: Rep[String]): Rep[Date]
  def dateLessThan(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def dateLessThanEqual(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def dateGreaterThan(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def dateGreaterThanEqual(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def dateNotEqual(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]
  def dateEqual(ld: Rep[Date], rd: Rep[Date]): Rep[Boolean]

}

trait DateOpsExp extends DateOps with BaseExp { this: OptiQLExp =>
  
  //single tasks on pure operations don't really do anything (due to code motion) and just hinder struct optimizations
  /*case class DateObjectApply(str: Rep[String]) extends DeliteOpSingleTask[Date](reifyEffectsHere(optiql_date_from_string(str)))  
  case class DateLessThan(ld: Rep[Date], rd: Rep[Date]) extends DeliteOpSingleTask[Boolean](reifyEffectsHere(optiql_date_lt(ld,rd)))
  case class DateLessThanEqual(ld: Rep[Date], rd: Rep[Date]) extends DeliteOpSingleTask[Boolean](reifyEffectsHere(optiql_date_lte(ld,rd)))
  case class DateGreaterThan(ld: Rep[Date], rd: Rep[Date]) extends DeliteOpSingleTask[Boolean](reifyEffectsHere(optiql_date_gt(ld,rd)))
  case class DateGreaterThanEqual(ld: Rep[Date], rd: Rep[Date]) extends DeliteOpSingleTask[Boolean](reifyEffectsHere(optiql_date_gte(ld,rd)))
  case class DateEqual(ld: Rep[Date], rd: Rep[Date]) extends DeliteOpSingleTask[Boolean](reifyEffectsHere(optiql_date_eq(ld,rd)))
  case class DateNotEqual(ld: Rep[Date], rd: Rep[Date]) extends DeliteOpSingleTask[Boolean](reifyEffectsHere(optiql_date_ne(ld,rd)))*/


  def dateObjectApply(str: Rep[String]) = optiql_date_from_string(str)
  def dateLessThan(ld: Rep[Date], rd: Rep[Date]) = optiql_date_lt(ld,rd)
  def dateLessThanEqual(ld: Rep[Date], rd: Rep[Date]) = optiql_date_lte(ld,rd)
  def dateGreaterThan(ld: Rep[Date], rd: Rep[Date]) = optiql_date_gt(ld,rd)
  def dateGreaterThanEqual(ld: Rep[Date], rd: Rep[Date]) = optiql_date_gte(ld,rd)
  def dateEqual(ld: Rep[Date], rd: Rep[Date]) = optiql_date_eq(ld,rd)
  def dateNotEqual(ld: Rep[Date], rd: Rep[Date]) = optiql_date_ne(ld,rd)


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    /*case DateObjectApply(str) => dateObjectApply(f(str))
    case DateLessThan(ld,rd) => dateLessThan(f(ld),f(rd))
    case DateLessThanEqual(ld,rd) => dateLessThanEqual(f(ld),f(rd))
    case DateGreaterThan(ld,rd) => dateGreaterThan(f(ld),f(rd))
    case DateGreaterThanEqual(ld,rd) => dateGreaterThanEqual(f(ld),f(rd))
    case DateEqual(ld,rd) => dateEqual(f(ld),f(rd))
    case DateNotEqual(ld,rd) => dateNotEqual(f(ld),f(rd))*/
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

trait DateImplOpsStandard extends DateImplOps with DeliteStructsExp { this: OptiQLExp with OptiQLLift =>

  private def makeDate(year: Rep[Int], month: Rep[Int], day: Rep[Int]) = (year << 9) + (month << 5) + day
  private def year(date: Rep[Int]) = date >>> 9
  private def month(date: Rep[Int]) = (date >>> 5) & 0xf
  private def day(date: Rep[Int]) = date & 0x1f

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

  //trick to eliminate structs of a single field without relying on struct unwrapping optimizations
  private def infix_toDate(d: Rep[Int]): Rep[Date] = d.asInstanceOf[Rep[Date]] //struct(classTag[Date], "value" -> d)
  private def infix_toInt(d: Rep[Date]): Rep[Int] = d.asInstanceOf[Rep[Int]] //field[Int](d, "value")

  /*override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if t.erasure == classOf[Date] => Some(classTag(t), List("value" -> manifest[Int]))
    case _ => super.unapplyStructType
  }*/

}
