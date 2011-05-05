package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.DataTable
import scala.virtualization.lms.common.{ScalaGenEffect, EffectExp, Base}
import java.io.PrintWriter

trait DataTableOps extends Base {



  // Lifting and Interface Injection
  implicit def dataTableRepToDataTableRepOps[T:Manifest](d: Rep[DataTable[T]]) = new DataTableRepOps(d)

  object DataTable {
    def apply[T:Manifest](): Rep[DataTable[T]] = dataTableObjectApply()
  }

  class DataTableRepOps[T:Manifest](t:Rep[DataTable[T]]) {
    def apply(i: Rep[Int]): Rep[T] = dataTableApply(t, i)
  }


  def infix_size[T:Manifest](t:Rep[DataTable[T]]): Rep[Int] = dataTableSize(t)
  def infix_printAsTable[T:Manifest](t: Rep[DataTable[T]]): Rep[Unit] = dataTablePrintAsTable(t)




  //implementation method defintions
  def dataTableApply[T:Manifest](t: Rep[DataTable[T]], i: Rep[Int]): Rep[T]
  def dataTableObjectApply[T:Manifest](): Rep[DataTable[T]]
  def dataTableSize[T:Manifest](t: Rep[DataTable[T]]): Rep[Int]
  def dataTablePrintAsTable[T:Manifest](t: Rep[DataTable[T]]): Rep[Unit]



}

trait DataTableOpsExp extends DataTableOps with EffectExp {

  case class DataTableApply[T:Manifest](t: Rep[DataTable[T]], i: Rep[Int]) extends Def[T]
  case class DataTableObjectApply[T:Manifest](mnfst: Manifest[DataTable[T]]) extends Def[DataTable[T]]
  case class DataTableSize[T](t: Rep[DataTable[T]]) extends Def[Int]
  case class DataTablePrintAsTable[T](t: Rep[DataTable[T]]) extends Def[Unit]

  def dataTableApply[T:Manifest](t: Exp[DataTable[T]], i: Exp[Int]): Exp[T] = DataTableApply(t, i)
  def dataTableObjectApply[T:Manifest](): Exp[DataTable[T]] = reflectEffect(DataTableObjectApply[T](manifest[DataTable[T]]))
  def dataTableSize[T:Manifest](t: Exp[DataTable[T]]): Exp[Int] = DataTableSize(t)
  def dataTablePrintAsTable[T:Manifest](t: Exp[DataTable[T]]): Exp[Unit] = reflectEffect(DataTablePrintAsTable(t))



}

trait ScalaGenDataTableOps extends ScalaGenEffect {
  val IR: DataTableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DataTableApply(t, i) => emitValDef(sym, quote(t) + "(" + quote(i) + ")")
    case DataTableObjectApply(mnfst) => emitValDef(sym, "new " + remap(mnfst))
    case DataTableSize(t) => emitValDef(sym, quote(t) + " .size")
    case DataTablePrintAsTable(t) => emitValDef(sym, quote(t) + ".printAsTable()")
    case _ => super.emitNode(sym, rhs)
  }

}