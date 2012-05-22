package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.DataTable
import scala.virtualization.lms.common.{ScalaGenFat, BaseFatExp, Base}
import scala.reflect.SourceContext
import ppl.dsl.optiql.OptiQLExp
import java.io.PrintWriter

trait DataTableOps extends Base {



  // Lifting and Interface Injection
  implicit def dataTableRepToDataTableRepOps[T:Manifest](d: Rep[DataTable[T]]) = new DataTableRepOps(d)

  object DataTable {
    def apply[T:Manifest](): Rep[DataTable[T]] = dataTableObjectApply()
	def apply[T:Manifest](initSize: Rep[Int]): Rep[DataTable[T]] = dataTableObjectApply(initSize)
  }

  class DataTableRepOps[T:Manifest](t:Rep[DataTable[T]]) {
    def apply(i: Rep[Int]): Rep[T] = dataTableApply(t, i)
  }


  def infix_size[T:Manifest](t:Rep[DataTable[T]]): Rep[Int] = dataTableSize(t)
  def infix_printAsTable[T:Manifest](t: Rep[DataTable[T]]): Rep[Unit] = dataTablePrintAsTable(t)




  //implementation method defintions
  def dataTableApply[T:Manifest](t: Rep[DataTable[T]], i: Rep[Int]): Rep[T]
  def dataTableObjectApply[T:Manifest](): Rep[DataTable[T]]
  def dataTableObjectApply[T:Manifest](initSize: Rep[Int]): Rep[DataTable[T]]
  def dataTableSize[T:Manifest](t: Rep[DataTable[T]]): Rep[Int]
  def dataTablePrintAsTable[T:Manifest](t: Rep[DataTable[T]]): Rep[Unit]



}

trait DataTableOpsExp extends DataTableOps with BaseFatExp { this: DataTableOpsExp with OptiQLExp =>

  case class DataTableApply[T:Manifest](t: Rep[DataTable[T]], i: Rep[Int]) extends Def[T]
  case class DataTableObjectApply[T:Manifest](mnfst: Manifest[DataTable[T]], initSize: Rep[Int]) extends Def[DataTable[T]]
  case class DataTableSize[T](t: Rep[DataTable[T]]) extends Def[Int]
  case class DataTablePrintAsTable[T](t: Rep[DataTable[T]]) extends Def[Unit]

  def dataTableApply[T:Manifest](t: Exp[DataTable[T]], i: Exp[Int]): Exp[T] = DataTableApply(t, i)
  def dataTableObjectApply[T:Manifest](): Exp[DataTable[T]] = reflectEffect(DataTableObjectApply[T](manifest[DataTable[T]], unit(0)))
  def dataTableObjectApply[T:Manifest](initSize: Exp[Int]): Exp[DataTable[T]] = reflectEffect(DataTableObjectApply[T](manifest[DataTable[T]], initSize))
  def dataTableSize[T:Manifest](t: Exp[DataTable[T]]): Exp[Int] = DataTableSize(t)
  def dataTablePrintAsTable[T:Manifest](t: Exp[DataTable[T]]): Exp[Unit] = reflectEffect(DataTablePrintAsTable(t))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = e match {
    case DataTableApply(t,i) => dataTableApply(f(t), f(i))
    case _ => super.mirror(e,f)
  }

}

trait ScalaGenDataTableOps extends ScalaGenFat {
  val IR: DataTableOpsExp with OptiQLExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DataTableApply(t, i) => emitValDef(sym, quote(t) + "(" + quote(i) + ")")
    case DataTableObjectApply(mnfst, initSize) => emitValDef(sym, "new " + remap(mnfst) + "(" + quote(initSize) + ")")
    case DataTableSize(t) => emitValDef(sym, quote(t) + ".size")
    case DataTablePrintAsTable(t) => emitValDef(sym, quote(t) + ".printAsTable()")
    case _ => super.emitNode(sym, rhs)
  }

}
