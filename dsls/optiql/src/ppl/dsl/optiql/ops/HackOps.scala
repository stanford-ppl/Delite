package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{ScalaGenEffect, Base, EffectExp}
import java.io.PrintWriter
import ppl.delite.framework.datastructures._
import ppl.dsl.optiql.datastruct.scala.tpch._
import ppl.dsl.optiql.datastruct.scala.container.DataTable

trait HackOps extends Base {

  object TPCH {
    def loadCustomers(path: Rep[String]) = hackops_obj_loadcustomers(path)
    def loadLineItems(path: Rep[String]) = hackops_obj_loadlineitems(path)
    def loadNations(path: Rep[String]) = hackops_obj_loadnations(path)
    def loadOrders(path: Rep[String]) = hackops_obj_loadorders(path)
    def loadParts(path: Rep[String]) = hackops_obj_loadparts(path)
    def loadPartSuppliers(path: Rep[String]) = hackops_obj_loadpartsuppliers(path)
    def loadRegions(path: Rep[String]) = hackops_obj_loadregions(path)
    def loadSuppliers(path: Rep[String]) = hackops_obj_loadsuppliers(path)
  }

  def hackops_obj_loadcustomers(path: Rep[String]): Rep[CustomerTable]
  def hackops_obj_loadlineitems(path: Rep[String]): Rep[LineItemTable]
  def hackops_obj_loadnations(path: Rep[String]): Rep[NationTable]
  def hackops_obj_loadorders(path: Rep[String]): Rep[OrderTable]
  def hackops_obj_loadparts(path: Rep[String]): Rep[PartTable]
  def hackops_obj_loadpartsuppliers(path: Rep[String]): Rep[PartSupplierTable]
  def hackops_obj_loadregions(path: Rep[String]): Rep[RegionTable]
  def hackops_obj_loadsuppliers(path: Rep[String]): Rep[SupplierTable]
  
  
}

trait HackOpsExp extends HackOps with FieldAccessOpsExp with EffectExp {

  case class HackOpsObjLoadCustomers(path: Rep[String]) extends Def[CustomerTable]
  case class HackOpsObjLoadLineItems(path: Rep[String]) extends Def[LineItemTable]
  case class HackOpsObjLoadOrders(path: Rep[String]) extends Def[OrderTable]
  case class HackOpsObjLoadParts(path: Rep[String]) extends Def[PartTable]
  case class HackOpsObjLoadNations(path: Rep[String]) extends Def[NationTable]
  case class HackOpsObjLoadPartSuppliers(path: Rep[String]) extends Def[PartSupplierTable]
  case class HackOpsObjLoadRegions(path: Rep[String]) extends Def[RegionTable]
  case class HackOpsObjLoadSuppliers(path: Rep[String]) extends Def[SupplierTable]


  def hackops_obj_loadcustomers(path: Rep[String]): Rep[CustomerTable] = reflectEffect(HackOpsObjLoadCustomers(path))
  def hackops_obj_loadlineitems(path: Rep[String]): Rep[LineItemTable] = reflectEffect(HackOpsObjLoadLineItems(path))
  def hackops_obj_loadnations(path: Rep[String]): Rep[NationTable] = reflectEffect(HackOpsObjLoadNations(path))
  def hackops_obj_loadorders(path: Rep[String]): Rep[OrderTable] = reflectEffect(HackOpsObjLoadOrders(path))
  def hackops_obj_loadparts(path: Rep[String]): Rep[PartTable] = reflectEffect(HackOpsObjLoadParts(path))
  def hackops_obj_loadpartsuppliers(path: Rep[String]): Rep[PartSupplierTable] = reflectEffect(HackOpsObjLoadPartSuppliers(path))
  def hackops_obj_loadregions(path: Rep[String]): Rep[RegionTable] = reflectEffect(HackOpsObjLoadRegions(path))
  def hackops_obj_loadsuppliers(path: Rep[String]): Rep[SupplierTable] = reflectEffect(HackOpsObjLoadSuppliers(path))
  
  

}

trait ScalaGenHackOps extends ScalaGenEffect {
  val IR: HackOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case HackOpsObjLoadCustomers(path) => emitValDef(sym, "generated.scala.tpch.TPCHData.loadCustomers(" + quote(path) + ")")
    case HackOpsObjLoadLineItems(path) => emitValDef(sym, "generated.scala.tpch.TPCHData.loadLineItems(" + quote(path) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}