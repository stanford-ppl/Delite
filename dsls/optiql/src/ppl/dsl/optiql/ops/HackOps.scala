package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{ScalaGenEffect, Base, EffectExp}
import java.io.PrintWriter
import ppl.dsl.optiql.datastruct.scala.{CustomerTable}

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
  def hackops_obj_loadlineitems(path: Rep[String]): Rep[Unit]
  def hackops_obj_loadnations(path: Rep[String]): Rep[Unit]
  def hackops_obj_loadorders(path: Rep[String]): Rep[Unit]
  def hackops_obj_loadparts(path: Rep[String]): Rep[Unit]
  def hackops_obj_loadpartsuppliers(path: Rep[String]): Rep[Unit]
  def hackops_obj_loadregions(path: Rep[String]): Rep[Unit]
  def hackops_obj_loadsuppliers(path: Rep[String]): Rep[Unit]
}

trait HackOpsExp extends HackOps with EffectExp {

  case class HackOpsObjLoadCustomers(path: Rep[String]) extends Def[CustomerTable]
  case class HackOpsObjLoadLineItems(path: Rep[String]) extends Def[Unit]
  case class HackOpsObjLoadOrders(path: Rep[String]) extends Def[Unit]
  case class HackOpsObjLoadParts(path: Rep[String]) extends Def[Unit]
  case class HackOpsObjLoadNations(path: Rep[String]) extends Def[Unit]
  case class HackOpsObjLoadPartSuppliers(path: Rep[String]) extends Def[Unit]
  case class HackOpsObjLoadRegions(path: Rep[String]) extends Def[Unit]
  case class HackOpsObjLoadSuppliers(path: Rep[String]) extends Def[Unit]


  def hackops_obj_loadcustomers(path: Rep[String]): Rep[CustomerTable] = reflectEffect(HackOpsObjLoadCustomers(path))
  def hackops_obj_loadlineitems(path: Rep[String]): Rep[Unit] = reflectEffect(HackOpsObjLoadLineItems(path))
  def hackops_obj_loadnations(path: Rep[String]): Rep[Unit] = reflectEffect(HackOpsObjLoadNations(path))
  def hackops_obj_loadorders(path: Rep[String]): Rep[Unit] = reflectEffect(HackOpsObjLoadOrders(path))
  def hackops_obj_loadparts(path: Rep[String]): Rep[Unit] = reflectEffect(HackOpsObjLoadParts(path))
  def hackops_obj_loadpartsuppliers(path: Rep[String]): Rep[Unit] = reflectEffect(HackOpsObjLoadPartSuppliers(path))
  def hackops_obj_loadregions(path: Rep[String]): Rep[Unit] = reflectEffect(HackOpsObjLoadRegions(path))
  def hackops_obj_loadsuppliers(path: Rep[String]): Rep[Unit] = reflectEffect(HackOpsObjLoadSuppliers(path))

}

trait ScalaGenHackOps extends ScalaGenEffect {
  val IR: HackOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case HackOpsObjLoadCustomers(path) => emitValDef(sym, "generated.scala.TPCHData.loadCustomers(" + quote(path) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}