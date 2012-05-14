package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{ScalaGenEffect, Base, EffectExp, ArrayOpsExp}
import java.io.PrintWriter
import ppl.delite.framework.datastructures._
import ppl.dsl.optiql.datastruct.scala.tpch._
import ppl.dsl.optiql.datastruct.scala.container.DataTable
import ppl.dsl.optiql.OptiQLExp

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

trait HackOpsExp extends HackOps with FieldAccessOpsExp with EffectExp with ArrayOpsExp { this: OptiQLExp =>

  case class HackOpsObjLoadCustomers(path: Rep[String]) extends Def[CustomerTable]
  case class HackOpsObjLoadLineItems(path: Rep[String]) extends Def[LineItemTable]
  case class HackOpsObjLoadOrders(path: Rep[String]) extends Def[OrderTable]
  case class HackOpsObjLoadParts(path: Rep[String]) extends Def[PartTable]
  case class HackOpsObjLoadNations(path: Rep[String]) extends Def[NationTable]
  case class HackOpsObjLoadPartSuppliers(path: Rep[String]) extends Def[PartSupplierTable]
  case class HackOpsObjLoadRegions(path: Rep[String]) extends Def[RegionTable]
  case class HackOpsObjLoadSuppliers(path: Rep[String]) extends Def[SupplierTable]

  case class InputColumn[T](x: Exp[Any], id: String) extends Def[Array[T]]
  case class InputSize[T](x: Exp[Any]) extends Def[Int]
  def get_column[T:Manifest](x: Exp[Any], id: String): Exp[Array[T]] = reflectEffect(InputColumn[T](x, id))
  def get_size[T:Manifest](x: Exp[Any]): Exp[Int] = InputSize(x)

  def soa_convert[T:Manifest](x: Exp[Any], tp: String, fields: Map[String,Manifest[_]]): Exp[T] = {
    struct[T](List("Array", tp), Map[String,Exp[Any]]() ++ (fields map (p => (p._1, get_column(x, p._1)(p._2)))))
  }

  def hackops_obj_loadlineitems(path: Rep[String]): Rep[LineItemTable] = {
    import ppl.dsl.optiql.datastruct.scala.liftables.LineItem
    import ppl.dsl.optiql.datastruct.scala.util.Date

    val labels = Map(
    "l_orderkey"-> manifest[Int],
    "l_partkey"->manifest[Int],
    "l_suppkey"->manifest[Int],
    "l_linenumber"->manifest[Int],
    "l_quantity"->manifest[Double],
    "l_extendedprice"->manifest[Double],
    "l_discount"->manifest[Double],
    "l_tax"->manifest[Double],
    "l_returnflag"->manifest[Char],
    "l_linestatus"->manifest[Char],
    "l_shipdate"->manifest[Date],
    "l_commitdate"->manifest[Date],
    "l_receiptdate"->manifest[Date],
    "l_shipinstruct"->manifest[String],
    "l_shipmode"->manifest[String],
    "l_comment"->manifest[String])

    val aos = reflectEffect(HackOpsObjLoadLineItems(path))
    val data = soa_convert[Array[LineItem]](aos, "LineItem", labels) // should be Array[LineItem]
    struct[LineItemTable](List("DataTable"), Map("size" -> get_size(aos), "data" -> data))
  }

  def hackops_obj_loadparts(path: Rep[String]): Rep[PartTable] = {
    import ppl.dsl.optiql.datastruct.scala.liftables.Part

    val labels = Map(
      "p_partkey"->manifest[Int],
      "p_name"->manifest[String],
      "p_mfgr"->manifest[String],
      "p_brand"->manifest[String],
      "p_type"->manifest[String],
      "p_size"->manifest[Int],
      "p_container"->manifest[String],
      "p_retailprice"->manifest[Double],
      "p_comment"->manifest[String])

    val aos = reflectEffect(HackOpsObjLoadParts(path))
    val data = soa_convert[Array[Part]](aos, "Part", labels) // should be Array[Part]
    struct[PartTable](List("DataTable"), Map("size" -> get_size(aos), "data" -> data))
  }

  def hackops_obj_loadpartsuppliers(path: Rep[String]): Rep[PartSupplierTable] = {
    import ppl.dsl.optiql.datastruct.scala.liftables.PartSupplier

    val labels = Map(
      "ps_partkey"->manifest[Int],
      "ps_suppkey"->manifest[Int],
      "ps_availqty"->manifest[Int],
      "ps_supplycost"->manifest[Double],
      "ps_comment"->manifest[String])

    val aos = reflectEffect(HackOpsObjLoadPartSuppliers(path))
    val data = soa_convert[Array[PartSupplier]](aos, "PartSupplier", labels) // should be Array[PartSupplier]
    struct[PartSupplierTable](List("DataTable"), Map("size" -> get_size(aos), "data" -> data))
  }
  
  def hackops_obj_loadsuppliers(path: Rep[String]): Rep[SupplierTable] = {
    import ppl.dsl.optiql.datastruct.scala.liftables.Supplier

    val labels = Map(
      "s_suppkey"->manifest[Int],
      "s_name"->manifest[String],
      "s_address"->manifest[String],
      "s_nationkey"->manifest[Int],
      "s_phone"->manifest[String],
      "s_acctbal"->manifest[Double],
      "s_comment"->manifest[String])

    val aos = reflectEffect(HackOpsObjLoadSuppliers(path))
    val data = soa_convert[Array[Supplier]](aos, "Supplier", labels) // should be Array[Supplier]
    struct[SupplierTable](List("DataTable"), Map("size" -> get_size(aos), "data" -> data))
  }

  def hackops_obj_loadnations(path: Rep[String]): Rep[NationTable] = {
    import ppl.dsl.optiql.datastruct.scala.liftables.Nation

    val labels = Map(
      "n_nationkey"->manifest[Int],
      "n_name"->manifest[String],
      "n_regionkey"->manifest[Int],
      "n_comment"->manifest[String])

    val aos = reflectEffect(HackOpsObjLoadNations(path))
    val data = soa_convert[Array[Nation]](aos, "Nation", labels) // should be Array[Nation]
    struct[NationTable](List("DataTable"), Map("size" -> get_size(aos), "data" -> data))
  }

  def hackops_obj_loadregions(path: Rep[String]): Rep[RegionTable] = {
    import ppl.dsl.optiql.datastruct.scala.liftables.Region

    val labels = Map(
      "r_regionkey"->manifest[Int],
      "r_name"->manifest[String],
      "r_comment"->manifest[String])

    val aos = reflectEffect(HackOpsObjLoadRegions(path))
    val data = soa_convert[Array[Region]](aos, "Region", labels) // should be Array[Region]
    struct[RegionTable](List("DataTable"), Map("size" -> get_size(aos), "data" -> data))
  }



  override def array_length[A:Manifest](a: Rep[Array[A]]): Rep[Int] = a match {
    case Def(InputColumn(x,_)) => get_size(x)
    case _ => super.array_length(a)
  }

/*
  override def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = body match {
    case ArrayElem(Block(Def(Struct(tag, elems)))) => 
      struct[A]("Array"::tag, elems.map(p=>(p._1,simpleLoop(size, v, ArrayElem(Block(p._2))))))
    case ArrayElem(Block(Def(ArrayIndex(b,v)))) if infix_length(b) == size => b.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
    case _ => super.simpleLoop(size, v, body)
  }
  
  override def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = a match {
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[T]]])) =>
      assert(pre == "Array")
      struct[T](tag, elems.map(p=>(p._1,infix_at(p._2, i))))
    case _ => super.infix_at(a,i)
  }
  
  override def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int] = a match {
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[T]]])) =>
      assert(pre == "Array")
      val ll = elems.map(p=>infix_length(p._2)) // all arrays must have same length!
      ll reduceLeft { (a1,a2) => assert(a1 == a2); a1 }
    case _ => super.infix_length(a)
  }

*/





  def hackops_obj_loadcustomers(path: Rep[String]): Rep[CustomerTable] = reflectEffect(HackOpsObjLoadCustomers(path))
  //def hackops_obj_loadlineitems(path: Rep[String]): Rep[LineItemTable] = reflectEffect(HackOpsObjLoadLineItems(path))
  //def hackops_obj_loadnations(path: Rep[String]): Rep[NationTable] = reflectEffect(HackOpsObjLoadNations(path))
  def hackops_obj_loadorders(path: Rep[String]): Rep[OrderTable] = reflectEffect(HackOpsObjLoadOrders(path))
  //def hackops_obj_loadparts(path: Rep[String]): Rep[PartTable] = reflectEffect(HackOpsObjLoadParts(path))
  //def hackops_obj_loadpartsuppliers(path: Rep[String]): Rep[PartSupplierTable] = reflectEffect(HackOpsObjLoadPartSuppliers(path))
  //def hackops_obj_loadregions(path: Rep[String]): Rep[RegionTable] = reflectEffect(HackOpsObjLoadRegions(path))
  //def hackops_obj_loadsuppliers(path: Rep[String]): Rep[SupplierTable] = reflectEffect(HackOpsObjLoadSuppliers(path))
  
  

}

trait ScalaGenHackOps extends ScalaGenEffect {
  val IR: HackOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case HackOpsObjLoadCustomers(path) => emitValDef(sym, "generated.scala.tpch.TPCHData.loadCustomers(" + quote(path) + ")")
    case HackOpsObjLoadLineItems(path) => emitValDef(sym, "generated.scala.tpch.TPCHData.loadLineItems(" + quote(path) + ")")
    case HackOpsObjLoadParts(path) => emitValDef(sym, "generated.scala.tpch.TPCHData.loadParts(" + quote(path) + ")")
    case HackOpsObjLoadPartSuppliers(path) => emitValDef(sym, "generated.scala.tpch.TPCHData.loadPartSuppliers(" + quote(path) + ")")
    case HackOpsObjLoadSuppliers(path) => emitValDef(sym, "generated.scala.tpch.TPCHData.loadSuppliers(" + quote(path) + ")")
    case HackOpsObjLoadNations(path) => emitValDef(sym, "generated.scala.tpch.TPCHData.loadNations(" + quote(path) + ")")
    case HackOpsObjLoadRegions(path) => emitValDef(sym, "generated.scala.tpch.TPCHData.loadRegions(" + quote(path) + ")")
    case InputColumn(x,id) => emitValDef(sym, quote(x) + ".map(_."+id+").toArray // TODO: store as columns during read")
    case InputSize(x) => emitValDef(sym, quote(x) + ".size")
    case _ => super.emitNode(sym, rhs)
  }
}