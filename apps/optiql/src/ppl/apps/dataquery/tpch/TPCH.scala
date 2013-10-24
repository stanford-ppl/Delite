package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import scala.virtualization.lms.common.Record
import ppl.delite.framework.datastructures.DeliteArrayBuffer

object TPCHQ1 extends OptiQLApplicationRunner with TPCHQ1Trait
object TPCHQ2 extends OptiQLApplicationRunner with TPCHQ2Trait
object TPCHQ14 extends OptiQLApplicationRunner with TPCHQ14Trait

trait TPCHBaseTrait extends OptiQLApplication with Types {

  def printUsage = {
    println("Usage: TPCHQ## <input directory>")
    exit(-1)
  }

  val queryName: String
  
  var tpchDataPath: Rep[String] = _
  def loadCustomers() = TableInputReader(tpchDataPath+"/customer.tbl", Customer())
  def loadLineItems() = TableInputReader[LineItem](tpchDataPath+"/lineitem.tbl")
  def loadOrders() = TableInputReader(tpchDataPath+"/orders.tbl", Order())
  def loadNations() = TableInputReader(tpchDataPath+"/nation.tbl", Nation())
  def loadParts() = TableInputReader(tpchDataPath+"/part.tbl", Part())
  def loadPartSuppliers() = TableInputReader(tpchDataPath+"/partsupp.tbl", PartSupplier())
  def loadRegions() = TableInputReader(tpchDataPath+"/region.tbl", Region())
  def loadSuppliers() = TableInputReader(tpchDataPath+"/supplier.tbl", Supplier())
  
  def query(): Rep[_]
  
  def main() = {
    println("TPC-H " + queryName)
    if (args.length < 1) printUsage
    
    tpchDataPath = args(0)
    query()
  }

}


trait TPCHQ1Trait extends TPCHBaseTrait {

  val queryName = "Q1"  
  def query() = {  

    val lineItems = loadLineItems()         
    tic(lineItems.size)
    
    val q = lineItems Where(_.l_shipdate <= Date("1998-12-01")) GroupBy(l => (l.l_returnflag,l.l_linestatus)) Select(g => new Result {
      val returnFlag = g.key._1
      val lineStatus = g.key._2
      val sumQty = g.Sum(_.l_quantity)
      val sumBasePrice = g.Sum(_.l_extendedprice)
      val sumDiscountedPrice = g.Sum(l => l.l_extendedprice * (infix_-(1.0d, l.l_discount)))                // FIXME: ambiguous numeric ops problem and compiler crash in 2.10.0
      val sumCharge = g.Sum(l=> l.l_extendedprice * infix_-(1.0d, l.l_discount) * infix_+(1.0d, l.l_tax))   // FIXME: ambiguous numeric ops problem and compiler crash in 2.10.0
      val avgQty = g.Average(_.l_quantity)
      val avgPrice = g.Average(_.l_extendedprice)
      val avgDiscount = g.Average(_.l_discount)
      val countOrder = g.Count
    }) OrderBy(_.returnFlag) ThenBy(_.lineStatus)
    
    toc(q)
    q.printAsTable()
  }    
}


trait TPCHQ2Trait extends TPCHBaseTrait {
  val queryName = "Q2"  
  
  def query() = {
    val parts = loadParts(); val partSuppliers = loadPartSuppliers(); val suppliers = loadSuppliers(); val nations = loadNations(); val regions = loadRegions()
    tic(parts.size, partSuppliers.size, suppliers.size, nations.size, regions.size)

    /* //succinct version where all join result types are inferred
    val allSuppliers = regions.Where(_.r_name == "EUROPE")
      .Join(nations).WhereEq(_.r_regionkey, _.n_regionkey)
      .Join(suppliers).WhereEq(_.n_nationkey, _.s_nationkey)
      .Join(partSuppliers).WhereEq(_.s_suppkey, _.ps_suppkey)

    val res = allSuppliers.Join(parts)
      .Where(p => (p.p_size == 15) && (p.p_type endsWith "BRASS")).WhereEq(_.ps_partkey, _.p_partkey)
      .Where(s => s.ps_supplycost == allSuppliers.Where(_.ps_partkey == s.p_partkey).Min(_.ps_supplycost))
      .Select(s => new Record {
        val s_acctbal = s.s_acctbal
        val s_name = s.s_name
        val n_name = s.n_name
        val p_partkey = s.p_partkey
        val p_mfgr = s.p_mfgr
        val s_address = s.s_address
        val s_phone = s.s_phone
        val s_comment = s.s_comment
      }) */

    val allSuppliers = regions.Where(_.r_name == "EUROPE")
      .Join(nations).WhereEq(_.r_regionkey, _.n_regionkey).Select((r,n) => new Record {
        val n_nationkey = n.n_nationkey
        val n_name = n.n_name
      })
      .Join(suppliers).WhereEq(_.n_nationkey, _.s_nationkey).Select((n,s) => new Record {
        val s_suppkey = s.s_suppkey
        val s_acctbal = s.s_acctbal
        val s_name = s.s_name
        val n_name = n.n_name
        val s_address = s.s_address
        val s_phone = s.s_phone
        val s_comment = s.s_comment
      })
      .Join(partSuppliers).WhereEq(_.s_suppkey, _.ps_suppkey).Select((s,ps) => new Record {
        val s_acctbal = s.s_acctbal
        val s_name = s.s_name
        val n_name = s.n_name
        val s_address = s.s_address
        val s_phone = s.s_phone
        val s_comment = s.s_comment
        val ps_partkey = ps.ps_partkey
        val ps_supplycost = ps.ps_supplycost
      })    

    val res = allSuppliers.Join(parts)
      .Where(p => (p.p_size == 15) && (p.p_type endsWith "BRASS")).WhereEq(_.ps_partkey, _.p_partkey)
      .Select((s,p) => new Record {
        val s_acctbal = s.s_acctbal
        val s_name = s.s_name
        val n_name = s.n_name
        val p_partkey = p.p_partkey
        val p_mfgr = p.p_mfgr
        val s_address = s.s_address
        val s_phone = s.s_phone
        val s_comment = s.s_comment
        val ps_supplycost = s.ps_supplycost
      })
      .Where(s => s.ps_supplycost == allSuppliers.Where(_.ps_partkey == s.p_partkey).Min(_.ps_supplycost)).Select(s => new Record {
        val s_acctbal = s.s_acctbal
        val s_name = s.s_name
        val n_name = s.n_name
        val p_partkey = s.p_partkey
        val p_mfgr = s.p_mfgr
        val s_address = s.s_address
        val s_phone = s.s_phone
        val s_comment = s.s_comment
      })
    
    toc(res)
    res.printAsTable(10)
  }    
}

trait TPCHQ14Trait extends TPCHBaseTrait {
  val queryName = "Q14"

  def query() = {
    val parts = loadParts(); val lineItems = loadLineItems()
    tic(parts.size, lineItems.size)

    val q = /*lineItems.Join(parts)*/ parts.Join(lineItems)
      .Where(li => li.l_shipdate >= Date("1995-09-01") && li.l_shipdate < Date("1995-10-01")).WhereEq(_.p_partkey, _.l_partkey)
      .Select((p,l) => new Record { //TODO: this post-Join Select is very boilerplate but we need to get the type right
        val l_extendedprice = l.l_extendedprice
        val l_discount = l.l_discount
        val p_type = p.p_type
      })

    val promoRevenue = q.Sum(l => if (l.p_type startsWith "PROMO") l.l_extendedprice * (1.0 - l.l_discount) else 0.0)
    val totalRevenue = q.Sum(l => l.l_extendedprice * (1.0 - l.l_discount))
    val promoPercentage = 100 * promoRevenue / totalRevenue
    toc(promoPercentage)
    println(promoPercentage)
  }
}
