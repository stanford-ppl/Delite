package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import scala.virtualization.lms.common.Record

object TPCHQ1 extends OptiQLApplicationRunner with TPCHQ1Trait
object TPCHQ2 extends OptiQLApplicationRunner with TPCHQ2Trait
object TPCHQ3 extends OptiQLApplicationRunner with TPCHQ3Trait
object TPCHQ4 extends OptiQLApplicationRunner with TPCHQ4Trait
object TPCHQ6 extends OptiQLApplicationRunner with TPCHQ6Trait
object TPCHQ14 extends OptiQLApplicationRunner with TPCHQ14Trait

trait TPCHBaseTrait extends OptiQLApplication with Types {

  def printUsage = {
    println("Usage: TPCHQ## <input directory>")
    exit(-1)
  }

  //timing decided at staging time so we can fuse across I/O when possible
  val timeIO: Boolean = System.getProperty("tpch.time.io", "true") != "false"
  override def tic(in: Rep[Any]*) = {
    if (timeIO) super.tic() //start timing immediately
    else super.tic(in:_*) //start timing after input loaded
  }

  val queryName: String
  
  var tpchDataPath: Rep[String] = _
  val sep = "\\|"
  def loadCustomers() = Table.fromFile[Customer](tpchDataPath+"/customer.tbl", sep)
  def loadLineItems() = Table.fromFile[LineItem](tpchDataPath+"/lineitem.tbl", sep)
  def loadOrders() = Table.fromFile[Order](tpchDataPath+"/orders.tbl", sep)
  def loadNations() = Table.fromFile[Nation](tpchDataPath+"/nation.tbl", sep)
  def loadRegions() = Table.fromFile[Region](tpchDataPath+"/region.tbl", sep)
  def loadParts() = Table.fromFile[Part](tpchDataPath+"/part.tbl", sep)
  def loadPartSuppliers() = Table.fromFile[PartSupplier](tpchDataPath+"/partsupp.tbl", sep)
  def loadSuppliers() = Table.fromFile[Supplier](tpchDataPath+"/supplier.tbl", sep)
  
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
      val sumDiscountedPrice = g.Sum(l => l.l_extendedprice * (infix_-(1.0, l.l_discount)))                // FIXME: ambiguous numeric ops problem and compiler crash in 2.10.0
      val sumCharge = g.Sum(l=> l.l_extendedprice * infix_-(1.0, l.l_discount) * infix_+(1.0, l.l_tax))   // FIXME: ambiguous numeric ops problem and compiler crash in 2.10.0
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

    /* //succinct version where all join result types are inferred, but we lose Scala's type checking
    val allSuppliers = regions.Where(_.r_name == "EUROPE")
      .Join(nations).WhereEq(_.r_regionkey, _.n_regionkey)
      .Join(suppliers).WhereEq(_.n_nationkey, _.s_nationkey)
      .Join(partSuppliers).WhereEq(_.s_suppkey, _.ps_suppkey)

    val res = allSuppliers.Join(parts).Where(p => (p.p_size == 15) && (p.p_type endsWith "BRASS"))
      .WhereEq(_.ps_partkey, _.p_partkey)
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

    val res = allSuppliers.Join(parts).Where(p => (p.p_size == 15) && (p.p_type endsWith "BRASS"))
      .WhereEq(_.ps_partkey, _.p_partkey).Select((s,p) => new Record {
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
      }) OrderByDescending(_.s_acctbal) ThenBy(_.n_name) ThenBy(_.s_name) ThenBy(_.p_partkey)
      
    toc(res)
    res.printAsTable(10)
  }    
}


trait TPCHQ3Trait extends TPCHBaseTrait {
  val queryName = "Q3"

  def query() = {
    val lineItems = loadLineItems(); val customers = loadCustomers(); val orders = loadOrders()
    tic(lineItems.size, customers.size, orders.size)

    val shippingOrders = customers.Where(_.c_mktsegment == "BUILDING")
      .Join(orders).Where(_.o_orderdate < Date("1995-03-15"))
      .WhereEq(_.c_custkey,_.o_custkey).Select((c,o) => new Record {
        val o_orderkey = o.o_orderkey
        val o_orderdate = o.o_orderdate
        val o_shippriority = o.o_shippriority
      })
      .Join(lineItems).Where(_.l_shipdate > Date("1995-03-15"))
      .WhereEq(_.o_orderkey, _.l_orderkey).Select((o,l) => new Record {
        val l_orderkey = l.l_orderkey
        val o_orderdate = o.o_orderdate
        val o_shippriority = o.o_shippriority
        val l_extendedprice = l.l_extendedprice
        val l_discount = l.l_discount
      })

    val q = shippingOrders GroupBy(e => (e.l_orderkey, e.o_orderdate, e.o_shippriority)) Select { g => new Record {
      val orderKey = g.key._1
      val orderDate = g.key._2
      val shipPriority = g.key._3
      val revenue = g.Sum(l => l.l_extendedprice * infix_-(1.0, l.l_discount)) //FIXME: ambiguous numeric ops problem and compiler crash in 2.10.0
    }} OrderByDescending(_.revenue) //ThenBy(_.orderDate) //FIXME: no Ordering defined on Date

    toc(q)
    q.printAsTable(10) //FIXME: Date.toString
  }
}


trait TPCHQ4Trait extends TPCHBaseTrait {
  val queryName = "Q4"

  def query() = {
    val lineItems = loadLineItems(); val orders = loadOrders()
    tic(lineItems.size, orders.size)

    /*val lateItems = lineItems Where(l => l.l_commitdate < l.l_receiptdate)
    val lateOrders = orders.Where(o => o.o_orderdate >= Date("1993-07-01") && o.o_orderdate < Date("1993-10-01"))
      .Where(o => lateItems.Count(l => l.l_orderkey == o.o_orderkey) > 0)*/

    val lateOrders = orders.Where(o => o.o_orderdate >= Date("1993-07-01") && o.o_orderdate < Date("1993-10-01"))
      .Join(lineItems).Where(l => l.l_commitdate < l.l_receiptdate)
      .WhereEq(_.o_orderkey, _.l_orderkey).Select((o,l) => new Record {
        val o_orderkey = o.o_orderkey
        val o_orderpriority = o.o_orderpriority
      }) Distinct(_.o_orderkey)
    
    val q = lateOrders GroupBy(_.o_orderpriority) Select { g => new Record {
      val orderPriority = g.key
      val orderCount = g.Count
    }} OrderBy(_.orderPriority)

    toc(q)
    q.printAsTable()
  }
}


trait TPCHQ6Trait extends TPCHBaseTrait {
  val queryName = "Q6"

  def query() = {
    val lineItems = loadLineItems()
    tic(lineItems.size)

    val q = lineItems Where (l => l.l_shipdate >= Date("1994-01-01") && l.l_shipdate < Date("1995-01-01") && l.l_discount >= 0.05 && l.l_discount <= 0.07 && l.l_quantity < 24) 
    val revenue = q.Sum(l => l.l_extendedprice * l.l_discount)

    toc(revenue)
    println(revenue)
  }
}


trait TPCHQ14Trait extends TPCHBaseTrait {
  val queryName = "Q14"

  def query() = {
    val parts = loadParts(); val lineItems = loadLineItems()
    tic(parts.size, lineItems.size)

    val q = parts.Join(lineItems)
      .Where(li => li.l_shipdate >= Date("1995-09-01") && li.l_shipdate < Date("1995-10-01")).WhereEq(_.p_partkey, _.l_partkey)
      .Select((p,l) => new Record { //this post-Join Select is very boilerplate but we need to get the type right
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
