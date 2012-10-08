package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}

object TPCHQ0 extends OptiQLApplicationRunner with TPCHQ0Trait
object TPCHQ1 extends OptiQLApplicationRunner with TPCHQ1Trait
object TPCHQ2 extends OptiQLApplicationRunner with TPCHQ2Trait

trait TPCHBaseTrait extends OptiQLApplication with Types {

  def printUsage = {
    println("Usage: TPCH <input tpch directory>")
    exit(-1)
  }

  val queryName: String
  
  var tpchDataPath: Rep[String] = _
  def loadCustomers() = TableInputReader(tpchDataPath+"/customer.tbl", Customer())
  def loadLineItems() = TableInputReader(tpchDataPath+"/lineitem.tbl", LineItem())
  def loadOrders() = TableInputReader(tpchDataPath+"/orders.tbl", Order())
  def loadNations() = TableInputReader(tpchDataPath+"/nation.tbl", Nation())
  def loadParts() = TableInputReader(tpchDataPath+"/part.tbl", Part())
  def loadPartSuppliers() = TableInputReader(tpchDataPath+"/partsupp.tbl", PartSupplier())
  def loadRegions() = TableInputReader(tpchDataPath+"/region.tbl", Region())
  def loadSuppliers() = TableInputReader(tpchDataPath+"/supplier.tbl", Supplier())
  
  def query(): Rep[_]
  
  def main() = {
    println("TPCH style benchmarking " + queryName)
    if (args.length < 1) printUsage
    
    tpchDataPath = args(0)
    query()
  }

}

trait TPCHQ0Trait extends TPCHBaseTrait {
  val queryName = "Q0"
  def query() = {
    val lineItems = loadLineItems()

    val q = lineItems /*Where(_.l_shipdate <= Date("1998-12-01"))*/ Select(l => new Result {
      val divided = l.l_quantity / l.l_extendedprice
      val other = l.l_tax / l.l_discount 
    })
    q.printAsTable

    val u = lineItems Sum(_.l_quantity)
    println(u)

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
      val sumDiscountedPrice = g.Sum(l => l.l_extendedprice * (1.0d - l.l_discount))
      val sumCharge = g.Sum(l=> l.l_extendedprice * (1.0d - l.l_discount) * (1.0d + l.l_tax))
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
    tic(parts.size)
    val q = parts.Where(p => {val res:Rep[Boolean] = p.p_size == 15; res}).Where(_.p_type.endsWith("BRASS")).Join(partSuppliers).WhereEq(_.p_partkey, _.ps_partkey).Select((p, ps) => new  Result {
      val p_partkey = p.p_partkey
      val p_mfgr = p.p_mfgr
      val ps_suppkey = ps.ps_suppkey
      val ps_supplycost = ps.ps_supplycost
    }).Join(suppliers).WhereEq(_.ps_suppkey,_.s_suppkey).Select((j, s) => new Result {
      val s_acctbal = s.s_acctbal
      val s_name = s.s_name
      val p_partkey = j.p_partkey
      val p_mfgr = j.p_mfgr
      val s_address = s.s_address
      val s_phone = s.s_phone
      val s_comment = s.s_comment
      val ps_supplycost = j.ps_supplycost
      val s_nationkey = s.s_nationkey
    }).Join(nations).WhereEq(_.s_nationkey, _.n_nationkey).Select((j, n) => new Result {
      val s_acctbal = j.s_acctbal
      val s_name = j.s_name
      val n_name = n.n_name
      val p_partkey = j.p_partkey
      val p_mfgr = j.p_mfgr
      val s_address = j.s_address
      val s_phone = j.s_phone
      val s_comment = j.s_comment
      val ps_supplycost = j.ps_supplycost
      val n_regionkey = n.n_regionkey
    }).Join(regions).WhereEq(_.n_regionkey, _.r_regionkey).Select((j, r) => new Result {
      val s_acctbal = j.s_acctbal
      val s_name = j.s_name
      val n_name = j.n_name
      val p_partkey = j.p_partkey
      val p_mfgr = j.p_mfgr
      val s_address = j.s_address
      val s_phone = j.s_phone
      val s_comment = j.s_comment
      val ps_supplycost = j.ps_supplycost
      val r_name = r.r_name
    }).Where(_.r_name == "EUROPE").Where(j1 => j1.ps_supplycost == {
      val pssc = partSuppliers.Where(_.ps_partkey == j1.p_partkey).
        Join(suppliers).WhereEq(_.ps_suppkey, _.s_suppkey).Select((ps, s) => new Result {
        val ps_supplycost = ps.ps_supplycost
        val s_nationkey = s.s_nationkey
       }).Join(nations).WhereEq(_.s_nationkey, _.n_nationkey).Select((jj1, n) => new Result {
         val ps_supplycost = jj1.ps_supplycost
         val n_regionkey = n.n_regionkey
       }).Join(regions).WhereEq(_.n_regionkey, _.r_regionkey).Select((jj2, r) => new Result {
         val ps_supplycost = jj2.ps_supplycost
         val r_name = r.r_name
       }).Where(_.r_name == "EUROPE").Min(_.ps_supplycost)
    }) OrderByDescending(_.s_acctbal) ThenBy(_.n_name) ThenBy(_.s_name) ThenBy(_.p_partkey)
    toc(q)
    q.printAsTable(10)
  }    
}
