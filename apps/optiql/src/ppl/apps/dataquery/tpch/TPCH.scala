package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}

object TPCHQ1 extends OptiQLApplicationRunner with TPCHQ1Trait
object TPCHQ2 extends OptiQLApplicationRunner with TPCHQ2Trait

trait TPCHBaseTrait extends OptiQLApplication with Types {

  val queryName: String
  
  var customers: Rep[CustomerTable] = _
  var lineItems: Rep[LineItemTable] = _
  var orders: Rep[OrderTable] = _
  var nations: Rep[NationTable] = _
  var parts: Rep[PartTable] = _
  var partSuppliers: Rep[PartSupplierTable] = _
  var regions: Rep[RegionTable] = _
  var suppliers: Rep[SupplierTable] = _

    
  def printUsage = {
    println("Usage: TPCH <input tpch directory>")
    exit(-1)
  }
  
  def query(): Rep[_]
  
  def main() = {
    println("TPCH style benchmarking " + queryName )
    if (args.length < 1) printUsage
    
    val tpchDataPath = args(0) 

    //load TPCH data
    customers = TableInputReader(tpchDataPath+"/customer.tbl", Customer())
    lineItems = TableInputReader(tpchDataPath+"/lineitem.tbl", LineItem())
    orders = TableInputReader(tpchDataPath+"/orders.tbl", Order())
    nations = TableInputReader(tpchDataPath+"/nation.tbl", Nation())
    parts = TableInputReader(tpchDataPath+"/part.tbl", Part())
    partSuppliers = TableInputReader(tpchDataPath+"/partsupp.tbl", PartSupplier())
    regions = TableInputReader(tpchDataPath+"/region.tbl", Region())
    suppliers = TableInputReader(tpchDataPath+"/supplier.tbl", Supplier())
    println("Loading Complete")	
    //tic(customers, lineItems, orders, nations, parts, partSuppliers, regions, suppliers)
    //TODO: by tic'ing on all input we force a bunch of loading that is otherwise dead... what's the proper solution? soft dependencies?
    query()
  }

}

trait TPCHQ1Trait extends TPCHBaseTrait {
  val queryName = "Q1"  
  def query() = {           
    tic(lineItems.size)
    //TODO: Get rid of this hack with general GPU hashreduce
    //val q = lineItems Where(_.l_shipdate <= Date("1998-12-01")) GroupBy(l => (l.l_returnflag,l.l_linestatus)) Select(g => new Result {
    val q = lineItems Where(_.l_shipdate <= Date("1998-12-01")) GroupBy(l => (l.l_returnflag=='A',l.l_linestatus=='O')) Select(g => new Result {
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
       }).Where(_.r_name == "EUROPE")
       val minIdx = pssc.MinIndex(_.ps_supplycost)
       if (minIdx >= 0) pssc(minIdx).ps_supplycost else -10

       //}).Where(_.r_name == "EUROPE").Min(_.ps_supplycost)
       // Min will return either a a struct or null right now. This forces Map creation.
       // TODO: refactor to return Option and soa transform across option.
       //if(pssc != null) pssc.ps_supplycost else -10
    }) OrderByDescending(_.s_acctbal) ThenBy(_.n_name) ThenBy(_.s_name) ThenBy(_.p_partkey)
    toc(q)
    q.printAsTable(10)
  }    
}
