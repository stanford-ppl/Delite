package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import java.io.File

object TPCHQ2Runner extends OptiQLApplicationRunner with TPCHQ2

trait TPCHQ2 extends OptiQLApplication {

  val s = File.separator

  def printUsage = {
    println("Usage: TPCH <input tpch directory>")
    exit(-1)
  }
  
  
  val debug = true

  def main() = {
  
  
    println("TPCH Q2 style benchmarking")
    if (args.length < 1) printUsage
    
    val tpchDataPath = args(0)
    
    //load TPCH data
    val parts = TPCH.loadParts(tpchDataPath)
    val partSuppliers = TPCH.loadPartSuppliers(tpchDataPath)
    val suppliers = TPCH.loadSuppliers(tpchDataPath)
    val nations = TPCH.loadNations(tpchDataPath)
    val regions = TPCH.loadRegions(tpchDataPath)
    println("Loading Complete")
	  tic(parts.size,suppliers.size)
    
    /*nations.Select(n => new Result {
      val n_name = n.n_name
      val n_regionkey = n.n_regionkey
    }).printAsTable

    regions.Select(r => new Result {
      val r_regionkey = r.r_regionkey
      val r_name = r.r_name
    }).printAsTable*/
    
    println("A")
    /*val res = nations.Join(regions).WhereEq(_.n_regionkey, _.r_regionkey).Select((n,r) => new Result {
      val n_name = n.n_name
      val n_regionkey = n.n_regionkey
      val r_regionkey = r.r_regionkey
      val r_name = r.r_name
    })*/

    // p_type x13, p_partkey x19, p_comment x11, p_mfgr x15  <--- comment columns is used to obtain length of result
    // x92 += x19
    // x76 += x11
    // x84 += x15
    
    /*
    val res0 = parts.Where(_.p_type.endsWith("BRASS")).Join(partSuppliers).WhereEq(_.p_partkey, _.ps_partkey).Select((p, ps) => new  Result {
      val p_partkey = p.p_partkey
      val p_mfgr = p.p_mfgr
      val ps_suppkey = ps.ps_suppkey
    }).Join(suppliers).WhereEq(_.ps_suppkey,_.s_suppkey).Select((j, s) => new Result {
      val p_partkey = j.p_partkey
      val p_mfgr = j.p_mfgr
      val s_nationkey = s.s_nationkey
    }).Join(nations).WhereEq(_.s_nationkey, _.n_nationkey).Select((j, n) => new Result {
      val p_partkey = j.p_partkey
      val p_mfgr = j.p_mfgr
      val n_regionkey = n.n_regionkey
    })
    
    val res1 = res0.Join(regions).WhereEq(_.n_regionkey, _.r_regionkey).Select((j, r) => new Result {
      val p_partkey = j.p_partkey
      val p_mfgr = j.p_mfgr   //<--- BREAKS!!  WHY ???
      val r_name = r.r_name
    })

    val res = res1*/
    





    //FIXME: effect order violation
    //TODO: soa for tuple reduce

    val res = parts.Where(_.p_type.endsWith("BRASS")).Join(partSuppliers).WhereEq(_.p_partkey, _.ps_partkey).Select((p, ps) => new  Result {
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

    println("B1")
    toc(res)
    println(res)
    res.printAsTable(10)

    println("C")
	
  }
  
  
  

}
