package ppl.delite.benchmarking.sorting

object Config {
  
  val num_procs = Integer.parseInt(System.getProperty("threads", "1"))
  val tpch_factor = Integer.parseInt(System.getProperty("tpch.factor" , "0"))
  val tpch_dir = {
    val dir = System.getProperty("tpch.dir", "")
    if(dir == "")
      sys.error("TPCH data directory not specified, specify using -Dtpch.dir=\"PATH_TO_DATA_DIR\"")
    else dir
  }
  val tpch_num_elems = Integer.parseInt(System.getProperty("tpch.num_elems","0"))
  

}
