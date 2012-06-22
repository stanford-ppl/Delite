package ppl.delite.runtime

import graph.targets.OS

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:43:14 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Config {

  private def getProperty(prop: String, default: String) = {
    val p1 = System.getProperty(prop)
    val p2 = System.getProperty(prop.substring(1))
    if (p1 != null && p2 != null) {
      assert(p1 == p2, "ERROR: conflicting properties")
      p1
    }
    else if (p1 != null) p1 else if (p2 != null) p2 else default
  }

  val numThreads: Int = getProperty("delite.threads", "1").toInt
  val numGPUs: Int = getProperty("delite.gpus", "0").toInt
  val useOpenCL: Boolean = getProperty("delite.use.opencl", "false") != "false"
  val scheduler: String = getProperty("delite.scheduler", "default")
  val executor: String = getProperty("delite.executor", "default")
  val numRuns: Int = getProperty("delite.runs", "1").toInt
  val deliteHome: String = getProperty("delite.home", System.getProperty("user.dir"))
  val codeCacheHome: String = getProperty("delite.code.cache.home", deliteHome + java.io.File.separator + "generatedCache")
  val useFsc: Boolean = getProperty("delite.usefsc", "false") != "false"

  /* Debug options */
  val queueSize: Int = getProperty("delite.debug.queue.size", "128").toInt
  val noRegenerate: Boolean = getProperty("delite.debug.noregenerate", "false") != "false"
  val gpuBlackList: Array[String] = getProperty("delite.debug.gpu.blacklist","").split(",")
  val profile: Boolean = getProperty("delite.debug.profile", "false") != "false"
  val printSources: Boolean = getProperty("delite.debug.print.sources", "false") != "false"


  var degFilename = System.getProperty("delite.deg.filename", "out.deg")
  
  /**
   * DEG specific, set after its parsed
   * TODO: handle this more rigorously
   */
  var deliteBuildHome: String = ""

  /***********
   *	Cost Modeling
   */
  
	val whileCostThreshold: Int = getProperty("delite.while.threshold", "-1").toInt
	val loopCostThreshold: Int = getProperty("delite.loop.threshold", "-1").toInt
	 
  /***********
    * Statistics and Metrics Section
    */
   val dumpStats: Boolean = getProperty("stats.dump", "false") != "false"
   val dumpStatsComponent: String = getProperty("stats.dump.component", "all")
   val dumpStatsOverwrite: Boolean = getProperty("stats.dump.overwrite", "false") != "false"
       
   val statsOutputDirectory: String = getProperty("stats.output.dir", "")
   if(dumpStats && statsOutputDirectory == "") error("stats.dump option enabled but did not provide a statsOutputDirectory")

   val statsOutputFilename: String = getProperty("stats.output.filename", "")
   if(dumpStats && statsOutputFilename == "") error("stats.dump option enabled but did not provide a statsOutputFilename")

   val dumpProfile: Boolean = getProperty("profile.dump", "false") != "false"   
   val profileOutputDirectory: String = getProperty("profile.output.dir", "")
   if(dumpProfile && profileOutputDirectory == "") error("profile.dump option enabled but did not provide a profileOutputDirectory")   
}
