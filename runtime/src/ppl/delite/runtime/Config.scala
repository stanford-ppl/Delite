package ppl.delite.runtime

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:43:14 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Config {

  val numThreads: Int = System.getProperty("numThreads", "1").toInt

  val numGPUs: Int = System.getProperty("numGPUs", "0").toInt

  val queueSize: Int = System.getProperty("queueSize", "128").toInt

  val scheduler: String = System.getProperty("scheduler", "default")

  val executor: String = System.getProperty("executor", "default")

  val printSources: Boolean = if (System.getProperty("debug-printSources") == null) false else true

  val numRuns: Int = System.getProperty("numRuns", "1").toInt


  /***********
   * Statistics and Metrics Section
   */
  val dumpStats: Boolean = if(System.getProperty("dump-stats") == null) false else true

  val dumpStatsComponent: String = System.getProperty("dumpStatsComponent", "all")

  val dumpStatsOverwrite: Boolean = if(System.getProperty("dump-stats-overwrite")== null) false else true

  val statsOutputDirectory: String = System.getProperty("statsOutputDirectory")
  if(dumpStats && statsOutputDirectory == null) throw new RuntimeException("dump-stats option enabled but did not provide a statsOutputDirectory")

  val statsOutputFilename: String = System.getProperty("statsOutputFilename")
  if(dumpStats && statsOutputFilename == null) throw new RuntimeException("dump-stats option enabled but did not provide a statsOutputFilename")



}