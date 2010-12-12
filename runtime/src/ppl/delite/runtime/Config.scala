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

}