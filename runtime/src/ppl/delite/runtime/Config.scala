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

  val numThreads: Int = System.getProperty("delite.threads", "1").toInt

  val numGPUs: Int = System.getProperty("delite.gpus", "0").toInt

  val queueSize: Int = System.getProperty("delite.debug.queue.size", "128").toInt

  val scheduler: String = System.getProperty("delite.scheduler", "default")

  val executor: String = System.getProperty("delite.executor", "default")

  val printSources: Boolean = if (System.getProperty("delite.debug.print.sources") == null) false else true

  val numRuns: Int = System.getProperty("delite.runs", "1").toInt

  val deliteHome: String = System.getProperty("delite.home", System.getProperty("user.dir"))

}
