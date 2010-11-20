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

  val queueSize: Int = System.getProperty("queueSize", "128").toInt

  val scheduler: String = System.getProperty("scheduler", "default")

  val executor: String = System.getProperty("executor", "default")

}