package ppl.delite.walktime.codegen.kernels.scala

/**
 * Author: Kevin J. Brown
 * Date: Nov 14, 2010
 * Time: 10:19:53 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * The Delite framework only provides the reduce function as a kernel to the runtime
 * The full reduce kernel must be provided, as illustrated below
 * This is an example of what a generated Reduce kernel for an Array should look like; it should not be invoked
 */

object ExampleReduce {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def func(bound0: Double, bound1: Double): Double = {
    bound0 + bound1
  }

  private val chunkIdx: Int = 3
  private val numChunks: Int = 4

  //TODO: want to make multi-level tree reduction(s); could have multiple versions to select from
  /* def apply(in: Array[Double]) = {
    var acc = in(first)
    var i = first+1
    val end = last
    while (i < end) {
      acc = func(acc, in(i))
      i += 1
    }
  } */

}
