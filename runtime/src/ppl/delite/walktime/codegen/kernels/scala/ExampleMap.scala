package ppl.delite.walktime.codegen.kernels.scala

/**
 * Author: Kevin J. Brown
 * Date: Nov 13, 2010
 * Time: 3:36:12 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * The Delite framework only provides the map function as a kernel to the runtime
 * The full map kernel must be provided, as illustrated below
 * This is an example of what a generated Map kernel for an Array should look like; it should not be invoked
 */
//TODO: do we want to use dynamic codegen for this? could specialize on function, types, and chunking
object ExampleMap {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def func(bound0: Int, free0: Array[Double], free1: Double): Double = {
    free0(bound0)-free1
  }

  private val chunkIdx: Int = 3
  private val numChunks: Int = 4

  //TODO: should these types be "Array" or some DSL type that uses an array as its backing store?
  //TODO: dependent on just how we decide to lift & represent DSL data structures in the Delite runtime
  //TODO: the size of the collection and length of the array may not be the same
  def apply(out: Array[Double], in: Array[Int], free0: Array[Double], free1: Double) = {
    val size = in.length
    var i = size*chunkIdx/numChunks
    val end = size*(chunkIdx+1)/numChunks
    while (i < end) {
      out(i) = func(in(i), free0, free1)
      i += 1
    }
  }

}
