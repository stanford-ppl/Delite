package ppl.delite.runtime.codegen.kernels.scala

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
object ExampleMap {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def func(bound0: Int, free0: Array[Double], free1: Double): Double = {
    free0(bound0)-free1
  }

  //this is the kernel
  //note: the types are probably some DSL type rather than Array
  def apply(out: Array[Double], in: Array[Int], free0: Array[Double], free1: Double) {
    val size = in.length //for a DSL type extending DeliteCollection this should really be "in.size"
    var i = size*2/4 //size*chunkIdx/numChunks
    val end = size*3/4 //size*(chunkIdx+1)/numChunks
    while (i < end) {
      out(i) = func(in(i), free0, free1)
      i += 1
    }
  }

}
