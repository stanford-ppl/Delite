package ppl.delite.runtime.walktime.codegen.kernels.scala

/**
 * Author: Kevin J. Brown
 * Date: Nov 17, 2010
 * Time: 8:54:55 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * The Delite framework only provides the Zip function as a kernel to the runtime
 * The full Zip kernel must be provided, as illustrated below
 * This is an example of what a generated Zip kernel for an Array should look like; it should not be invoked
 */
object ExampleZip {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def func(bound0: Double, bound1: Double): Double = {
    bound0 - bound1
  }

  //this is the kernel
  //note: the types are probably some DSL type rather than Array
  def apply(out: Array[Double], in0: Array[Double], in1: Array[Double]) {
    val size = in0.size //choose in0 arbitrarily (out, in0, in1 should all have same size)
    var i = size*2/4 //size*chunkIdx/numChunks
    val end = size*3/4 //size*(chunkIdx+1)/numChunks
    while (i < end) {
      out(i) = func(in0(i), in1(i))
      i += 1
    }
  }

}
