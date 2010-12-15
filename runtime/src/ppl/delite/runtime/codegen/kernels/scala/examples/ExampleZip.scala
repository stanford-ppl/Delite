package ppl.delite.runtime.codegen.kernels.scala.examples

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
  def kernel_apply(in0: Array[Double], in1: Array[Double], in2: Array[Double]): Zip = {
    new Zip {
      def inA = in0
      def inB = in1
      def out = in2
      //note that functions have fixed parameter lists
      def zip(a: Double, b: Double): Double = a + b
    }
  }

  abstract class Zip {
    def inA: Array[Double]
    def inB: Array[Double]
    def out: Array[Double]
    def zip(a: Double, b: Double): Double
  }

  //this is the kernel
  def apply(in0: Array[Double], in1: Array[Double], in2: Array[Double]) {
    val zip = kernel_apply(in0, in1, in2)
    val inA = zip.inA
    val inB = zip.inB
    val out = zip.out
    val size = inA.size
    var i = size*2/4 //size*chunkIdx/numChunks
    val end = size*3/4 //size*(chunkIdx+1)/numChunks
    while (i < end) {
      out(i) = zip.zip(inA(i), inB(i))
      i += 1
    }
  }

}
