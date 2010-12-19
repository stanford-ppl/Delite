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
object ExampleZipHeader {
  def apply(in0: Array[Int], in1: Array[Int]) = new ExampleZipHeader(in0, in1)
}

final class ExampleZipHeader(in0: Array[Int], in1: Array[Int]) {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def kernel_apply(in0: Array[Int], in1: Array[Int]): Zip = {
    new Zip {
      def inA = in0
      def inB = in1
      def alloc = new Array[Int](in0.length)
      //note that functions have fixed parameter lists
      def zip(a: Int, b: Int): Int = a + b
    }
  }

  abstract class Zip {
    def inA: Array[Int]
    def inB: Array[Int]
    def alloc: Array[Int] //can allocate a new output structure (immutable Zip) or pass a reference to an existing one (mutable Zip)
    def zip(a: Int, b: Int): Int
  }

  val closure = kernel_apply(in0, in1)
  val out: Array[Int] = closure.alloc

}

object ExampleZip {

  //this is the kernel
  //note: the types are probably some DSL type rather than Array
  def apply(zip: ExampleZipHeader): Array[Int] = {
    val inA = zip.closure.inA
    val inB = zip.closure.inB
    val out = zip.out
    val size = inA.size
    var i = size*2/4 //size*chunkIdx/numChunks
    val end = size*3/4 //size*(chunkIdx+1)/numChunks
    while (i < end) {
      out(i) = zip.closure.zip(inA(i), inB(i))
      i += 1
    }
    out
  }
}
