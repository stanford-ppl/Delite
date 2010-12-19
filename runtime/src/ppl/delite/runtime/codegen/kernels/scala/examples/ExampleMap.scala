package ppl.delite.runtime.codegen.kernels.scala.examples

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
object ExampleMapHeader {
  def apply(in0: Array[Int], in1: Double) = new ExampleMapHeader(in0, in1)
}

final class ExampleMapHeader(in0: Array[Int], in1: Double) {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def kernel_apply(in0: Array[Int], in1: Double): Map = {
    new Map {
      def in = in0
      def out = new Array[Double](in0.length)
      //note that functions have fixed parameter lists
      def map(elem: Int): Double = elem*in1
    }
  }

  abstract class Map {
    def in: Array[Int]
    def out: Array[Double] //can allocate a new output structure (immutable map) or pass a reference to an existing one (mutable map)
    def map(elem: Int): Double
  }

  val closure = kernel_apply(in0, in1)
  val out: Array[Double] = closure.out

}

object ExampleMap {

  //this is the kernel
  //note: the types are probably some DSL type rather than Array
  def apply(map: ExampleMapHeader): Array[Double] = {
    val in = map.closure.in
    val out = map.out
    val size = in.size
    var i = size*2/4 //size*chunkIdx/numChunks
    val end = size*3/4 //size*(chunkIdx+1)/numChunks
    while (i < end) {
      out(i) = map.closure.map(in(i))
      i += 1
    }
    out
  }
}
