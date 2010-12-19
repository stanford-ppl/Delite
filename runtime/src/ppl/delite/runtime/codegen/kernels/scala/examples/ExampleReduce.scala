package ppl.delite.runtime.codegen.kernels.scala.examples

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
 * This example is specifically for a Reduce across 8 threads: the tree reduction should be customized to the actual number
 */
object ExampleReduceHeader {
  def apply(in0: Array[Double], in1: Double) = new ExampleReduceHeader(in0, in1)
}

final class ExampleReduceHeader(in0: Array[Double], in1: Double) {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def kernel_apply(in0: Array[Double], in1: Double): Reduce = {
    new Reduce {
      def in = in0
      //note that functions have fixed parameter lists
      def reduce(r1: Double, r2: Double): Double = r1 + r2 + in1
    }
  }

  abstract class Reduce {
    def in: Array[Double]
    def reduce(r1: Double, r2: Double): Double
  }

  val closure = kernel_apply(in0, in1)

  def get6: Double = Result6.get
  def set6(result: Double) = Result6.set(result)

  private object Result6 {

    @volatile
    private var notReady: Boolean = true

    private var _result: Double = _

    //TODO: what sync impl do we want here: 1)lock/condition 2) busy spin on a volatile
    def get: Double = {
      while (notReady) { } //spin
      _result
    }

    def set(result: Double) {
      _result = result
      notReady = false
    }
  }

  //stubs for compilation - see get6
  def get1: Double = 0.0
  def get2: Double = 0.0
  def get4: Double = 0.0
  def get7: Double = 0.0

}

object ExampleReduce0 {

  //apply for the master reducer returns the result
  def apply(reduce: ExampleReduceHeader): Double = {
    //first level of tree (log(numChunks) levels total)
    var acc = collReduce(reduce)
    //second level of tree
    acc = reduce.closure.reduce(acc, reduce.get1)
    //third level of tree
    acc = reduce.closure.reduce(acc, reduce.get2)
    //fourth level of tree
    acc = reduce.closure.reduce(acc, reduce.get4)
    //return result
    acc
  }

  private def collReduce(reduce: ExampleReduceHeader): Double = {
    val in = reduce.closure.in
    val size = in.size
    var idx = size*0/8 //size*chunkIdx/numChunks
    val end = size*1/8 //size*(chunkIdx+1)/numChunks
    var acc = in(idx) //TODO: this assumes that (end - idx) >= 1; what should we do if 0?
    idx += 1
    while (idx < end) {
      acc = reduce.closure.reduce(acc, in(idx))
      idx += 1
    }
    acc
  }

}

object ExampleReduce6 {

  //apply for the helper reducers all return Unit
  def apply(reduce: ExampleReduceHeader) {
    //first level of tree (log(numChunks) levels total)
    var acc = collReduce(reduce)
    //second level of tree
    acc = reduce.closure.reduce(acc, reduce.get7)
    //this chunk doesn't participate in third level => return
    reduce.set6(acc)
  }

  private def collReduce(reduce: ExampleReduceHeader): Double = {
    val in = reduce.closure.in
    val size = in.size
    var idx = size*6/8 //size*chunkIdx/numChunks
    val end = size*7/8 //size*(chunkIdx+1)/numChunks
    var acc = in(idx)
    idx += 1
    while (idx < end) {
      acc = reduce.closure.reduce(acc, in(idx))
      idx += 1
    }
    acc
  }

}
