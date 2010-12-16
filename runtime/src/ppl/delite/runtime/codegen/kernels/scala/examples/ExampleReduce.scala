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

object ExampleReduce0 {

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

  //apply for the master reducer returns the result
  def apply(in0: Array[Double], in1: Double): Double = {
    val reduce = kernel_apply(in0, in1)
    //first level of tree (log(numChunks) levels total)
    var acc = collReduce(reduce)
    //second level of tree
    acc = reduce.reduce(acc, ExampleReduce1.get)
    //third level of tree
    acc = reduce.reduce(acc, ExampleReduce2.get)
    //fourth level of tree
    acc = reduce.reduce(acc, ExampleReduce4.get)
    //return result
    acc
  }

  private def collReduce(reduce: Reduce): Double = {
    val in = reduce.in
    val size = in.size
    var idx = size*0/8 //size*chunkIdx/numChunks
    val end = size*1/8 //size*(chunkIdx+1)/numChunks
    var acc = in(idx) //TODO: this assumes that (end - idx) >= 1; what should we do if 0?
    idx += 1
    while (idx < end) {
      acc = reduce.reduce(acc, in(idx))
      idx += 1
    }
    acc
  }

}

object ExampleReduce6 {

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

  //apply for the helper reducers all return Unit
  def apply(in0: Array[Double], in1: Double) {
    val reduce = kernel_apply(in0, in1)
    //first level of tree (log(numChunks) levels total)
    var acc = collReduce(reduce)
    //second level of tree
    acc = reduce.reduce(acc, ExampleReduce7.get)
    //this chunk doesn't participate in third level => return
    set(acc)
  }

  @volatile
  private var notReady: Boolean = true

  private var _result: Double = _

  //TODO: what sync impl do we want here: 1)lock/condition 2) busy spin on a volatile
  def get: Double = {
    while (notReady) { } //spin
    _result
  }

  private def set(result: Double) {
    _result = result
    notReady = false
  }

  private def collReduce(reduce: Reduce): Double = {
    val in = reduce.in
    val size = in.size
    var idx = size*6/8 //size*chunkIdx/numChunks
    val end = size*7/8 //size*(chunkIdx+1)/numChunks
    var acc = in(idx)
    idx += 1
    while (idx < end) {
      acc = reduce.reduce(acc, in(idx))
      idx += 1
    }
    acc
  }

}

//the example ends here: following are stubs for compilation
object ExampleReduce1 {
  def get: Double = 0.0
}

object ExampleReduce2 {
  def get: Double = 0.0
}

object ExampleReduce4 {
  def get: Double = 0.0
}

object ExampleReduce7 {
  def get: Double = 0.0
}
