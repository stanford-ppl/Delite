package ppl.delite.runtime.codegen.kernels.scala

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
  def func(bound0: Double, bound1: Double): Double = {
    bound0 + bound1
  }

  //apply for the master reducer returns the result
  def apply(in: Array[Double]): Double = {
    //first level of tree (log(numChunks) levels total)
    var acc = collReduce(in)
    //second level of tree
    acc = func(acc, ExampleReduce1.get)
    //third level of tree
    acc = func(acc, ExampleReduce2.get)
    //fourth level of tree
    acc = func(acc, ExampleReduce4.get)
    //return result
    acc
  }

  private def collReduce(in: Array[Double]): Double = {
    val size = in.size
    var idx = size*0/8 //size*chunkIdx/numChunks
    val end = size*1/8 //size*(chunkIdx+1)/numChunks
    var acc = in(idx) //TODO: this assumes that (end - idx) >= 1; what should we do if 0?
    idx += 1
    while (idx < end) {
      acc = func(acc, in(idx))
      idx += 1
    }
    acc
  }

}

object ExampleReduce6 {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def func(bound0: Double, bound1: Double): Double = {
    bound0 + bound1
  }

  //apply for the helper reducers all return Unit
  def apply(in: Array[Double]) {
    //first level of tree
    var acc = collReduce(in)
    //second level of tree
    acc = func(acc, ExampleReduce7.get)
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

  private def collReduce(in: Array[Double]): Double = {
    val size = in.size
    var idx = size*6/8 //size*chunkIdx/numChunks
    val end = size*7/8 //size*(chunkIdx+1)/numChunks
    var acc = in(idx)
    idx += 1
    while (idx < end) {
      acc = func(acc, in(idx))
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
