package ppl.delite.runtime.codegen.kernels.scala

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 7:43:16 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

  /**
 * The Delite framework only provides the map-reduce function as a kernel to the runtime
 * The full reduce kernel must be provided, as illustrated below
 * This is an example of what a generated MapReduce kernel for an Array should look like; it should not be invoked
 * This example is specifically for a MapReduce across 8 threads: the tree reduction should be customized to the actual number
 */

object ExampleMapReduce0 {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def reduceFunc(bound0: Double, bound1: Double, free0: Double): Double = {
    bound0 + bound1
  }

  def mapFunc(bound0: Double, free0: Double): Double = {
    bound0 * bound0
  }

  //apply for the master reducer returns the result
  def apply(in0: Array[Double], in1: Double, in2: Double): Double = {
    //first level of tree (log(numChunks) levels total)
    var acc = collMapReduce(in0, in1, in2)
    //second level of tree
    acc = reduceFunc(acc, ExampleReduce1.get, in2)
    //third level of tree
    acc = reduceFunc(acc, ExampleReduce2.get, in2)
    //fourth level of tree
    acc = reduceFunc(acc, ExampleReduce4.get, in2)
    //return result
    acc
  }

  private def collMapReduce(in0: Array[Double], in1: Double, in2: Double): Double = {
    val size = in0.size
    var idx = size*0/8 //size*chunkIdx/numChunks
    val end = size*1/8 //size*(chunkIdx+1)/numChunks
    var acc = mapFunc(in0(idx), in1) //TODO: this assumes that (end - idx) >= 1; what should we do if 0?
    idx += 1
    while (idx < end) {
      acc = reduceFunc(acc, mapFunc(in0(idx), in1), in2)
      idx += 1
    }
    acc
  }

}

object ExampleMapReduce6 {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def reduceFunc(bound0: Double, bound1: Double, free0: Double): Double = {
    bound0 + bound1 + free0
  }

  def mapFunc(bound0: Double, free0: Double): Double = {
    bound0 * bound0 * free0
  }

  //apply for the helper reducers all return Unit
  def apply(in0: Array[Double], in1: Double, in2: Double) {
    //first level of tree
    var acc = collMapReduce(in0, in1, in2)
    //second level of tree
    acc = reduceFunc(acc, ExampleReduce7.get, in2)
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

  private def collMapReduce(in0: Array[Double], in1: Double, in2: Double): Double = {
    val size = in0.size
    var idx = size*6/8 //size*chunkIdx/numChunks
    val end = size*7/8 //size*(chunkIdx+1)/numChunks
    var acc = mapFunc(in0(idx),in1)
    idx += 1
    while (idx < end) {
      acc = reduceFunc(acc, mapFunc(in0(idx), in1), in2)
      idx += 1
    }
    acc
  }

}

//the example ends here: following are stubs for compilation
object ExampleMapReduce1 {
  def get: Double = 0.0
}

object ExampleMapReduce2 {
  def get: Double = 0.0
}

object ExampleMapReduce4 {
  def get: Double = 0.0
}

object ExampleMapReduce7 {
  def get: Double = 0.0
}
