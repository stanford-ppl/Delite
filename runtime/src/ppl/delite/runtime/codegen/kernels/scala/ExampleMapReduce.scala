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
  def kernel_apply(in0: Array[Double], in1: Double, in2: Double): MapReduce = {
    new MapReduce {
      def in = in0
      //note that functions have fixed parameter lists
      def map(elem: Double): Double = elem * in1 * in2
      def reduce(acc: Double, elem: Double) = acc + elem
    }
  }

  abstract class MapReduce {
    def in: Array[Double]
    def map(elem: Double): Double
    def reduce(acc: Double, elem: Double): Double
  }

  //apply for the master reducer returns the result
  def apply(in0: Array[Double], in1: Double, in2: Double): Double = {
    val mapReduce = kernel_apply(in0, in1, in2)
    //first level of tree (log(numChunks) levels total)
    var acc = collMapReduce(mapReduce)
    //second level of tree
    acc = mapReduce.reduce(acc, ExampleReduce1.get)
    //third level of tree
    acc = mapReduce.reduce(acc, ExampleReduce2.get)
    //fourth level of tree
    acc = mapReduce.reduce(acc, ExampleReduce4.get)
    //return result
    acc
  }

  private def collMapReduce(mapReduce: MapReduce): Double = {
    val in = mapReduce.in
    val size = in.size
    var idx = size*0/8 //size*chunkIdx/numChunks
    val end = size*1/8 //size*(chunkIdx+1)/numChunks
    var acc = mapReduce.map(in(idx)) //TODO: this assumes that (end - idx) >= 1; what should we do if 0?
    idx += 1
    while (idx < end) {
      acc = mapReduce.reduce(acc, mapReduce.map(in(idx)))
      idx += 1
    }
    acc
  }

}

object ExampleMapReduce6 {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def kernel_apply(in0: Array[Double], in1: Double, in2: Double): MapReduce = {
    new MapReduce {
      def in = in0
      //note that functions have fixed parameter lists
      def map(elem: Double): Double = elem * in1 * in2
      def reduce(acc: Double, elem: Double) = acc + elem
    }
  }

  abstract class MapReduce {
    def in: Array[Double]
    def map(elem: Double): Double
    def reduce(acc: Double, elem: Double): Double
  }

  //apply for the helper reducers all return Unit
  def apply(in0: Array[Double], in1: Double, in2: Double) {
    val mapReduce = kernel_apply(in0, in1, in2)
    //first level of tree
    var acc = collMapReduce(mapReduce)
    //second level of tree
    acc = mapReduce.reduce(acc, ExampleReduce7.get)
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

  private def collMapReduce(mapReduce: MapReduce): Double = {
    val in = mapReduce.in
    val size = in.size
    var idx = size*6/8 //size*chunkIdx/numChunks
    val end = size*7/8 //size*(chunkIdx+1)/numChunks
    var acc = mapReduce.map(in(idx))
    idx += 1
    while (idx < end) {
      acc = mapReduce.reduce(acc, mapReduce.map(in(idx)))
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
