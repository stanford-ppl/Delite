package ppl.delite.runtime.codegen.kernels.scala.examples

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
object ExampleMapReduceHeader {
  def apply(in0: Array[Double], in1: Double, in2: Double) = new ExampleMapReduceHeader(in0, in1, in2)
}

final class ExampleMapReduceHeader(in0: Array[Double], in1: Double, in2: Double) {

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
    def mapreduce(acc: Double, elem: Double): Double = reduce(acc, map(elem))
  }

  val closure = kernel_apply(in0, in1, in2)

  @volatile private var notReady6: Boolean = true
  private var _result6: Double = _

  //TODO: what sync impl do we want here: 1)lock/condition 2) busy spin on a volatile
  def get6: Double = {
    while (notReady6) { } //spin
    _result6
  }

  def set6(result: Double) {
    _result6 = result
    notReady6 = false
  }

  //stubs for compilation - see get6
  def get1: Double = 0.0
  def get2: Double = 0.0
  def get4: Double = 0.0
  def get7: Double = 0.0

}

object ExampleMapReduce0 {

  //apply for the master reducer returns the result
  def apply(mapReduce: ExampleMapReduceHeader): Double = {
    //first level of tree (log(numChunks) levels total)
    var acc = collMapReduce(mapReduce)
    //second level of tree
    acc = mapReduce.closure.reduce(acc, mapReduce.get1)
    //third level of tree
    acc = mapReduce.closure.reduce(acc, mapReduce.get2)
    //fourth level of tree
    acc = mapReduce.closure.reduce(acc, mapReduce.get4)
    //return result
    acc
  }

  private def collMapReduce(mapReduce: ExampleMapReduceHeader): Double = {
    val in = mapReduce.closure.in
    val size = in.size
    var idx = size*0/8 //size*chunkIdx/numChunks
    val end = size*1/8 //size*(chunkIdx+1)/numChunks
    var acc = mapReduce.closure.map(in(idx)) //TODO: this assumes that (end - idx) >= 1; what should we do if 0?
    idx += 1
    while (idx < end) {
      acc = mapReduce.closure.mapreduce(acc, in(idx))
      idx += 1
    }
    acc
  }

}

object ExampleMapReduce6 {

  //apply for the helper reducers all return Unit
  def apply(mapReduce: ExampleMapReduceHeader) {
    //first level of tree
    var acc = collMapReduce(mapReduce)
    //second level of tree
    acc = mapReduce.closure.reduce(acc, mapReduce.get7)
    //this chunk doesn't participate in third level => return
    mapReduce.set6(acc)
  }

  private def collMapReduce(mapReduce: ExampleMapReduceHeader): Double = {
    val in = mapReduce.closure.in
    val size = in.size
    var idx = size*6/8 //size*chunkIdx/numChunks
    val end = size*7/8 //size*(chunkIdx+1)/numChunks
    var acc = mapReduce.closure.map(in(idx))
    idx += 1
    while (idx < end) {
      acc = mapReduce.closure.mapreduce(acc, in(idx))
      idx += 1
    }
    acc
  }

}
