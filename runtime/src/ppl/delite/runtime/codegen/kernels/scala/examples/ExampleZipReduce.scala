package ppl.delite.runtime.codegen.kernels.scala.examples

 /**
 * The Delite framework only provides the zip-reduce function as a kernel to the runtime
 * The full reduce kernel must be provided, as illustrated below
 * This is an example of what a generated ZipReduce kernel for an Array should look like; it should not be invoked
 * This example is specifically for a ZipReduce across 8 threads: the tree reduction should be customized to the actual number
 */
object ExampleZipReduceHeader {
  def apply(in0: Array[Double], in1: Array[Double]) = new ExampleZipReduceHeader(in0, in1)
}

final class ExampleZipReduceHeader(in0: Array[Double], in1: Array[Double]) {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def kernel_apply(in0: Array[Double], in1: Array[Double]): ZipReduce = {
    new ZipReduce {
      def inA = in0
      def inB = in1
      //note that functions have fixed parameter lists
      def zip(a: Double, b: Double): Double = a * b
      def reduce(acc: Double, elem: Double) = acc + elem
    }
  }

  abstract class ZipReduce {
    def inA: Array[Double]
    def inB: Array[Double]
    def zip(a: Double, b: Double): Double
    def reduce(acc: Double, elem: Double): Double
    def zipreduce(acc: Double, a: Double, b: Double): Double = reduce(acc, zip(a, b))
  }

  val closure = kernel_apply(in0, in1)

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

object ExampleZipReduce0 {

  //apply for the master reducer returns the result
  def apply(zipReduce: ExampleZipReduceHeader): Double = {
    //first level of tree (log(numChunks) levels total)
    var acc = collZipReduce(zipReduce)
    //second level of tree
    acc = zipReduce.closure.reduce(acc, zipReduce.get1)
    //third level of tree
    acc = zipReduce.closure.reduce(acc, zipReduce.get2)
    //fourth level of tree
    acc = zipReduce.closure.reduce(acc, zipReduce.get4)
    //return result
    acc
  }

  private def collZipReduce(zipReduce: ExampleZipReduceHeader): Double = {
    val inA = zipReduce.closure.inA
    val inB = zipReduce.closure.inB
    val size = inA.size
    var idx = size*0/8 //size*chunkIdx/numChunks
    val end = size*1/8 //size*(chunkIdx+1)/numChunks
    var acc = zipReduce.closure.zip(inA(idx), inB(idx)) //TODO: this assumes that (end - idx) >= 1; what should we do if 0?
    idx += 1
    while (idx < end) {
      acc = zipReduce.closure.zipreduce(acc, inA(idx), inB(idx))
      idx += 1
    }
    acc
  }

}

object ExampleZipReduce6 {

  //apply for the helper reducers all return Unit
  def apply(zipReduce: ExampleZipReduceHeader) {
    //first level of tree
    var acc = collZipReduce(zipReduce)
    //second level of tree
    acc = zipReduce.closure.reduce(acc, zipReduce.get7)
    //this chunk doesn't participate in third level => return
    zipReduce.set6(acc)
  }

  private def collZipReduce(zipReduce: ExampleZipReduceHeader): Double = {
    val inA = zipReduce.closure.inA
    val inB = zipReduce.closure.inB
    val size = inA.size
    var idx = size*6/8 //size*chunkIdx/numChunks
    val end = size*7/8 //size*(chunkIdx+1)/numChunks
    var acc = zipReduce.closure.zip(inA(idx), inB(idx))
    idx += 1
    while (idx < end) {
      acc = zipReduce.closure.zipreduce(acc, inA(idx), inB(idx))
      idx += 1
    }
    acc
  }

}
