package ppl.delite.runtime.codegen.kernels.scala.examples

 /**
 * The Delite framework only provides the multi-loop function to the runtime
 * The full kernel must be provided, as illustrated below
 * This is an example of what a generated MultiLoop kernel for an Array should look like; it should not be invoked
 * This example is specifically for a MultiLoop across 8 threads: the tree reduction should be customized to the actual number
 */

object ExampleMultiLoopHeader {
  def apply(in0: Array[Double], in1: Double, in2: Double) = new ExampleMultiLoopHeader(in0, in1, in2)
}

//this is the activation record provided by the kernel; contains all kernel outputs; shouldn't be generated
final class Activation {
  var out1: Array[Double] = _
  var out2: Double = _
}

final class ExampleMultiLoopHeader(in0: Array[Double], in1: Double, in2: Double) {

  //the multi-loop API
  abstract class MultiLoop {
    def size: Int //loop iterates 0..size
    def alloc: Activation //allocate shared data
    def init(act: Activation, index: Int): Activation //initialize and split chunks
    def process(act: Activation, index: Int) //primary loop
    def combine(act: Activation, rhs: Activation) //combine chunks after loop
  }

  //this is the apply method of another (kernel) object: shouldn't be generated
  def kernel_apply(in0: Array[Double], in1: Double, in2: Double): MultiLoop = {
    new MultiLoop {
      def size = in0.size

      def alloc = {
        val act = new Activation
        act.out1 = new Array[Double](in0.size)
        act
      }

      //initial iteration(s) handled specially to initialize chunked accumulator(s)
      def init(act: Activation, index: Int) = {
        val actCopy = new Activation
        actCopy.out1 = act.out1 //array shared
        actCopy.out2 = 0.0 //private accumulator
        process(act, index)
        actCopy
      }

      //note that functions have fixed parameter lists, closure used to pass free parameters
      def process(act: Activation, index: Int) {
        act.out1(index) = in0(index) * in1 + in2 // out1 = in0.map(e => e * in1 + in2)
        act.out2 += in0(index) //out2 = in0.reduce(_ + _)
      }

      def combine(act: Activation, rhs: Activation) {
        act.out2 += rhs.out2
      }
    }
  }

  //header allocates closure and output sequentially before parallel chunks run
  val closure = kernel_apply(in0, in1, in2)
  val out = closure.alloc

  //synchronization for combine phase (if required)
  @volatile private var notReady6: Boolean = true
  private var _result6: Activation = _

  //TODO: what sync impl do we want here: 1)lock/condition 2) busy spin on a volatile
  def get6: Activation = {
    while (notReady6) { } //spin
    _result6
  }

  def set6(result: Activation) {
    _result6 = result
    notReady6 = false
  }

  //stubs for compilation - see get6
  def get1: Activation = null
  def get2: Activation = null
  def get4: Activation = null
  def get7: Activation = null

}

object ExampleMultiLoop0 {

  //apply for the master (chunk 0) returns the result
  def apply(header: ExampleMultiLoopHeader): Activation = {
    //first level of tree (log(numChunks) levels total)
    val acc = process(header)
    //second level of tree
    header.closure.combine(acc, header.get1)
    //third level of tree
    header.closure.combine(acc, header.get2)
    //fourth level of tree
    header.closure.combine(acc, header.get4)
    //return result
    acc
  }

  private def process(header: ExampleMultiLoopHeader): Activation = {
    val size = header.closure.size
    val out = header.out
    var idx = size*0/8 //size*chunkIdx/numChunks
    val end = size*1/8 //size*(chunkIdx+1)/numChunks

    val acc = header.closure.init(out, idx) //TODO: this assumes (end - idx) > 0; support empty collection chunks
    idx += 1
    while (idx < end) {
      header.closure.process(acc, idx)
      idx += 1
    }
    acc
  }

}

object ExampleMultiLoop6 {

  //apply for the helper chunks all return Unit
  def apply(header: ExampleMultiLoopHeader) {
    //first level of tree
    val acc = process(header)
    //second level of tree
    header.closure.combine(acc, header.get7)
    //this chunk doesn't participate in third level => return
    header.set6(acc)
  }

  private def process(header: ExampleMultiLoopHeader): Activation = {
    val size = header.closure.size
    val out = header.out
    var idx = size*6/8 //size*chunkIdx/numChunks
    val end = size*7/8 //size*(chunkIdx+1)/numChunks

    val acc = header.closure.init(out, idx)
    idx += 1
    while (idx < end) {
      header.closure.process(acc, idx)
      idx += 1
    }
    acc
  }

}
