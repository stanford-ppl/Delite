package ppl.delite.runtime.codegen.examples

import ppl.delite.runtime.data.Data
import java.util.concurrent.locks.ReentrantLock
import ppl.delite.runtime.codegen.DeliteExecutable

/**
 * Author: Kevin J. Brown
 * Date: Nov 28, 2010
 * Time: 10:53:51 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * This is an example of what a generated DeliteExecutable that hosts a gpu device should look like; it should not be invoked
 *
 */

object ExampleGPUExecutable extends DeliteExecutable {

  def run() {
    hostGPU
  }

  /**
   * The gpu host thread will be executed in a JNI call in order to use CUDA
   * native C code is generated as illustrated in the example below
   */
  @native def hostGPU : Unit

  System.loadLibrary("cuda") //link the compiled native code

  /**
   * An example of the code generated to manage the GPU device; this example is written in Scala for simplicity
   * An actual implementation should consider generating C code instead (one long JNI call rather than many small ones)
   */
  //TODO: this example currently only supports one kernel stream: supporting multiple streams requires more thought
  private trait HostGPUDeviceExample  {
    val stream_0 = 0 //stream 0 = kernel stream
    val stream_1 = 1 //stream 1 = h2d stream
    val stream_2 = 2 //stream 2 = d2h stream

    //a "C" on a variable name/type signifies data resides in CPU (java) memory, a "G" signifies resides in GPU memory

    def hostGPUDevice {
      val x1g: GData[Float] = outputAlloc1() //allocate device memory for the kernel's output
      kernel1(x1g) //all kernels return Unit, OP output is always passed as the kernel's first input

      val x2g: GData[Float] = outputAlloc2(x1g) //DSL constructor
      kernel2(x2g, x1g)

      val x99c: CInt = get99 //get result from a CPU thread
      val x99g: GInt = toInt(x99c)  //for {Int,Float} we can retrieve the value and simply pass into kernel launch

      val x42c: CData[Float] = get42 //blocks until ready

      //copy
      val x42g: GData[Float] = inputAlloc_Data(x42c) //copies object from java memory to pinned host memory, then host memory to device memory (provided by DSL since object structure is unknown)

      //sync kernel launch with input copy completion
      streamWaitEvent(stream_0, record(newEvent,stream_1)) //kernel_stream_0 waits until h2d_stream_1 records complete

      val x3g: GFloat = deviceMalloc() //for primitive types Delite can handle the malloc (no DSL constructor)
      kernel3(x3g, x2g, x42g, x99g) //launch kernel (all inputs now on device or can be passed in)

      //sync copy output back with kernel completion
      streamWaitEvent(stream_2, record(newEvent,stream_0)) //d2h_stream_2 waits until kernel stream records complete

      //copy
      val x3c : CFloat = outputSet_Float(x3g) //copies object from device memory to pinned host memory, then host memory to java memory (must block until d2h copy completes)

      set3(x3c) //set result as available on CPU

      free(x1g) //frees could be inserted intelligently through liveness analysis (right after last consumer)
      free(x2g)
      free(x3g)
      free(x42g)
      
    }

    def set3(result: CFloat): Unit
      //jvm.GPUExecutable0.set3(result) //run the java-side setter

    def get99: CInt //blocking retrieve of Int
      //jvm.Executable0.get99 //run the cpu getter

    def get42: CData[Float] //blocking retrieve of reference to java object
      //jvm.Executable1.get42 //run the cpu getter

    //below are compile stubs, should not be generated
    type CData[T]
    type CFloat
    type CInt
    type GData[T]
    type GFloat
    type GInt
    type Event

    def outputAlloc1(): GData[Float]
    def outputAlloc2(in: GData[Float]): GData[Float]

    def kernel1(out: GData[Float]): Unit
    def kernel2(out: GData[Float], in0: GData[Float]): Unit
    def kernel3(out: GFloat, in0: GData[Float], in1: GData[Float], in2: GInt): Unit

    def deviceMalloc(): GFloat
    def free(ref: Any): Unit

    def toInt(in: CInt): GInt
    def inputAlloc_Data(in: CData[Float]): GData[Float]
    def outputSet_Float(in: GFloat): CFloat

    def streamWaitEvent(stream: Int, event: Event): Unit
    def record(event: Event, stream: Int): Event
    def newEvent: Event
  }

  /**
   *  An accessor for a CPU thread to retrieve data from the GPU device
   */
  def get3: Float = Res3.get
  def set3(result: Float) = Res3.set(result)

  private object Res3 {

    private var notReady: Boolean = true
    private var res: Float = _

    private val lock = new ReentrantLock
    private val cond = lock.newCondition

    /**
     * getter: used to pass result to other threads
     * implementation: a blocking read
     */

    def get: Float = {
      if (notReady) block //try to avoid locking
      res
    }

    private def block {
      lock.lock
      try {
        while (notReady) cond.await
      }
      finally {
        lock.unlock
      }
    }

    /**
     * setters: used to set results as complete so they can be passed to other threads
     * implementation: store result and signal ready
     */
    def set(result: Float) {
      lock.lock
      try {
        res = result
        notReady = false
        cond.signalAll //signalling no one should be cheap (null check)
      }
      finally {
        lock.unlock
      }
    }

  }

}