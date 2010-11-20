package ppl.delite.runtime.runtime.executor.gpu

/**
 * Author: Kevin J. Brown
 * Date: Nov 17, 2010
 * Time: 3:41:51 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Methods for calling CUDA driver functions from the JVM
 */
object CudaDriver {

  //load native libraries
  System.loadLibrary("cuda")

  //global device initialization
  val driver = new CudaDriver
  driver.init

  def numDevices = driver.numDevices

  def createStream = driver.createStream
  
}

//NOTE: all the native methods are put in a class with the singleton instance created manually above simply to please javah
//constructor is private so only the companion object can create an instance
class CudaDriver private {

  //initialization methods
  @native def init: Unit //initialize CUDA: should only be called once and must be called before anything else

  @native def initializeDevice(deviceIdx: Int): Long

  @native def destroyContext(deviceIdx: Int): Unit //TODO: do we need this?

  @native def numDevices: Int

  @native def createStream: Long

  @native def destroyStream(stream: Long): Unit //TODO: do we need this?

  @native def awaitEvent(stream: Long, event: Long)

  @native def createEvent //TODO: to be used along with awaitEvent (set flags appropriately in CUDA); recordEvent; is our usage simple enough to know when to call destroy in the CUDA code?

  @native def getFunction(module: Long, str: String): Long


  //memory allocation methods
  @native def memAlloc(size: Int): Long

  @native def memFree(devicePtr: Long): Unit

  @native def memAllocHost(size: Int): Long

  @native def memHostGetDevicePointer(hostPtr: Long): Long


  //asynchronous memory copy operations
  @native def memCopyHostToDeviceAsync(devicePtr: Long, hostArray: Array[Double], offset: Int, count: Int, hostMem: Long, stream: Long): Unit

  @native def memCopyDeviceToHostAsync(hostArray: Array[Double], devicePtr: Long, offset: Int, count: Int, hostMem: Long, stream: Long): Unit

  @native def memCopyHostToDeviceAsync(devicePtr: Long, hostArray: Array[Float], offset: Int, count: Int, hostMem: Long, stream: Long): Unit

  @native def memCopyDeviceToHostAsync(hostArray: Array[Float], devicePtr: Long, offset: Int, count: Int, hostMem: Long, stream: Long): Unit

  @native def memCopyHostToDeviceAsync(devicePtr: Long, hostArray: Array[Int], offset: Int, count: Int, hostMem: Long, stream: Long): Unit

  @native def memCopyDeviceToHostAsync(hostArray: Array[Int], devicePtr: Long, offset: Int, count: Int, hostMem: Long, stream: Long): Unit

  @native def memCopyDevicetoDevice(dstPtr: Long, srcPtr: Long, size: Int): Unit

}
