package ppl.delite.runtime.codegen

import ppl.delite.runtime.data.Data
import java.util.concurrent.locks.ReentrantLock

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
    hostGPUDevice
  }

  /**
   * The gpu host thread will be executed in a JNI call in order to use CUDA
   * native C code is generated as illustrated in the example below
   */
  @native def hostGPUDevice: Unit

  System.loadLibrary("cuda") //link the compiled native code

  /**
   * An example of the code generated to manage the GPU device; this example is written in Scala for simplicity
   * An actual implementation should consider generating C code instead (one long JNI call rather than many small ones)
   */
  private object HostGPUDeviceExample

  /**
   *  An accessor for a CPU thread to retrieve data from the GPU device
   */
  def get1: Data[Int] = Res1.get

  private object Res1 {

    private var notReady: Boolean = true
    private var res: Data[Int] = _

    private val lock = new ReentrantLock
    private val cond = lock.newCondition

    /**
     * getter: used to pass result to other threads
     * implementation: a blocking read
     */

    def get: Data[Int] = {
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
    def set(result: Data[Int]) {
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
