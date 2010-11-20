package ppl.delite.runtime.runtime.executor.gpu

import ppl.delite.runtime.walktime.codegen.DeliteExecutable

/**
 * Author: Kevin J. Brown
 * Date: Nov 17, 2010
 * Time: 5:38:27 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class ExecutionStream {

  private val nativeStream = CudaDriver.createStream

  /**
   * Add the executable to the corresponding CUDA stream
   */
  def put(work: DeliteExecutable) {
      
  }

}