package ppl.delite.runtime.executor

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.OS

/**
 * Author: Kevin J. Brown
 * Date: 12/14/10
 * Time: 12:42 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class GPUExecutionThread(deviceNum: Int) extends ExecutionThread {

  override def run {
    initializeDevice(deviceNum)
    super.run
  }

  @native def initializeDevice(deviceNum: Int): Unit

  load()

  def load() {
    val sep = System.getProperty("file.separator")

    if(Config.useOpenCL) {
      System.load(Config.deliteHome + sep + "runtime" + sep + "opencl" + sep + "openclInit." + OS.libExt)
    }
    else
      System.load(Config.deliteHome + sep + "runtime" + sep + "cuda" + sep + "cudaInit." + OS.libExt)
  }

}
