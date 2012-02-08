package ppl.delite.runtime.executor

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.OS
import ppl.delite.runtime.codegen.{CCompile, CudaCompile, OpenCLCompile}
import java.io.File

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

  //load gpu init library (and create it if absent)
  private def load() {
    def loadGPU(target: String, compiler: CCompile) = {
      val sep = File.separator
      val path = Config.deliteHome + sep + "runtime" + sep + target + sep + target + "Init." + OS.libExt
      val lib = new File(path)
      if (!lib.exists)
        compiler.compileInit()
      System.load(path)
    }

    if (Config.useOpenCL)
      loadGPU("opencl", OpenCLCompile)
    else
      loadGPU("cuda", CudaCompile)
  }

}
