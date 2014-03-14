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

class AccExecutionThread(deviceNum: Int) extends ExecutionThread {

  override def run {
    if (deviceNum >= Config.numCpp)
      initializeDevice(deviceNum)
    super.run
  }

  @native def initializeDevice(deviceNum: Int): Unit

  loadSlave()

  //load gpu init library (and create it if absent)
  private def loadSlave() {
    def loadGPU(target: String, compiler: CCompile) = {
      val sep = File.separator
      val path = compiler.staticResources + target + "Init." + OS.libExt
      val lib = new File(path)
      if (!lib.exists)
        compiler.compileInit()
      System.load(path)
    }

    if (deviceNum >= Config.numCpp && deviceNum < Config.numCpp + Config.numCuda) {
      loadGPU("cuda", CudaCompile)
    }
    else if (deviceNum >= Config.numCpp + Config.numCuda) {
      loadGPU("opencl", OpenCLCompile)
    }
    //else
    //  throw new RuntimeException("Cannot load unknown device ID " + deviceNum)
  }

}
