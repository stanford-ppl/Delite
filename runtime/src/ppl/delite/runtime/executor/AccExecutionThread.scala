package ppl.delite.runtime.executor

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.OS
import ppl.delite.runtime.codegen.{CCompile, CppCompile, CudaCompile, OpenCLCompile}
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
    if (deviceNum < Config.numCpp) initializeThread(deviceNum, Config.numCpp)
    else initializeDevice(deviceNum)
    super.run
  }

  @native def initializeThread(threadId: Int, numThreads:Int): Unit
  @native def initializeDevice(deviceNum: Int): Unit

  initNative()

  //load native init library (and create it if absent)
  private def initNative() {
    def loadNative(target: String, fileName: String, compiler: CCompile) = {
      val sep = File.separator
      val root = compiler.staticResources + fileName
      val path = root + "." + OS.libExt
      val lib = new File(path)
      if (!lib.exists) compiler.compileInit(root)
      System.load(path)
    }

    if (deviceNum < Config.numCpp) {
      loadNative("cpp", "Config", CppCompile)  
    }
    if (deviceNum >= Config.numCpp && deviceNum < Config.numCpp + Config.numCuda) {
      loadNative("cuda", "cudaInit", CudaCompile)
    }
    else if (deviceNum >= Config.numCpp + Config.numCuda) {
      loadNative("opencl", "openclInit", OpenCLCompile)
    }
  }

}
