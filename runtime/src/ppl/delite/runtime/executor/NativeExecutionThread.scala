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

class NativeExecutionThread(threadId: Int, numThreads: Int) extends ExecutionThread {

  override def run {
    initializeThread(threadId, numThreads)
    super.run
  }

  @native def initializeThread(threadId: Int, numThreads:Int): Unit

  protected def loadNative(fileName: String, compiler: CCompile) = {
    val sep = File.separator
    val root = compiler.staticResources + fileName
    val path = root + "." + OS.libExt
    val lib = new File(path)
    if (!lib.exists) compiler.compileInit(root)
    System.load(path)
  }

}

class CppExecutionThread(threadId: Int, numThreads: Int) extends NativeExecutionThread(threadId, numThreads) {
  loadNative("Config", CppCompile)
}

class CudaExecutionThread(threadId: Int, numThreads: Int) extends NativeExecutionThread(threadId, numThreads) {
  loadNative("cudaInit", CudaCompile)
}

class OpenCLExecutionThread(threadId: Int, numThreads: Int) extends NativeExecutionThread(threadId, numThreads) {
  loadNative("openclInit", OpenCLCompile)
}
