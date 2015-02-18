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

abstract class NativeExecutionThread(threadId: Int, numThreads: Int) extends ExecutionThread {

  override def run {
    initializeThread(threadId, numThreads)
    super.run
  }

  def initializeThread(threadId: Int, numThreads:Int): Unit

  protected def loadNative(fileName: String, compiler: CCompile) = {
    val sep = File.separator
    val root = compiler.staticResources + fileName
    val path = root + "." + OS.libExt
    val lib = new File(path)
    if (!lib.exists) compiler.compile(path,Array(root+"."+compiler.ext))
    System.load(path) //TODO: doesn't work properly with sbt test suite
  }

}

class CppExecutionThread(threadId: Int, numThreads: Int) extends ExecutionThread

class PinnedExecutionThread(threadId: Int, numThreads: Int) extends NativeExecutionThread(threadId, numThreads) {
  @native def initializeThread(threadId: Int, numThreads: Int): Unit
  loadNative("cppInit", CppCompile)
}

class CudaExecutionThread(threadId: Int, numThreads: Int) extends NativeExecutionThread(threadId, numThreads) {
  @native def initializeThread(threadId: Int, numThreads:Int): Unit
  loadNative("cudaInit", CudaCompile)
}

class OpenCLExecutionThread(threadId: Int, numThreads: Int) extends NativeExecutionThread(threadId, numThreads) {
  @native def initializeThread(threadId: Int, numThreads:Int): Unit
  loadNative("openclInit", OpenCLCompile)
}
