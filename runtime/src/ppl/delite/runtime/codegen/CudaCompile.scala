package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 9:39:10 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object CudaCompile {

  private val sourceBuffer = new ArrayBuffer[String]
  private val pathBuffer = new ArrayBuffer[String]

  def addSource(source: String) {
    sourceBuffer += source
  }

  def addSourcePath(path: String) {
    pathBuffer += path
  }

  def compile {
    if (sourceBuffer.length == 0) return //escape if no sources to compile
    val sources = sourceBuffer.toArray
    sourceBuffer.clear
    val paths = pathBuffer.toArray
    pathBuffer.clear
    compile(sources, paths)
  }

  def compile(sources: Array[String], paths: Array[String]) {
    //TODO: this is fragile: requires a specific linux setup
    for (i <- 0 until sources.length) {
      val process = Runtime.getRuntime.exec(Array[String](
        "nvcc",
        "-I{java.dir}/include", "-I{java.dir}/include/linux", //jni
        "-O2", //optimized
        "-shared", "-Xcompiler", "\'-fPIC\'", //dynamic shared library
        "-o", "cudaHost"+i+".so", //output name
        "cudaHost"+i+".cpp" //input name
        ))
      if (process.getErrorStream.read != -1) error("nvcc compilation failed")
    }

    //TODO: do we want to put the loop inside instead?
    for (i <- 0 until paths.length) {
      val process = Runtime.getRuntime.exec(Array[String](
        "nvcc",
        "-O2", //optimized
        "-ptx", //generate ptx
        "-o kernels", //output name
        "kernels.cu" //input name
        ))
      if (process.getErrorStream.read != -1) error("nvcc compilation failed")
    }
  }

  def printSources {
    for (i <- 0 until sourceBuffer.length) {
      print(sourceBuffer(i))
      print("\n /*********/ \n \n")
    }
  }

}
