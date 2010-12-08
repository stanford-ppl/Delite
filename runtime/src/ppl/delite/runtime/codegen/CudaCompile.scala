package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import java.io.{FileWriter, FileOutputStream, File}

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

  def addSource(source: String) {
    sourceBuffer += source
  }

  def compile(path: String) {
    if (sourceBuffer.length == 0) return
    val source = sourceBuffer(0)
    sourceBuffer.clear
    compile(source, path)
  }

  //TODO: handle more than one source
  def compile(source: String, path: String) {
    val write = new FileWriter(path+"cuda/cudaHost.cu")
    write.write(source)
    write.close

    //TODO: this is fragile: requires a specific linux setup
    val process = Runtime.getRuntime.exec(Array[String](
      "nvcc",
      "-I$JDK_HOME/include", "-I$JDK_HOME/include/linux", //jni
      "-O2", //optimized
      "-shared", "-Xcompiler", "\'-fPIC\'", //dynamic shared library
      "-o", "cudaHost.so", //output name
      "cudaHost.cu" //input name
      ), Array[String](), new File(path+"cuda/"))

    process.waitFor //wait for compilation to complete
    val first = process.getErrorStream.read
    if (first != -1) { //compilation failed
      val errorBuffer = new Array[Byte](1000)
      val num = process.getErrorStream.read(errorBuffer)
      print(first.asInstanceOf[Char])
      for (i <- 0 until num) print(errorBuffer(i).asInstanceOf[Char])
      println()
      error("nvcc compilation failed")
    }
  }

  def printSources {
    for (i <- 0 until sourceBuffer.length) {
      print(sourceBuffer(i))
      print("\n /*********/ \n \n")
    }
  }

}
