package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import java.io.File
import ppl.delite.runtime.Config
import tools.nsc.io.{Directory, Path}

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 9:39:10 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object CudaCompile extends GPUCompile {

  def target = "cuda"
  override def ext = "cu"

  //TODO: handle more than one runtime object
  def compile(destination: String, source: String, paths: Array[String]) {
    Directory(Path(destination)).createDirectory()

    val sep = File.separator
    //figure out where the jni header files are for this machine
    val javaHome = System.getProperty("java.home")
    val os = System.getProperty("os.name")
    val suffix =
      if (os.contains("Linux")) "linux"
      else if (os.contains("Windows")) "win32"
      //else if (os.contains("Mac")) "??"
      else error("OS " + os + " not currently supported with CUDA")

    val deliteHome = Config.deliteHome
    val deliteLibs = Config.deliteBuildHome + sep + "libraries"

    val process = Runtime.getRuntime.exec(Array[String](
      "nvcc",
      "-w", //suppress warnings
      "-I" + javaHome + sep + ".." + sep + "include" + "," + javaHome + sep + ".." + sep + "include" + sep + suffix, //jni
      "-I" + paths.mkString(","),
      "-I" + deliteHome + sep + "runtime" + sep + "cuda",
      "-O2", //optimized
      "-arch", "compute_20",
      "-code", "sm_20",
      "-shared", "-Xcompiler", "\'-fPIC\'", //dynamic shared library
      "-L" + deliteLibs)
      ++ linkGeneratedLibs(deliteLibs) ++ Array[String](
      "-o", "cudaHost.so", //output name
      source //input name
      ), null, new File(destination))

    process.waitFor //wait for compilation to complete
    checkError(process)
  }


}
