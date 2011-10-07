package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import java.io.File
import ppl.delite.runtime.Config
import tools.nsc.io.{Directory, Path}
import ppl.delite.runtime.graph.targets.OS

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 9:39:10 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object CudaCompile extends GPUCompile {

  override def target = "cuda"
  override def ext = "cu"

  //TODO: handle more than one runtime object
  def compile(destination: String, source: String, paths: Array[String]) {
    Directory(Path(destination)).createDirectory()

    val cmdString = Array[String](
      "nvcc",
      "-w", //suppress warnings
      "-I" + javaHome + sep + ".." + sep + "include" + "," + javaHome + sep + ".." + sep + "include" + sep + OS.jniMD, //jni
      "-I" + paths.mkString(","),
      "-I" + deliteHome + sep + "runtime" + sep + "cuda",
      "-O2", //optimized
      "-arch", "compute_20",
      "-code", "sm_20",
      "-shared", "-Xcompiler", "\'-fPIC\'", //dynamic shared library
      "-L" + deliteLibs) ++ linkGeneratedLibs(deliteLibs) ++ Array[String](
      "-o", "cudaHost.so", //output name
      source //input name
      )
    //println("cmd is " + cmdString.mkString(","))
    val process = Runtime.getRuntime.exec(cmdString, null, new File(destination))
    process.waitFor //wait for compilation to complete
    checkError(process)
  }

  def compileInit() {
    val cmdString = Array[String](
      "nvcc",
      "-w", //suppress warnings
      "-I" + javaHome + sep + ".." + sep + "include" + "," + javaHome + sep + ".." + sep + "include" + sep + OS.jniMD, //jni
      "-arch", "compute_20",
      "-code", "sm_20",
      "-shared", "-Xcompiler", "\'-fPIC\'", //dynamic shared library
      "-o", Config.deliteHome + sep + "runtime" + sep + "cuda" + sep + "cudaInit.so", //output name
      Config.deliteHome + sep + "runtime" + sep + "cuda" + sep + "cudaInit.cu"
      )
    val process = Runtime.getRuntime.exec(cmdString, null, new File(Config.deliteHome))
    process.waitFor //wait for compilation to complete
    checkError(process)
  }

}
