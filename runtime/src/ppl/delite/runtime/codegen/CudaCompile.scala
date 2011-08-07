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

object CudaCompile extends CodeCache {

  val binCacheHome = cacheHome + "bin" + File.separator + "runtime" + File.separator

  private val sourceBuffer = new ArrayBuffer[(String, String)]

  def target = "cuda"

  override def ext = "cu"

  def addSource(source: String, name: String) {
    if (!sourceBuffer.contains((source, name)))
      sourceBuffer += Pair(source, name)
  }

  def compile() {
    if (sourceBuffer.length == 0) return
    cacheRuntimeSources(sourceBuffer.toArray)

    val paths = modules.map(m => Path(sourceCacheHome + m.name).path).toArray

	for (src <- sourceBuffer)
		compile(binCacheHome, sourceCacheHome + "runtime" + File.separator + src._2 + ".cu", paths)
	
	sourceBuffer.clear()
  }

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

  private def linkGeneratedLibs(source: String): List[String] = {
    var linkLibs = List[String]()
    val libs = Directory(Path(source))
    for (file <- libs.files) {
      val name = file.stripExtension
      if (name.startsWith("lib")) {
        linkLibs = linkLibs :+ "-l"+name.drop(3)
      }
    }
    linkLibs
  }

  private def checkError(process: Process) {
    val errorStream = process.getErrorStream
    var err = errorStream.read()
    if (err != -1) {
      while (err != -1) {
        print(err.asInstanceOf[Char])
        err = errorStream.read()
      }
      println()
      error("nvcc compilation failed")
    }
  }

  def printSources() {
    for (i <- 0 until sourceBuffer.length) {
      print(sourceBuffer(i))
      print("\n /*********/ \n \n")
    }
  }

}
