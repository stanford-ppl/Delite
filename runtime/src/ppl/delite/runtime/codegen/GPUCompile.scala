package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import java.io.File
import ppl.delite.runtime.Config
import tools.nsc.io.{Directory, Path}

trait GPUCompile extends CodeCache {

  val binCacheHome = cacheHome + "bin" + File.separator + "runtime" + File.separator

  private val sourceBuffer = new ArrayBuffer[(String, String)]

  //def cmdString: Array[String]

  val sep = File.separator
  //figure out where the jni header files are for this machine
  val javaHome = System.getProperty("java.home")

  def deliteHome = Config.deliteHome
  def deliteLibs = Config.deliteBuildHome + sep + "libraries" + sep + target

  def addSource(source: String, name: String) {
    if (!sourceBuffer.contains((source, name)))
      sourceBuffer += Pair(source, name)
  }

  def compile() {
    if (sourceBuffer.length == 0) return

    cacheRuntimeSources(sourceBuffer.toArray)

    val paths = modules.map(m => Path(sourceCacheHome + m.name).path).toArray

	  for (src <- sourceBuffer) {
		  compile(binCacheHome, sourceCacheHome + "runtime" + File.separator + src._2 + "." + ext, paths)
    }
    sourceBuffer.clear()
  }

  def compile(destination: String, source: String, paths: Array[String])

  def compileInit(): Unit

  def printSources() {
    for (i <- 0 until sourceBuffer.length) {
      print(sourceBuffer(i))
      print("\n /*********/ \n \n")
    }
  }

  def checkError(process: Process) {
    val errorStream = process.getErrorStream
    val inputStream = process.getInputStream

    var err = errorStream.read()
    if (err != -1) {
      while (err != -1) {
        print(err.asInstanceOf[Char])
        err = errorStream.read()
      }
      println()
    }

    var in = inputStream.read()
    if (in != -1) {
      while (in != -1) {
        print(in.asInstanceOf[Char])
        in = inputStream.read()
      }
      println()
    }

    if (process.exitValue != 0)
      error(target + " host compilation failed with exirValue " + process.exitValue)
  }


  //TODO: Needs to be different for windows
  protected def linkGeneratedLibs(source: String): List[String] = {
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

}