package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import java.io.File
import ppl.delite.runtime.Config
import tools.nsc.io.{Directory, Path}

trait GPUCompile extends CodeCache {

  val binCacheHome = cacheHome + "bin" + File.separator + "runtime" + File.separator

  private val sourceBuffer = new ArrayBuffer[(String, String)]
  private val headerBuffer = new ArrayBuffer[(String, String)]

  def sources = sourceBuffer.map(s => s._2)
  def headers = headerBuffer.map(s => s._2)

  val sep = File.separator
  //figure out where the jni header files are for this machine
  val javaHome = System.getProperty("java.home")

  def deliteHome = Config.deliteHome
  def deliteLibs = Config.deliteBuildHome + sep + "libraries" + sep + target

  def addHeader(source: String, name: String) {
    if (!headerBuffer.contains((source, name+".h")))
      headerBuffer += Pair(source, name+".h")
  }
  def addSource(source: String, name: String) {
    if (!sourceBuffer.contains((source, name+"."+ext)))
      sourceBuffer += Pair(source, name+"."+ext)
  }

  def compile() {
    if (sourceBuffer.length == 0) return

    cacheRuntimeSources((sourceBuffer++headerBuffer).toArray)

    val paths = modules.map(m => Path(sourceCacheHome + m.name).path).toArray
    
    val sources = sourceBuffer.map(s => sourceCacheHome + "runtime" + File.separator + s._2).mkString(" ")
    compile(binCacheHome, sources, paths)
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
      error(target + " host compilation failed with exit value " + process.exitValue)
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
