package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.Config
import xml.XML
import java.io.FileNotFoundException
import tools.nsc.io.{Directory, Path, File}
import ppl.delite.runtime.graph.targets.OS

trait CCompile extends CodeCache {
  
  protected class CompilerConfig(
    val compiler: String, 
    val headerDir: Array[String],
    val sourceHeader: Array[String],
    val libs: Array[String],
    val headerPrefix: String, 
    val libPrefix: String
  )

  protected def configFile: String // name of file, will always be searched for inside config/delite/
  protected def compileFlags: Array[String] // machine-independent flags that are always passed to the compiler for this lib
  protected def outputSwitch: String // compiler parameter that allows us to specify destination dir (e.g. -o)
  protected lazy val config = loadConfig(configFile)
  
  val binCacheHome = cacheHome + "bin" + sep + "runtime" + sep

  private val headerBuffer = new ArrayBuffer[(String, String)]

  def headers = headerBuffer.map(_._2)

  protected def deliteLibs = Config.deliteBuildHome + sep + "libraries" + sep + target

  def addHeader(source: String, name: String) {
    if (!headerBuffer.contains((source, name+".h")))
      headerBuffer += Pair(source, name+".h")
  }

  protected def loadConfig(f: String): CompilerConfig = {
    // parse XML, return configuration
    val configFile = File(Config.deliteHome + sep + "config" + sep + "delite" + sep + f)
    if (!configFile.isValid) throw new FileNotFoundException("could not load compiler configuration: " + configFile)

    val body = XML.loadFile(configFile.jfile)
    val compiler = (body \\ "compiler").text.trim
    val headerPrefix = (body \\ "headers" \ "prefix").text.trim
    val headerDir = body \\ "headers" flatMap { e => val prefix = e \ "prefix"; e \\ "path" map { prefix.text.trim + _.text.trim } } toArray
    val sourceHeader = body \\ "headers" flatMap { e => e \\ "include" map { _.text.trim } } toArray
    val libPrefix = (body \\ "libs" \ "prefix").text.trim
    val libs = body \\ "libs" flatMap { e => val prefix = e \ "prefix"; (e \\ "path").map(p => prefix.text.trim + p.text.trim) ++ (e \\ "library").map(l => l.text.trim) } toArray

    new CompilerConfig(compiler, headerDir, sourceHeader, libs, headerPrefix, libPrefix)
  }

  def compile() {
    if (sourceBuffer.length == 0) return
    cacheRuntimeSources((sourceBuffer ++ headerBuffer).toArray)
    
    if (modules.exists(_.needsCompile)) {
      val includes = modules.map(m => config.headerPrefix + sourceCacheHome + m.name).toArray
      val libs = Directory(deliteLibs).files.withFilter(f => f.extension == OS.libExt).map(_.path).toArray
      val paths = includes ++ config.headerDir ++ Array(config.headerPrefix + "runtime" + sep + target) ++ config.libs ++ libs
      val sources = sourceBuffer.map(s => sourceCacheHome + "runtime" + sep + s._2).toArray
      val dest = binCacheHome + target + "Host." + OS.libExt

      compile(dest, sources, paths)
    }
    sourceBuffer.clear()
    headerBuffer.clear()
  }

  def compile(destination: String, sources: Array[String], paths: Array[String]) {
    Path(destination).parent.createDirectory()
    val output = Array(outputSwitch, destination)
    val args = Array(config.compiler) ++ paths ++ compileFlags ++ output ++ sources
    
    val process = Runtime.getRuntime.exec(args)
    process.waitFor
    checkError(process, args)
  }
  
  def compileInit() {
    val root = Config.deliteHome + sep + "runtime" + sep + target + sep + target + "Init."
    val source = root + ext
    val dest = root + OS.libExt
    compile(dest, Array(source), config.headerDir)
  }

  protected def checkError(process: Process, args: Array[String]) {
    val errorStream = process.getErrorStream
    val inputStream = process.getInputStream

    var err = errorStream.read()
    if (err != -1) {
      println("--" + target + " compile args: " + args.mkString(","))
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
      sys.error(target + " compilation failed with exit value " + process.exitValue)
  }

}
