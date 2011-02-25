package ppl.delite.runtime.codegen

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.Config
import java.io.{FileWriter, PrintWriter}

object ScalaCompile {

  private var compiler: Global = _
  private var reporter: ConsoleReporter = _

  private val sourceBuffer = new ArrayBuffer[String]
  private val pathBuffer = new ArrayBuffer[String]

  private def setupCompiler() = {

    val settings = new Settings()

    //settings.classpath.value = System.getProperty("java.class.path")
    //settings.bootclasspath.value = System.getProperty("sun.boot.class.path")
    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(java.io.File.pathSeparator)
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(java.io.File.pathSeparator)
      case _ => System.getProperty("sun.boot.class.path")
    }
    
    settings.encoding.value = "UTF-8"
    settings.outdir.value = Config.codeCacheHome + java.io.File.separator + "scala"
    settings.extdirs.value = ""
    settings.nowarn.value = true
    //settings.unchecked.value = true
    //settings.verbose.value = true

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }

  def addSource(source: String) {
    if (!sourceBuffer.contains(source)) //avoid duplicate kernels //TODO: there must be a better way
      sourceBuffer += source
  }

  def addSourcePath(path: String) {
    pathBuffer += path
  }

  def compile: ClassLoader = {
    val sources = sourceBuffer.toArray
    sourceBuffer.clear
    val paths = (pathBuffer ++ writeSources(sources)).toArray
    pathBuffer.clear
    compile(paths)
  }

  def writeSources(sources: Array[String]) = {
    val sep = File.separator
    //not much of a cache at the moment
    val path = Directory(Path(Config.codeCacheHome + sep + "scala"))
    path.deleteRecursively() //TODO: dangerous: assumes code cache not mixed with any other files
    path.createDirectory()

    val paths = new Array[String](sources.length)
    for (i <- 0 until sources.length) {
      val sourcePath = path.toString + sep + "source" + i + ".scala"
      paths(i) = sourcePath
      val write = new FileWriter(sourcePath)
      write.write(sources(i))
      write.close()
    }
    paths
  }

  def compile(paths: Array[String]): ClassLoader = {
    if (this.compiler eq null)
      setupCompiler()

    val compiler = this.compiler
    val run = new compiler.Run

    //val fileSystem = new VirtualDirectory("<vfs>", None)
    //compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //compiler.genJVM.outputDir = fileSystem

    val sourceFiles = paths.toList map { path => new BatchSourceFile(AbstractFile.getFile(path)) }
    run.compileSources(sourceFiles)
    reporter.printSummary()

    if (reporter.hasErrors) error("Compilation Failed")

    reporter.reset
    //output.reset

    val loader = new AbstractFileClassLoader(AbstractFile.getDirectory(Path(Config.codeCacheHome + File.separator + "scala")), this.getClass.getClassLoader)
    loader
  }

  def printSources {
    for (i <- 0 until sourceBuffer.length) {
      print(sourceBuffer(i))
      print("\n /*********/ \n \n")
    }
  }

}