package ppl.delite.runtime.codegen

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import java.io.PrintWriter
import collection.mutable.ArrayBuffer

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
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("sun.boot.class.path")
    }
    
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    settings.nowarn.value = true
    //settings.unchecked.value = true
    //settings.verbose.value = true

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }

  def addSource(source: String) {
    sourceBuffer += source
  }

  def addSourcePath(path: String) {
    pathBuffer += path
  }

  def compile: ClassLoader = {
    val sources = sourceBuffer.toArray
    sourceBuffer.clear
    val paths = pathBuffer.toArray
    pathBuffer.clear
    compile(sources, paths)
  }

  def compile(sources: Array[String], paths: Array[String]): ClassLoader = {
    if (this.compiler eq null)
      setupCompiler()

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //compiler.genJVM.outputDir = fileSystem

    var sourceFiles: List[SourceFile] = Nil
    for (i <- 0 until sources.length) {
      val file = new BatchSourceFile(new VirtualFile("source"+i) { 
        override def container = AbstractFile.getDirectory(".")   // bug in scalac...
      }, sources(i).toArray)
      sourceFiles = file :: sourceFiles
    }

    for (i <- 0 until paths.length) {
      val file = new BatchSourceFile(AbstractFile.getFile(paths(i)))
      sourceFiles = file :: sourceFiles
    }

    run.compileSources(sourceFiles)
    reporter.printSummary()

    if (reporter.hasErrors) system.error("Compilation Failed")

    reporter.reset
    //output.reset

    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)
    loader
  }

  def printSources {
    for (i <- 0 until sourceBuffer.length) {
      print(sourceBuffer(i))
      print("\n /*********/ \n \n")
    }
  }

}