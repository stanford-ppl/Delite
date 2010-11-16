package ppl.delite.walktime.codegen

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

  private def setupCompiler() = {

    val settings = new Settings()

    settings.classpath.value = System.getProperty("java.class.path")
    settings.bootclasspath.value = System.getProperty("sun.boot.class.path")
    
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    //settings.verbose.value = true

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }

  def addSource(source: String) {
    sourceBuffer += source
  }

  def compile: ClassLoader = {
    val sources = sourceBuffer.toArray
    sourceBuffer.clear
    compile(sources)
  }

  def compile(sources: Array[String]): ClassLoader = {
    if (this.compiler eq null)
      setupCompiler()

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //compiler.genJVM.outputDir = fileSystem

    var sourceFiles: List[SourceFile] = Nil
    for (i <- 0 until sources.length) {
      val file = new BatchSourceFile("source"+i, sources(i))
      sourceFiles = file :: sourceFiles
    }

    run.compileSources(sourceFiles)
    reporter.printSummary()

    if (reporter.hasErrors) error("Compilation Failed")

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