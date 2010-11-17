package ppl.delite.walktime.codegen

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import java.io.PrintWriter

object ScalaCompile {

  var compiler: Global = _
  var reporter: ConsoleReporter = _

  def setupCompiler() = {

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

  def compile(sources: Array[String]) = {
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
}