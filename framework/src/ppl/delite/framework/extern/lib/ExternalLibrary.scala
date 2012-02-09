package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import java.io._
import scala.xml._

class ExternalLibConfiguration (
  val compiler: String,
  val headerDir: List[String],
  val sourceHeader: List[String],
  val libs: List[String]
)

trait ExternalLibrary {
  //val target: Target
  val libName: String
  val ext: String // native file extension (can this ever be anything besides .c or .cpp??)
  val libExt: String // library file extension (dynamic: .so, static: .a)
  val configFile: String // name of file, will always be searched for inside extern/src/ppl/delite/extern/lib/config
  def compileFlags: List[String] // machine-independent flags that are always passed to the compiler for this lib
  val outputSwitch: String // compiler parameter that allows us to specify destination dir (e.g. -o)
  lazy val header: String = "" // an optional header to be included at the top of every generated call for this lib

  lazy val name = "lib" + libName // generated scala interface for this library will use this as its object name

  /**
   * machine dependent, sourced from XML configuration 
   */  
  lazy val config = loadConfig(configFile)
  lazy val compiler = config.compiler
  lazy val headerDir = config.headerDir
  lazy val libs = config.libs
  lazy val configHeader = config.sourceHeader //additional user-specified headers

  protected def sep = File.separator
  
  def compile(src: String, destDir: String) {
    val srcFile = new File(src)
    if (!srcFile.exists) throw new FileNotFoundException("source file does not exist: " + src)
    
    // invoke the compiler using Runtime.exec
    val buildPath = new File(Config.buildDir, "scala" + sep + "kernels")
    val destPath = new File(destDir, sep + name + "." + libExt)
    val outputFlags = List(outputSwitch, destPath.toString)

    // this call is based on the gcc/icc invocation signature.. do we need to generalize it?
    val args = Array(compiler) ++ headerDir ++ libs ++ compileFlags ++ outputFlags ++ Array(srcFile.toString)
    val process = Runtime.getRuntime.exec(args, null, buildPath)
    process.waitFor
    checkError(process, args)
  }

  private def checkError(process: Process, args: Array[String]) {
    val errorStream = process.getErrorStream
    var err = errorStream.read()
    if (err != -1) {
      println("--external compile args: " + (args mkString ","))
      while (err != -1) {
        print(err.asInstanceOf[Char])
        err = errorStream.read()
      }
      println()
      sys.error("external library compilation failed")
    }
  }

  def loadConfig(f: String): ExternalLibConfiguration = {
    // parse XML, return configuration
    val configFile = new File(Config.homeDir, "config" + sep + "delite" + sep + f)
    if (!configFile.exists) throw new FileNotFoundException("could not load library configuration: " + configFile)    
    
    val body = XML.loadFile(configFile)
    val compiler = (body \\ "compiler").text.trim
    val headerDir = body \\ "headers" flatMap { e => val prefix = e \ "prefix"; e \\ "path" map { prefix.text.trim + _.text.trim } } toList
    val sourceHeader = body \\ "headers" flatMap { e => e \\ "include" map { _.text.trim } } toList
    val libs = body \\ "libs" flatMap { e => val prefix = e \ "prefix"; (e \\ "path").map(p => prefix.text.trim + p.text.trim) ++ (e \\ "library").map(l => l.text.trim) } toList
    
    new ExternalLibConfiguration(compiler, headerDir, sourceHeader, libs)
  }
}
