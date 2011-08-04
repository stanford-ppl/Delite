package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import java.io._
import scala.xml._

trait ExternalLibConfiguration {
  val compiler: String
  val include: List[String]
}

trait ExternalLibrary {
  //val target: Target
  val libName: String
  val ext: String // native file extension (can this ever be anything besides .c or .cpp??)
  val configFile: String // name of file, will always be searched for inside extern/src/ppl/delite/extern/lib/config
  val compileFlags: List[String] // machine-independent flags that are always passed to the compiler for this lib
  val outputSwitch: String // compiler parameter that allows us to specify destination dir (e.g. -o)
  val header: String = "" // an optional header to be included at the top of every generated call for this lib
  
  lazy val name = "lib" + libName // generated scala interface for this library will use this as its object name 

  /**
   * machine dependent, sourced from XML configuration 
   */  
  lazy val config = loadConfig(configFile)
  lazy val compiler: String = config.compiler
  lazy val includeFlags: List[String] = config.include
  
  def compile(src: String, destDir: String) {
    val srcFile = new File(src)
    if (!srcFile.exists) throw new FileNotFoundException("source file does not exist: " + src)
    
    // invoke the compiler using Runtime.exec
    val javaHome = System.getProperty("java.home")
    val buildPath = new File(Config.buildDir, "scala/kernels")
    val destPath = new File(destDir, "/" + name + ".so")    
    val outputFlags = List(outputSwitch, destPath.toString)

    // this call is based on the gcc/icc invocation signature.. do we need to generalize it?
    val args = Array(compiler) ++ compileFlags ++ includeFlags ++ outputFlags ++ Array(srcFile.toString)
    //println("--external compile args: " + (args mkString ","))
    val process = Runtime.getRuntime.exec(args, null, buildPath)
    process.waitFor
	  checkError(process)
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
      sys.error("external library compilation failed")
    }
  }

  def loadConfig(f: String): ExternalLibConfiguration = {
    // parse XML, return configuration
    val configFile = new File(Config.homeDir, "/framework/src/ppl/delite/framework/extern/lib/config/" + f)
    if (!configFile.exists) throw new FileNotFoundException("could not load library configuration: " + configFile)    
    
    val body = XML.loadFile(configFile)
    val compilerVal = body \\ "compiler" text
    val includeVal = body \\ "include" flatMap { e => val prefix = e \ "prefix"; e \\ "path" map { prefix.text.trim + _.text.trim } } toList

    new ExternalLibConfiguration {
      val compiler = compilerVal
      val include = includeVal
    }
  }
}
