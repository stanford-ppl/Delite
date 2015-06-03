package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.Config
import xml.XML
import java.io.FileNotFoundException
import tools.nsc.io.{Directory, Path, File}
import ppl.delite.runtime.graph.targets.{OS, Targets}
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.scheduler._
import java.io.FileWriter

trait CCompile extends CodeCache {
  
  protected class CompilerConfig(
    val compiler: String, 
    val make: String,
    val headerDir: Array[String],
    val sourceHeader: Array[String],
    val libs: Array[String],
    val headerPrefix: String, 
    val libPrefix: String,
    val compileFlags: String,
    val linkFlags: String,
    val features: Array[String]
  )

  protected def configFile: String // name of file, will always be searched for inside config/delite/
  protected def compileFlags: Array[String] // machine-independent flags that are always passed to the compiler for this lib
  protected def linkFlags: Array[String] // machine-independent flags that are always passed to the linker for this lib
  protected def optionalFeatures: Array[String] = Array() //features (e.g., headers) that users can enable but we don't want to require
  protected lazy val config = loadConfig(configFile)
  
  private val headerBuffer = new ArrayBuffer[(String, String)]
  private val kernelBuffer = new ArrayBuffer[String] // list of kernel filenames to be included in the compilation (non-multiloop kernels)
  protected def auxSourceList = List[String]() // additional source filenames to be included in the compilation
  protected val shared = CppCompile

  def headers = headerBuffer.map(_._2)

  protected def deliteLibs = Seq(Config.deliteBuildHome + sep + "libraries" + sep + target, staticResources)

  def addKernel(op: DeliteOP) { 
    op match {
      case _:EOP | _:Arguments => // kernelBuffer is used to hold the actual generated kernels to compile 
      case _ if kernelBuffer.contains (op.id + "." + ext) => // same kernel may be used multiple times in a deg 
      case _ => kernelBuffer += (op.id + "." + ext)
    }
  }

  def addHeader(source: String, name: String) {
    if (!headerBuffer.contains((source, name+".h")))
      headerBuffer += Pair(source, name+".h")
  }

  protected def loadConfig(f: String): CompilerConfig = {
    // parse XML, return configuration
    val configFile = File(Config.deliteHome + sep + "config" + sep + "delite" + sep + f)
    if (!configFile.exists) throw new FileNotFoundException("could not load compiler configuration: " + configFile)

    val body = XML.loadFile(configFile.jfile)
    val compiler = (body \\ "compiler").text.trim
    val make = (body \\ "make").text.trim
    val headerPrefix = (body \\ "headers" \ "prefix").text.trim
    val headerDir = body \\ "headers" flatMap { e => val prefix = e \ "prefix"; e \\ "path" map { prefix.text.trim + _.text.trim } } toArray
    val sourceHeader = body \\ "headers" flatMap { e => e \\ "include" map { _.text.trim } } toArray
    val libPrefix = (body \\ "libs" \ "prefix").text.trim
    val libs = body \\ "libs" flatMap { e => val prefix = e \ "prefix"; (e \\ "path").map(p => prefix.text.trim + p.text.trim) ++ (e \\ "library").map(l => l.text.trim) } toArray
    val compileFlags = (body \\ "compileFlags").text.trim
    val linkFlags = (body \\ "linkFlags").text.trim
    val features = (body \\ "feature" map { _.text.trim }).toArray
    new CompilerConfig(compiler, make, headerDir, sourceHeader, libs, headerPrefix, libPrefix, compileFlags, linkFlags, features)
  }

  def compile() {
    if (sourceBuffer.length == 0) return
    if (Config.verbose) println("[delite]: starting C compile")
    val start = System.currentTimeMillis
    cacheRuntimeSources((sourceBuffer ++ headerBuffer).toArray)
    
    if (modules.exists(_.needsCompile)) {
      val includes = (modules.flatMap(m => List(config.headerPrefix + sourceCacheHome + m.name, config.headerPrefix + Compilers(Targets.getHostTarget(target)).sourceCacheHome + m.name)).toArray ++ 
                     config.headerDir ++ Array(config.headerPrefix + staticResources, config.headerPrefix + shared.staticResources)).distinct
      val libs = config.libs ++ deliteLibs.map(Directory(_).files.withFilter(f => f.extension == OS.libExt).map(_.path)).flatten
      val sources = (sourceBuffer.map(s => sourceCacheHome + "runtime" + sep + s._2) ++ kernelBuffer.map(k => sourceCacheHome + "kernels" + sep + k) ++ auxSourceList).toArray
      val degName = ppl.delite.runtime.Delite.inputArgs(0).split('.')
      val configString = Config.numThreads.toString + Config.numCpp + Config.numCuda + Config.numOpenCL
      val dest = binCacheHome + target + "Host" + degName(degName.length-2) + "_" + configString + "." + OS.libExt
      compile(dest, sources, includes, libs)
    }

    val time = (System.currentTimeMillis - start)/1e3
    if (Config.verbose) println("[delite]: finished C compile in " + time + "s")
    sourceBuffer.clear()
    headerBuffer.clear()
    kernelBuffer.clear()
  }

  def compileInit(root: String) {
    val source = root + "." + ext
    val dest = root + "." + OS.libExt
    compile(dest, Array(source), config.headerDir, config.libs)
  }

  // emit Makefile and call make
  def compile(destination: String, sources: Array[String], includes: Array[String], libs: Array[String]) {
    val destDir = Path(destination).parent
    destDir.createDirectory()

    // generate Makefile
    val makefile = destDir + sep + "Makefile"
    if(!Config.noRegenerate) {
      val writer = new FileWriter(makefile)
      writer.write(makefileString(destination, sources, includes, libs, optionalFeatures))
      writer.close()
    }

    if (config.compiler == "")
      throw new RuntimeException("compiler path is not set. Please specify in $DELITE_HOME/config/delite/" + configFile + " (<compiler> </compiler>)")
    if (config.make == "")
      throw new RuntimeException("make command path is not set. Please specify in $DELITE_HOME/config/delite/" + configFile + " (<make> </make>)")
    if (config.headerDir.length == 0)
      throw new RuntimeException("JNI header paths are not set. Please specify in $DELITE_HOME/config/delite/" + configFile + " (<headers> </headers>)")

    val args = config.make.split(" ") ++ Array("-f", makefile, "all")
    val pb = new java.lang.ProcessBuilder(args:_*)
    pb.redirectErrorStream(true) //merge stdout and stderr
    val outFile = File("compile.log")
    pb.redirectOutput(outFile.jfile)
    val process = pb.start()
    process.waitFor()
    checkError(process, args, outFile)
  }

  protected def checkError(process: Process, args: Array[String], file: File) {
    val out = "--" + target + " compile args: " + args.mkString(",")
    if (Config.verbose || process.exitValue != 0) {
      if(Config.clusterMode == 2) // Send the error message to the master node
        ppl.delite.runtime.DeliteMesosExecutor.sendDebugMessage(out)
      else 
        println(out)
    }

    if (process.exitValue == 0) {
      file.delete() //remove log
    } else {
      sourceBuffer.clear()
      headerBuffer.clear()
      kernelBuffer.clear()
      sys.error(target + " compilation failed with exit value " + process.exitValue + " (log in "+file+")")
    }
  }

  protected def verbosity = if (Config.verbose) "-DDELITE_VERBOSE" else ""

  // string for the Makefile
  def makefileString(destination: String, sources: Array[String], includes: Array[String], libs: Array[String], features:Array[String]) = s"""
## Makefile: Generated by Delite Runtime ##
CC = ${config.compiler}
DELITE_HOME = ${Config.deliteHome}
SOURCECACHE_HOME = ${sourceCacheHome}
BINCACHE_HOME = ${binCacheHome}
//INCLUDES = ${includes.mkString(" ")}
INCLUDES = ${includes.mkString(" ") + " -I/home/jithinpt/memory_profiling/IntelPerformanceCounterMonitorV2.8"}

CFLAGS = ${(compileFlags ++ Array(config.compileFlags)).mkString(" ")}
LDFLAGS = ${(linkFlags ++ Array(config.linkFlags) ++ libs).mkString(" ")}
SOURCES = ${sources.mkString(" ")}
OBJECTS = $$(SOURCES:.${ext}=.o)
OUTPUT = ${destination}

all: $$(OUTPUT)

# The order of objects and libraries matter because of the dependencies
$$(OUTPUT): $$(OBJECTS)
\t$$(CC) $$(OBJECTS) $$(LDFLAGS) -o $$(OUTPUT)

%.o: %.${ext}
\t$$(CC) -c ${verbosity} -DDELITE_CPP=${Config.numCpp} -DMEMMGR_${Config.cppMemMgr.toUpperCase} ${features.map("-D"+_).mkString(" ")} $$(INCLUDES) $$(CFLAGS) $$< -o $$@

clean:
\trm -f $$(OBJECTS) $$(OUTPUT)

.PHONY: all clean

"""
}
