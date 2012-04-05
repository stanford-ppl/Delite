/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */
 
package ppl.delite.runtime.codegen

import ppl.delite.runtime.Config
import tools.nsc.io._
import java.io.FileWriter
import collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * @author Kevin J. Brown
 */
 
trait CodeCache {

  val cacheHome = Config.codeCacheHome + File.separator + target + File.separator
  val sourceCacheHome = cacheHome + "src" + File.separator
  protected var modules = List.empty[Module]
  
  def target: String
  def ext: String = target //source file extension

  def cacheDegSources(directory: Directory) {
    parseModules(directory)
    for (m <- modules if (m.name != "runtime")) {
      val sourceDir = Directory(Path(directory.path + File.separator + m.name))
      val cacheDir = Directory(Path(sourceCacheHome + m.name))
      if (!directoriesMatch(sourceDir, cacheDir)) {
        copyDirectory(sourceDir, cacheDir)
        m.needsCompile = true
      }
      if (m.deps.exists(_.needsCompile))
        m.needsCompile = true
    }
  }

  protected val sourceBuffer = new ArrayBuffer[(String, String)]

  def addSource(source: String, name: String) {
    if (!sourceBuffer.contains((source, name+"."+ext))) //avoid duplicate kernels //TODO: there must be a better way
      sourceBuffer += Pair(source, name+"."+ext)
  }

  private def parseModules(directory: Directory) {
    def parseError() = error("Invalid module dependency file")

    val file = Path(directory.path + File.separator + "modules.dm")
    if (!file.exists)
      error("Could not find module dependency file for generated code")

    val moduleList = new ListBuffer[String]
    val moduleDepsList = new ListBuffer[List[String]]
    for (line <- scala.io.Source.fromFile(file.jfile).getLines()) {
      val split = line.split(":")
      val deps = if (split.length == 1 && line == split(0) + ":")
        Nil
      else if (split.length == 2)
        split(1).split(",").toList
      else parseError()
      moduleList += split(0)
      moduleDepsList += deps
    }
    for (deps <- moduleDepsList; dep <- deps; if (!moduleList.contains(dep))) parseError()

    val orderedList = new ListBuffer[Module]
    while (!moduleList.isEmpty) {
      var updated = false
      for ((module, deps) <- moduleList zip moduleDepsList) {
        if (!orderedList.contains(module)) {
          if (deps.forall(d => orderedList.exists(m => m.name == d))) {
            val moduleDeps = deps.map(d => orderedList.find(m => m.name == d).get)
            orderedList += new Module(module, moduleDeps)
            moduleList -= module
            moduleDepsList -= deps
            updated = true
          }
        }
      }
      if (!updated) error("Module dependencies are unsatisfiable")
    }

    orderedList += new Module("runtime", orderedList.toList)
    modules = orderedList.toList
  }

  protected class Module(val name: String, val deps: List[Module]) { var needsCompile = false }

  protected def cacheRuntimeSources(sources: Array[(String,String)]) {
    if (Config.noRegenerate) {
      modules.last.needsCompile = true
      return
    }

    val dir = Directory(Path(sourceCacheHome + "runtime"))
    val tempDir = Directory(Path(sourceCacheHome + "runtimeTemp"))
    tempDir.createDirectory()

    for (i <- 0 until sources.length) {
      val sourcePath = tempDir.path + File.separator + sources(i)._2 // + "." + ext
      val writer = new FileWriter(sourcePath)
      writer.write(sources(i)._1)
      writer.close()
    }

    if (!directoriesMatch(tempDir, dir)) { //TODO: due to using HashSets, this isn't perfect (generated code ordering varies)
      copyDirectory(tempDir, dir)
      modules.last.needsCompile = true
    }
    tempDir.deleteRecursively()
  }
  
  protected def sep = File.separator
  
  protected def directoriesMatch(source: Directory, cache: Directory): Boolean = {
    //quick check: see if directory structure & file names match
    assert(source.exists, "source directory does not exist: " + source.path)
    if (!cache.exists)
      return false

    if (!(source.deepList() map(_.name) filterNot { cache.deepList() map(_.name) contains }).isEmpty)
      return false

    //full check: compare all the files
    var allMatch = true
    for ((sourceFile, cacheFile) <- source.deepFiles zip cache.deepFiles; if (allMatch)) {
      val sourceChars = sourceFile.bytes()
      val cacheChars = cacheFile.bytes()
      while (sourceChars.hasNext && cacheChars.hasNext && allMatch) {
        if (sourceChars.next != cacheChars.next)
          allMatch = false
      }
      if (sourceChars.hasNext || cacheChars.hasNext)
        allMatch = false
    }
    allMatch
  }

  protected def copyDirectory(source: Directory, destination: Directory) {
    if (destination exists)
      destination.deleteRecursively()
    destination.createDirectory()

    val base = destination.toString + File.separator

    for (dir <- source.dirs)
      copyDirectory(dir, Directory(Path(base + dir.name)))

    for (file <- source.files)
      file.copyTo(Path(base + file.name))
  }

  def printSources() {
    for (i <- 0 until sourceBuffer.length) {
      print(sourceBuffer(i))
      print("\n /*********/ \n \n")
    }
  }

}
