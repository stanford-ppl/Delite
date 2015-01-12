/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

package ppl.delite.runtime.codegen

import collection.mutable.{ArrayBuffer, ListBuffer}

import java.io.{File, FileWriter}
import java.security.MessageDigest
import org.apache.commons.io._
import scala.collection.JavaConverters._

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets

/**
 * @author Kevin J. Brown
 */

object CodeCache {
  private val checksum = new File(Config.codeCacheHome + File.separator + "cache.checksum")
  private val dir = new File(Config.codeCacheHome)

  def verifyCache() {
    if (!Config.alwaysKeepCache && (!checksum.exists || !checksumEqual)) {
      if (Config.verbose) println("[delite]: Clearing corrupt code cache")
      FileUtils.deleteDirectory(dir)
    }
  }

  private def checksumEqual(): Boolean = {
    val saved = FileUtils.readFileToByteArray(checksum)
    val current = computeChecksum
    if (saved.length != current.length) return false
    for (i <- 0 until saved.length) {
      if (saved(i) != current(i)) return false
    }
    true
  }

  private def computeChecksum() = {
    val hash = MessageDigest.getInstance("md5")
    for (file <- FileUtils.listFiles(dir, null, true).asScala if file.getName != "cache.checksum") { //we only check file names rather than entire contents for simplicity and speed
      hash.update(file.getName.getBytes)
    }
    hash.digest
  }

  def clearChecksum() {
    checksum.delete()
  }

  def addChecksum() {
    FileUtils.writeByteArrayToFile(checksum, computeChecksum)
  }
}

trait CodeCache {

  val cacheHome = Config.codeCacheHome + sep + target + sep
  val sourceCacheHome = cacheHome + "src" + sep
  val binCacheHome = cacheHome + "bin" + sep + "runtime" + sep
  protected var modules = List.empty[Module]

  def target: Targets.Value
  def hostCompiler = Compilers(Targets.getHostTarget(target))
  def ext: String = target.toString //source file extension
  def staticResources: String =  Config.deliteHome + sep + "runtime" + sep + "src" + sep + "static" + sep + target + sep

  def cacheDegSources(directory: File) {
    parseModules(directory)
    for (m <- modules if (m.name != "runtime")) {
      val sourceDir = new File(directory.getPath + sep + m.name)
      val cacheDir = new File(sourceCacheHome + m.name)
      if (!directoriesMatch(sourceDir, cacheDir)) {
        FileUtils.deleteDirectory(cacheDir)
        FileUtils.copyDirectory(sourceDir, cacheDir)
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

  private def parseModules(directory: File) {
    def parseError() = error("Invalid module dependency file")

    val file = new File(directory.getPath + File.separator + "modules.dm")
    if (!file.exists)
      error("Could not find module dependency file for generated code")

    val moduleList = new ListBuffer[String]
    val moduleDepsList = new ListBuffer[List[String]]
    for (line <- scala.io.Source.fromFile(file).getLines()) {
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

  protected class Module(val name: String, val deps: List[Module]) {
    var needsCompile = false
    override def toString = name
  }

  protected def cacheRuntimeSources(sources: Array[(String,String)]) {
    if (Config.noRegenerate) {
      modules.last.needsCompile = true
      return
    }

    val dir = new File(sourceCacheHome + "runtime")
    val tempDir = new File(sourceCacheHome + "runtimeTemp")
    tempDir.mkdir()

    for (i <- 0 until sources.length) {
      val sourcePath = tempDir.getPath + File.separator + sources(i)._2 // + "." + ext
      val writer = new FileWriter(sourcePath)
      writer.write(sources(i)._1)
      writer.close()
    }

    if (!directoriesMatch(tempDir, dir)) {
      FileUtils.deleteDirectory(dir)
      FileUtils.copyDirectory(tempDir, dir)
      modules.last.needsCompile = true
    }
    FileUtils.deleteDirectory(tempDir)
  }

  protected def sep = File.separator

  protected def directoriesMatch(source: File, cache: File): Boolean = {
    // quick check: see if directory structure & file names match
    assert(source.exists, "source directory does not exist: " + source.getPath)
    if (!cache.exists)
      return false

    if (!(FileUtils.listFiles(source, null, true).asScala map(_.getName) filterNot { FileUtils.listFiles(cache, null, true).asScala.toList map(_.getName) contains }).isEmpty)
      return false

    // full check: compare all the files
    (FileUtils.listFiles(source, null, true).asScala zip FileUtils.listFiles(cache, null, true).asScala) forall { fs => FileUtils.contentEquals(fs._1, fs._2) }
  }

  def printSources() {
    for (i <- 0 until sourceBuffer.length) {
      print(sourceBuffer(i))
      println("\n /*********/ \n")
    }
  }

}
