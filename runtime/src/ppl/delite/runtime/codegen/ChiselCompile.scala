package ppl.delite.runtime.codegen

import xml.XML
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets
import tools.nsc.io._
import java.io.{File, FileInputStream, FileOutputStream}

object ChiselCompile extends CCompile {

  def target = Targets.Chisel
  override def ext = "cpp"

  protected def configFile = "Chisel.xml"
  protected def compileFlags = Array("")
  protected def linkFlags = Array("")

  /**
   * Copies a file from src to dst
   * @param src: Path of source file
   * @param dst: Path of destination
   */
  def copyFile(src: String, dst: String) = {
    val srcFile = new File(src)
    val dstFile = new File(dst)
    new FileOutputStream(dstFile)
            .getChannel()
            .transferFrom (
              new FileInputStream(srcFile).getChannel(), 0, Long.MaxValue
            )
  }

  /**
   * Copies a source directory to a destination directory recursively
   * @param srcDirFile: File object for source diectory
   * @param dstDirFile: File object for destination directory
   */
  def copyDir(srcDirFile: File, dstDirFile: File): Unit = {
    for (f <- srcDirFile.listFiles) {
      if (f.isDirectory) {
        val dstDir = new File(s"${dstDirFile.getAbsolutePath}/${f.getName}")
        dstDir.mkdirs()
        copyDir(f, dstDir)
      } else {
        val dst = s"${dstDirFile.getAbsolutePath()}/${f.getName}"
        val src = f.getAbsolutePath()
        copyFile(src, dst)
      }
    }
  }

  /**
   * Copies a source directory to a destination directory recursively
   * @param srcDir: Path to source directory
   * @param dstDir: Path to destination directory
   */
  def copyDir(srcDir: String, dstDir: String, ext: String = ""): Unit = {
    val srcDirFile = new File(srcDir)
    val srcDirName = srcDir.split("/").last
    val dstDirFile = new File(s"$dstDir/$srcDirName")
    dstDirFile.mkdirs()

    for (f <- srcDirFile.listFiles) {
      if (f.isDirectory) {
        val dstDir = new File(s"${dstDirFile.getAbsolutePath}/${f.getName}")
        dstDir.mkdirs()
        copyDir(f, dstDir)
      } else {
      	val dst = if (ext == "") s"${dstDirFile.getAbsolutePath()}/${f.getName}" else s"${dstDirFile.getAbsolutePath()}/${f.getName}.${ext}" 
        val src = f.getAbsolutePath()
        copyFile(src, dst)
      }
    }
  }

  /**
   * Emits the generated code from sourceBuffer
   * into files. The 'cacheRuntimeSources' method does this,
   * but factoring this into a separate method to enhance readability
   */
  def generateFiles() {
    cacheRuntimeSources(sourceBuffer.toArray)
    sourceBuffer.clear()
  }

  /** Copies static Chisel and build files to Chisel's runtime directory
   * The destination locations are important, as the build files rely on them.
   */
  def copyStaticFiles() {
    copyDir(s"""$staticResources/app-test""", s"""$sourceCacheHome""")
    copyDir(s"""$staticResources/templates""", s"""$sourceCacheHome/static""")
    copyFile(s"""$staticResources/app-build.sbt""", s"""${sourceCacheHome}/build.sbt""")
  }

  /** Copy static cpp files into a 'static' folder in the runtime cpp directory.
   * The correct place for this is in CppCompile, and must be optionally turned on/off
   * with a config flag
   */
  def copyCppStaticFiles() {
    val cppStaticPath = s"""$staticResources/../cpp"""
    copyDir(s"""$cppStaticPath""", s"""$sourceCacheHome/../../cpp/src/static""")
  }

  /**
   * Overriding compile method to only generate files,
   * not do actual compilation, for MaxJ
   */
  override def compile() {
    generateFiles()
    copyStaticFiles()
    copyCppStaticFiles()
  }

}
