package ppl.delite.runtime.codegen

import xml.XML
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets
import tools.nsc.io._

object MaxJCompile extends CCompile {

  def target = Targets.MaxJ
  override def ext = "cpp"

  protected def configFile = "MaxJ.xml"
  protected def compileFlags = Array("")
  protected def linkFlags = Array("")

  /**
   * Emits the generated code from sourceBuffer
   * into files. The 'cacheRuntimeSources' method does this,
   * but factoring this into a separate method to enhance readability
   */
  def generateFiles() {
    cacheRuntimeSources(sourceBuffer.toArray)
    sourceBuffer.clear()
  }

  /**
   * Overriding compile method to only generate files,
   * not do actual compilation, for MaxJ
   */
  override def compile() {
    generateFiles()
  }

}
