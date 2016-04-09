package ppl.delite.runtime.codegen

import xml.XML
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets
import tools.nsc.io._

object MaxJCompile extends CCompile {
  def target = Targets.MaxJ
  override def ext = "maxj"
  protected def configFile = "MaxJ.xml"
  protected def compileFlags: Array[String] = Array("")
  protected def linkFlags: Array[String] = Array("")

}
