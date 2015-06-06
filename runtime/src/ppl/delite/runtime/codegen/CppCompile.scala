package ppl.delite.runtime.codegen

import xml.XML
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets
import tools.nsc.io._

object CppCompile extends CCompile {

  def target = Targets.Cpp
  override def ext = "cpp"

  protected def configFile = "CPP.xml"
  protected def compileFlags = Array()
  protected def linkFlags = Array()
  override protected def optionalFeatures = {
    config.features.collect {
      case "numa" => "__DELITE_CPP_NUMA__"
    }
  }
}
