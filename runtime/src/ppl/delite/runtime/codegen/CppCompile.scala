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
  
  private val dsFiles = Directory(Path(sourceCacheHome + "datastructures")).files.toList
  override protected def auxSourceList = dsFiles.filter(_.extension == ext).map(_.toAbsolute.toString) ++ Array(sourceCacheHome + "kernels" + sep + target + "helperFuncs." + ext) ++ Array("DeliteCpp", "DeliteCppProfiler", "DeliteMemory", "Config", "cppInit").map(staticResources+_+"."+ext)
}
