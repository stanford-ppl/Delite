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

  val pcmSourcesList: Array[String] = if (Config.enablePCM) Array("cpucounters", "msr", "pci", "client_bw") else Array()
  var staticResourcesList: Array[String] = Array("DeliteCpp", "DeliteCppProfiler", "DeliteMemory", "DeliteThreadPool", "Config", "cppInit")
  if (Config.enablePCM) {
    staticResourcesList = staticResourcesList ++ Array("pcmHelper")
  }
  
  private val dsFiles = Directory(Path(sourceCacheHome + "datastructures")).files.toList
  //override protected def auxSourceList = dsFiles.filter(_.extension == ext).map(_.toAbsolute.toString) ++ Array(sourceCacheHome + "kernels" + sep + target + "helperFuncs." + ext) ++ Array("DeliteCpp", "DeliteCppProfiler", "DeliteMemory", "DeliteThreadPool", "Config", "cppInit", "pcmHelper").map(staticResources+_+"."+ext) ++ pcmSourcesList.map(pcmResources + _ + "." + ext)
  override protected def auxSourceList = dsFiles.filter(_.extension == ext).map(_.toAbsolute.toString) ++ Array(sourceCacheHome + "kernels" + sep + target + "helperFuncs." + ext) ++ staticResourcesList.map(staticResources+_+"."+ext) ++ pcmSourcesList.map(pcmResources + _ + "." + ext)
}
