package ppl.delite.runtime.codegen

import xml.XML
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets

object CppCompile extends CCompile {

  def target = Targets.Cpp
  override def ext = "cpp"

  protected def configFile = "CPP.xml"
  protected def compileFlags = Array("-w", "-O3", "-shared", "-fPIC")
  protected def outputSwitch = "-o"

  override protected def auxSourceList = List(sourceCacheHome + "kernels" + sep + target + "helperFuncs." + ext) 

}
