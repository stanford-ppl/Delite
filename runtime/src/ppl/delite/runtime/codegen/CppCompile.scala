package ppl.delite.runtime.codegen

import xml.XML
import ppl.delite.runtime.Config

object CppCompile extends CCompile {

  def target = "cpp"
  override def ext = "cpp"

  protected def configFile = "CPP.xml"
  protected def compileFlags = Array("-w", "-O3", "-shared", "-fPIC")
  protected def outputSwitch = "-o"

}
