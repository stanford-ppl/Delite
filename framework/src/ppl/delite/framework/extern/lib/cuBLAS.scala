package ppl.delite.framework.extern.lib

import xml.XML
import ppl.delite.framework.Config

object cuBLAS extends ExternalLibrary {
  val libName = "cudaBLAS"
  val configFile = "cuBLAS.xml"  
  val ext = "cu"
  // TODO: go back to creating shared library (latest cuda compiler creates some issues linking shared object with other shared object)
  val libExt = "o"
  def compileFlags = List("-w", "-O3", "-arch", "compute_"+arch, "-code", "sm_"+arch, "-c", /*"-shared", */"-Xcompiler", "-fPIC")
  val outputSwitch = "-o"

  lazy val arch = {
    val body = XML.loadFile(Config.homeDir + sep + "config" + sep + "delite" + sep + configFile)
    val arch = (body \\ "arch").text.trim
    arch.split('.').reduceLeft(_ + _) //remove 'dot' if it exists (e.g., 2.0 => 20)
  }
  
  override lazy val header = """
#include <stdlib.h>
#include <stdio.h>
#include <limits>
#include <cuda_runtime.h>
#include <thrust/device_ptr.h>
#include <thrust/transform.h>
""" + configHeader.map(h => "#include \"" + h + "\"\n").mkString("")
}
