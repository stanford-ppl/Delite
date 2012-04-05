package ppl.delite.framework.extern.lib

import xml.XML
import ppl.delite.framework.Config

object cuBLAS extends ExternalLibrary {
  val libName = "cudaBLAS"
  val configFile = "cuBLAS.xml"  
  val ext = "cu"
  val libExt = "so"
  def compileFlags = List( "-w", "-O3", "-arch", "compute_"+arch, "-code", "sm_"+arch, "-shared", "-Xcompiler", "-fPIC")
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
#include <thrust/transform.h>
""" + configHeader.map(h => "#include \"" + h + "\"\n").mkString("")
}
