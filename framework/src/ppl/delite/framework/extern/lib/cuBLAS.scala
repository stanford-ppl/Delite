package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.codegen.scala._
import java.io._

object cuBLAS extends ExternalLibrary {
  val name = "cudaBLAS"
  val configFile = "cuBLAS.xml"  
  val ext = "cu"
  val compileFlags = List( "-w", "-O3", "-arch", "compute_20", "-code", "sm_20", "-shared", "-Xcompiler", "-fPIC") // HJ TODO: these are Fermi-specific; where should they be specified?                           
  val outputSwitch = "-o"
  
  override val header = """
#include <stdlib.h>
#include <stdio.h>
#include <cuda_runtime.h>
"""
}