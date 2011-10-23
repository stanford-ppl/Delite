package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.codegen.scala._
import java.io._

object cuBLAS extends ExternalLibrary {
  val libName = "cudaBLAS"
  val configFile = "cuBLAS.xml"  
  val ext = "cu"
  val libExt = "so"
  val compileFlags = List( "-w", "-lcublas", "-O3", "-arch", "compute_20", "-code", "sm_20", "-shared", "-Xcompiler", "-fPIC") // HJ TODO: these are Fermi-specific; where should they be specified?
  val outputSwitch = "-o"
  
  override val header = """
#include <stdlib.h>
#include <stdio.h>
#include <limits>
#include <cuda_runtime.h>
#include <cublas.h>
"""
}
