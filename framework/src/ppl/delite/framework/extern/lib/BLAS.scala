package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.codegen.scala._
import java.io._

object BLAS extends ExternalLibrary {
  //val target = "scala" // this should be well-typed, but we don't have an IR reference yet, so we need to generalize that...  
  val libName = "scalaBLAS"
  val configFile = "BLAS.xml"  
  val ext = "c"
  val libExt = "so"
  // should we consider library linking machine dependent? do we have a different external lib
  // for unix and windows?
  val compileFlags = List( "-w", "-O3", "-shared", "-fPIC") // dynamic shared library
  val outputSwitch = "-o"

  override lazy val header = """
#include <stdlib.h>
#include <stdio.h>

#include <jni.h>
""" + configHeader.map(h => "#include \"" + h + "\"\n").mkString("")
}
