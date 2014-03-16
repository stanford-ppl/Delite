package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.codegen.scala._
import java.io._

object BLAS extends ExternalLibrary {
  val libName = "BLAS"
  val configFile = "BLAS.xml"
  val ext = "cpp"
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
