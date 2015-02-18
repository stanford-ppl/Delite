package ppl.delite.runtime.codegen

import xml.XML
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets
import tools.nsc.io._

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 9:39:10 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object CudaCompile extends CCompile {

  def target = Targets.Cuda
  override def ext = "cu"

  lazy val arch = {
    val body = XML.loadFile(Config.deliteHome + sep + "config" + sep + "delite" + sep + configFile)
    val arch = (body \\ "arch").text.trim
    arch.split(',').map(_.trim.split('.').reduceLeft(_ + _)) //remove 'dot' if it exists (e.g., 2.0 => 20)
  }

  protected def configFile = "CUDA.xml"
  protected def compileFlags = Array("-m64", "-w", "-O3", "-shared", "-Xcompiler", "\'-fPIC\'") ++ arch.flatMap(a => Array("-gencode", "arch=compute_"+a+",code=sm_"+a))
  protected def linkFlags = Array("-lcublas", "-shared", "-Xcompiler", "\'-fPIC\'")
}
