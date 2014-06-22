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
    arch.split('.').reduceLeft(_ + _) //remove 'dot' if it exists (e.g., 2.0 => 20)
  }

  protected def configFile = "CUDA.xml"
  protected def compileFlags = Array("-m64", "-w", "-O3", "-arch", "compute_"+arch, "-code", "sm_"+arch, "-shared", "-Xcompiler", "\'-fPIC\'")
  protected def linkFlags = Array("-lcublas", "-shared", "-Xcompiler", "\'-fPIC\'")
  protected def outputSwitch = "-o"

  lazy val deviceDSFiles = Directory(Path(sourceCacheHome + "datastructures")).files.toList.filter(f => f.extension==ext)
  lazy val hostDSFiles = Directory(Path(hostCompiler.sourceCacheHome + "datastructures")).files.toList.filter(f => f.extension==hostCompiler.ext)
  override protected def auxSourceList = (deviceDSFiles++hostDSFiles).map(_.toAbsolute.toString).distinct ++ List(sourceCacheHome + "kernels" + sep + target + "helperFuncs." + ext, staticResources + "DeliteCuda." + ext) 
}
