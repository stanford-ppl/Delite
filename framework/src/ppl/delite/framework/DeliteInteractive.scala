package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common.{BaseExp, Base, SynchronizedArrayBufferOps}
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

import codegen.cpp.TargetCpp
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.opencl.TargetOpenCL
import codegen.scala.TargetScala
import codegen.Target
import ops.DeliteOpsExp

import org.scala_lang.virtualized.virtualize

trait DeliteInteractive extends Base {
  implicit def staticArrayBuffer[A:Manifest](x: ArrayBuffer[A]): Rep[ArrayBuffer[A]]
}

trait DeliteInteractiveRunner[R] extends DeliteApplication with DeliteInteractive {
  def staticArrayBuffer[A:Manifest](x: ArrayBuffer[A]): Rep[ArrayBuffer[A]] = staticData(x)
  
  def apply: R
  def result: R = _mainResult.asInstanceOf[R]
  def main = error("should not be called")
  override def mainWithResult = apply  
  def run = { 
    val name = "scope-temp"
    Config.degFilename = name
    main(scala.Array())
    ppl.delite.runtime.Delite.embeddedMain(scala.Array(name), staticDataMap)
  }
  run
}
