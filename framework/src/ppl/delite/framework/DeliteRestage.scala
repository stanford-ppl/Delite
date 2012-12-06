package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common.{BaseExp, Base, SynchronizedArrayBufferOps}
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

import codegen.c.TargetC
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.opencl.TargetOpenCL
import codegen.scala.TargetScala
import codegen.restage.{RestageCodegen,TargetRestage}
import codegen.Target
import ops.DeliteOpsExp

trait DeliteRestageOps extends SynchronizedArrayBufferOps {
  def previous(n: Rep[Int]): Rep[Any]
}

trait DeliteRestageOpsExp extends DeliteRestageOps with BaseExp {
  case class PreviousStageData(n: Exp[Int]) extends Def[Any]
  def previous(n: Exp[Int]) = PreviousStageData(n)  
}

trait DeliteRestageRunner extends DeliteApplication with DeliteRestageOpsExp {    
  def apply: Any
  def main = apply
  def run = { 
    val name = "scope-temp" 
    
    // stage the program with re-stage generator (needs to include a generator for PreviousStageData(n))
    // val generator: RestageCodegen { val IR: DeliteApplication.this.type } = getCodeGenPkg(restageTarget)
    val generator = getCodeGenPkg(restageTarget)
    val baseDir = Config.buildDir + File.separator + generator.toString + File.separator    
    
    val append = (new File(name)).exists
    val stream = new PrintWriter(new FileWriter(name, append))
    if (!append) {    
      // restage header
      stream.println("import ppl.delite.framework.{DeliteRestage,DeliteApplication}")
      stream.println()
      stream.println("object RestageApplicationRunner extends DeliteApplication with RestageApplication")
      stream.println("trait RestageApplication extends DeliteRestage {")      
      stream.println("/* Emitting re-stageable code */")
    }
    else {
      stream.println("{")
    }
    
    generator.emitSource(liftedMain, "Application", stream) 
    
    // TODO: write output to previuos()
    // 
    stream.println("}")
    stream.close()
    // main(scala.Array())
    // ppl.delite.runtime.Delite.embeddedMain(scala.Array(name), staticDataMap) 
  }
    
  System.out.println("object created")
  run
}
