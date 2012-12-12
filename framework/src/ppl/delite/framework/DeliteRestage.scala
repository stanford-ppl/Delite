package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common.{BaseExp, Base, EffectExp, SynchronizedArrayBufferOps}
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

import codegen.c.TargetC
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.opencl.TargetOpenCL
import codegen.scala.TargetScala
import codegen.restage.{RestageCodegen,TargetRestage}
import codegen.Target
import ops.DeliteOpsExp

trait DeliteRestageOps extends Base {
  // scope-facing placeholders for data exchange
  def lastScopeResult: Rep[Any]
  def returnScopeResult(n: Rep[Any]): Rep[Unit]
}

trait DeliteRestageOpsExp extends DeliteRestageOps with EffectExp {
  case class LastScopeResult() extends Def[Any]
  def lastScopeResult = LastScopeResult()
  
  case class ReturnScopeResult(n: Rep[Any]) extends Def[Unit]
  def returnScopeResult(n: Rep[Any]) = reflectEffect(ReturnScopeResult(n))
}

trait DeliteRestageRunner extends DeliteApplication with DeliteRestageOpsExp {    
  def apply: Any
  def main = apply
  def run = { 
    val name = "restage-scopes" 
    
    // stage the program with re-stage generator (needs to include a generator for PreviousStageData(n))
    // val generator: RestageCodegen { val IR: DeliteApplication.this.type } = getCodeGenPkg(restageTarget)
    val generator = getCodeGenPkg(restageTarget)
    val baseDir = Config.buildDir + File.separator + generator.toString + File.separator    
    
    val append = (new File(name)).exists
    val stream = new PrintWriter(new FileWriter(name, append))
    if (!append) {    
      // restage header
      stream.println("import ppl.delite.framework.{DeliteILApplication,DeliteILApplicationRunner}")
      stream.println()
      stream.println("object RestageApplicationRunner extends DeliteILApplicationRunner with RestageApplication")
      stream.println("trait RestageApplication extends DeliteILApplication {")      
      stream.println("/* Emitting re-stageable code */")
      stream.println("def main() {")
      stream.println("val x0 = args")
    }
    else {
      stream.println("{")
    }
    
    generator.emitSource(liftedMain, "Application", stream) 
    
    stream.println("}")
    stream.println("}")
    stream.close()
    // main(scala.Array())
    // ppl.delite.runtime.Delite.embeddedMain(scala.Array(name), staticDataMap) 
  }
    
  System.out.println("object created")
  run
}
