package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.reflect.SourceContext
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

import codegen.c.TargetC
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.opencl.TargetOpenCL
import codegen.scala.TargetScala
import codegen.restage.{RestageCodegen,TargetRestage}
import codegen.Target
import ops.DeliteOpsExp
import datastructures.DeliteArrayOpsExpOpt

trait DeliteRestageOps extends Base {
  // scope-facing placeholders for data exchange
  def lastScopeResult: Rep[Any]
  def returnScopeResult(n: Rep[Any]): Rep[Unit]

  // DSL independent timing
  def dtic(deps: Rep[Any]*)(implicit ctx: SourceContext) = delite_profile_start(unit("app"),deps)
  def dtic(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = delite_profile_start(component, deps)
  def dtoc(deps: Rep[Any]*)(implicit ctx: SourceContext) = delite_profile_stop(unit("app"),deps)
  def dtoc(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = delite_profile_stop(component, deps)

  def delite_profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def delite_profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]

  // hack for bound syms escaping
  def bind[A:Manifest](x: Rep[A]): Rep[Unit]
}

trait DeliteRestageOpsExp extends DeliteRestageOps with EffectExp with StructExp 
  with RangeOpsExp with HashMapOpsExp {
  this: DeliteArrayOpsExpOpt =>
  
  case class LastScopeResult() extends Def[Any]
  def lastScopeResult = LastScopeResult()
  
  case class ReturnScopeResult(n: Rep[Any]) extends Def[Unit]
  def returnScopeResult(n: Rep[Any]) = reflectEffect(ReturnScopeResult(n))
  
  case class DeliteProfileStart(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]
  case class DeliteProfileStop(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]

  def delite_profile_start(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(DeliteProfileStart(component, deps.toList))
  def delite_profile_stop(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(DeliteProfileStop(component, deps.toList))

  case class Bind[A:Manifest](x: Exp[A]) extends Def[Unit]
  def bind[A:Manifest](x: Rep[A]) = reflectEffect(Bind(x))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case LastScopeResult() => lastScopeResult
    case Reflect(Bind(x), u, es) => reflectMirrored(Reflect(Bind(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ReturnScopeResult(n),u,es) => reflectMirrored(Reflect(ReturnScopeResult(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))   
    case Reflect(DeliteProfileStart(c,deps), u, es) => reflectMirrored(Reflect(DeliteProfileStart(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DeliteProfileStop(c,deps), u, es) => reflectMirrored(Reflect(DeliteProfileStop(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  

}

object EndScopes {
  val scopeFile = "restage-scopes"
  
  def apply() = {
    val append = (new File(scopeFile)).exists
    if (!append) throw new RuntimeException("EndScopes marker encountered without prior scopes")
    val stream = new PrintWriter(new FileWriter(scopeFile, append))
    stream.println("}")    
    stream.println("}")    
    stream.close()
  }
}

trait DeliteRestageRunner extends DeliteApplication with DeliteRestageOpsExp {  
  this: DeliteArrayOpsExpOpt =>
  
  import EndScopes._
  
  def apply: Any
  def main = apply
  def run = { 
    // stage the program with re-stage generator (needs to include a generator for PreviousStageData(n))
    // val generator: RestageCodegen { val IR: DeliteRestageRunner.this.type } = getCodeGenPkg(restageTarget)
    val generator = getCodeGenPkg(restageTarget).asInstanceOf[RestageCodegen{val IR: DeliteRestageRunner.this.type }]
    val baseDir = Config.buildDir + File.separator + generator.toString + File.separator    
    
    val append = (new File(scopeFile)).exists
    val stream = new PrintWriter(new FileWriter(scopeFile, append))
    generator.emitHeader(stream, append)
    generator.transformers = transformers
    generator.emitSource(liftedMain, "Application", stream)     
    stream.println("}")
    stream.close()
    
    // main(scala.Array())
    // ppl.delite.runtime.Delite.embeddedMain(scala.Array(scopeFile), staticDataMap) 
  }
    
  System.out.println("object created")
  run
}
