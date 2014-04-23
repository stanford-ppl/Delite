package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.reflect.SourceContext
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

import Config._
import codegen.cpp.TargetCpp
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.opencl.TargetOpenCL
import codegen.scala.TargetScala
import codegen.restage.{RestageCodegen,TargetRestage}
import codegen.Target
import ops.DeliteOpsExp
import datastructures.DeliteArrayOpsExpOpt

/**
 * For scope communication
 */
object ScopeCommunication {
  /* Path independent */  
  abstract class DRef[+T]  
  case class WrapSymAsDRef[T:Manifest](drefId: Int) extends DRef[T] 
    
  // global id
  var curDrefId = 0  
  def drefBox(id: Int) = "dref_" + id  
}

/**
 * Scope markers
 */
object BeginScopes {
  def apply() = {
    val f = new File(restageFile)
    if (f.exists) f.delete
  }
}

object EndScopes {
  def apply() = {
    val append = (new File(restageFile)).exists
    if (!append) throw new RuntimeException("EndScopes marker encountered without prior scopes")
    val stream = new PrintWriter(new FileWriter(restageFile, append))
    stream.println("}")    
    stream.println("}")    
    stream.close()
  }
}
  
trait DeliteRestageOps extends Base {
  import ScopeCommunication._
  
  // scope-facing placeholders for data exchange
  def lastScopeResult: Rep[Any]
  def returnScopeResult(n: Rep[Any]): Rep[Unit]
    
  object DRef {
    def apply[T:Manifest](x: Rep[T]) = dref_new(x)
  }
  implicit def dRefToRep[T:Manifest](ref: DRef[T]) = new DRefOpsCls(ref)  
  class DRefOpsCls[T:Manifest](ref: DRef[T]) {
    def get = dref_get(ref)
  }
  
  def dref_get[T:Manifest](x: DRef[T]): Rep[T]
  def dref_new[T:Manifest](x: Rep[T]): DRef[T]
  
  // implicit def recToExtRec(x: Rep[Record]) // we actually have a (DeliteArray[Record],DeliteArray[Record])
  
  // DSL independent timing
  def dtic(deps: Rep[Any]*)(implicit ctx: SourceContext) = delite_profile_start(unit("app"),deps)
  def dtic(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = delite_profile_start(component, deps)
  def dtoc(deps: Rep[Any]*)(implicit ctx: SourceContext) = delite_profile_stop(unit("app"),deps)
  def dtoc(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = delite_profile_stop(component, deps)

  def delite_profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def delite_profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
}

trait DeliteRestageOpsExp extends DeliteRestageOps with EffectExp {
  this: DeliteArrayOpsExpOpt =>
  
  import ScopeCommunication._
    
  case class LastScopeResult() extends Def[Any]
  def lastScopeResult = LastScopeResult()
  
  case class ReturnScopeResult(n: Rep[Any]) extends Def[Unit]
  def returnScopeResult(n: Rep[Any]) = reflectEffect(ReturnScopeResult(n))
    
  case class WrapDRefAsSym[T:Manifest](drefId: Int) extends Def[T]  
  case class SetDRefOutput[T:Manifest](sym: Rep[T], drefId: Int) extends Def[Unit]
  def dref_get[T:Manifest](ref: DRef[T]) = ref match {
    case WrapSymAsDRef(id) => toAtom(WrapDRefAsSym(id))
  }
  def dref_new[T:Manifest](sym: Rep[T]) = {
    // ideally you would want nothing in the restaged code - just a short-circuit via the box. the problem is that 
    // the generated code is scoped, so we don't have access to it unless we generate something to pass it along, too.    
    curDrefId += 1
    reflectEffect(SetDRefOutput(sym, curDrefId)) 
    WrapSymAsDRef(curDrefId) 
  }
  
  case class DeliteProfileStart(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]
  case class DeliteProfileStop(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]

  def delite_profile_start(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(DeliteProfileStart(component, deps.toList))
  def delite_profile_stop(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(DeliteProfileStop(component, deps.toList))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case LastScopeResult() => lastScopeResult
    case WrapDRefAsSym(id) => toAtom(WrapDRefAsSym(id))
    case Reflect(SetDRefOutput(s,id),u,es) => reflectMirrored(Reflect(SetDRefOutput(f(s),id), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ReturnScopeResult(n),u,es) => reflectMirrored(Reflect(ReturnScopeResult(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)   
    case Reflect(DeliteProfileStart(c,deps), u, es) => reflectMirrored(Reflect(DeliteProfileStart(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DeliteProfileStop(c,deps), u, es) => reflectMirrored(Reflect(DeliteProfileStop(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  

}

trait DeliteRestageRunner[R] extends DeliteApplication with DeliteRestageOpsExp {  
  this: DeliteArrayOpsExpOpt =>
  
  import EndScopes._
  import ScopeCommunication._
  
  def result: R = _mainResult.asInstanceOf[R] 
  def apply: R
  def main = error("should not be called")
  override def mainWithResult = apply
  def run = { 
    // stage the program with re-stage generator (needs to include a generator for PreviousStageData(n))
    // val generator: RestageCodegen { val IR: DeliteRestageRunner.this.type } = getCodeGenPkg(restageTarget)
    val generator = getCodeGenPkg(restageTarget).asInstanceOf[RestageCodegen{val IR: DeliteRestageRunner.this.type }]
    val baseDir = Config.buildDir + File.separator + generator.toString + File.separator    
    
    val append = (new File(restageFile)).exists
    val stream = new PrintWriter(new FileWriter(restageFile, append))
    
    // curScopeId += 1    
    generator.emitHeader(stream, append)
    generator.transformers = transformers        
    generator.emitSource(liftedMain, "Application", stream)     
    stream.println("}")
    stream.close()
    
    // main(scala.Array())
    // ppl.delite.runtime.Delite.embeddedMain(scala.Array(restageFile), staticDataMap) 
  }      
  run
}
