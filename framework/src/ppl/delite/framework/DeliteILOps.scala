package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.reflect.SourceContext
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common.{BaseExp, Base, EffectExp, SynchronizedArrayBufferOps, StructTags, StructExp}
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}
import scala.virtualization.lms.util.OverloadHack

import codegen.c.TargetC
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.opencl.TargetOpenCL
import codegen.scala.TargetScala
import codegen.restage.{RestageCodegen,TargetRestage}
import codegen.Target
import ops.{DeliteCollection,DeliteOpsExp}
import ppl.delite.framework.datastructures.DeliteArrayStructTags

trait DeliteILOps extends Base with StructTags with DeliteArrayStructTags with OverloadHack {
  
  // data exchange
  // note that these are different than the methods in DeliteRestage.scala, because these are constructed in
  // the restaged code directly - we need to resolve rather than code generate them.
  def getScopeResult: Rep[Any]
  def setScopeResult(n: Rep[Any]): Rep[Unit]
  
  // expose struct method from StructExp for restaging
  def struct[T:Manifest](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit o: Overloaded1, pos: SourceContext): Rep[T]
  def field_update[T:Manifest](struct: Rep[Any], index: String, rhs: Rep[T]): Rep[Unit] 
    
  // delite ops
  def collect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
    (size: Rep[Int], allocN: Rep[Int] => Rep[I], func: (Rep[A], Rep[Int]) => Rep[A], update: (Rep[I],Rep[A],Rep[Int]) => Rep[Unit], finalizer: Rep[I] => Rep[CA]): Rep[CA]
    
  def extern[A:Manifest](funcName: String, alloc: => Rep[A], inputs: List[Rep[Any]]): Rep[A]
}

trait DeliteILOpsExp extends DeliteILOps with DeliteOpsExp {
  
  case class GetScopeResult() extends Def[Any]
  def getScopeResult = {
    // find last SetScopeResult effect and unpack it to directly connect
    val e = context.collect{ case Def(Reflect(SetScopeResult(n), u, es)) => n }
    e.last
  }
  
  case class SetScopeResult(n: Rep[Any]) extends Def[Unit]
  def setScopeResult(n: Rep[Any]) = reflectEffect(SetScopeResult(n))
  
  case class DeliteILCollect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
    (size: Exp[Int], callocN: Exp[Int] => Exp[I], cfunc: (Exp[A], Exp[Int]) => Rep[A], cupdate: (Exp[I],Exp[A],Exp[Int]) => Rep[Unit], cfinalizer: Exp[I] => Exp[CA]) extends DeliteOpMapLike[A,I,CA] {
          
    def finalizer(x: Exp[I]) = cfinalizer(x)
    
    lazy val body: Def[CA] = copyBodyOrElse(DeliteCollectElem[A,I,CA](  
      eV = this.eV,     
      sV = this.sV,      
      allocVal = this.allocVal,      
      allocN = reifyEffects(callocN(sV)),
      func = reifyEffects(cfunc(eV,v)),
      update = reifyEffects(cupdate(allocVal,eV,v)),
      finalizer = reifyEffects(this.finalizer(allocVal)),
      par = ParFlat, // TODO: filter
      buf = null
    ))    
  }
  
  def collect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
    (size: Exp[Int], allocN: Exp[Int] => Rep[I], func: (Exp[A], Exp[Int]) => Exp[A], update: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], finalizer: Exp[I] => Exp[CA]) = {
      
      val c = DeliteILCollect(size,allocN,func,update,finalizer)
      reflectEffect(c, summarizeEffects(c.body.asInstanceOf[DeliteCollectElem[A,I,CA]].func))
    }  
    
  case class DeliteILExtern[A:Manifest](funcName: String, allocFunc: () => Exp[A], override val inputs: List[Exp[Any]]) extends DeliteOpExternal[A] {
    def alloc = allocFunc()
  }
    
  def extern[A:Manifest](funcName: String, alloc: => Exp[A], inputs: List[Exp[Any]]) = DeliteILExtern(funcName, () => alloc, inputs)
  
}
