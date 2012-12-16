package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.reflect.SourceContext
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
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
import datastructures.{DeliteArray,DeliteArrayFatExp}
import datastructures.DeliteArrayStructTags

trait DeliteILOps extends Variables with StructOps with StructTags with DeliteArrayStructTags with OverloadHack {
  this: DeliteIL =>
  
  implicit def varManifest[T:Manifest](x: Var[T]): Manifest[Var[T]]
  implicit def daVarManifest: Manifest[Var[DeliteArray[Record]]] // why is this not covered by the previous def?
  
  // data exchange
  // note that these are different than the methods in DeliteRestage.scala, because these are constructed in
  // the restaged code directly - we need to resolve rather than code generate them.
  def getScopeResult: Rep[Any]
  def setScopeResult(n: Rep[Any]): Rep[Unit]
  
  // expose delite array stuff that is needed for buffer elems
  def darray_unsafe_set_act_buf[A:Manifest](da: Rep[DeliteArray[A]]): Rep[Unit]
  def darray_unsafe_set_act_final[A:Manifest](da: Rep[DeliteArray[A]]): Rep[Unit]
  def darray_unsafe_get_act_size(): Rep[Int]
  def darray_unsafe_copy[T:Manifest](src: Rep[DeliteArray[T]], srcPos: Rep[Int], dest: Rep[DeliteArray[T]], destPos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  
  // expose struct method from StructExp for restaging
  def struct[T:Manifest](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit o: Overloaded1, pos: SourceContext): Rep[T]
  def mstruct[T:Manifest](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit o: Overloaded1, pos: SourceContext): Rep[T]
  def field_update[T:Manifest](struct: Rep[Any], index: String, rhs: Rep[T]): Rep[Unit] 
    
  // delite ops
  def collect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
    (size: Rep[Int], allocN: Rep[Int] => Rep[I], func: (Rep[A], Rep[Int]) => Rep[A], update: (Rep[I],Rep[A],Rep[Int]) => Rep[Unit], finalizer: Rep[I] => Rep[CA],
     cond: List[Rep[Int] => Rep[Boolean]], par: String, 
     append: (Rep[I],Rep[A],Rep[Int]) => Rep[Boolean], setSize: (Rep[I],Rep[Int]) => Rep[Unit], allocRaw: (Rep[I],Rep[Int]) => Rep[I], copyRaw: (Rep[I],Rep[Int],Rep[I],Rep[Int],Rep[Int]) => Rep[Unit]
    ): Rep[CA]
    
  def foreach[A:Manifest](size: Rep[Int], func: Rep[Int] => Rep[Unit]): Rep[Unit]
    
  def extern[A:Manifest](funcName: String, alloc: => Rep[A], inputs: List[Rep[Any]]): Rep[A]
}

trait DeliteILOpsExp extends DeliteILOps with DeliteOpsExp with DeliteArrayFatExp with ListOpsExp {
  this: DeliteILExp =>
  
  def varManifest[T:Manifest](x: Var[T]) = manifest[Variable[T]]
  def daVarManifest = manifest[Variable[DeliteArray[Record]]]
  
  case class GetScopeResult() extends Def[Any]
  def getScopeResult = {
    // find last SetScopeResult effect and unpack it to directly connect
    val e = context.collect{ case Def(Reflect(SetScopeResult(n), u, es)) => n }
    e.last
  }
  
  case class SetScopeResult(n: Rep[Any]) extends Def[Unit]
  def setScopeResult(n: Rep[Any]) = reflectEffect(SetScopeResult(n))
  
  // case class DeliteILCollect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
  //   (size: Exp[Int], callocN: Exp[Int] => Exp[I], cfunc: (Exp[A], Exp[Int]) => Rep[A], cupdate: (Exp[I],Exp[A],Exp[Int]) => Rep[Unit], cfinalizer: Exp[I] => Exp[CA]) extends DeliteOpMapLike[A,I,CA] {
  case class DeliteILCollect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
    (size: Exp[Int], callocN: Exp[Int] => Exp[I], cfunc: (Exp[A], Exp[Int]) => Exp[A], cupdate: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], cfinalizer: Exp[I] => Exp[CA],
     ccond: List[Exp[Int] => Exp[Boolean]], cpar: DeliteParallelStrategy, 
     cappend: (Exp[I],Exp[A],Exp[Int]) => Exp[Boolean], csetSize: (Exp[I],Exp[Int]) => Exp[Unit], callocRaw: (Exp[I],Exp[Int]) => Exp[I], ccopyRaw: (Exp[I],Exp[Int],Exp[I],Exp[Int],Exp[Int]) => Exp[Unit]
    ) extends DeliteOpMapLike[A,I,CA] {
    
          
    def finalizer(x: Exp[I]) = cfinalizer(x)
    
    lazy val body: Def[CA] = copyBodyOrElse(DeliteCollectElem[A,I,CA](  
      eV = this.eV,     
      sV = this.sV,      
      allocVal = this.allocVal,      
      allocN = reifyEffects(callocN(sV)),
      func = reifyEffects(cfunc(eV,v)),
      update = reifyEffects(cupdate(allocVal,eV,v)),
      finalizer = reifyEffects(this.finalizer(allocVal)),
      cond = ccond.map(cf => reifyEffects(cf(v))),
      par = cpar,
      buf = DeliteBufferElem[A,I,CA](
        iV = this.iV,
        iV2 = this.iV2,
        aV = this.aV,
        append = reifyEffects(cappend(allocVal,eV,v)),
        setSize = reifyEffects(csetSize(allocVal,sV)),
        allocRaw = reifyEffects(callocRaw(allocVal,sV)),
        copyRaw = reifyEffects(ccopyRaw(aV,iV,allocVal,iV2,sV))
      )
    ))
    
    val mA = manifest[A]
    val mI = manifest[I]  
    val mCA = manifest[CA]  
  }
  
  // def collect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
  //   (size: Exp[Int], allocN: Exp[Int] => Rep[I], func: (Exp[A], Exp[Int]) => Exp[A], update: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], finalizer: Exp[I] => Exp[CA]) = {
    def collect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
      (size: Exp[Int], allocN: Exp[Int] => Exp[I], func: (Exp[A], Exp[Int]) => Exp[A], update: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], finalizer: Exp[I] => Exp[CA],
       cond: List[Exp[Int] => Exp[Boolean]], par: String, 
       append: (Exp[I],Exp[A],Exp[Int]) => Exp[Boolean], setSize: (Exp[I],Exp[Int]) => Exp[Unit], allocRaw: (Exp[I],Exp[Int]) => Exp[I], copyRaw: (Exp[I],Exp[Int],Exp[I],Exp[Int],Exp[Int]) => Exp[Unit]
      ) = {
      
      val parStrategy = par match {
        case "ParFlat" => ParFlat
        case "ParBuffer" => ParBuffer
      }
      
      // -- hack: need to get the RefinedManifest from allocN, but we don't want to expose its effects
      // if we enclose it in a reify, we lose the manifest
      val save = context
      context = Nil      
      //val a1 = reifyEffects(allocN(fresh[Int]))
      val a1 = allocN(fresh[Int])
      context = save
      val refTp = a1.tp
      val c = DeliteILCollect(size,allocN,func,update,finalizer,cond,parStrategy,append,setSize,allocRaw,copyRaw)(manifest[A],refTp,refTp.asInstanceOf[Manifest[CA]]) // HACK: forcing I and CA to be the same in order to retain RefinedManifest from I
      reflectEffect(c, summarizeEffects(c.body.asInstanceOf[DeliteCollectElem[_,_,_]].func))(refTp.asInstanceOf[Manifest[CA]],implicitly[SourceContext])
    }  
    
  case class DeliteILForeach[A:Manifest](size: Exp[Int], ffunc: Exp[Int] => Exp[Unit]) extends DeliteOpLoop[Unit] {
    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = reifyEffects(ffunc(v)),
      sync = reifyEffects(List[Any]())
    ))    
    
    val mA = manifest[A]
  }
  
  def foreach[A:Manifest](size: Exp[Int], func: Exp[Int] => Exp[Unit]) = {
    val f = DeliteILForeach(size,func)
    reflectEffect(f, summarizeEffects(f.body.asInstanceOf[DeliteForeachElem[A]].func))
  }
    
  case class DeliteILExtern[A:Manifest](funcName: String, allocFunc: () => Exp[A], override val inputs: List[Exp[Any]]) extends DeliteOpExternal[A] {
    def alloc = allocFunc()
    val mA = manifest[A]
  }
    
  def extern[A:Manifest](funcName: String, alloc: => Exp[A], inputs: List[Exp[Any]]) = DeliteILExtern(funcName, () => alloc, inputs)
  
  def mstruct[T](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit m: Manifest[T], o: Overloaded1, pos: SourceContext) = reflectMutable(SimpleStruct(tag, elems.map(p=>(p._1,var_new(p._2)(p._2.tp,pos).e))))(m,pos)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case GetScopeResult() => getScopeResult
    case Reflect(SetScopeResult(n),u,es) => reflectMirrored(Reflect(SetScopeResult(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))   
    
    case e@DeliteILCollect(s,a,fu,up,fi,c,p,ap,sz,al,co) => reflectPure(new { override val original = Some(f,e) } with DeliteILCollect(f(s),f(a),f(fu),f(up),f(fi),c.map(f(_)),p,f(ap),f(sz),f(al),f(co))(e.mA,e.mI,e.mCA))(mtype(manifest[A]),implicitly[SourceContext])  
    case Reflect(e@DeliteILCollect(s,a,fu,up,fi,c,p,ap,sz,al,co), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILCollect(f(s),f(a),f(fu),f(up),f(fi),c.map(f(_)),p,f(ap),f(sz),f(al),f(co))(e.mA,e.mI,e.mCA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case e@DeliteILForeach(s,fu) => reflectPure(new { override val original = Some(f,e) } with DeliteILForeach(f(s),f(fu))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])  
    case Reflect(e@DeliteILForeach(s,fu), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILForeach(f(s),f(fu))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case e@DeliteILExtern(s,fu,i) => reflectPure(new { override val original = Some(f,e) } with DeliteILExtern(s,() => f(fu()),f(i))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])  
    case Reflect(e@DeliteILExtern(s,fu,i), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILExtern(s,() => f(fu()),f(i))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    
    // TODO: move to LMS
    case e@HashMapSize(m) => hashmap_size(f(m))(e.mK,e.mV,pos)
    case Reflect(e@HashMapSize(m), u, es) => reflectMirrored(Reflect(HashMapSize(f(m))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))            
    
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  
      
}

trait ScalaGenDeliteILOps extends GenericFatCodegen with ScalaGenFat {
  val IR: DeliteILOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SetScopeResult(n) => emitValDef(sym, "() // set scope result happened here (all getScopeResults should have been short-circuited)")
    case _ => super.emitNode(sym, rhs)
  }
}
