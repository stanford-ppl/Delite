package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.reflect.SourceContext
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}
import scala.virtualization.lms.util.OverloadHack

import codegen.cpp.TargetCpp
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
  
  // profiling
  def tic(deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_start(unit("app"),deps)
  def tic(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_start(component,deps)
  def toc(deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_stop(unit("app"),deps.map(_.unsafeImmutable))
  def toc(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_stop(component,deps)
  
  def profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]

  // data exchange
  // note that these are different than the methods in DeliteRestage.scala, because these are constructed in
  // the restaged code directly - we need to resolve rather than code generate them.
  def getScopeResult: Rep[Any]
  def setScopeResult(n: Rep[Any]): Rep[Unit]
  
  // expose delite array stuff that is needed for buffer elems
  def darray_unsafe_set_act_buf[A:Manifest](da: Rep[DeliteArray[A]]): Rep[Unit]
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
     appendable: (Rep[I],Rep[A],Rep[Int]) => Rep[Boolean], append: (Rep[I],Rep[A],Rep[Int]) => Rep[Unit], setSize: (Rep[I],Rep[Int]) => Rep[Unit], allocRaw: (Rep[I],Rep[Int]) => Rep[I], copyRaw: (Rep[I],Rep[Int],Rep[I],Rep[Int],Rep[Int]) => Rep[Unit]
    ): Rep[CA]
    
  def foreach[A:Manifest](size: Rep[Int], func: Rep[Int] => Rep[Unit]): Rep[Unit]
  
  def reduce[A:Manifest](size: Rep[Int], func: Rep[Int] => Rep[A], cond: List[Rep[Int] => Rep[Boolean]], zero: => Rep[A], accInit: => Rep[A], rFunc: (Rep[A],Rep[A]) => Rep[A], stripFirst: Boolean): Rep[A]
    
  def extern[A:Manifest](funcName: String, alloc: => Rep[A], inputs: List[Rep[Any]]): Rep[A]
  
  def single[A:Manifest](func: => Rep[A]): Rep[A]
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
  
  case class DeliteILCollect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest](
      size: Exp[Int], callocN: Exp[Int] => Exp[I], cfunc: (Exp[A], Exp[Int]) => Exp[DeliteCollection[A]], cupdate: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], cfinalizer: Exp[I] => Exp[CA],
      cunknownOutputSize: Boolean, cappendable: Option[(Exp[I],Exp[A],Exp[Int]) => Exp[Boolean]], cappend: Option[(Exp[I],Exp[A],Exp[Int]) => Exp[Unit]],
      csetSize: Option[(Exp[I],Exp[Int]) => Exp[Unit]], callocRaw: Option[(Exp[I],Exp[Int]) => Exp[I]], 
      ccopyRaw: Option[(Exp[I],Exp[Int],Exp[I],Exp[Int],Exp[Int]) => Exp[Unit]])
      extends DeliteOpFlatMapLike[A,I,CA] {
          
    def finalizer(x: Exp[I]) = cfinalizer(x)
    def flatMapLikeFunc(): Exp[DeliteCollection[A]] = cfunc(eV,v)
    override val unknownOutputSize = cunknownOutputSize

    override lazy val buf = cappendable match {
      case None => DeliteCollectFlatOutput[A,I,CA](
        eV = this.eV,
        sV = this.sV,
        allocVal = this.allocVal,
        alloc = reifyEffects(callocN(sV)),
        update = reifyEffects(cupdate(allocVal,eV,v)),        
        finalizer = reifyEffects(this.finalizer(allocVal))
      )

      case Some(_) => DeliteCollectBufferOutput[A,I,CA](
        eV = this.eV,
        sV = this.sV,
        iV = this.iV,
        iV2 = this.iV2,
        allocVal = this.allocVal,
        aV2 = this.aV2,
        alloc = reifyEffects(callocN(sV)),
        update = reifyEffects(cupdate(allocVal,eV,v)),        
        appendable = reifyEffects(cappendable.get(allocVal,eV,v)),
        append = reifyEffects(cappend.get(allocVal,eV,v)),
        setSize = reifyEffects(csetSize.get(allocVal,sV)),
        allocRaw = reifyEffects(callocRaw.get(allocVal,sV)),
        copyRaw = reifyEffects(ccopyRaw.get(aV2,iV,allocVal,iV2,sV)),
        finalizer = reifyEffects(this.finalizer(allocVal))
      )
    }

    val mA = manifest[A]
    val mI = manifest[I]
    val mCA = manifest[CA]  
  }
  
  def collect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest](
      size: Exp[Int], allocN: Exp[Int] => Exp[I], func: (Exp[A], Exp[Int]) => Exp[DeliteCollection[A]], update: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], finalizer: Exp[I] => Exp[CA],
      unknownOutputSize: Boolean, cappendable: Option[(Exp[I],Exp[A],Exp[Int]) => Exp[Boolean]], cappend: Option[(Exp[I],Exp[A],Exp[Int]) => Exp[Unit]],
      csetSize: Option[(Exp[I],Exp[Int]) => Exp[Unit]], callocRaw: Option[(Exp[I],Exp[Int]) => Exp[I]], 
      ccopyRaw: Option[(Exp[I],Exp[Int],Exp[I],Exp[Int],Exp[Int]) => Exp[Unit]]) = {
    
    // -- hack: need to get the RefinedManifest from allocN, but we don't want to expose its effects
    // if we enclose it in a reify, we lose the manifest
    val save = context
    context = Nil      
    //val a1 = reifyEffects(allocN(fresh[Int]))
    val a1 = allocN(fresh[Int])
    context = save
    val refTp = a1.tp

    val c = DeliteILCollect(size,allocN,func,update,finalizer,unknownOutputSize,cappendable,cappend,csetSize,callocRaw,ccopyRaw)(manifest[A],refTp,refTp.asInstanceOf[Manifest[CA]]) // HACK: forcing I and CA to be the same in order to retain RefinedManifest from I
    reflectEffect(c, summarizeEffects(c.body.asInstanceOf[DeliteCollectElem[_,_,_]].iFunc).star)(refTp.asInstanceOf[Manifest[CA]],implicitly[SourceContext])
  }  
  
  case class DeliteILForeach[A:Manifest](size: Exp[Int], ffunc: Exp[Int] => Exp[Unit]) extends DeliteOpLoop[Unit] {
    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = reifyEffects(ffunc(v)),
      numDynamicChunks = this.numDynamicChunks   
    ))    
    
    val mA = manifest[A]
  }
  
  def foreach[A:Manifest](size: Exp[Int], func: Exp[Int] => Exp[Unit]) = {
    val f = DeliteILForeach(size,func)
    val u = summarizeEffects(f.body.asInstanceOf[DeliteForeachElem[A]].func).star
    reflectEffect(f, summarizeEffects(f.body.asInstanceOf[DeliteForeachElem[A]].func).star andAlso Simple()) // TODO: don't want Simple()
  }


  case class DeliteILReduce[A:Manifest](size: Exp[Int], ilfunc: Exp[Int] => Exp[DeliteCollection[A]], reduce: (Exp[A],Exp[A]) => Exp[A]) extends DeliteOpReduceLike[A] {

    def flatMapLikeFunc(): Exp[DeliteCollection[A]] = ilfunc(v)
    
    val mA = manifest[A]
  }

  def reduce[A:Manifest](size: Exp[Int], ilfunc: Exp[Int] => Exp[DeliteCollection[A]], rFunc: (Exp[A],Exp[A]) => Exp[A]): Rep[A] = {

    val f = DeliteILReduce(size, ilfunc, rFunc)
    reflectEffect(f, summarizeEffects(f.body.asInstanceOf[DeliteReduceElem[A]].iFunc).star)
  }

  case class DeliteILExtern[A:Manifest](funcName: String, allocFunc: () => Exp[A], override val inputs: List[Exp[Any]]) extends DeliteOpExternal[A] {
    def alloc = allocFunc()
    val mA = manifest[A]
  }
    
  def extern[A:Manifest](funcName: String, alloc: => Exp[A], inputs: List[Exp[Any]]) = DeliteILExtern(funcName, () => alloc, inputs)
 
  
  case class DeliteILSingleTask[A:Manifest](func: () => Exp[A]) extends DeliteOpSingleTask[A](reifyEffectsHere(func())) {
    val mA = manifest[A]
  }

  def single[A:Manifest](func: => Exp[A]) = {
    val f = DeliteILSingleTask(() => func)
    reflectEffect(f, summarizeEffects(f.block).star)
    //func
  }
 

  // mutable structs
  def mstruct[T](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit m: Manifest[T], o: Overloaded1, pos: SourceContext) = reflectMutable(SimpleStruct(tag, elems.map(p=>(p._1,var_new(p._2)(p._2.tp,pos).e))))(m,pos)

  // profiling
  case class ProfileStart(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]
  case class ProfileStop(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]

  def profile_start(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ProfileStart(component, deps.toList))
  def profile_stop(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ProfileStop(component, deps.toList))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case GetScopeResult() => getScopeResult
    case Reflect(SetScopeResult(n),u,es) => reflectMirrored(Reflect(SetScopeResult(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)   
    case e@DeliteILCollect(s,a,fu,up,fi,uos,ap,app,se,al,co) => reflectPure(new { override val original = Some(f,e) } with DeliteILCollect(
        f(s),f(a),f(fu),f(up),f(fi),uos,ap.map(f(_)),app.map(f(_)),se.map(f(_)),al.map(f(_)),co.map(f(_))
      )(mtype(e.mA),mtype(e.mI),mtype(e.mCA)))(mtype(manifest[A]),implicitly[SourceContext])  
    case Reflect(e@DeliteILCollect(s,a,fu,up,fi,uos,ap,app,se,al,co), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILCollect(
        f(s),f(a),f(fu),f(up),f(fi),uos,ap.map(f(_)),app.map(f(_)),se.map(f(_)),al.map(f(_)),co.map(f(_))
      )(mtype(e.mA),mtype(e.mI),mtype(e.mCA)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case e@DeliteILForeach(s,fu) => reflectPure(new { override val original = Some(f,e) } with DeliteILForeach(f(s),f(fu))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])  
    case Reflect(e@DeliteILForeach(s,fu), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILForeach(f(s),f(fu))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case e@DeliteILReduce(s,ilf,red) => 
      e.asInstanceOf[DeliteILReduce[A]] match {
        case e@DeliteILReduce(s,ilf,red) => 
          reflectPure(new { override val original = Some(f,e) } with DeliteILReduce(f(s),f(ilf),f(red))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])  
      }
    case Reflect(e@DeliteILReduce(s,ilf,red), u, es) => 
      e.asInstanceOf[DeliteILReduce[A]] match {
        case e@DeliteILReduce(s,ilf,red) => 
          reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILReduce(f(s),f(ilf),f(red))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      }
    case e@DeliteILExtern(s,fu,i) => reflectPure(new { override val original = Some(f,e) } with DeliteILExtern(s,() => f(fu()),f(i))(mtype(e.mA)))(mtype(manifest[A]),pos)  
    case Reflect(e@DeliteILExtern(s,fu,i), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILExtern(s,() => f(fu()),f(i))(mtype(e.mA)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)    
    case e@DeliteILSingleTask(fu) => reflectPure(new { override val original = Some(f,e) } with DeliteILSingleTask(() => f(fu()))(mtype(e.mA)))(mtype(manifest[A]),implicitly[SourceContext])  
    case Reflect(e@DeliteILSingleTask(fu), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILSingleTask(() => f(fu()))(mtype(e.mA)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)    

    case Reflect(ProfileStart(c,deps), u, es) => reflectMirrored(Reflect(ProfileStart(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ProfileStop(c,deps), u, es) => reflectMirrored(Reflect(ProfileStop(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  
 
  override def syms(e: Any): List[Sym[Any]] = e match {
    case SetScopeResult(x) => Nil
    case _ => super.syms(e)
  } 

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case SetScopeResult(x) => Nil
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenDeliteILOps extends GenericFatCodegen with ScalaGenFat {
  val IR: DeliteILOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SetScopeResult(n) => emitValDef(sym, "() // set scope result happened here (all getScopeResults should have been short-circuited)")
    case ProfileStart(c,deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(" + quote(c) + ", false)")
    case ProfileStop(c,deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.stop(" + quote(c) + ", false)")
    case _ => super.emitNode(sym, rhs)
  }
}
