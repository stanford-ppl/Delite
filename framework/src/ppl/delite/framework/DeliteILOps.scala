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
  
  case class DeliteILCollect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
    (size: Exp[Int], callocN: Exp[Int] => Exp[I], cfunc: (Exp[A], Exp[Int]) => Exp[A], cupdate: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], cfinalizer: Exp[I] => Exp[CA],
     ccond: List[Exp[Int] => Exp[Boolean]], cpar: DeliteParallelStrategy, 
     cappendable: (Exp[I],Exp[A],Exp[Int]) => Exp[Boolean], cappend: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], csetSize: (Exp[I],Exp[Int]) => Exp[Unit], callocRaw: (Exp[I],Exp[Int]) => Exp[I], ccopyRaw: (Exp[I],Exp[Int],Exp[I],Exp[Int],Exp[Int]) => Exp[Unit]
    ) extends DeliteOpMapLike[A,I,CA] {
    
          
    def finalizer(x: Exp[I]) = cfinalizer(x)
    
    lazy val body: Def[CA] = copyBodyOrElse(DeliteCollectElem[A,I,CA](       
      func = reifyEffects(cfunc(eV,v)),
      cond = ccond.map(cf => reifyEffects(cf(v))),
      par = cpar,
      buf = DeliteBufferElem[A,I,CA](
        eV = this.eV,
        sV = this.sV,
        iV = this.iV,
        iV2 = this.iV2,
        allocVal = this.allocVal,
        aV2 = this.aV2,
        alloc = reifyEffects(callocN(sV)),
        apply = unusedBlock, 
        update = reifyEffects(cupdate(allocVal,eV,v)),        
        appendable = reifyEffects(cappendable(allocVal,eV,v)),
        append = reifyEffects(cappend(allocVal,eV,v)),
        setSize = reifyEffects(csetSize(allocVal,sV)),
        allocRaw = reifyEffects(callocRaw(allocVal,sV)),
        copyRaw = reifyEffects(ccopyRaw(aV2,iV,allocVal,iV2,sV)),
        finalizer = reifyEffects(this.finalizer(allocVal))
      ),
      numDynamicChunks = this.numDynamicChunks
    ))
    
    val mA = manifest[A]
    val mI = manifest[I]  
    val mCA = manifest[CA]  
  }
  
  def collect[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest]
    (size: Exp[Int], allocN: Exp[Int] => Exp[I], func: (Exp[A], Exp[Int]) => Exp[A], update: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], finalizer: Exp[I] => Exp[CA],
     cond: List[Exp[Int] => Exp[Boolean]], par: String, 
     appendable: (Exp[I],Exp[A],Exp[Int]) => Exp[Boolean], append: (Exp[I],Exp[A],Exp[Int]) => Exp[Unit], setSize: (Exp[I],Exp[Int]) => Exp[Unit], allocRaw: (Exp[I],Exp[Int]) => Exp[I], copyRaw: (Exp[I],Exp[Int],Exp[I],Exp[Int],Exp[Int]) => Exp[Unit]
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
    val c = DeliteILCollect(size,allocN,func,update,finalizer,cond,parStrategy,appendable,append,setSize,allocRaw,copyRaw)(manifest[A],refTp,refTp.asInstanceOf[Manifest[CA]]) // HACK: forcing I and CA to be the same in order to retain RefinedManifest from I
    reflectEffect(c, summarizeEffects(c.body.asInstanceOf[DeliteCollectElem[_,_,_]].func).star)(refTp.asInstanceOf[Manifest[CA]],implicitly[SourceContext])
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
  
  
  case class DeliteILReduce[A:Manifest](size: Exp[Int], rfunc: Exp[Int] => Exp[A], rcond: List[Exp[Int] => Exp[Boolean]], rzero: () => Exp[A], raccInit: () => Exp[A], rrfunc: (Exp[A],Exp[A]) => Exp[A], rstripFirst: Boolean) extends DeliteOpReduceLike[A] {
    lazy val body: Def[A] = copyBodyOrElse(DeliteReduceElem[A](
      func = reifyEffects(rfunc(v)),
      cond = rcond.map(cf => reifyEffects(cf(v))),
      zero = reifyEffects(rzero()),
      accInit = reifyEffects(raccInit()),
      rV = this.rV,
      rFunc = reifyEffects(rrfunc(rV._1, rV._2)),
      stripFirst = rstripFirst,
      numDynamicChunks = this.numDynamicChunks
    ))    
    
    val mA = manifest[A]
  }

  def reduce[A:Manifest](size: Exp[Int], func: Exp[Int] => Exp[A], cond: List[Exp[Int] => Exp[Boolean]], zero: => Exp[A], accInit: => Exp[A], rFunc: (Exp[A],Exp[A]) => Exp[A], stripFirst: Boolean): Rep[A] = {
    val f = DeliteILReduce(size,func,cond,() => zero,() => accInit,rFunc,stripFirst)
    reflectEffect(f, summarizeEffects(f.body.asInstanceOf[DeliteReduceElem[A]].func).star)
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
    
    case e@DeliteILCollect(s,a,fu,up,fi,c,p,ape,ap,sz,al,co) => reflectPure(new { override val original = Some(f,e) } with DeliteILCollect(f(s),f(a),f(fu),f(up),f(fi),c.map(f(_)),p,f(ape),f(ap),f(sz),f(al),f(co))(mtype(e.mA),mtype(e.mI),mtype(e.mCA)))(mtype(manifest[A]),implicitly[SourceContext])  
    case Reflect(e@DeliteILCollect(s,a,fu,up,fi,c,p,ape,ap,sz,al,co), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILCollect(f(s),f(a),f(fu),f(up),f(fi),c.map(f(_)),p,f(ape),f(ap),f(sz),f(al),f(co))(mtype(e.mA),mtype(e.mI),mtype(e.mCA)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case e@DeliteILForeach(s,fu) => reflectPure(new { override val original = Some(f,e) } with DeliteILForeach(f(s),f(fu))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])  
    case Reflect(e@DeliteILForeach(s,fu), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILForeach(f(s),f(fu))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case e@DeliteILReduce(s,rf,c,rz,ra,rrf,sf) => 
      e.asInstanceOf[DeliteILReduce[A]] match {
        case e@DeliteILReduce(s,rf,c,rz,ra,rrf,sf) => 
          reflectPure(new { override val original = Some(f,e) } with DeliteILReduce(f(s),f(rf),c.map(f(_)),() => f(rz()),() => f(ra()),f(rrf),sf)(e.mA))(mtype(manifest[A]),implicitly[SourceContext])  
      }
    case Reflect(e@DeliteILReduce(s,rf,c,rz,ra,rrf,sf), u, es) => 
      e.asInstanceOf[DeliteILReduce[A]] match {
        case e@DeliteILReduce(s,rf,c,rz,ra,rrf,sf) => 
          reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteILReduce(f(s),f(rf),c.map(f(_)),() => f(rz()),() => f(ra()),f(rrf),sf)(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
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
