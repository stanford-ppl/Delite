package ppl.delite.framework.transform

import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.AbstractSubstTransformer
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.Config

/**
 * Transform nested multiloops by selecting a dimension to parallelize for the GPU.
 * Different traits represent different policies / implementations.
 */

/* Useful methods for manipulating multiloops regardless of policy */
trait GenericMultiloopTransform extends DeliteTransform 
  with DeliteApplication with DeliteOpsExp 
  with BooleanOpsExp with MiscOpsExp with StringOpsExp with ObjectOpsExp with PrimitiveOpsExp
  with LiftString with LiftBoolean with LiftPrimitives { // may want to use self-types instead of mix-in to decrease risk of accidental inclusion
  
  this: DeliteApplication =>
  
  val cudaGen = getCodeGenPkg(cudaTarget) // use a fresh CUDA generator to avoid any interference
     
  private val t = deviceDependentLowering
  
  def isGPUable(block: Block[Any]): Boolean = {
    try {
      cudaGen.innerScope = t.innerScope
      cudaGen.withStream(new PrintWriter(new ByteArrayOutputStream()))(cudaGen.emitBlock(block))
    }
    catch {
      case _ => return false
    }
    true
  }
  
  def blockContainsGPUableCollect(block: Block[Any]) = {
    var foundCollect = false
    t.focusBlock(block) {
      t.focusExactScope(block) { levelScope => 
        foundCollect = levelScope.filter(_.rhs.isInstanceOf[DeliteOpLoop[Any]])
                                 .filter(_.rhs.asInstanceOf[DeliteOpLoop[Any]].body.isInstanceOf[DeliteCollectElem[_,_,_]])
                                 .exists(e=>isGPUable(e.rhs.asInstanceOf[DeliteOpLoop[Any]].body.asInstanceOf[DeliteCollectElem[_,_,_]].func))
      }
    }
    foundCollect    
  }
  
  def loopContainsGPUableCollect(l: DeliteOpLoop[_]) = l.body match {
    case e:DeliteCollectElem[_,_,_] => blockContainsGPUableCollect(e.func)
    case e:DeliteForeachElem[_] => blockContainsGPUableCollect(e.func)
    case e:DeliteReduceElem[_] => blockContainsGPUableCollect(e.func) || blockContainsGPUableCollect(e.rFunc)
    case _ => false
  }
    
  def transformCollectToWhile[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest](s: Sym[_], loop: DeliteOpLoop[_], e: DeliteCollectElem[A,I,CA]): Exp[CA] = {
    // inlined to expose interior parallelism    
    def collectToWhile(size: Rep[Int],
                       alloc: Rep[Int] => Rep[I],
                       f: Rep[Int] => Rep[A],                        
                       upd: (Rep[I],Rep[Int],Rep[A]) => Rep[Unit], 
                       cond: List[Rep[Int] => Rep[Boolean]], 
                       append: (Rep[I],Rep[Int],Rep[A]) => Rep[Boolean], 
                       setSize: (Rep[I],Rep[Int]) => Rep[Unit],
                       finalizer: Rep[I] => Rep[CA]) = {      
                         
      val i = var_new(0) 
      val res = if (cond.nonEmpty) alloc(0) else alloc(size) 
      val appended = var_new(0)
      while (i < size) {
        // mixed staged and interpreted code follows (operations on cond are unlifted)
        if (cond.nonEmpty) {
          if (cond.map(c=>c(i)).reduce((a,b) => a && b)) 
             if (append(res,i,f(i))) appended += 1 
        }
        else 
          upd(res,i,f(i))
        
        i += 1
      }
      if (cond.nonEmpty) setSize(res,appended)
      
      finalizer(res).unsafeImmutable 
    }       
    
    s.asInstanceOf[Sym[CA]].atPhase(t) {
        collectToWhile(      
          t(loop.size),
          {sz => transformBlockWithBound(t, e.allocN, List(e.sV -> sz))},
          {n => transformBlockWithBound(t, e.func, List(loop.v -> n))},                          
          {(col,n,x) => transformBlockWithBound(t, e.update, List(e.allocVal -> col, loop.v -> n, e.eV -> x))},                
          e.cond.map{c => { n: Rep[Int] => transformBlockWithBound(t, c, List(loop.v -> n)) }},
          {(col,n,x) => transformBlockWithBound(t, e.buf.append, List(e.allocVal -> col, loop.v -> n, e.eV -> x))},                
          {(col,sz) => transformBlockWithBound(t, e.buf.setSize, List(e.allocVal -> col, e.sV -> sz))},                
          {col => transformBlockWithBound(t, e.finalizer, List(e.allocVal -> col))})
    }
  }  
  
  def transformForeachToWhile[A:Manifest](s: Sym[_], loop: DeliteOpLoop[_], e: DeliteForeachElem[A]): Exp[Unit] = {
    // inlined to expose interior parallelism    
    def foreachToWhile(size: Rep[Int],
                       f: Rep[Int] => Rep[A]) = {
                         
      val i = var_new(0) 
      while (i < size) {
        f(i)
        i += 1
      }
    }       
    
    s.asInstanceOf[Sym[Unit]].atPhase(t) {
        foreachToWhile(      
          t(loop.size),
          {n => transformBlockWithBound(t, e.func, List(loop.v -> n))})
    }
  }  
  
  def transformReduceToWhile[A:Manifest](s: Sym[_], loop: DeliteOpLoop[_], e: DeliteReduceElem[A]): Exp[A] = {
    // inlined to expose interior parallelism    
    def reduceToWhile(size: Rep[Int],
                      f: Rep[Int] => Rep[A],                        
                      cond: List[Rep[Int] => Rep[Boolean]], 
                      zero: Rep[A],
                      rFunc: (Rep[A],Rep[A]) => Rep[A],
                      stripFirst: Boolean,
                      mutable: Boolean) = {
      
      // mixed staged and interpreted code follows (operations on cond, stripFirst and mutable unlifted)      
      // zero can be primitive even when mutable == true, so we need to save the var regardless        
      val i = if (stripFirst) var_new(1) else var_new(0) 
      val acc = if (stripFirst) var_new(f(0)) else var_new(zero.unsafeImmutable) 
      while (i < size) {
        if (cond.nonEmpty) {
          if (cond.map(c=>c(i)).reduce((a,b) => a && b)) {
            if (mutable) acc = (rFunc(acc.unsafeMutable,f(i))).unsafeImmutable 
            else acc = rFunc(acc, f(i))
           }
        }
        else {
          if (mutable) acc = (rFunc(acc.unsafeMutable,f(i))).unsafeImmutable
          else acc = rFunc(acc, f(i))
        }
        i += 1
      }
      acc.unsafeImmutable      
    }       
    
    s.asInstanceOf[Sym[A]].atPhase(t) {
        reduceToWhile(      
          t(loop.size),
          {n => transformBlockWithBound(t, e.func, List(loop.v -> n))},                          
          e.cond.map{c => { n: Rep[Int] => transformBlockWithBound(t, c, List(loop.v -> n)) }},
          t.reflectBlock(e.zero),
          {(a,b) => transformBlockWithBound(t, e.rFunc, List(e.rV._1 -> a, e.rV._2 -> b))},                
          e.stripFirst,
          loop.asInstanceOf[DeliteOpReduceLike[_]].mutable)
    }
  }  
  
}

/* Always parallelize inner multiloop, if GPUable */ 
// should this happen before or after fusion?
// should we be using something more like NestedBlockTransformer inside TestMiscTransform to find the nested multiloop? 
trait MultiloopTransformOuter extends GenericMultiloopTransform {  
  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = d match {    
    case l:DeliteOpLoop[_] if Config.generateCUDA && loopContainsGPUableCollect(l) => l.body match {
      case e:DeliteCollectElem[_,_,_] => (transformCollectToWhile(s,l,e)(e.mA,e.mI,e.mCA)).asInstanceOf[Exp[A]]
      case e:DeliteForeachElem[_] => (transformForeachToWhile(s,l,e)(e.mA)).asInstanceOf[Exp[A]]
      case e:DeliteReduceElem[_] => (transformReduceToWhile(s,l,e)(e.mA)).asInstanceOf[Exp[A]]
      case _ => super.onCreate(s,d)
    }
    
    case Reflect(l:DeliteOpLoop[_],u,es) if Config.generateCUDA && loopContainsGPUableCollect(l) => l.body match {
      case e:DeliteCollectElem[_,_,_] => reflectTransformed(deviceDependentLowering, (transformCollectToWhile(s,l,e)(e.mA,e.mI,e.mCA)).asInstanceOf[Exp[A]], u, es) 
      case e:DeliteForeachElem[_] => reflectTransformed(deviceDependentLowering, (transformForeachToWhile(s,l,e)(e.mA)).asInstanceOf[Exp[A]], u, es) 
      case e:DeliteReduceElem[_] => reflectTransformed(deviceDependentLowering, (transformReduceToWhile(s,l,e)(e.mA)).asInstanceOf[Exp[A]], u, es)       
      case _ => super.onCreate(s,d)
    } 
       
    case _ => super.onCreate(s,d)
  }
}