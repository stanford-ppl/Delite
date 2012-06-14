package ppl.delite.framework

import java.util.IdentityHashMap
import scala.virtualization.lms.internal.{Expressions,FatExpressions}
import scala.virtualization.lms.internal.FatScheduling

/**
 * Optimizes dependency calculations in LMS by caching framework results.
 * Currently, the caches are never cleared. Should they be?
 */ 

trait Cache {
  /*
   * IdentityHashMap is used intentionally here. Equality on Any is overriden in
   * DeliteOps, which can cause us problems because we really want strict reference
   * equality for the cache to be safe.
   */  
  def cachedOrElse[K,V](e: K, x: IdentityHashMap[K,V])(f: => V) = {
    if (Config.cacheSyms) {
      var res = x.get(e)
      if (res == null) {
        res = f
        x.put(e, res)      
      }
      res
    }
    else f
  }
  
}

trait ExpressionsOpt extends Expressions with Cache {  
  val symsCache = new IdentityHashMap[Any,List[Sym[Any]]](128)
  val boundSymsCache = new IdentityHashMap[Product,List[Sym[Any]]]()
  val effectSymsCache = new IdentityHashMap[Product,List[Sym[Any]]]()
  val rsymsCache = new IdentityHashMap[Product,List[Any]]()
  val symsFreqCache = new IdentityHashMap[Product,List[(Sym[Any],Double)]]()  
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case i: Iterable[_] => cachedOrElse(i,symsCache)(super.syms(e)) 
    case p: Product => cachedOrElse(p,symsCache)(super.syms(e)) 
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case p: Product => cachedOrElse(p,boundSymsCache)(super.boundSyms(e))
    case _ => super.boundSyms(e)
  }

  override def effectSyms(e: Any): List[Sym[Any]] = e match {
    case p: Product => cachedOrElse(p,effectSymsCache)(super.effectSyms(e))
    case _ => super.effectSyms(e)
  }

  override def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case p: Product => cachedOrElse(p,rsymsCache)(super.rsyms(e)(f)).asInstanceOf[List[T]]
    case _ => super.rsyms(e)(f)
  }
  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case p: Product => cachedOrElse(p,symsFreqCache)(super.symsFreq(e))
    case _ => super.symsFreq(e)
  }
}

trait SchedulingOpt extends FatScheduling with Cache {  
  val IR: FatExpressions
  import IR._    
  
  // val schedCache = new IdentityHashMap[Exp[Any],List[TP[Any]]]()
  // val depCache = new IdentityHashMap[List[Sym[Any]],List[TP[Any]]]()
  // val fatDepCache = new IdentityHashMap[(List[TTP],List[Sym[Any]]),List[TTP]]()
  
  // override def buildScheduleForResult(start: Exp[Any]): List[TP[Any]] = {
  //   cachedOrElse(start, schedCache)(super.buildScheduleForResult(start))
  // }  
  
  // override def getDependentStuff(start: List[Sym[Any]]): List[TP[Any]] = {
  //   Nil
    // cachedOrElse(start, depCache)(super.getDependentStuff(start))
  // }  
  
  // override def getFatDependentStuff(scope: List[TTP])(sts: List[Sym[Any]]): List[TTP] = {
  //   cachedOrElse((scope,sts), fatDepCache)(super.getFatDependentStuff(scope)(sts))
  // }
}