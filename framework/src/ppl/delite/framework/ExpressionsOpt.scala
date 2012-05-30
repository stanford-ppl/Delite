package ppl.delite.framework

import java.util.IdentityHashMap
import scala.virtualization.lms.internal.Expressions

/**
 * Optimizes dependency calculations in LMS by caching sym results.
 * Currently, the cache is never cleared. Should it be?
 */ 
trait ExpressionsOpt extends Expressions {  
  
 /*
  * IdentityHashMap is used intentionally here. Equality on Any is overriden in
  * DeliteOps, which can cause us problems because we really want strict reference
  * equality for the cache to be safe.
  */
  val symsCache = new IdentityHashMap[Product,List[Sym[Any]]]()
  val boundSymsCache = new IdentityHashMap[Product,List[Sym[Any]]]()
  val effectSymsCache = new IdentityHashMap[Product,List[Sym[Any]]]()
  val rsymsCache = new IdentityHashMap[Product,List[Any]]()
  val symsFreqCache = new IdentityHashMap[Product,List[(Sym[Any],Double)]]()  
  
  def cachedOrElse[V](p: Product, x: IdentityHashMap[Product,V])(f: => V) = {
    var res = x.get(p)
    if (res == null) {
      res = f
      x.put(p, res)      
    }
    res
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case p: Product if Config.cacheSyms => cachedOrElse(p,symsCache)(super.syms(e)) 
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case p: Product if Config.cacheSyms => cachedOrElse(p,boundSymsCache)(super.boundSyms(e))
    case _ => super.boundSyms(e)
  }

  override def effectSyms(e: Any): List[Sym[Any]] = e match {
    case p: Product if Config.cacheSyms => cachedOrElse(p,effectSymsCache)(super.effectSyms(e))
    case _ => super.effectSyms(e)
  }

  override def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case p: Product if Config.cacheSyms => cachedOrElse(p,rsymsCache)(super.rsyms(e)(f)).asInstanceOf[List[T]]
    case _ => super.rsyms(e)(f)
  }
  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case p: Product if Config.cacheSyms => cachedOrElse(p,symsFreqCache)(super.symsFreq(e))
    case _ => super.symsFreq(e)
  }
}