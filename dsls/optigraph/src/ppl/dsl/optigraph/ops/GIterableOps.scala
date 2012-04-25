package ppl.dsl.optigraph.ops

import ppl.delite.framework.ops._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.dsl.optigraph._
import scala.virtualization.lms.common._
import scala.virtualization.lms.util._
import reflect.Manifest
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import java.io.PrintWriter

trait GIterableOps extends Variables {
  this: OptiGraph =>

  /** 
   * GIterable objects are returned by range words on graphs, nodes, and collections
   * and can be used in iterations and parallel filter and reduction expressions 
   * (they cannot be created)
   */
    
  implicit def repGIterableToGIterableOps[T:Manifest](iter: Rep[GIterable[T]]) = new GIterableOpsCls(iter)

  /** Operations of GIterables */
  class GIterableOpsCls[T:Manifest](iter: Rep[GIterable[T]]) {
    /** Parallel iteration */
    def foreach(block: Rep[T] => Rep[Unit]): Rep[Unit] = iter_foreach(iter, block)
    /** Sequential iteration */
    def forseq(block: Rep[T] => Rep[Unit]): Rep[Unit] = iter_for(iter, None, block)
    def forseq(filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Unit]): Rep[Unit] = iter_for(iter, Some(filter), block)
    /** Returns a filtered GIterable collection of elements */
    def filter(pred: Rep[T] => Rep[Boolean]): Rep[GIterable[T]] = iter_filter(iter, pred)
    /** Reductions */
    def sum[A:Manifest:Numeric](block: Rep[T] => Rep[A]): Rep[A] = iter_sum(iter, block)
    def sum[A:Manifest:Numeric](filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A] = iter_sumIf(iter, filter, block)
    def product[A:Manifest:Numeric](block: Rep[T] => Rep[A]): Rep[A] = iter_product(iter, block)
    def product[A:Manifest:Numeric](filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A] = iter_productIf(iter, filter, block)
    def max[A:Manifest:Ordering](block: Rep[T] => Rep[A]): Rep[A] = iter_max(iter, block)
    def max[A:Manifest:Ordering](filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A] = iter_maxIf(iter, filter, block)
    def min[A:Manifest:Ordering](block: Rep[T] => Rep[A]): Rep[A] = iter_min(iter, block)
    def min[A:Manifest:Ordering](filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A] = iter_minIf(iter, filter, block)
    // counts the number of elements for which the predicate holds
    def count(pred: Rep[T] => Rep[Boolean]): Rep[Int] = iter_count(iter, pred)
    // boolean AND
    def all(block: Rep[T] => Rep[Boolean]): Rep[Boolean] = iter_all(iter, block)
    def all(filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Boolean]): Rep[Boolean] = iter_allIf(iter, filter, block)
    // boolean OR
    def any(block: Rep[T] => Rep[Boolean]): Rep[Boolean] = iter_any(iter, block)
    def any(filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Boolean]): Rep[Boolean] = iter_anyIf(iter, filter, block)
    /** Returns the elements as a set */
    def toSet(): Rep[GSet[T]] = iter_toset(iter)
  }
  
  def iter_foreach[T:Manifest](iter: Rep[GIterable[T]], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def iter_for[T:Manifest](iter: Rep[GIterable[T]], filter: Option[Rep[T] => Rep[Boolean]], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def iter_filter[T:Manifest](iter: Rep[GIterable[T]], pred: Rep[T] => Rep[Boolean]): Rep[GIterable[T]]
  def iter_sum[T:Manifest, A:Manifest:Numeric](iter: Rep[GIterable[T]], block: Rep[T] => Rep[A]): Rep[A]
  def iter_sumIf[T:Manifest, A:Manifest:Numeric](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A]
  def iter_product[T:Manifest, A:Manifest:Numeric](iter: Rep[GIterable[T]], block: Rep[T] => Rep[A]): Rep[A]
  def iter_productIf[T:Manifest, A:Manifest:Numeric](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A]
  def iter_max[T:Manifest, A:Manifest:Ordering](iter: Rep[GIterable[T]], block: Rep[T] => Rep[A]): Rep[A]
  def iter_maxIf[T:Manifest, A:Manifest:Ordering](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A]
  def iter_min[T:Manifest, A:Manifest:Ordering](iter: Rep[GIterable[T]], block: Rep[T] => Rep[A]): Rep[A]
  def iter_minIf[T:Manifest, A:Manifest:Ordering](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A]
  def iter_count[T:Manifest](iter: Rep[GIterable[T]], block: Rep[T] => Rep[Boolean]): Rep[Int]
  def iter_all[T:Manifest](iter: Rep[GIterable[T]], block: Rep[T] => Rep[Boolean]): Rep[Boolean]
  def iter_allIf[T:Manifest](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Boolean]): Rep[Boolean]
  def iter_any[T:Manifest](iter: Rep[GIterable[T]], block: Rep[T] => Rep[Boolean]): Rep[Boolean]
  def iter_anyIf[T:Manifest](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Boolean]): Rep[Boolean]
  def iter_toset[T:Manifest](iter: Rep[GIterable[T]]): Rep[GSet[T]]                                                                                                                        
}

trait GIterableOpsExp extends GIterableOps with VariablesExp with BaseFatExp {
  this: OptiGraphExp =>
  
  case class GIterableNewEmpty[T]()(val mGIt: Manifest[GIterable[T]]) extends Def[GIterable[T]]
  case class GIterableToList[T:Manifest](iter: Exp[GIterable[T]]) extends Def[List[T]]
  case class GIterableToSet[T:Manifest](iter: Exp[GIterable[T]]) extends Def[GSet[T]]
  
  // parallel iteration (no reduction assignments in the loop body)
  case class GIterableForeach[T:Manifest](in: Exp[GIterable[T]], func: Exp[T] => Exp[Unit])
   extends DeliteOpForeach[T] {
    val size = dc_size(in)
    def sync = n => List()
  }
  
  // parallel filter
  case class GIterableFilter[T:Manifest](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean])
    extends DeliteOpFilter[T,T,GIterable[T]] {
    def alloc = new_empty_iterable()
    def func = e => e 
    val size = dc_size(in)
  }
  
  // called during a BFS traversal
  // returns the down neighbors of all the nodes in the GItarable 'in' object (i.e. the next BFS level)
  case class GIterableNextBFSLevel(in: Exp[GIterable[Node]]) extends DeliteOpMapReduce[Node, GSet[Node]] {
    //override val mutable = true
    val size = dc_size(in)
    val zero: Exp[NodeSet] = NodeSet()
    // get the down neighbors of each node
    def map: Exp[Node] => Exp[NodeSet] = {
      n => iter_toset(n.DownNbrs)
    }
    // concatenate the down neighbor sets of the nodes
    // TODO: optimize such that elements are added to one set
    def reduce: (Exp[NodeSet], Exp[NodeSet]) => Exp[NodeSet] = {
      (a,b) => {
        val out = NodeSet()
        for(n <- a.Items) {
          out.Add(n)
        }
        for(n <- b.Items) {
          out.Add(n)
        }
        out
        
        //gset_addset(a, b); a
      }
    }
  }
  
  // Parallel reductions
  
  case class GIterableSum[T:Manifest, A:Manifest:Numeric](in: Exp[GIterable[T]], map: Exp[T] => Exp[A]) 
   extends DeliteOpMapReduce[T, A] {
    val size = dc_size(in)
    val zero = unit(0.asInstanceOf[A])
    def reduce = (a,b) => a + b
  }
  
  case class GIterableSumIf[T:Manifest, A:Manifest:Numeric](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[A]) 
   extends DeliteOpFilterReduce[T, A] {
    val size = dc_size(in)
    val zero = unit(0.asInstanceOf[A])
    def reduce = (a,b) => a + b
  }
  
  case class GIterableProduct[T:Manifest, A:Manifest:Numeric](in: Exp[GIterable[T]], map: Exp[T] => Exp[A]) 
   extends DeliteOpMapReduce[T,A] {
    val size = dc_size(in)
    val zero = unit(1.asInstanceOf[A])
    def reduce = (a,b) => a * b
  }
  
  case class GIterableProductIf[T:Manifest, A:Manifest:Numeric](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[A]) 
   extends DeliteOpFilterReduce[T,A] {
    val size = dc_size(in)
    val zero = unit(1.asInstanceOf[A])
    def reduce = (a,b) => a * b
  }
  
  case class GIterableMax[T:Manifest, A:Manifest:Ordering](in: Exp[GIterable[T]], map: Exp[T] => Exp[A]) 
   extends DeliteOpMapReduce[T,A] {
    val size = dc_size(in)
    val mA: Manifest[A] = manifest[A]
    val zero = (mA match {
      case Manifest.Double => MIN_DOUBLE
      case Manifest.Float => MIN_FLOAT
      case Manifest.Int =>  MIN_INT
      case _ => throw new RuntimeException()
    }).asInstanceOf[Exp[A]]
    def reduce = (a,b) => if (a > b) a else b
  }
  
  case class GIterableMaxIf[T:Manifest, A:Manifest:Ordering](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[A]) 
   extends DeliteOpFilterReduce[T,A] {
    val size = dc_size(in)
    val mA: Manifest[A] = manifest[A]
    val zero = (mA match {
      case Manifest.Double => MIN_DOUBLE
      case Manifest.Float => MIN_FLOAT
      case Manifest.Int =>  MIN_INT
      case _ => throw new RuntimeException()
    }).asInstanceOf[Exp[A]]
    def reduce = (a,b) => if (a > b) a else b
  }
  
  case class GIterableMin[T:Manifest, A:Manifest:Ordering](in: Exp[GIterable[T]], map: Exp[T] => Exp[A]) 
   extends DeliteOpMapReduce[T,A] {
    val size = dc_size(in)
    val mA: Manifest[A] = manifest[A]
    val zero = (mA match {
      case Manifest.Double => MAX_DOUBLE
      case Manifest.Float => MAX_FLOAT
      case Manifest.Int =>  MAX_INT
      case _ => throw new RuntimeException()
    }).asInstanceOf[Exp[A]]
    def reduce = (a,b) => if (a < b) a else b
  }
  
  case class GIterableMinIf[T:Manifest, A:Manifest:Ordering](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[A]) 
   extends DeliteOpFilterReduce[T,A] {
    val size = dc_size(in)
    val mA: Manifest[A] = manifest[A]
    val zero = (mA match {
      case Manifest.Double => MAX_DOUBLE
      case Manifest.Float => MAX_FLOAT
      case Manifest.Int =>  MAX_INT
      case _ => throw new RuntimeException()
    }).asInstanceOf[Exp[A]]
    def reduce = (a,b) => if (a < b) a else b
  }
  
  case class GIterableCount[T:Manifest](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean]) 
   extends DeliteOpFilterReduce[T,Int] {
    val size = dc_size(in)
    val zero = unit(0)
    def func = e => unit(1)
    def reduce = (a,b) => a + b
  }
  
  case class GIterableAll[T:Manifest](in: Exp[GIterable[T]], map: Exp[T] => Exp[Boolean]) 
   extends DeliteOpMapReduce[T,Boolean] {
    val size = dc_size(in)
    val zero = unit(true)
    def reduce = (a,b) => a && b
  }
  
  case class GIterableAllIf[T:Manifest](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[Boolean]) 
   extends DeliteOpFilterReduce[T,Boolean] {
    val size = dc_size(in)
    val zero = unit(true)
    def reduce = (a,b) => a && b
  }
  
  case class GIterableAny[T:Manifest](in: Exp[GIterable[T]], map: Exp[T] => Exp[Boolean]) 
   extends DeliteOpMapReduce[T,Boolean] {
    val size = dc_size(in)
    val zero = unit(false)
    def reduce = (a,b) => a || b
  }
  
  case class GIterableAnyIf[T:Manifest](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[Boolean]) 
   extends DeliteOpFilterReduce[T,Boolean] {
    val size = dc_size(in)
    val zero = unit(false)
    def reduce = (a,b) => a || b
  }
 
  // parallel foreach with reduction assignments
  case class ConstructFatLoop(val size: Exp[Int], val v: Sym[Int], body: scala.List[Def[Any]], symList: scala.List[Sym[Any]]) extends Def[Unit]
  
  def new_empty_iterable[T:Manifest]() = reflectMutable(GIterableNewEmpty()(manifest[GIterable[T]]))
  def iter_sum[T:Manifest, A:Manifest:Numeric](iter: Exp[GIterable[T]], block: Exp[T] => Exp[A]) = reflectPure(GIterableSum(iter, block))
  def iter_sumIf[T:Manifest, A:Manifest:Numeric](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[A]) = reflectPure(GIterableSumIf(iter, filter, block))
  def iter_product[T:Manifest, A:Manifest:Numeric](iter: Exp[GIterable[T]], block: Exp[T] => Exp[A]) = {
    if(dc_size(iter) == 0) {
      unit(0.asInstanceOf[A])
    } else {
      reflectPure(GIterableProduct(iter, block))
    }
  }
  def iter_productIf[T:Manifest, A:Manifest:Numeric](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[A]) = {
    if(dc_size(iter) == 0) {
      unit(0.asInstanceOf[A])
    } else {
      reflectPure(GIterableProductIf(iter, filter, block))
    }
  }
  def iter_max[T:Manifest, A:Manifest:Ordering](iter: Exp[GIterable[T]], block: Exp[T] => Exp[A]) = reflectPure(GIterableMax(iter, block))
  def iter_maxIf[T:Manifest, A:Manifest:Ordering](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[A]) = reflectPure(GIterableMaxIf(iter, filter, block))
  def iter_min[T:Manifest, A:Manifest:Ordering](iter: Exp[GIterable[T]], block: Exp[T] => Exp[A]) = reflectPure(GIterableMin(iter, block))
  def iter_minIf[T:Manifest, A:Manifest:Ordering](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[A]) = reflectPure(GIterableMinIf(iter, filter, block))
  def iter_count[T:Manifest](iter: Exp[GIterable[T]], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableCount(iter, block))
  def iter_all[T:Manifest](iter: Exp[GIterable[T]], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableAll(iter, block))
  def iter_allIf[T:Manifest](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableAllIf(iter, filter, block))
  def iter_any[T:Manifest](iter: Exp[GIterable[T]], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableAny(iter, block))
  def iter_anyIf[T:Manifest](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableAnyIf(iter, filter, block))
  def iter_tolist[T:Manifest](iter: Exp[GIterable[T]]) = reflectPure(GIterableToList(iter))
  def iter_toset[T:Manifest](iter: Exp[GIterable[T]]) = reflectPure(GIterableToSet(iter))
  def iter_next_bfs_level(iter: Exp[GIterable[Node]]) = reflectPure(GIterableNextBFSLevel(iter))
  def iter_filter[T:Manifest](iter: Exp[GIterable[T]], pred: Exp[T] => Exp[Boolean]) = reflectPure(GIterableFilter(iter, pred))
  def iter_foreach_default[T:Manifest](iter: Exp[GIterable[T]], block: Exp[T] => Exp[Unit]) = reflectEffect(GIterableForeach(iter, block))
  
  // sequential iteration
  def iter_for[T:Manifest](iter: Exp[GIterable[T]], filter: Option[Exp[T] => Exp[Boolean]], block: Exp[T] => Exp[Unit]) = {
    var i = var_new(unit(0)) 
    while(i < dc_size(iter)) {
      val elem = dc_apply(iter, i)
      filter match {
        case None => block(elem)
        case Some(pred) => if(pred(elem)) { block(elem) }
      }
      i += 1
    }    
  }
     
  // parallel iteration
  def iter_foreach[T:Manifest](in: Exp[GIterable[T]], func: Exp[T] => Exp[Unit]): Exp[Unit] = {
    // get list of effects of the foreach block (will contain the reduction assignments (if any))
    val v = fresh[Int] 
    val (blockSummary, blockEffects) = 
      reifyEffects((func(dc_apply(in, v)))) match {
        	case Def(Reify(_, summary, effects)) => (summary, effects)
        	case _ => return Const(())
      }
   
    // check if the loop contains a reduction expression
    if(!blockEffects.exists(sym => sym match {
    								// TODO: check using abstract ReductionOp type
      								case Def(Reflect(r@RedSum(_,_),_,_)) => true
      								case Def(Reflect(r@RedProd(_,_),_,_)) => true
      								case Def(Reflect(r@RedCount(_,_),_,_)) => true
      								case Def(Reflect(r@RedMin(_,_),_,_)) => true
      								case Def(Reflect(r@RedMax(_,_),_,_)) => true
      								case Def(Reflect(r@RedAll(_,_),_,_)) => true
      								case Def(Reflect(r@RedAny(_,_),_,_)) => true
      								case _ => false })) 
    {
    	iter_foreach_default(in, func)
    } else {
    
    	// collect all non-reduction related effects into ForeachElems
    	// replace reduction effects with a ReduceElem
    	// construct a multiloop (fat loop) from the resulting elements 

    	var mapRedVarToOut = new scala.collection.immutable.HashMap[Exp[Reduceable[Any]], (Exp[Any], String)]()
    	var fatLoopBody = scala.List[Def[Any]]()
    	var fatLoopSyms = scala.List[Sym[Any]]()
    	var j = 0
    	while (j < blockEffects.size) {
    		var sym = blockEffects(j)
    		sym match {
    		   //TODO:  case e: ReductionOp[a]? => 
    		   case Def(Reflect(r@RedSum(_,_),_,_)) => processReduceEffect(r.rv.asInstanceOf[Exp[Reduceable[Any]]], sym, r.rhs, r.zero, r.reduce, r.reduceStr)(r.m)
    		   case Def(Reflect(r@RedProd(_,_),_,_)) => processReduceEffect(r.rv.asInstanceOf[Exp[Reduceable[Any]]], sym, r.rhs, r.zero, r.reduce, r.reduceStr)(r.m)
    		   case Def(Reflect(r@RedCount(_,_),_,_)) => processReduceEffect(r.rv.asInstanceOf[Exp[Reduceable[Any]]], sym, r.rhs, r.zero, r.reduce, r.reduceStr)(r.m)
    		   case Def(Reflect(r@RedMin(_,_),_,_)) => processReduceEffect(r.rv.asInstanceOf[Exp[Reduceable[Any]]], sym, r.rhs, r.zero, r.reduce, r.reduceStr)(r.m)
    		   case Def(Reflect(r@RedMax(_,_),_,_)) => processReduceEffect(r.rv.asInstanceOf[Exp[Reduceable[Any]]], sym, r.rhs, r.zero, r.reduce, r.reduceStr)(r.m)
    		   case Def(Reflect(r@RedAll(_,_),_,_)) => processReduceEffect(r.rv.asInstanceOf[Exp[Reduceable[Any]]], sym, r.rhs, r.zero, r.reduce, r.reduceStr)(r.m)
    		   case Def(Reflect(r@RedAny(_,_),_,_)) => processReduceEffect(r.rv.asInstanceOf[Exp[Reduceable[Any]]], sym, r.rhs, r.zero, r.reduce, r.reduceStr)(r.m)
    		   case _ => processOtherEffect(sym)
    		}    
    		j += 1
    	}
    
    	def processReduceEffect[A:Manifest](rv: Exp[Reduceable[Any]], sym: Exp[Any], func: Exp[A], zero: Exp[A], 
    			reduce: (Exp[A], Exp[A]) => Exp[A], reduceStr: String): Unit =
    	{
    		val rV = (fresh[A], fresh[A])
    		val rFunc = reifyEffects(reduce(rV._1, rV._2))
    		val elemDef = DeliteReduceElem[A](reifyEffects(func), Nil, reifyEffects(zero), rV, rFunc, true)
      
    		val temp = toAtom(elemDef).asInstanceOf[Sym[A]]
    		fatLoopSyms :+= temp//toAtom(elemDef).asInstanceOf[Sym[A]]
    		fatLoopBody :+= elemDef
    		mapRedVarToOut += Pair(rv, Pair(temp, reduceStr))
    	}
    
    	def processOtherEffect(sym: Exp[Any]): Unit = { //Def[Unit] = {
    		//val feSync = reifyEffects(List())
    		def noSync: Exp[Int] => Exp[List[Any]] = n => List()
    		val i = fresh[Int]
    		val elemDef = DeliteForeachElem[Unit](
    				func = reifyEffects(toAtom(Reify(Const(()), blockSummary, scala.List(sym)))),
    				sync = reifyEffects(noSync(i))
    		)
    		fatLoopSyms :+= toAtom(elemDef).asInstanceOf[Sym[Any]]
    		fatLoopBody :+= elemDef     
        }
    
    	def noSync: Exp[Int] => Exp[List[Any]] = n => List()
    	val i = fresh[Int]
    	val elemDef = DeliteForeachElem[Unit](
    			func = reifyEffects(toAtom(Reify(Const(()), blockSummary, scala.List()))),
    			sync = reifyEffects(noSync(i))
    	)
    	//fatLoopSyms :+= toAtom(elemDef).asInstanceOf[Sym[Any]]
    	fatLoopBody :+= elemDef 
    
    	// fat loop fields
    	val size = dc_size(in)
    	val body = fatLoopBody
    	val c = reflectEffect(ConstructFatLoop(size, v, body, fatLoopSyms))
    
    	mapRedVarToOut.keys.foreach(key => 
    	{
    		//reflectEffect[Unit](RedSetOutput(key, mapRedVarToOut(key), c), Global())
    		val z = fresh[Unit]
    		var deps: List[Exp[Any]] = scala.List()
    		deps :+= c
    		deps :+= key
    	    //val zd = Reflect(RedSetOutput(key, mapRedVarToOut(key)._1,  mapRedVarToOut(key)._2, c), Global(), deps)
    	    val zd = Reflect(RedSetOutput(key, mapRedVarToOut(key)._1,  mapRedVarToOut(key)._2, c), Global(), deps)
    	    internalReflect(z, zd)
    	} )

    	// TODO: fix dependencies when other effects follow reductions in the block
    }
  }
  
  /*case class TransformedForeach[T:Manifest](in: Exp[GIterable[T]], func: Exp[T]=>Exp[Unit], symId: Int, uid: Sym[Any]) 
  extends DeliteOpLoop[Unit] {
    val size = dc_size(in)
    val i = dc_apply(in, v)
	val (blockSummary, blockEffects: List[Exp[Any]]) = reifyEffects(func(i)) match {
      case Def(Reify(_, summary, effects)) => (summary, effects)
    }
    val sym = blockEffects(symId)
    val newSym = sym match {
      case Def(Reflect(e, summary, deps)) => {
        toAtom(Reflect(e, summary, deps.filterNot(d=>blockEffects.take(symId).contains(d)))) 
      }
    }
    val body = DeliteForeachElem(
      func = toAtom(Reify(Const(()), blockSummary, scala.List(newSym))),
      sync = reifyEffects(List())
    )
  }*/
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ConstructFatLoop(size, v, body, symList) => syms(size) ::: body.flatMap(syms) 
    case _ => super.syms(e)
  }
  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
   
    case ConstructFatLoop(size, v, body, symList) => freqNormal(size) ::: body.flatMap(freqHot)
    case _ => super.symsFreq(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case ConstructFatLoop(size, v, body, symList) => readSyms(size) ::: body.flatMap(readSyms)
    case _ => super.readSyms(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ConstructFatLoop(size, v, body, symList) => v :: body.flatMap(boundSyms)
    case _ => super.boundSyms(e)
  }
}

trait BaseGenGIterableOps extends GenericFatCodegen {
  val IR: GIterableOpsExp
  import IR._

}

trait ScalaGenGIterableOps extends BaseGenGIterableOps with ScalaGenFat {
  val IR: GIterableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case it@GIterableNewEmpty() => emitValDef(sym, "new " + remap(it.mGIt) +"()")
      case GIterableToList(iter) => emitValDef(sym, quote(iter) + ".toList")
      case GIterableToSet(iter) => emitValDef(sym, quote(iter) + ".toSet")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

//experimental fusing
trait FuseTransformedForeach extends GenericFatCodegen with SimplifyTransform {
  val IR: GIterableOpsExp with LoopsFatExp with IfThenElseFatExp with ReduceableOpsExp
  import IR._ 
  
  override def buildScheduleForResult(start: Exp[Any]): List[TP[Any]] = {
    def deps(st: List[Sym[Any]]): List[TP[Any]] =
      availableDefs.filter(st contains _.sym)
      //syms(e).flatMap(d => findDefinition(d).toList)

    val st = start match {
      case Def(Reflect(RedSetOutput(r, out, red, dep),_,_)) => {
        deps(syms(r))
      }
      case _ => deps(syms(start))
    }
    
    GraphUtil.stronglyConnectedComponents[TP[Any]](st, t => {
      t.rhs match {
        case Reflect(RedSetOutput(r, out, red, dep),_,_) => {
        	deps(syms(r)) }
        case _ => deps(syms(t.rhs)) 
     }
    }
    ).flatten.reverse
    
  } 
  
  override def getFatSchedule(scope: List[TTP])(result: Any): List[TTP] = {
    def deps(st: List[Sym[Any]]): List[TTP] = 
      scope.filter(d => (st intersect d.lhs).nonEmpty)

    val start = result match {
      case RedSetOutput(r, out, red, dep) => deps(syms(r))
      case _ => deps(syms(result))
    }
    val xx = GraphUtil.stronglyConnectedComponents[TTP](start, t => deps(syms(t.rhs)))
    //val xx = GraphUtil.stronglyConnectedComponents[TTP](deps(syms(result)), t => deps(syms(t.rhs)))
    
    
    xx.foreach { x => 
      if (x.length > 1)
        printerr("warning: recursive schedule for result " + result + ": " + x)
    }
    xx.flatten.reverse
  }
  
  override def fatten(e: TP[Any]): TTP = e.rhs match {
    case Reflect(ConstructFatLoop(size, v, body, symList), _, _) => TTP(symList:::List(e.sym), SimpleFatLoop(size, v, body))
    case _ => super.fatten(e)
  }
  
  // old
  
  /*override def fatten(e: TP[Any]): TTP = e.rhs match {
    case Reflect(TransformedForeach(in, func, symId, uid), _, _) => TTP(List(e.sym), ThinDef(e.rhs))
    case _ => super.fatten(e)
  }*/
  
  /*override def focusExactScopeFat[A](currentScope0: List[TTP])(result0: List[Exp[Any]])(body: List[TTP] => A): A = {
    var result: List[Exp[Any]] = result0
    var currentScope = currentScope0
    
    var Wloops = currentScope collect { case e@TTP(syms, ThinDef(Reflect(TransformedForeach(_,_,_,_),_,_))) => e}
    val groupedTTPs = Wloops.groupBy(ttp=>{ttp.rhs match {case ThinDef(Reflect(TransformedForeach(_,_,_,uid),_,_)) => uid}}).values
    
    val ttpLoops = groupedTTPs.map(ttpGroup => {
      val tfList = ttpGroup.map(e => {e.rhs match { case ThinDef(Reflect(t@TransformedForeach(_,_,_,_),_,_)) => t.body.asInstanceOf[Def[Any] ]}})
      val size = ttpGroup(0).rhs match { case ThinDef(Reflect(t@TransformedForeach(_,_,_,_),_,_)) => t.size}
      val symList = ttpGroup.flatMap(e => {e.lhs})
      //val size = tfList(0) match { case Reflect(t@TransformedForeach(_,_,_,_),_,_) => t.size }
      val v = fresh[Int]
      TTP(symList, SimpleFatLoop(size, v, tfList))
    })
    currentScope = currentScope.filterNot(e=>Wloops.contains(e))
    currentScope :::= ttpLoops.toList
  
    //transformAllFully(currentScope, result, new SubstTransformer) match { case (a,b) => 
      //currentScope = a
      //result = b
    //}
    
    super.focusExactScopeFat(currentScope)(result0)(body)
    //(currentScope, result)
  }*/
}