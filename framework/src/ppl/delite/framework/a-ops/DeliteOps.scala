package ppl.delite.framework.ops

import java.io.{FileWriter, File, PrintWriter}

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericCodegen, GenericFatCodegen, GenerationFailedException}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.Config

//trait DeliteOpsExp extends BaseFatExp with EffectExp with VariablesExp with LoopsFatExp {
trait DeliteOpsExp extends BaseFatExp with EffectExp with VariablesExp with LoopsFatExp 
    with VariantsOpsExp with DeliteCollectionOpsExp
    with OrderingOpsExp with CastingOpsExp with ImplicitOpsExp with WhileExp  {
  
  /**
   * The base type of the DeliteOp hierarchy.
   */
  /*sealed*/ trait DeliteOp[A] extends Def[A] {
		val transform: Transformer = IdentityTransformer // default
	}
//  sealed trait DeliteFatOp extends FatDef

  /**
   * A sequential task - will execute block in a single thread and respect any free variable dependencies inside it.
   *
   * @param  block   the task to execute; must be reified if it contains effectful operations!
   */
  class DeliteOpSingleTask[A](val block: Exp[A], val requireInputs: Boolean = false) extends DeliteOp[A]

  abstract class DeliteOpLoop[A] extends AbstractLoop[A] with DeliteOp[A]

//  case class DeliteOpFatLoop(val size: Exp[Int], val v: Sym[Int], val body: List[Def[Any]]) extends AbstractFatLoop with DeliteFatOp
  
  
  // for use in loops:

	case class DeliteForeachElem[A](
		func: Exp[A],
		sync: Exp[List[Any]],
		cond: List[Exp[Boolean]] = Nil
	) extends Def[Unit]
	
  case class DeliteCollectElem[A, CA <: DeliteCollection[A]]( 
    alloc: Exp[CA],
    func: Exp[A],
    cond: List[Exp[Boolean]] = Nil
    // TODO: note that the alloc block right now directly references the size
    // which is not part of DeliteCollectElem instance. we might want to fix that 
  ) extends Def[CA]
  
  case class DeliteReduceElem[A](
    func: Exp[A],
    cond: List[Exp[Boolean]] = Nil,
    zero: Exp[A],
    rV: (Sym[A], Sym[A]),
    rFunc: Exp[A]
  ) extends Def[A]
  
  
  def loopBodyNeedsCombine[A](e: Def[A]) = e match {
    case e:DeliteReduceElem[_] => true
    case e:DeliteCollectElem[_,_] => e.cond.nonEmpty
    case _ => false
  }
  
  
  /**
   * A Conditional task - will emit a Conditional DEG node as well as kernels for the then and else clauses
   *
   * @param  cond    the condition of the Conditional
   * @param  thenp   the Then block to execute if condition is true
   * @param  elsep   the Else block to execute if condition is false
   */
  trait DeliteOpCondition[A] extends DeliteOp[A] {
    val cond: Exp[Boolean]
    val thenp: Exp[A]
    val elsep: Exp[A]
  }

  /**
   * An indexed loop - will emit an indexed loop DEG node as well as a kernel for the body
   *
   * @param  start  starting index
   * @param  end    ending index (not included in loop)
   * @param  idx    index id that will be refered to in the body, this could be passed in as input to the body or the body could be inlined
   * @param  body   the body of the loop
   */
  /*
  trait DeliteOpIndexedLoop extends DeliteOp[Unit] {
    val start: Exp[Int]
    val end: Exp[Int]
    val index: Sym[Int]
    val body: Exp[Unit]
  }
  */

  /**
   * An while loop - will emit an while loop DEG node as well as a kernel for the body
   *
   * @param  cond  condition expression, will be emitted as a kernel
   * @param  body   the body of the loop
   */
  trait DeliteOpWhileLoop extends DeliteOp[Unit] {
    val cond: Exp[Boolean]
    val body: Exp[Unit]
  }

	/**
	 *	New Delite parallel ops; will eventually completely replace old versions.
	 */

	abstract class DeliteOpMap2[A:Manifest,
	 														B:Manifest, CB <: DeliteCollection[B]:Manifest]
	 	extends DeliteOpLoop[CB] {

		// supplied by subclass
	  val in: Exp[DeliteCollection[A]]
		val size: Exp[Int] // could be dc_size(in), but we want type-specific pattern matching to work
		def func: Exp[A] => Exp[B]
	  def alloc: Exp[CB]

	  // loop
	  final val v = fresh[Int]
	  lazy val body: Def[CB] = DeliteCollectElem[B, CB](
			alloc = transform(reifyEffects(this.alloc)),
			func = transform(reifyEffects(this.func(dc_apply(in,v))))
		)
	}

	/**
	 *  Currently conditionally appends values to buffers, which are concatenated in the combine stage.
	 *  Note that it is also implicitly a Map-Filter (it accepts a mapping function). Should this be renamed? 
	 *	
	 *	Should eventually be implemented as a parallel scan.
	 */
	abstract class DeliteOpFilter2[A:Manifest,
	 															 B:Manifest, CB <: DeliteCollection[B]:Manifest]
		extends DeliteOpMap2[A,B,CB] {

		// supplied by subclass
		def cond: Exp[A] => Exp[Boolean] // does this need to be more general (i.e. a List?)

	  // loop
	  override lazy val body: Def[CB] = DeliteCollectElem[B, CB](
			alloc = transform(reifyEffects(this.alloc)),
			func = transform(reifyEffects(this.func(dc_apply(in,v)))),
			cond = transform(reifyEffects(this.cond(dc_apply(in,v))))::Nil			
		)
	}
      	
	abstract class DeliteOpZipWith2[A:Manifest,
																  B:Manifest,
																	R:Manifest, CR <: DeliteCollection[R]:Manifest]
		extends DeliteOpLoop[CR] {
			
		// supplied by subclass		
    val inA: Exp[DeliteCollection[A]]
    val inB: Exp[DeliteCollection[B]]
		val size: Exp[Int]
    def func: (Exp[A], Exp[B]) => Exp[R]
    def alloc: Exp[CR]
		
	  // loop
	  final val v = fresh[Int]
	  lazy val body: Def[CR] = DeliteCollectElem[R, CR](
			alloc = transform(reifyEffects(this.alloc)),
			func = transform(reifyEffects(this.func(dc_apply(inA,v), dc_apply(inB,v))))
		)		
  }
 
  /**
   * @param zero  only used if the input DeliteCollection is empty	
   */
  abstract class DeliteOpReduce2[A:Manifest] extends DeliteOpLoop[A] {
			
		// supplied by subclass		
    val in: Exp[DeliteCollection[A]]
		val size: Exp[Int]
		val zero: Exp[A] 
    def func: (Exp[A], Exp[A]) => Exp[A]
		
	  // loop
	  final val v = fresh[Int]
		final protected val rV = (fresh[A], fresh[A])
	  lazy val body: Def[A] = DeliteReduceElem[A](
			func = transform(reifyEffects(dc_apply(in,v))),
			zero = transform(this.zero),			
			rV = (transform(rV._1).asInstanceOf[Sym[A]], transform(rV._2).asInstanceOf[Sym[A]]),
			rFunc = transform(reifyEffects(this.func(rV._1, rV._2)))
		)		
  }
  
	/**
   * @param zero  only used if the input DeliteCollection is empty	
   */
	abstract class DeliteOpMapReduce2[A:Manifest,R:Manifest]
		extends DeliteOpLoop[R] {
			
		// supplied by subclass		
    val in: Exp[DeliteCollection[A]]
		val size: Exp[Int]
		val zero: Exp[R] 
		def map: Exp[A] => Exp[R]
    def reduce: (Exp[R], Exp[R]) => Exp[R]
		
	  // loop
	  final val v = fresh[Int]
		final protected val rV = (fresh[R], fresh[R])
	  lazy val body: Def[R] = DeliteReduceElem[R](
			func = transform(reifyEffects(map(dc_apply(in,v)))),
			zero = transform(this.zero),			
			rV = (transform(rV._1).asInstanceOf[Sym[R]], transform(rV._2).asInstanceOf[Sym[R]]),
			rFunc = transform(reifyEffects(reduce(rV._1, rV._2)))
		)			
	}
	
	// should this be folded into DeliteOpMapReduce (or into DeliteOpFilter)?
	abstract class DeliteOpFilterReduce2[A:Manifest,R:Manifest]
		extends DeliteOpMapReduce2[A,R] {
		
		def cond: Exp[A] => Exp[Boolean] // does this need to be more general (i.e. a List?)
		def func: Exp[A] => Exp[R]		
		def map = func
		
		override lazy val body: Def[R] = DeliteReduceElem[R](
			func = transform(reifyEffects(map(dc_apply(in,v)))),
			cond = transform(reifyEffects(cond(dc_apply(in,v))))::Nil,
			zero = transform(this.zero),			
			rV = (transform(rV._1).asInstanceOf[Sym[R]], transform(rV._2).asInstanceOf[Sym[R]]),
			rFunc = transform(reifyEffects(reduce(rV._1, rV._2)))
		)	
	}
	
	/**
   * @param zero  only used if the input DeliteCollection is empty	
   */
	abstract class DeliteOpZipWithReduce2[A:Manifest,B:Manifest,R:Manifest]
		extends DeliteOpLoop[R] {
			
		// supplied by subclass		
    val inA: Exp[DeliteCollection[A]]
    val inB: Exp[DeliteCollection[B]]
		val size: Exp[Int]
		val zero: Exp[R]
    def zip: (Exp[A], Exp[B]) => Exp[R]
		def reduce: (Exp[R], Exp[R]) => Exp[R]
		
		// loop
	  final val v = fresh[Int]
		final protected val rV = (fresh[R], fresh[R])
	  lazy val body: Def[R] = DeliteReduceElem[R](
			func = transform(reifyEffects(zip(dc_apply(inA,v), dc_apply(inB,v)))),
			zero = transform(this.zero),			
			rV = (transform(rV._1).asInstanceOf[Sym[R]], transform(rV._2).asInstanceOf[Sym[R]]),
			rFunc = transform(reifyEffects(reduce(rV._1, rV._2)))
		)				  
  }

	
	abstract class DeliteOpForeach2[A:Manifest] extends DeliteOpLoop[Unit] { //DeliteOp[Unit] {		
		val in: Exp[DeliteCollection[A]]
		def func: Exp[A] => Exp[Unit]
		def sync: Exp[Int] => Exp[List[Any]] // TODO: need to extend runtime to do something with sync in multiloop
		
		final val v = fresh[Int]
		final val i = fresh[Int]
		lazy val body: Def[Unit] = DeliteForeachElem(
			func = transform(reifyEffects(this.func(dc_apply(in,v)))),
		 	sync = transform(reifyEffects(this.sync(i)))
		)
	}
	
	abstract class DeliteOpIndexedLoop extends DeliteOpLoop[Unit] {
		def func: Exp[Int] => Exp[Unit]
		
		final val v = fresh[Int]
		lazy val body: Def[Unit] = DeliteForeachElem(
			func = transform(reifyEffects(this.func(v))),
			sync = transform(unit(List())) 
		)	
	}
	
	/**
	 * Deprecated Delite ops	
	 */
	
	/**
   * Parallel map from DeliteCollection[A] => DeliteCollection[B]. Input functions can depend on free
   * variables, but they cannot depend on other elements of the input or output collection (disjoint access).
   *
   * @param  in    the input collection
   * @param  v     the bound symbol that the mapping function operates over
   * @param  func  the mapping function; reified version of Exp[A] => Exp[B]
   * @param  alloc function returning the output collection. if it is the same as the input collection,
   *               the operation is mutable; reified version of Unit => DeliteCollection[B].
   */
	
  trait DeliteOpMap[A,B,C[X] <: DeliteCollection[X]] extends DeliteOp[C[B]] with DeliteOpMapLikeWhileLoopVariant {
    val in: Exp[C[A]]
    val v: Sym[A]
    val func: Exp[B]
    val alloc: Exp[C[B]]
    
    lazy val variant = {
      implicit val mA: Manifest[A] = v.Type.asInstanceOf[Manifest[A]]
      implicit val mB: Manifest[B] = func.Type.asInstanceOf[Manifest[B]]
      implicit val mCA: Manifest[C[A]] = in.Type.asInstanceOf[Manifest[C[A]]]
      implicit val mCB: Manifest[C[B]] = alloc.Type.asInstanceOf[Manifest[C[B]]]
      reifyEffects {
        var i = var_new(unit(0))
        var vs = var_new(unit(null).asInstanceOfL[A])
        while (i < in.size) {
          vs = in(i)
          rebind(v.asInstanceOf[Sym[A]], ReadVar(vs))
          // why is the alloc dependency not lifted out of the loop when the variant block is emitted?
          alloc(i) = func
          i += 1
        }
        alloc
      }
    }
  }

  /**
   * Parallel 2 element zipWith from (DeliteCollection[A],DeliteCollection[B]) => DeliteCollection[R].
   * Input functions can depend on free variables, but they cannot depend on other elements of the input or
   * output collection (disjoint access).
   *
   * @param  inA   the first input collection
   * @param  inB   the second input collection
   * @param  v     the bound symbol that the zipWith function operates over
   * @param  func  the zipWith function; reified version of ([Exp[A],Exp[B]) => Exp[R]
   * @param  alloc function returning the output collection. if it is the same as the input collection,
   *               the operation is mutable; reified version of Unit => DeliteCollection[B].
   */
  abstract class DeliteOpZipWith[A,B,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[C[R]] {
    val inA: Exp[C[A]]
    val inB: Exp[C[B]]
    val v: (Sym[A],Sym[B])
    val func: Exp[R]
    val alloc: Exp[C[R]]
  }

  /**
   * Parallel reduction of a DeliteCollection[A]. Reducing function must be associative.
   *
   * @param  in    the input collection
   * @param  v     the bound symbol that the reducing function operates over
   * @param  func  the reduction function; reified version of ([Exp[A],Exp[A]) => Exp[A]. Must be associative.
   */
  abstract class DeliteOpReduce[A]() extends DeliteOp[A] {
    val in: Exp[DeliteCollection[A]]
    val v: (Sym[A],Sym[A])
    val func: Exp[A]
  }

  /**
   * Parallel map-reduction from a DeliteCollection[A] => R. The map-reduce is composed, so no temporary collection
   * is instantiated to hold the result of the map.
   *
   * @param  in      the input collection
   * @param  mV      the bound symbol that the mapping function operates over
   * @param  map     the mapping function; reified version of Exp[A] => Exp[R]
   * @param  rV      the bound symbol that the reducing function operates over
   * @param  reduce  the reduction function; reified version of ([Exp[R],Exp[R]) => Exp[R]. Must be associative.
   */
  abstract class DeliteOpMapReduce[A,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[R] with DeliteOpReduceLikeWhileLoopVariant {
    val in: Exp[C[A]]
    //val acc: Exp[R]

    // for accumulating each partial sum
    val mV: Sym[A]
    //val mapreduce: Exp[R] // reified of Exp[(R,A)] => Exp[R] composition of map and reduce
    val map: Exp[R]

    // for reducing remaining partial sums
    val rV: (Sym[R],Sym[R])
    val reduce: Exp[R]

    lazy val acc = {
      implicit val mR = map.Type.asInstanceOf[Manifest[R]]
      var_new(unit(null).asInstanceOfL[R])
    }
    lazy val index = {
      var_new(unit(0))
    }
    // this is a workaround for reification and vars not really working well together
    // we need to output the block as an Exp, but the nested scopes need to use it as a var
    lazy val Acc = {
      implicit val mR = map.Type.asInstanceOf[Manifest[R]]
      reifyEffects(acc)
    }
    lazy val Index = reifyEffects(index)
    // end workaround
    lazy val init = {
      implicit val mA = mV.Type.asInstanceOf[Manifest[A]]
      implicit val mR = map.Type.asInstanceOf[Manifest[R]]
      reifyEffects {
        var vs = var_new(in(index))
        rebind(mV.asInstanceOf[Sym[A]], ReadVar(vs))
        acc = map
      }
    }
    lazy val variant = {
      implicit val mA = mV.Type.asInstanceOf[Manifest[A]]
      implicit val mR = map.Type.asInstanceOf[Manifest[R]]
      reifyEffects {
        index += 1
        while (index < in.size) {
          var vs = var_new(in(index))
          //rebind(mV.asInstanceOf[Sym[A]], ReadVar(vs))
          var x = var_new(map)
          rebind(rV._1.asInstanceOf[Sym[R]], ReadVar(acc))
          rebind(rV._2.asInstanceOf[Sym[R]], ReadVar(x))
          acc = reduce
          index += 1
        }
        acc
      }
    }
  }
		
  /**
   * Parallel zipWith-reduction from a (DeliteCollection[A],DeliteCollection[A]) => R. The map-reduce is composed,
   * so no temporary collection is instantiated to hold the result of the map.
   *
   * @param  inA     the first input collection
   * @param  inB     the second input collection
   * @param  zV      the bound symbol that the zipWith function operates over
   * @param  zip     the zipWith function; reified version of (Exp[A],Exp[B]) => Exp[R]
   * @param  rV      the bound symbol that the reducing function operates over
   * @param  reduce  the reduction function; reified version of ([Exp[R],Exp[R]) => Exp[R]. Must be associative.
   */
  abstract class DeliteOpZipWithReduce[A,B,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[R] {
    val inA: Exp[C[A]]
    val inB: Exp[C[B]]
    // for accumulating each partial sum
    val zV: (Sym[A],Sym[B])
    val zip: Exp[R]
    // for reducing remaining partial sums
    val rV: (Sym[R],Sym[R])
    val reduce: Exp[R]
  }


  /**
   * Parallel foreach from DeliteCollection[A] => Unit. Input functions must specify any free variables that it
   * requires are protected (e.g. locked before chunk execution) using the sync list.
   *
   * @param  in     the input collection
   * @param  v      the bound symbol that the foreach function operates over
   * @param  func   the foreach function; reified version of Exp[A] => Exp[Unit]
   * @param  i      the bound symbol that the sync function operates over
   * @param  sync   a function from an index to a list of objects that should be locked, in a total ordering,
   *                prior to chunk execution, and unlocked after; reified version of Exp[Int] => Exp[List[_]]
   */
  abstract class DeliteOpForeach[A,C[X] <: DeliteCollection[X]]() extends DeliteOp[Unit] with DeliteOpMapLikeWhileLoopVariant {
    val in: Exp[C[A]]
    val v: Sym[A]
    val func: Exp[Unit]
    val i: Sym[Int]
    val sync: Exp[List[Any]]

    lazy val alloc = Const()
    lazy val variant = {
      implicit val mA: Manifest[A] = v.Type.asInstanceOf[Manifest[A]]
      reifyEffects {
        var index = var_new(unit(0))
        var vs = var_new(unit(null).asInstanceOfL[A])
        while (index < in.size) {
          vs = in(index)
          rebind(v.asInstanceOf[Sym[A]], ReadVar(vs))
          //reflectEffect(findDefinition(func.asInstanceOf[Sym[Unit]]).get.rhs)
          var x = var_new(func)
          index += 1
        }
        alloc
      }
    }
  }

  // TODO: should we make all DeliteOps be boundable? This is probably not the right way to do this anyways.
  abstract class DeliteOpForeachBounded[B,A <: B,C[X <: B] <: DeliteCollection[X]] extends DeliteOp[Unit] {
    val in: Exp[C[A]]
    val v: Sym[A]
    val func: Exp[Unit]
    val i: Sym[Int]
    val sync: Exp[List[Any]]
  }

  // used by delite code generators to handle nested delite ops
  var deliteKernel: Boolean = false //_
  var deliteResult: Option[List[Exp[Any]]] = None//_
  var deliteInputs: List[Sym[Any]] = Nil//_

  // TODO: move to lms?
  def rebind(sym: Sym[Any], rhs: Def[Any]) = createDefinition(sym, rhs).rhs

  //////////////
  // mirroring

  override def mirrorFatDef[A:Manifest](d: Def[A], f: Transformer): Def[A] = mirrorLoopBody(d,f) // TODO: cleanup

  def mirrorLoopBody[A](d: Def[A], f: Transformer): Def[A] = {
    d match {
      case e: DeliteCollectElem[a,ca] => 
        DeliteCollectElem[a,ca]( // need to be a case class for equality (do we rely on equality?)
          alloc = f(e.alloc),
          func = f(e.func),
          cond = f(e.cond)
        ).asInstanceOf[Def[A]]
		  case e: DeliteForeachElem[a] => 
        DeliteForeachElem[a](
 					func = f(e.func),
					sync = f(e.sync),
					cond = f(e.cond)
				).asInstanceOf[Def[A]] // reasonable?
      case e: DeliteReduceElem[a] => 
        DeliteReduceElem[a](
          func = f(e.func),
          cond = f(e.cond),
          zero = f(e.zero),
          rV = (f(e.rV._1).asInstanceOf[Sym[a]], f(e.rV._2).asInstanceOf[Sym[a]]), // need to transform bound vars ??
          rFunc = f(e.rFunc)
        ).asInstanceOf[Def[A]]
    }
  }
  

  //////////////
  // dependencies

  override def syms(e: Any): List[Sym[Any]] = e match { //TR TODO: question -- is alloc a dependency (should be part of result) or a definition (should not)???
                                                        // aks: answer -- we changed it to be internal to the op to make things easier for CUDA. not sure if that still needs
                                                        // to be the case. similar question arises for sync
    case s: DeliteOpSingleTask[_] if s.requireInputs => syms(s.block) ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case s: DeliteOpSingleTask[_] => syms(s.block)
    case op: DeliteCollectElem[_,_] => syms(op.func) ++ syms(op.cond) ++ syms(op.alloc)
    case op: DeliteForeachElem[_] => syms(op.func) ++ syms(op.cond) ++ syms(op.sync)
    case op: DeliteReduceElem[_] => syms(op.func) ++ syms(op.cond) ++ syms(op.zero) ++ syms(op.rFunc)
    case map: DeliteOpMap[_,_,_] => /*if (shallow) syms(map.in) else */ syms(map.in) ++ syms(map.alloc) ++ syms(map.func)
    case zip: DeliteOpZipWith[_,_,_,_] => /*if (shallow) syms(zip.inA) ++ syms(zip.inB) else*/ syms(zip.inA) ++ syms(zip.inB) ++ syms(zip.alloc) ++ syms(zip.func)
    case red: DeliteOpReduce[_] => /*if (shallow) syms(red.in) else*/ syms(red.in) ++ syms(red.func)
    case mapR: DeliteOpMapReduce[_,_,_] => /*if (shallow) syms(mapR.in) else*/ syms(mapR.in) ++ syms(mapR.map) ++ syms(mapR.reduce)
    case zipR: DeliteOpZipWithReduce[_,_,_,_] => /*if (shallow) syms(zipR.inA) ++ syms(zipR.inB) else*/ syms(zipR.inA) ++ syms(zipR.inB) ++ syms(zipR.zip) ++ syms(zipR.reduce)
    case foreach: DeliteOpForeach[_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ++ syms(foreach.func) ++ syms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ++ syms(foreach.func) ++ syms(foreach.sync)
    case _ => super.syms(e)
  }
	  
  override def readSyms(e: Any): List[Sym[Any]] = e match { 
    case map: DeliteOpMap[_,_,_] => readSyms(map.in) 
    case zip: DeliteOpZipWith[_,_,_,_] => readSyms(zip.inA) ++ readSyms(zip.inB) 
    case red: DeliteOpReduce[_] => readSyms(red.in)
    case mapR: DeliteOpMapReduce[_,_,_] => readSyms(mapR.in) 
    case zipR: DeliteOpZipWithReduce[_,_,_,_] => readSyms(zipR.inA) ++ readSyms(zipR.inB) 
    case foreach: DeliteOpForeach[_,_] => readSyms(foreach.in) 
    case foreach: DeliteOpForeachBounded[_,_,_] => readSyms(foreach.in) 
    case _ => super.readSyms(e)
  }  

  override def boundSyms(e: Any): List[Sym[Any]] = e match { //TR TODO
    case s: DeliteOpSingleTask[_] => effectSyms(s.block)
    case op: DeliteCollectElem[_,_] => effectSyms(op.func) ++ effectSyms(op.cond) ++ effectSyms(op.alloc)
    case op: DeliteReduceElem[_] => List(op.rV._1, op.rV._2) ++ effectSyms(op.func) ++ effectSyms(op.cond) ++ effectSyms(op.rFunc)
    case op: DeliteForeachElem[_] => effectSyms(op.func) ++ effectSyms(op.cond) ++ effectSyms(op.sync)
    case zip: DeliteOpZipWith[_,_,_,_] => zip.v._1::zip.v._2::effectSyms(zip.alloc):::effectSyms(zip.func)
    case map: DeliteOpMap[_,_,_] => map.v::effectSyms(map.alloc):::effectSyms(map.func)
    case mapR: DeliteOpMapReduce[_,_,_] => mapR.mV::mapR.rV._1::mapR.rV._2::effectSyms(mapR.map):::effectSyms(mapR.reduce)
    case zipR: DeliteOpZipWithReduce[_,_,_,_] => zipR.zV._1::zipR.zV._2::zipR.rV._1::zipR.rV._2::effectSyms(zipR.zip) ++ effectSyms(zipR.reduce)
    case red: DeliteOpReduce[_] => red.v._1::red.v._2::effectSyms(red.func)
    case foreach: DeliteOpForeach[_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case _ => super.boundSyms(e)
  }

  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: DeliteOpSingleTask[_] if s.requireInputs => freqNormal(s.block) ++ super.symsFreq(e) // super call: add case class syms (iff flag is set)
    case s: DeliteOpSingleTask[_] => freqNormal(s.block)
    case op: DeliteCollectElem[_,_] => freqNormal(op.alloc) ++ freqHot(op.cond) ++ freqHot(op.func)
    case op: DeliteForeachElem[_] => freqNormal(op.sync) ++ freqHot(op.cond) ++ freqHot(op.func)
    case op: DeliteReduceElem[_] => freqHot(op.cond) ++ freqHot(op.func) ++ freqNormal(op.zero) ++ freqHot(op.rFunc)
    case map: DeliteOpMap[_,_,_] => freqNormal(map.in) ++ freqNormal(map.alloc) ++ freqHot(map.func)
    case zip: DeliteOpZipWith[_,_,_,_] => freqNormal(zip.inA) ++ freqNormal(zip.inB) ++ freqNormal(zip.alloc) ++ freqHot(zip.func)
    case red: DeliteOpReduce[_] => freqNormal(red.in) ++ freqHot(red.func)
    case mapR: DeliteOpMapReduce[_,_,_] => freqNormal(mapR.in) ++ freqHot(mapR.map) ++ freqHot(mapR.reduce)
    case zipR: DeliteOpZipWithReduce[_,_,_,_] => freqNormal(zipR.inA) ++ freqNormal(zipR.inB) ++ freqHot(zipR.zip) ++ freqHot(zipR.reduce)
    case foreach: DeliteOpForeach[_,_] => freqNormal(foreach.in) ++ freqHot(foreach.func) ++ freqHot(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => freqNormal(foreach.in) ++ freqHot(foreach.func) ++ freqHot(foreach.sync)
    case _ => super.symsFreq(e)
  }

}

trait BaseGenDeliteOps extends BaseGenLoopsFat with LoopFusionOpt {
  val IR: DeliteOpsExp
  import IR._

/*
  // overridden only to attach DeliteFatOp trait to result ...
  override def fatten(e: TP[Any]): TTP = e.rhs match {
    case op: DeliteOpLoop[_] => 
      TTP(List(e.sym), DeliteFatLoop(op.size, op.v, List(op.body)))
    case _ => super.fatten(e)
  }
*/

/*
  // TODO: can implement generically? or need override for VectorSize and all others?
  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLength(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }
*/
  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case e: DeliteCollectElem[_,_] if e.cond.isEmpty => Some(e.func)
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case e: DeliteCollectElem[_,_] => Some((e.func, e.cond))
    case _ => super.unapplySimpleCollectIf(e)
  }

  override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match {
    case e: DeliteCollectElem[_,_] => e.copy(cond = e.cond ++ c)
    case e: DeliteReduceElem[_] => e.copy(cond = e.cond ++ c)
    case _ => super.applyAddCondition(e,c)
  }

  override def shouldApplyFusion(currentScope: List[TTP])(result: Exp[Any]) = Config.opfusionEnabled

}

trait ScalaGenDeliteOps extends ScalaGenLoopsFat with BaseGenDeliteOps {
  import IR._

  def quotearg(x: Sym[Any]) = quote(x) + ": " + quotetp(x)
  def quotetp(x: Sym[Any]) = remap(x.Type)
/*
  def quoteZero(x: Sym[Any]) = x.Type.toString match { 
    case "Int" | "Long" | "Float" | "Double" => "0" 
    case "Boolean" => "false"
    case _ => "null" 
  }
*/
	/**
	 * MultiLoop components
	 */
	def emitCollectElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteCollectElem[_,_], prefixSym: String = "")(implicit stream: PrintWriter) {
		//emitBlock(elem.func)
    if (elem.cond.nonEmpty) {
      stream.print("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") ")
      stream.println(prefixSym + quote(sym) + ".insert(" + prefixSym + quote(sym) + ".length, " + quote(getBlockResult(elem.func)) + ")")
    } else
      stream.println(prefixSym + quote(sym) + ".dcUpdate(" + quote(op.v) + ", " + quote(getBlockResult(elem.func)) + ")")
	}
	
	def emitForeachElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteForeachElem[_])(implicit stream: PrintWriter) {
		if (elem.cond.nonEmpty)
    	stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
    emitBlock(elem.func)
		stream.println(quote(getBlockResult(elem.func)))
		if (elem.cond.nonEmpty) {
			stream.println("}")													
		}
	}
	
	def emitFirstReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_])(implicit stream: PrintWriter) {
		// zero if empty, deferred if conditional, initialized otherwise
		stream.println("if (" + quote(op.size) + " == 0) {" + quote(elem.zero) + "}")
		stream.println("else {")
		if (elem.cond.nonEmpty) {
			// if we have conditionals, we have to delay the the initialization of the accumulator to the
			// first element where the condition is true
			if (sym.Type <:< manifest[AnyVal]) {
				stream.println(quote(elem.zero))
			}
			else {
				stream.println("null.asInstanceOf[" + remap(elem.zero.Type) + "]")
			}
		}
		else {
			emitBlock(elem.func)
			stream.println(quote(getBlockResult(elem.func)))				
		}
		stream.println("}")			
	}

	def emitReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "")(implicit stream: PrintWriter) {
		//emitBlock(elem.func)
    if (elem.cond.nonEmpty){
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {"/*}*/)
			emitInitializeOrReduction(op, sym, elem, prefixSym)
			stream.println("}")
		}
		else {
			emitReduction(op, sym, elem, prefixSym)
		}
  }
	
	def emitInitializeOrReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "")(implicit stream: PrintWriter) {
		stream.println("// TODO: we could optimize this check away with more convoluted runtime support if necessary")					
		if (sym.Type <:< manifest[AnyVal]) {
			stream.println("if (" + prefixSym + quote(sym) + " == " + quote(elem.zero) + ")" + prefixSym + quote(sym) + " = {")
		}
		else {
			stream.println("if (" + prefixSym + quote(sym) + " == null) " + prefixSym + quote(sym) + " = {")
		}
		// initialize
		emitBlock(elem.func)
		stream.println(quote(getBlockResult(elem.func)))
		stream.println("}")
		// or reduce
		stream.println("else {")
		emitReduction(op, sym, elem, prefixSym)
		stream.println("}")
	}	
				
	def emitReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "")(implicit stream: PrintWriter) {
		stream.println("val " + quote(elem.rV._1) + " = " + prefixSym + quote(sym))
    stream.println("val " + quote(elem.rV._2) + " = " + quote(getBlockResult(elem.func)))
    emitBlock(elem.rFunc)
    stream.println(prefixSym + quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))    
	}
	
	def emitMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]])(implicit stream: PrintWriter) {
		val elemFuncs = op.body flatMap { // don't emit dependencies twice!
      case elem: DeliteCollectElem[_,_] => elem.func :: elem.cond
      case elem: DeliteForeachElem[_] => elem.cond // only emit func inside condition! TODO: how to avoid emitting deps twice? // elem.func :: elem.cond
      case elem: DeliteReduceElem[_] => elem.func :: elem.cond
    }
  	emitFatBlock(elemFuncs)
	}
	
	def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]])(implicit stream: PrintWriter) {
		/* strip first iteration */	
		stream.println("var " + quote(op.v) + " = 0")          					
		// initialization					
		emitMultiLoopFuncs(op, symList)
		(symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) =>
        stream.println("val " + quote(sym) + " = {"/*}*/)
        emitBlock(elem.alloc) // FIXME: how do we know it is the right size? conditions could have been added retrospectively!!
        if (elem.cond.nonEmpty)
          stream.println("//TODO: buffer size might be wrong (loop has conditions)")
        stream.println(quote(getBlockResult(elem.alloc)))
				stream.println(/*{*/"}")              
				emitCollectElem(op, sym, elem)
			case (sym, elem: DeliteForeachElem[_]) => 
				stream.println("var " + quotearg(sym) + " = {")
				emitForeachElem(op, sym, elem)
				stream.println("}")
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println("var " + quotearg(sym) + " = {")
				emitFirstReduceElem(op, sym, elem)
				stream.println("}")
    }
		stream.println(quote(op.v) + " = 1")
    stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(",")/*}*/)
		// body
    emitMultiLoopFuncs(op, symList)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) =>
				emitCollectElem(op, sym, elem)
			case (sym, elem: DeliteForeachElem[_]) => 
				stream.println(quote(sym) + " = {")             								
				emitForeachElem(op, sym, elem)
				stream.println("}")
      case (sym, elem: DeliteReduceElem[_]) =>
				emitReduceElem(op, sym, elem)
    }
    stream.println(quote(op.v) + " += 1")
    stream.println(/*{*/"} // end fat loop " + symList.map(quote).mkString(","))
	}
	
	def emitKernelAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]])(implicit stream: PrintWriter) {
		// kernel mode
    val kernelName = symList.map(quote).mkString("")
    val actType = "activation_"+kernelName
    deliteKernel = false
    stream.println("val " + kernelName + " = new generated.scala.DeliteOpMultiLoop[" + actType + "] {"/*}*/)
    // TODO: if there are conditions, the output size is not known (but for now it is known to be smaller than the input size)
    // two options:
    // - combine (reduce step) using concat <-- simpler to implement but slow
    // - use a pre-combine scan step to find out where each processor should write the data
    //   and do the copying in another parallel map <-- faster but more work
    
    stream.println("def size = " + quote(op.size))
    stream.println("def alloc: " + actType + " = {"/*}*/)
    stream.println("val __act = new " + actType)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) =>
        emitBlock(elem.alloc) // FIXME: how do we know it is the right size? conditions could have been added retrospectively!!
        if (elem.cond.nonEmpty)
          stream.println("//TODO: buffer size might be wrong (loop has conditions)")
        stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.alloc)))
			case (sym, elem: DeliteForeachElem[_]) => // initialized in init below
      case (sym, elem: DeliteReduceElem[_]) => // initialized in init below
    }
    stream.println("__act")
    stream.println(/*{*/"}")
    stream.println("def init(__act: " + actType + ", " + quotearg(op.v) + "): " + actType + " = {"/*}*/)
    if (op.body exists (loopBodyNeedsCombine _)) {
			emitMultiLoopFuncs(op, symList)				  											
      stream.println("val __act2 = new " + actType)
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_]) =>
          if (elem.cond.nonEmpty) {
            stream.println("//TODO: buffer size might be wrong (loop has conditions)") // separate buffer for each process
						stream.println("if (" + quote(op.v) + " != 0)")
            stream.println("__act2." + quote(sym) + " = " + "__act." + quote(sym) + ".cloneL")
						stream.println("else")
          } 
          stream.println("__act2." + quote(sym) + " = " + "__act." + quote(sym))
					emitCollectElem(op, sym, elem, "__act2.")
				case (sym, elem: DeliteForeachElem[_]) => 
					stream.println("__act2." + quote(sym) + " = {")
					emitForeachElem(op, sym, elem)
					stream.println("}")								
        case (sym, elem: DeliteReduceElem[_]) =>
          stream.println("__act2." + quote(sym) + " = {")					
					emitFirstReduceElem(op, sym, elem)
					stream.println("}")
      }
      stream.println("__act2")
    } else {
			stream.println("process(__act, " + quote(op.v) + ")")
			stream.println("__act")
    }
    stream.println(/*{*/"}")
    stream.println("def process(__act: " + actType + ", " + quotearg(op.v) + "): Unit = {")
		emitMultiLoopFuncs(op, symList)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) =>
				emitCollectElem(op, sym, elem, "__act.")
			case (sym, elem: DeliteForeachElem[_]) =>
				stream.println("val " + quote(sym) + " = {")
				emitForeachElem(op, sym, elem)
				stream.println("}")
      case (sym, elem: DeliteReduceElem[_]) =>
				emitReduceElem(op, sym, elem, "__act.")
    }
    stream.println(/*{*/"}")
    stream.println("def combine(__act: " + actType + ", rhs: " + actType + "): Unit = {"/*}*/)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) =>
        if (elem.cond.nonEmpty) {
          stream.println("//TODO: this is inefficient. should use a scan pass.")
          stream.println("__act." + quote(sym) + ".insertAll(__act." +quote(sym) + ".length, rhs." + quote(sym) + ")")
        }
			case (sym, elem: DeliteForeachElem[_]) => // nothing needed
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println("val " + quote(elem.rV._1) + " = " + "__act." + quote(sym))
        stream.println("val " + quote(elem.rV._2) + " = " + "rhs." + quote(sym))
        emitBlock(elem.rFunc)
        stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))
    }
    stream.println(/*{*/"}")

    stream.println(/*{*/"}")
    deliteKernel = true
  }
	
  // TODO: conditions!
  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case op: AbstractFatLoop =>
    	if (!deliteKernel) emitInlineAbstractFatLoop(op, symList)
			else emitKernelAbstractFatLoop(op, symList)
    case _ => super.emitFatNode(symList, rhs)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      //printlog("EMIT single "+s)
      val save = deliteKernel
      deliteKernel = false
      val b = s.block
      if (!save) {
        // straight-line
        stream.println("val " + quote(sym) + " = { " )
        emitBlock(b)
        stream.println(quote(getBlockResult(b)))
        stream.println("}")
      }
      else {
        // method wrapper
        stream.println("def " + quote(sym) + "_block = { ")
        emitBlock(b)
        stream.println(quote(getBlockResult(b)))
        stream.println("}")
        stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      deliteKernel = save
    }
    case op: AbstractLoop[_] => 
      // TODO: we'd like to always have fat loops but currently they are not allowed to have effects
      stream.println("// a *thin* loop follows: " + quote(sym))
      emitFatNode(List(sym), SimpleFatLoop(op.size, op.v, List(op.body)))
/*    
      if (!deliteKernel) { // FIXME cond!
        op.body match {
          case elem: DeliteCollectElem[_,_] =>
            stream.println("val " + quote(sym) + " = {")
            emitBlock(elem.alloc)
            stream.println(quote(getBlockResult(elem.alloc)))
            stream.println("}")
          case elem: DeliteReduceElem[_] =>
            stream.println("var " + quotearg(sym) + " = " + quote(elem.zero))
        }
        stream.println("var " + quote(op.v) + " = 0")
        stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin thin loop " + quote(sym))
        op.body match {
          case elem: DeliteCollectElem[_,_] =>
            emitBlock(elem.func)
            stream.println(quote(sym) + ".dcUpdate(" + quote(op.v) + ", " + quote(getBlockResult(elem.func)) + ")")
          case elem: DeliteReduceElem[_] =>
            emitBlock(elem.func)
            stream.println("val " + quote(elem.rV._1) + " = " + quote(sym))
            stream.println("val " + quote(elem.rV._2) + " = " + quote(getBlockResult(elem.func)))
            emitBlock(elem.rFunc)
            stream.println(quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))
        }
        stream.println(quote(op.v) + " += 1")
        stream.println("} // end thin loop " + quote(sym))
      } else {
        stream.println("TODO: thin loop codegen")
      }
*/      
    
		case map:DeliteOpMap[_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        emitBlock(map.alloc)
        stream.println("var mapIdx = 0")
        stream.println("while (mapIdx < " + quote(getBlockResult(map.in)) + ".size) { // begin map loop " + quote(sym))
        stream.println("val " + quote(map.v) + " = " + quote(getBlockResult(map.in)) + ".dcApply(mapIdx)")
        stream.println(quote(getBlockResult(map.alloc)) + ".dcUpdate(mapIdx, " + " {")
        emitBlock(map.func)
        stream.println(quote(getBlockResult(map.func)))
        stream.println("})")
        stream.println("mapIdx += 1")
        stream.println("} // end map loop " + quote(sym))
        stream.println(quote(getBlockResult(map.alloc)))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpMap[" + remap(map.v.Type) + "," + remap(map.func.Type) + "," + remap(map.alloc.Type) + "] {")
        stream.println("def in = " + quote(getBlockResult(map.in)))
        stream.println("def alloc = {")
        emitBlock(map.alloc)
        stream.println(quote(getBlockResult(map.alloc)))
        stream.println("}")
        stream.println("def map(" + quote(map.v) + ": " + remap(map.v.Type) + ") = {")
        emitBlock(map.func)
        stream.println(quote(getBlockResult(map.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case zip: DeliteOpZipWith[_,_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        emitBlock(zip.alloc)
        stream.println("var zipIdx = 0")
        stream.println("while (zipIdx < " + quote(getBlockResult(zip.inA)) + ".size) { // begin zip loop " + quote(sym))
        stream.println("val " + quote(zip.v._1) + " = " + quote(getBlockResult(zip.inA)) + ".dcApply(zipIdx)")
        stream.println("val " + quote(zip.v._2) + " = " + quote(getBlockResult(zip.inB)) + ".dcApply(zipIdx)")
        stream.println(quote(getBlockResult(zip.alloc)) + ".dcUpdate(zipIdx, " + " {")
        emitBlock(zip.func)
        stream.println(quote(getBlockResult(zip.func)))
        stream.println("})")
        stream.println("zipIdx += 1")
        stream.println("} // end zip loop " + quote(sym))
        stream.println(quote(getBlockResult(zip.alloc)))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpZipWith[" + remap(zip.v._1.Type) + "," + remap(zip.v._2.Type) + "," + remap(zip.func.Type) + "," + remap(zip.alloc.Type) +"] {")
        stream.println("def inA = " + quote(getBlockResult(zip.inA)))
        stream.println("def inB = " + quote(getBlockResult(zip.inB)))
        stream.println("def alloc = {")
        emitBlock(zip.alloc)
        stream.println(quote(getBlockResult(zip.alloc)))
        stream.println("}")
        stream.println("def zip(" + quote(zip.v._1) + ": " + remap(zip.v._1.Type) + ", " + quote(zip.v._2) + ": " + remap(zip.v._2.Type) + ") = {")
        emitBlock(zip.func)
        stream.println(quote(getBlockResult(zip.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case red: DeliteOpReduce[_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("var " + quote(red.v._1) + " = " + quote(getBlockResult(red.in)) + ".dcApply(0)")
        stream.println("var reduceIdx = 1")
        stream.println("while (reduceIdx < " + quote(getBlockResult(red.in)) + ".size) { // begin reduce loop " + quote(sym))
        stream.println("val " + quote(red.v._2) + " = " + quote(getBlockResult(red.in)) + ".dcApply(reduceIdx)")
        stream.println(quote(red.v._1) + " = {")
        emitBlock(red.func)
        stream.println(quote(getBlockResult(red.func)))
        stream.println("}")
        stream.println("reduceIdx += 1")
        stream.println("} // end reduce loop " + quote(sym))
        stream.println(quote(red.v._1))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpReduce[" + remap(red.func.Type) + "] {")
        stream.println("def in = " + quote(getBlockResult(red.in)))
        stream.println("def reduce(" + quote(red.v._1) + ": " + remap(red.v._1.Type) + "," + quote(red.v._2) + ": " + remap(red.v._2.Type) + ") = {")
        emitBlock(red.func)
        stream.println(quote(getBlockResult(red.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case mapR:DeliteOpMapReduce[_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("val " + quote(mapR.mV) + " = " + quote(getBlockResult(mapR.in)) + ".dcApply(0)")
        stream.println("var " + quote(mapR.rV._1) + " = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println("var mapReduceIdx = 1")
        stream.println("while (mapReduceIdx < " + quote(getBlockResult(mapR.in)) + ".size) { // begin mapReduce loop " + quote(sym))
        stream.println("val " + quote(mapR.mV) + " = " + quote(getBlockResult(mapR.in)) + ".dcApply(mapReduceIdx)")
        stream.println("val " + quote(mapR.rV._2) + " = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println(quote(mapR.rV._1) + " = {")
        emitBlock(mapR.reduce)
        stream.println(quote(getBlockResult(mapR.reduce)))
        stream.println("}")
        stream.println("mapReduceIdx += 1")
        stream.println("} // end mapReduce loop " + quote(sym))
        stream.println(quote(mapR.rV._1))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpMapReduce[" + remap(mapR.mV.Type) + "," + remap(mapR.reduce.Type) + "] {")
        stream.println("def in = " + quote(getBlockResult(mapR.in)))
        stream.println("def map(" + quote(mapR.mV) + ": " + remap(mapR.mV.Type) + ") = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println("")
        stream.println("def reduce(" + quote(mapR.rV._1) + ": " + remap(mapR.rV._1.Type) + "," + quote(mapR.rV._2) + ": " + remap(mapR.rV._2.Type) + ") = {")
        emitBlock(mapR.reduce)
        stream.println(quote(getBlockResult(mapR.reduce)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case zipR:DeliteOpZipWithReduce[_,_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("val " + quote(zipR.zV._1) + " = " + quote(getBlockResult(zipR.inA)) + ".dcApply(0)")
        stream.println("val " + quote(zipR.zV._2) + " = " + quote(getBlockResult(zipR.inB)) + ".dcApply(0)")
        stream.println("var " + quote(zipR.rV._1) + " = {")
        emitBlock(zipR.zip)
        stream.println(quote(getBlockResult(zipR.zip)))
        stream.println("}")
        stream.println("var zipReduceIdx = 1")
        stream.println("while (zipReduceIdx < " + quote(getBlockResult(zipR.inA)) + ".size) { // begin zipReduce loop " + quote(sym))
        stream.println("val " + quote(zipR.zV._1) + " = " + quote(getBlockResult(zipR.inA)) + ".dcApply(zipIdx)")
        stream.println("val " + quote(zipR.zV._2) + " = " + quote(getBlockResult(zipR.inB)) + ".dcApply(zipIdx)")
        stream.println("val " + quote(zipR.rV._2) + " = {")
        emitBlock(zipR.zip)
        stream.println(quote(getBlockResult(zipR.zip)))
        stream.println("}")
        stream.println(quote(zipR.rV._1) + " = {")
        emitBlock(zipR.reduce)
        stream.println(quote(getBlockResult(zipR.reduce)))
        stream.println("}")
        stream.println("zipReduceIdx += 1")
        stream.println("} // end zipReduce loop " + quote(sym))
        stream.println(quote(zipR.rV._1))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpZipWithReduce[" + remap(zipR.zV._1.Type) + "," + remap(zipR.zV._2.Type) + "," + remap(zipR.reduce.Type) + "] {")
        stream.println("def inA = " + quote(getBlockResult(zipR.inA)))
        stream.println("def inB = " + quote(getBlockResult(zipR.inB)))
        stream.println("def zip(" + quote(zipR.zV._1) + ": " + remap(zipR.zV._1.Type) + ", " + quote(zipR.zV._2) + ": " + remap(zipR.zV._2.Type) + ") = {")
        emitBlock(zipR.zip)
        stream.println(quote(getBlockResult(zipR.zip)))
        stream.println("}")
        stream.println("")
        stream.println("def reduce(" + quote(zipR.rV._1) + ": " + remap(zipR.rV._1.Type) + "," + quote(zipR.rV._2) + ": " + remap(zipR.rV._2.Type) + ") = {")
        emitBlock(zipR.reduce)
        stream.println(quote(getBlockResult(zipR.reduce)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case foreach:DeliteOpForeach[_,_] => {
      if (deliteKernel == false){
        //stream.println("def " + quote(sym) + "_block = {")
        stream.println("val " + quote(sym) + " = {")
        stream.println("var forIdx = 0")
        stream.println("while (forIdx < " + quote(getBlockResult(foreach.in)) + ".size) { // begin foreach loop " + quote(sym))
        stream.println("val " + quote(foreach.v) + " = " + quote(getBlockResult(foreach.in)) + ".dcApply(forIdx)")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("forIdx += 1")
        stream.println("} // end foreach loop " + quote(sym))
        stream.println("}")
	      //stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpForeach[" + remap(foreach.v.Type) + "] {")
        stream.println("def in = " + quote(getBlockResult(foreach.in)))
        stream.println("def sync(" + quote(foreach.i) + ": " + remap(foreach.i.Type) + ") = {")
        emitBlock(foreach.sync)
        stream.println(quote(getBlockResult(foreach.sync)))
        stream.println("}")
        stream.println("def foreach(" + quote(foreach.v) + ": " + remap(foreach.v.Type) + ") = {")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case foreach:DeliteOpForeachBounded[_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("var forIdx = 0")
        stream.println("while (forIdx < " + quote(getBlockResult(foreach.in.asInstanceOf[Exp[Any]])) + ".size) { // begin foreachBounded loop " + quote(sym))
        stream.println("val " + quote(foreach.v) + " = " + quote(getBlockResult(foreach.in.asInstanceOf[Exp[Any]])) + ".dcApply(forIdx)")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("forIdx += 1")
        stream.println("} // end foreachBounded loop " + quote(sym))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpForeach[" + remap(foreach.v.Type) + "] {")
        stream.println("def in = " + quote(getBlockResult(foreach.in.asInstanceOf[Exp[Any]])))
        stream.println("def sync(" + quote(foreach.i) + ": " + remap(foreach.i.Type) + ") = {")
        emitBlock(foreach.sync)
        stream.println(quote(getBlockResult(foreach.sync)))
        stream.println("}")
        stream.println("def foreach(" + quote(foreach.v) + ": " + remap(foreach.v.Type) + ") = {")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenDeliteOps extends CudaGenLoopsFat with BaseGenDeliteOps {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case op: AbstractFatLoop =>
      if(symList.length != 1) throw new GenerationFailedException("CudaGen: Only 1 output is supported for FatLoop (No fusing yet).")
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(op.v)+"->size()")
      stream.println(addTab()+"if( %s < %s ) {".format(currDimStr,quote(op.v)+".size()"))
      tabWidth += 1
      (symList zip op.body) foreach {
        case (sym, elem:DeliteCollectElem[_,_]) =>
          emitAllocFunc(sym,elem.alloc)
          val (loopFunc,freeVars) = emitDevFunc(elem.func, List(op.v))
          if(freeVars.length==0)
            stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s)));".format(quote(sym),currDimStr,loopFunc,quote(op.v),currDimStr))
          else
            stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s));".format(quote(sym),currDimStr,loopFunc,quote(op.v),currDimStr,freeVars.map(quote).mkString(",")))
        case _ =>
          throw new GenerationFailedException("CudaGen: DeliteReduceElem is not supported yet.")
      }
      tabWidth -= 1
      stream.println(addTab()+"}")
      currDim -= 1
      deliteKernel = true

    case _ => super.emitFatNode(symList, rhs)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] => throw new GenerationFailedException("CudaGen: DeliteOpSingleTask is not GPUable." + quote(sym))
      // TODO: Generate single thread version of this work

    case map:DeliteOpMap[_,_,_] => {
      if(!isPrimitiveType(map.func.Type)) throw new GenerationFailedException("CudaGen: Only primitive Types are allowed for map.")
      if(!isPrimitiveType(map.v.Type)) throw new GenerationFailedException("CudaGen: Only primitive Types are allowed for map.")
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(map.in)+"->size()")
      stream.println(addTab()+"if( %s < %s ) {".format(currDimStr,quote(map.in)+".size()"))
      tabWidth += 1
      val (mapFunc,freeVars) = emitDevFunc(map.func, List(map.v))
      if(freeVars.length==0)
        stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s)));".format(quote(sym),currDimStr,mapFunc,quote(map.in),currDimStr))
      else
        stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s));".format(quote(sym),currDimStr,mapFunc,quote(map.in),currDimStr,freeVars.map(quote).mkString(",")))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitAllocFunc(sym,map.alloc)
	    if(map.in==map.alloc) throw new GenerationFailedException("CudaGen: Mutable input is not supported yet.")
      currDim -= 1
    }

    case zip: DeliteOpZipWith[_,_,_,_] => {
      if(!isPrimitiveType(zip.func.Type)) throw new GenerationFailedException("CudaGen: Only primitive Types are allowed for zipwith.")
      if(!isPrimitiveType(zip.v._1.Type )) throw new GenerationFailedException("CudaGen: Only primitive Types are allowed for zipwith.")
      if(!isPrimitiveType(zip.v._2.Type)) throw new GenerationFailedException("CudaGen: Only primitive Types are allowed for zipwith.")
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(zip.inA)+"->size()")
      stream.println(addTab()+"if( %s < %s ) {".format(currDimStr,quote(zip.inA)+".size()"))
      tabWidth += 1
      val (zipFunc,freeVars) = emitDevFunc(zip.func, List(zip.v._1, zip.v._2))
      if(freeVars.length==0)
        stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s.dcApply(%s)));".format(quote(sym),currDimStr, zipFunc, quote(zip.inA),currDimStr,quote(zip.inB),currDimStr))
      else
        stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s.dcApply(%s),%s));".format(quote(sym),currDimStr, zipFunc, quote(zip.inA),currDimStr,quote(zip.inB),currDimStr,freeVars.map(quote).mkString(",")))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitAllocFunc(sym,zip.alloc)
	    if((zip.inA==zip.alloc) || (zip.inB==zip.alloc)) throw new GenerationFailedException("CudaGen: Mutable input is not supported yet.")
      currDim -= 1
    }

    case foreach:DeliteOpForeach[_,_] => {
      if(!isPrimitiveType(foreach.v.Type)) throw new GenerationFailedException("CudaGen: Only primitive Types are allowed for input of foreach.")
      currDim += 1
      setCurrDimLength(quote(foreach.in)+"->size()")
      val currDimStr = getCurrDimStr()
      stream.println(addTab()+"if( %s < %s ) {".format(currDimStr,quote(foreach.in)+".size()"))
      tabWidth += 1
      val (foreachFunc,freeVars) = emitDevFunc(foreach.func, List(foreach.v))
      if(freeVars.length==0)
        stream.println(addTab()+"%s(%s.dcApply(%s));".format(foreachFunc,quote(foreach.in),currDimStr))
      else
        stream.println(addTab()+"%s(%s.dcApply(%s),%s);".format(foreachFunc,quote(foreach.in),currDimStr,freeVars.map(quote).mkString(",")))
      tabWidth -= 1
      stream.println(addTab()+"}")
      currDim -= 1
    }

    case red: DeliteOpReduce[_] => {
      if(!isPrimitiveType(red.func.Type)) throw new GenerationFailedException("CudaGen: Only primitive Types are allowed for reduce.")
      if(currDim < 1) new GenerationFailedException("CudaGen: Reduction on the 1'st dimension is not supported yet.")
      val (reducFunc,freeVars) = emitDevFunc(red.func, List(red.v._1, red.v._2))
      stream.println(addTab()+"%s reducVal = %s.apply(0);".format(remap(sym.Type),quote(red.in)))
      stream.println(addTab()+"for(int i=1; i<%s.size(); i++) {".format(quote(red.in)))
      tabWidth += 1
	    if(freeVars.length==0)
      	stream.println(addTab()+"reducVal = %s(reducVal,%s.apply(i));".format(reducFunc,quote(red.in)))
	    else
      	stream.println(addTab()+"reducVal = %s(reducVal,%s.apply(i),%s);".format(reducFunc,quote(red.in),freeVars.map(quote).mkString(",")))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitValDef(sym,"reducVal")
    }

    case mapR:DeliteOpMapReduce[_,_,_] => {
      //if(!isPrimitiveType(mapR.mV.Type)) throw new GenerationFailedException("CudaGen: Only primitive Types are allowed for MapReduce.")
      //if(!isPrimitiveType(mapR.reduce.Type)) throw new GenerationFailedException("CudaGen: Only primitive Types are allowed for MapReduce.")
      val constrained = isPrimitiveType(mapR.mV.Type) && isPrimitiveType(mapR.reduce.Type)
      if(constrained) {
        stream.println(addTab()+"%s %s = %s.apply(0);".format(remap(mapR.mV.Type),quote(mapR.mV),quote(mapR.in)))
        emitBlock(mapR.map)
        emitValDef(mapR.rV._1.asInstanceOf[Sym[_]],quote(getBlockResult(mapR.map)))
        stream.println(addTab()+"for(int cnt=1; cnt<%s.size(); cnt++) {".format(quote(mapR.in)))
        tabWidth += 1
        stream.println(addTab()+"%s = %s.apply(cnt);".format(quote(mapR.mV),quote(mapR.in)))
        emitBlock(mapR.map)
        emitValDef(mapR.rV._2.asInstanceOf[Sym[_]],quote(getBlockResult(mapR.map)))
        emitBlock(mapR.reduce)
        stream.println(addTab()+"%s = %s;".format(quote(mapR.rV._1.asInstanceOf[Sym[_]]),quote(getBlockResult(mapR.reduce))))
        tabWidth -= 1
        stream.println(addTab()+"}")
        emitValDef(sym,quote(mapR.rV._1))
      }
      else {
        useLocalVar = true
        stream.println(addTab()+"%s = %s;".format(quote(mapR.rV._1),quote(sym)))
        val reduceLocalVar = getNewLocalVar()
        val outLocalVar = getNewLocalVar()
        val nextDimStr = getNextDimStr()
        saveLocalVar(sym,nextDimStr,outLocalVar)
        stream.println(addTab()+"%s %s = 0;".format(remap(sym.Type.typeArguments(0)),outLocalVar))
        stream.println(addTab()+"for(int cnt=0; cnt<%s.size(); cnt++) {".format(quote(mapR.in)))
        tabWidth += 1
        stream.println(addTab()+"%s %s = %s.dcApply(cnt);".format(remap(mapR.mV.Type),quote(mapR.mV),quote(mapR.in)))
        emitBlock(mapR.map)
        stream.println(addTab()+"%s = %s;".format(quote(mapR.rV._2),quote(getBlockResult(mapR.map))))
        stream.println(addTab()+"%s %s = %s;".format(remap(sym.Type.typeArguments(0)),reduceLocalVar,getLocalVar(getBlockResult(mapR.map),nextDimStr)))
        saveLocalVar(mapR.rV._1,nextDimStr,outLocalVar)
        saveLocalVar(mapR.rV._2,nextDimStr,reduceLocalVar)
        emitBlock(mapR.reduce)
        tabWidth -= 1
        stream.println(addTab()+"}")
        allocReference(mapR.rV._1.asInstanceOf[Sym[_]],getBlockResult(mapR.map).asInstanceOf[Sym[_]])
        allocReference(mapR.rV._2.asInstanceOf[Sym[_]],getBlockResult(mapR.map).asInstanceOf[Sym[_]])
        allocOutput(sym,getBlockResult(mapR.map).asInstanceOf[Sym[_]],true)
        stream.println(addTab()+"%s.dcUpdate(%s,%s);".format(quote(sym),nextDimStr,outLocalVar))
        useLocalVar = false
      }
    }

    case _ => super.emitNode(sym,rhs)
  }
}

trait CGenDeliteOps extends CGenEffect with BaseGenDeliteOps {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case op: AbstractFatLoop =>
      println("TODO: implement emitFatNode in CGenDeliteOps")
      throw new GenerationFailedException("TODO: implement emitFatNode in CGenDeliteOps")
    case _ => super.emitFatNode(symList, rhs)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] =>
      emitBlock(s.block)
      emitValDef(sym,quote(getBlockResult(s.block)))

    //TODO: implement deliteops
    //case map:DeliteOpMap[_,_,_] =>
    //case zip: DeliteOpZipWith[_,_,_,_] =>
    //case mapR:DeliteOpMapReduce[_,_,_] =>
    case _ => super.emitNode(sym,rhs)
  }
}
