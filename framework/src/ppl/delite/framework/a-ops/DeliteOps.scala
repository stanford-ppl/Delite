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
  
  
  case class FF[A,B](val x: Rep[A], val y: Rep[B])(val f: Rep[A]=>Rep[B])
  
  type ===>[A,B] = FF[A,B]
  
  def deliteFunc[A:Manifest,B](f: Rep[A]=>Rep[B]): A ===> B = { val x = fresh[A]; FF(x, f(x))(f) }
  
  
  
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

  abstract class DeliteOpLoop[A] extends AbstractLoop[A] with DeliteOp[A] {
    final val v = fresh[Int]
  }

//  case class DeliteOpFatLoop(val size: Exp[Int], val v: Sym[Int], val body: List[Def[Any]]) extends AbstractFatLoop with DeliteFatOp
  
  
  // for use in loops:

  case class DeliteForeachElem[A](
    func: Exp[A],
    sync: Exp[List[Any]]
    // TODO: this is sort of broken right now, re-enable when we figure out how to make this work without emitting dependencies twice
    //cond: List[Exp[Boolean]] = Nil
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
    rFunc: Exp[A],
    stripFirst: Boolean = true
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
   *  Delite parallel ops - represents common parallel execution patterns, most of which
   *  are represented by an underlying 'loop' abstraction.
   *  
   *  Note that size is supplied explicitly to allow domain-specific pattern rewrites.
   *  
   *  One design question moving forward: fusing can handle much of the composition here
   *  automatically (e.g. MapReduce / ZipWithReduce), but we currently don't have a way
   *  to represent this fused thing as a single parallel op (we have to unroll it).
   *  It would be nice to be general, so that we could have e.g. a ZipZipReduce op that is
   *  automatically fused and still a single IR node. OpComposite?
   *  
   *  NOTE ABOUT ZERO: 
   *    the supplied zero parameter is required to have value equality *in the generated code*
   *    it will not be used unless the collection is empty or in a conditional reduce where the
   *    first (or more) conditions fail. In both cases, we never try to actually reduce a zero
   *    element - we only return it or use it as an initialization check.
   *  
   *    if stripFirst is set to false, i.e. for a mutable reduction, then the zero value is used
   *    to allocate the accumulator, and it IS used in the initial reduction.
   */


  /**
   * Parallel map from DeliteCollection[A] => DeliteCollection[B]. Input functions can depend on free
   * variables, but they cannot depend on other elements of the input or output collection (disjoint access).
   *
   * @param  in    the input collection
   * @param  size  the size of the input collection
   * @param  func  the mapping function Exp[A] => Exp[B]
   * @param  alloc function returning the output collection. if it is the same as the input collection,
   *               the operation is mutable; (=> DeliteCollection[B]).
   */ 
  abstract class DeliteOpMap[A:Manifest,
                             B:Manifest, CB <: DeliteCollection[B]:Manifest]
    extends DeliteOpLoop[CB] {

    // supplied by subclass
    val in: Exp[DeliteCollection[A]]
    //val size: Exp[Int] // could be dc_size(in), but we want type-specific pattern matching to work
    def func: Exp[A] => Exp[B]
    def alloc: Exp[CB]

    // loop
    lazy val body: Def[CB] = DeliteCollectElem[B, CB](
      alloc = transform(reifyEffects(this.alloc)),
      func = transform(reifyEffects(this.func(dc_apply(in,v))))
    )
  }

  /**
   *  Currently conditionally appends values to buffers, which are concatenated in the combine stage.
   *  Note that it is also implicitly a Map-Filter (it accepts a mapping function). Should this be renamed? 
   *  
   *  Should eventually be implemented as a parallel scan.
   */
  abstract class DeliteOpFilter[A:Manifest,
                                B:Manifest, CB <: DeliteCollection[B]:Manifest]
    extends DeliteOpLoop[CB] {

    // supplied by subclass
    val in: Exp[DeliteCollection[A]]
    //val size: Exp[Int] // could be dc_size(in), but we want type-specific pattern matching to work
    def func: Exp[A] => Exp[B]
    def alloc: Exp[CB]
    def cond: Exp[A] => Exp[Boolean] // does this need to be more general (i.e. a List?)

    // loop
    lazy val body: Def[CB] = DeliteCollectElem[B, CB](
      alloc = transform(reifyEffects(this.alloc)),
      func = transform(reifyEffects(this.func(dc_apply(in,v)))),
      cond = transform(reifyEffects(this.cond(dc_apply(in,v))))::Nil      
    )
  }
  
  /*TR
  
  thoughts about filter:
  
  - fresh simple buffers for each chunk
  - scan pass to determine correct positions
  - copy each buffer into target data structure
  
  need: 
  - a way to store buffers in activation record (?) or kernel object itself (?)
    - problem: need additional fields in activation record, which is part of
      kernel header, defined in lms-core. how to communicate data into into its declaration?
  - implement scan pass
    - additional methods: pre-combine, post-combine (?)
  
  */
  
  
  
  
  /**
   * Parallel 2 element zipWith from (DeliteCollection[A],DeliteCollection[B]) => DeliteCollection[R].
   * Input functions can depend on free variables, but they cannot depend on other elements of the input or
   * output collection (disjoint access). 
   *
   * @param  inA   the first input collection
   * @param  inB   the second input collection
   * @param  size  the size of the collections (should be the same) 
   * @param  func  the zipWith function; ([Exp[A],Exp[B]) => Exp[R]
   * @param  alloc function returning the output collection. if it is the same as the input collection,
   *               the operation is mutable; (=> DeliteCollection[B]).
   */
  abstract class DeliteOpZipWith[A:Manifest,
                                 B:Manifest,
                                 R:Manifest, CR <: DeliteCollection[R]:Manifest]
    extends DeliteOpLoop[CR] {
      
    // supplied by subclass   
    val inA: Exp[DeliteCollection[A]]
    val inB: Exp[DeliteCollection[B]]
    def func: (Exp[A], Exp[B]) => Exp[R]
    def alloc: Exp[CR]
    
    // loop
    lazy val body: Def[CR] = DeliteCollectElem[R, CR](
      alloc = transform(reifyEffects(this.alloc)),
      func = transform(reifyEffects(this.func(dc_apply(inA,v), dc_apply(inB,v))))
    )   
  }
  
  trait DeliteOpReduceLike {
    val mutable: Boolean = false
  }
  
  /**
   * Parallel reduction of a DeliteCollection[A]. Reducing function must be associative.
   *
   * @param  in    the input collection
   * @param  size  the size of the input collection 
   * @param  zero  the "empty" value - must have value equality 
   * @param  func  the reduction function; ([Exp[A],Exp[A]) => Exp[A]. Must be associative.
   */
  abstract class DeliteOpReduce[A:Manifest] extends DeliteOpLoop[A] with DeliteOpReduceLike {
      
    // supplied by subclass   
    val in: Exp[DeliteCollection[A]]
    val zero: Exp[A] 
    def func: (Exp[A], Exp[A]) => Exp[A]
    
    // loop    
    final protected val rV = (fresh[A], fresh[A])
    lazy val body: Def[A] = DeliteReduceElem[A](
      func = transform(reifyEffects(dc_apply(in,v))),
      zero = transform(this.zero),      
      rV = (transform(rV._1).asInstanceOf[Sym[A]], transform(rV._2).asInstanceOf[Sym[A]]),
      rFunc = transform(reifyEffects(this.func(rV._1, rV._2))),
      stripFirst = !this.mutable
    )   
  }
  
  /**
   * Parallel map-reduction from a DeliteCollection[A] => R. The map-reduce is composed, so no temporary collection
   * is instantiated to hold the result of the map.
   *
   * @param  in      the input collection
   * @param  size    the size of the input collection
   * @param  zero    the "empty" value - must have value equality  
   * @param  map     the mapping function; Exp[A] => Exp[R]
   * @param  reduce  the reduction function; ([Exp[R],Exp[R]) => Exp[R]. Must be associative.
   */
  abstract class DeliteOpMapReduce[A:Manifest,R:Manifest]
    extends DeliteOpLoop[R] with DeliteOpReduceLike {
    
    // supplied by subclass   
    val in: Exp[DeliteCollection[A]]
    val zero: Exp[R] 
    def map: Exp[A] => Exp[R]
    def reduce: (Exp[R], Exp[R]) => Exp[R]
    
    // loop
    final protected val rV = (fresh[R], fresh[R])
    lazy val body: Def[R] = DeliteReduceElem[R](
      func = transform(reifyEffects(map(dc_apply(in,v)))),
      zero = transform(this.zero),      
      rV = (transform(rV._1).asInstanceOf[Sym[R]], transform(rV._2).asInstanceOf[Sym[R]]),
      rFunc = transform(reifyEffects(reduce(rV._1, rV._2))),
      stripFirst = !this.mutable
    )     
  }
  
  // should this be folded into DeliteOpMapReduce (or into DeliteOpFilter)?
  abstract class DeliteOpFilterReduce[A:Manifest,R:Manifest]
    extends DeliteOpLoop[R] with DeliteOpReduceLike {
    
    // supplied by subclass   
    val in: Exp[DeliteCollection[A]]
    val zero: Exp[R] 
    def func: Exp[A] => Exp[R]        
    def reduce: (Exp[R], Exp[R]) => Exp[R]
    def cond: Exp[A] => Exp[Boolean] // does this need to be more general (i.e. a List?)
    
    // loop
    final protected val rV = (fresh[R], fresh[R])    
    lazy val body: Def[R] = DeliteReduceElem[R](
      func = transform(reifyEffects(this.func(dc_apply(in,v)))),
      cond = transform(reifyEffects(this.cond(dc_apply(in,v))))::Nil,
      zero = transform(this.zero),      
      rV = (transform(rV._1).asInstanceOf[Sym[R]], transform(rV._2).asInstanceOf[Sym[R]]),
      rFunc = transform(reifyEffects(reduce(rV._1, rV._2))),
      stripFirst = !this.mutable
    ) 
  }
  
  /**
   * Parallel zipWith-reduction from a (DeliteCollection[A],DeliteCollection[A]) => R. The map-reduce is composed,
   * so no temporary collection is instantiated to hold the result of the map.
   *
   * @param  inA     the first input collection
   * @param  inB     the second input collection
   * @param  size    the size of the input collections (should be the same)
   * @param  zero    the "empty" value - must have value equality  
   * @param  zip     the zipWith function; reified version of (Exp[A],Exp[B]) => Exp[R]
   * @param  reduce  the reduction function; reified version of ([Exp[R],Exp[R]) => Exp[R]. Must be associative.
   */
  abstract class DeliteOpZipWithReduce[A:Manifest,B:Manifest,R:Manifest]
    extends DeliteOpLoop[R] with DeliteOpReduceLike {
      
    // supplied by subclass   
    val inA: Exp[DeliteCollection[A]]
    val inB: Exp[DeliteCollection[B]]
    val zero: Exp[R]
    def zip: (Exp[A], Exp[B]) => Exp[R]
    def reduce: (Exp[R], Exp[R]) => Exp[R]
    
    // loop
    final protected val rV = (fresh[R], fresh[R])
    lazy val body: Def[R] = DeliteReduceElem[R](
      func = transform(reifyEffects(zip(dc_apply(inA,v), dc_apply(inB,v)))),
      zero = transform(this.zero),      
      rV = (transform(rV._1).asInstanceOf[Sym[R]], transform(rV._2).asInstanceOf[Sym[R]]),
      rFunc = transform(reifyEffects(reduce(rV._1, rV._2))),
      stripFirst = !this.mutable
    )         
  }

  /**
   * Parallel foreach from DeliteCollection[A] => Unit. Input functions must specify any free variables that it
   * requires are protected (e.g. locked before chunk execution) using the sync list.
   *
   * @param  in     the input collection
   * @param  size   the size of the input collection  
   * @param  func   the foreach function Exp[A] => Exp[Unit]
   * @param  sync   a function from an index to a list of objects that should be locked, in a total ordering,
   *                prior to chunk execution, and unlocked after; (Exp[Int] => Exp[List[Any]])
   */  
  abstract class DeliteOpForeach[A:Manifest] extends DeliteOpLoop[Unit] { //DeliteOp[Unit] {    
    val in: Exp[DeliteCollection[A]]
    val size: Exp[Int]
    def func: Exp[A] => Exp[Unit]
    def sync: Exp[Int] => Exp[List[Any]] // TODO: need to extend runtime to do something with sync in multiloop
    
    final val i = fresh[Int]
    lazy val body: Def[Unit] = DeliteForeachElem(
      func = transform(reifyEffects(this.func(dc_apply(in,v)))),
      sync = transform(reifyEffects(this.sync(i)))
    )
  }
  
  abstract class DeliteOpIndexedLoop extends DeliteOpLoop[Unit] {
    def func: Exp[Int] => Exp[Unit]
    val size: Exp[Int]
    
    lazy val body: Def[Unit] = DeliteForeachElem(
      func = transform(reifyEffects(this.func(v))),
      sync = transform(unit(List())) 
    ) 
  }
  
  /**
   * Deprecated Delite ops  
   */
  
  /*
   * We temporarily need these two versions of 'foreach' until we get 'sync' working with the new version.
   */

  @deprecated("DeliteOpForeach2 should only be used if sync is required. It will be removed as soon as sync works with DeliteOpForeach", "") // TODO: swap names with DeliteOpForeach
  abstract class DeliteOpForeach2[A,C[X] <: DeliteCollection[X]]() extends DeliteOp[Unit] with DeliteOpMapLikeWhileLoopVariant {
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
  @deprecated("DeliteOpForeachBounded should only be used if sync is required. It will be removed as soon as sync works with DeliteOpForeach", "")
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
          sync = f(e.sync)
//          cond = f(e.cond)
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
//    case op: DeliteForeachElem[_] => syms(op.func) ++ syms(op.cond) ++ syms(op.sync)
    case op: DeliteForeachElem[_] => syms(op.func) ++ syms(op.sync)
    case op: DeliteReduceElem[_] => syms(op.func) ++ syms(op.cond) ++ syms(op.zero) ++ syms(op.rFunc)
    case foreach: DeliteOpForeach2[_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ++ syms(foreach.func) ++ syms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ++ syms(foreach.func) ++ syms(foreach.sync)
    case _ => super.syms(e)
  }
    
  override def readSyms(e: Any): List[Sym[Any]] = e match { //TR FIXME: check this is actually correct
    case s: DeliteOpSingleTask[_] if s.requireInputs => syms(s.block) ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case s: DeliteOpSingleTask[_] => syms(s.block)
    case op: DeliteCollectElem[_,_] => syms(op.func) ++ syms(op.cond) ++ syms(op.alloc)
//    case op: DeliteForeachElem[_] => syms(op.func) ++ syms(op.cond) ++ syms(op.sync)
    case op: DeliteForeachElem[_] => syms(op.func) ++ syms(op.sync)
    case op: DeliteReduceElem[_] => syms(op.func) ++ syms(op.cond) ++ syms(op.zero) ++ syms(op.rFunc)
    case foreach: DeliteOpForeach2[_,_] => readSyms(foreach.in) 
    case foreach: DeliteOpForeachBounded[_,_,_] => readSyms(foreach.in) 
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => effectSyms(s.block)
    case op: DeliteCollectElem[_,_] => effectSyms(op.func) ++ effectSyms(op.cond) ++ effectSyms(op.alloc)
    case op: DeliteReduceElem[_] => List(op.rV._1, op.rV._2) ++ effectSyms(op.func) ++ effectSyms(op.cond) ++ effectSyms(op.rFunc)
//    case op: DeliteForeachElem[_] => effectSyms(op.func) ++ effectSyms(op.cond) ++ effectSyms(op.sync)
    case op: DeliteForeachElem[_] => effectSyms(op.func) ++ effectSyms(op.sync)
    case foreach: DeliteOpForeach2[_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case _ => super.boundSyms(e)
  }

  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: DeliteOpSingleTask[_] if s.requireInputs => freqNormal(s.block) ++ super.symsFreq(e) // super call: add case class syms (iff flag is set)
    case s: DeliteOpSingleTask[_] => freqNormal(s.block)
    case op: DeliteCollectElem[_,_] => freqNormal(op.alloc) ++ freqHot(op.cond) ++ freqHot(op.func)
//    case op: DeliteForeachElem[_] => freqNormal(op.sync) ++ freqHot(op.cond) ++ freqHot(op.func)
    case op: DeliteForeachElem[_] => freqNormal(op.sync) ++ freqHot(op.func)
    case op: DeliteReduceElem[_] => freqHot(op.cond) ++ freqHot(op.func) ++ freqNormal(op.zero) ++ freqHot(op.rFunc)
    case foreach: DeliteOpForeach2[_,_] => freqNormal(foreach.in) ++ freqHot(foreach.func) ++ freqHot(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => freqNormal(foreach.in) ++ freqHot(foreach.func) ++ freqHot(foreach.sync)
    case _ => super.symsFreq(e)
  }

	/////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => syms(s.block)
    case op: DeliteCollectElem[_,_] => Nil // in particular not op.alloc !
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case foreach: DeliteOpForeach2[_,_] => Nil
    case foreach: DeliteOpForeachBounded[_,_,_] => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => Nil
    case op: DeliteCollectElem[_,_] => syms(op.func)
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case foreach: DeliteOpForeach2[_,_] => Nil
    case foreach: DeliteOpForeachBounded[_,_,_] => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => Nil
    case op: DeliteCollectElem[_,_] => Nil
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case foreach: DeliteOpForeach2[_,_] => Nil
    case foreach: DeliteOpForeachBounded[_,_,_] => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => Nil
    case op: DeliteCollectElem[_,_] => syms(op.alloc)
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case foreach: DeliteOpForeach2[_,_] => Nil
    case foreach: DeliteOpForeachBounded[_,_,_] => Nil
    case _ => super.copySyms(e)
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
    if (elem.cond.nonEmpty) {
      stream.print("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") ")
      if (deliteKernel)
        stream.println(prefixSym + quote(sym) + "_buf_append(" + quote(getBlockResult(elem.func)) + ")")
      else
        stream.println(prefixSym + quote(sym) + ".insert(" + prefixSym + quote(sym) + ".length, " + quote(getBlockResult(elem.func)) + ")")
    } else
      stream.println(prefixSym + quote(sym) + ".dcUpdate(" + quote(op.v) + ", " + quote(getBlockResult(elem.func)) + ")")
  }
  
  def emitForeachElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteForeachElem[_])(implicit stream: PrintWriter) {
    // if (elem.cond.nonEmpty)
    //   stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
    stream.println(quote(getBlockResult(elem.func)))
    // if (elem.cond.nonEmpty) {
    //   stream.println("}")                         
    // }
  }
  
  // -- begin emit reduce
  
  def emitFirstReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_])(implicit stream: PrintWriter) {
      if (elem.cond.nonEmpty) {
        // if we have conditionals, we have to delay the the initialization of the accumulator to the
        // first element where the condition is true
        stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
        stream.println(quote(getBlockResult(elem.func)))
        stream.println("} else {")
        stream.println(quote(elem.zero))
        stream.println("}")
      }
      else {
        stream.println(quote(getBlockResult(elem.func)))        
      }
  }

  def emitReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "")(implicit stream: PrintWriter) {
    if (elem.cond.nonEmpty){
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {"/*}*/)
      if (elem.stripFirst)
        emitInitializeOrReduction(op, sym, elem, prefixSym)
      else
        emitReduction(op, sym, elem, prefixSym)
      stream.println("}")
    }
    else {
      emitReduction(op, sym, elem, prefixSym)
    }
  }
  
  def emitInitializeOrReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "")(implicit stream: PrintWriter) {
    stream.println("// TODO: we could optimize this check away with more convoluted runtime support if necessary")          
    stream.println("if (" + prefixSym + quote(sym) + " == " + quote(elem.zero) + ") " + prefixSym + quote(sym) + " = {")
    
    // initialize
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
  
  // -- end emit reduce emit
  
  def emitMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]])(implicit stream: PrintWriter) {
    val elemFuncs = op.body flatMap { // don't emit dependencies twice!
      case elem: DeliteCollectElem[_,_] => elem.func :: elem.cond
//      case elem: DeliteForeachElem[_] => elem.cond // only emit func inside condition! TODO: how to avoid emitting deps twice? // elem.func :: elem.cond
      case elem: DeliteForeachElem[_] => List(elem.func) 
      case elem: DeliteReduceElem[_] => elem.func :: elem.cond
    }
    emitFatBlock(elemFuncs)
  }

/*  
  def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]])(implicit stream: PrintWriter) {
    // initialization             
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) =>
        stream.println("val " + quote(sym) + " = {"/*}*/)
        emitBlock(elem.alloc) // FIXME: how do we know it is the right size? conditions could have been added retrospectively!!
        if (elem.cond.nonEmpty)
          stream.println("//TODO: buffer size might be wrong (loop has conditions)")
        stream.println(quote(getBlockResult(elem.alloc)))
        stream.println(/*{*/"}")              
      case (sym, elem: DeliteForeachElem[_]) => 
        stream.println("var " + quotearg(sym) + " = ()") // must be type Unit
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println("var " + quotearg(sym) + " = " + quote(elem.zero))
    }
    stream.println("var " + quote(op.v) + " = 0")    
    stream.println("if (" + quote(op.size) + " > 0) {")
    /* strip first iteration */     
    emitMultiLoopFuncs(op, symList)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) =>
        emitCollectElem(op, sym, elem)
      case (sym, elem: DeliteForeachElem[_]) => 
        stream.println(quote(sym) + " = {")
        emitForeachElem(op, sym, elem)
        stream.println("}")
      case (sym, elem: DeliteReduceElem[_]) =>
        if (elem.stripFirst) {
          stream.println(quote(sym) + " = {")
          emitFirstReduceElem(op, sym, elem)
          stream.println("}")
        }
        else {
          emitReduceElem(op, sym, elem)
        }
    }
    stream.println("}")
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
*/  

  // FIXME: THIS IS THE OLD VERSION FOR TESTING PURPOSES
  def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]])(implicit stream: PrintWriter) {
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) =>
        stream.println("val " + quote(sym) + " = {"/*}*/)
        emitBlock(elem.alloc) // FIXME: how do we know it is the right size? conditions could have been added retrospectively!!
        if (elem.cond.nonEmpty)
          stream.println("//TODO: buffer size might be wrong (loop has conditions)")
        stream.println(quote(getBlockResult(elem.alloc)))
        stream.println(/*{*/"}")
      case (sym, elem: DeliteForeachElem[_]) => 
        stream.println("var " + quotearg(sym) + " = null //TODO: default value?")
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println("var " + quotearg(sym) + " = " + quote(elem.zero))
    }
    stream.println("var " + quote(op.v) + " = 0")
    stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(",")/*}*/)
    val elemFuncs = op.body flatMap { // don't emit dependencies twice!
      case elem: DeliteCollectElem[_,_] => elem.func :: elem.cond
      case elem: DeliteForeachElem[_] => elem.func :: Nil
      case elem: DeliteReduceElem[_] => elem.func :: elem.cond
    }
    emitFatBlock(elemFuncs)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) =>
        //emitBlock(elem.func)
        if (elem.cond.nonEmpty) {
          stream.print("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") ")
          stream.println(quote(sym) + ".insert(" + quote(sym) + ".length, " + quote(getBlockResult(elem.func)) + ")")
        } else
          stream.println(quote(sym) + ".dcUpdate(" + quote(op.v) + ", " + quote(getBlockResult(elem.func)) + ")")
      case (sym, elem: DeliteForeachElem[_]) =>
        stream.println(quote(getBlockResult(elem.func)))
      case (sym, elem: DeliteReduceElem[_]) =>
        //emitBlock(elem.func)
        if (elem.cond.nonEmpty)
          stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {"/*}*/)
        stream.println("val " + quote(elem.rV._1) + " = " + quote(sym))
        stream.println("val " + quote(elem.rV._2) + " = " + quote(getBlockResult(elem.func)))
        emitBlock(elem.rFunc)
        stream.println(quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))
        if (elem.cond.nonEmpty)
          stream.println(/*{*/"}")
    }
    stream.println(quote(op.v) + " += 1")
    stream.println(/*{*/"} // end fat loop " + symList.map(quote).mkString(","))
  }
  


  def emitAbstractFatLoopKernelExtra(op: AbstractFatLoop, symList: List[Sym[Any]])(implicit stream: PrintWriter): Unit = {
    val kernelName = symList.map(quote).mkString("")
    stream.println("final class activation_" + kernelName + " {"/*}*/)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_]) => 
        stream.println("var " + quote(sym) + ": " + remap(sym.Type) + " = _")
        if (elem.cond.nonEmpty) {
          stream.println("var " + quote(sym) + "_buf: Array[" + remap(getBlockResult(elem.func).Type) + "] = _")
          stream.println("var " + quote(sym) + "_size = 0")
          stream.println("def " + quote(sym) + "_buf_init: Unit = {"/*}*/)
          stream.println(quote(sym) + "_buf = new Array(128)")
          stream.println(/*{*/"}")
          stream.println("def " + quote(sym) + "_buf_append(x: " + remap(getBlockResult(elem.func).Type) + "): Unit = {"/*}*/)
          stream.println("if (" + quote(sym) + "_size >= " + quote(sym) + "_buf.length) {"/*}*/)
          stream.println("val old = " + quote(sym) + "_buf")
          stream.println(quote(sym) + "_buf = new Array(2*old.length)")
          stream.println("System.arraycopy(old, 0, " + quote(sym) + "_buf, 0, old.length)")
          stream.println(/*{*/"}")
          stream.println(quote(sym) + "_buf(" + quote(sym) + "_size) = x")
          stream.println(quote(sym) + "_size += 1")
          stream.println(/*{*/"}")
          stream.println("def " + quote(sym) + "_buf_appendAll(xs: Array[" + remap(getBlockResult(elem.func).Type) + "], len: Int): Unit = {"/*}*/)
          stream.println("if (" + quote(sym) + "_size + len >= " + quote(sym) + "_buf.length) {"/*}*/)
          stream.println("val old = " + quote(sym) + "_buf")
          stream.println(quote(sym) + "_buf = new Array(2*(old.length+len))")
          stream.println("System.arraycopy(old, 0, " + quote(sym) + "_buf, 0, old.length)")
          stream.println(/*{*/"}")
          stream.println("System.arraycopy(" + quote(sym) + "_buf, " + quote(sym) + "_size, xs, 0, len)")
          stream.println(quote(sym) + "_size += len")
          stream.println(/*{*/"}")
        }
      case (sym, elem: DeliteForeachElem[_]) =>
        stream.println("var " + quote(sym) + ": " + remap(sym.Type) + " = _")
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println("var " + quote(sym) + ": " + remap(sym.Type) + " = _")
    }
    stream.println(/*{*/"}")
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
        stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.alloc))) //FIXME: do in post-process
      case (sym, elem: DeliteForeachElem[_]) => // initialized in init below
      case (sym, elem: DeliteReduceElem[_]) => // initialized in init below
/*
      case (sym, elem: DeliteForeachElem[_]) => stream.println("__act. " + quote(sym) + " = ()") // must be type Unit, initialized in init below
      case (sym, elem: DeliteReduceElem[_]) => stream.println("__act. " + quote(sym) + " = " + quote(elem.zero))
*/
    }
    stream.println("__act")
    stream.println(/*{*/"}")
    stream.println("def init(__act: " + actType + ", " + quotearg(op.v) + "): " + actType + " = {"/*}*/)
    if (op.body exists (loopBodyNeedsCombine _)) {
      emitMultiLoopFuncs(op, symList)                               
      stream.println("val __act2 = new " + actType) //TODO: maybe not necessary to copy act
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_]) =>
          if (elem.cond.nonEmpty) {
            stream.println("__act2." + quote(sym) + "_buf_init")
          }
          stream.println("__act2." + quote(sym) + " = " + "__act." + quote(sym))
/*
          if (elem.cond.nonEmpty) {
            stream.println("//TODO: buffer size might be wrong (loop has conditions)") // separate buffer for each process
            stream.println("__act2." + quote(sym) + " = " + "__act." + quote(sym) + ".cloneL")
          } else { 
            stream.println("__act2." + quote(sym) + " = " + "__act." + quote(sym))
          }
*/
          emitCollectElem(op, sym, elem, "__act2.")
        case (sym, elem: DeliteForeachElem[_]) => 
          stream.println("__act2." + quote(sym) + " = {")
          emitForeachElem(op, sym, elem)
          stream.println("}")               
        case (sym, elem: DeliteReduceElem[_]) =>
          if (elem.stripFirst) {
            stream.println("__act2." + quote(sym) + " = {")         
            emitFirstReduceElem(op, sym, elem)
            stream.println("}")
          } 
          else { 
            if (sym.Type <:< manifest[AnyVal]) {
              stream.println("__act2." + quote(sym) + " = " + "__act." + quote(sym)) 
            } else {
              stream.println("__act2." + quote(sym) + " = " + "__act." + quote(sym) + ".cloneL") // separate zero buffer           
            }
            emitReduceElem(op, sym, elem, "__act2.")
          }  
      }
      stream.println("__act2")
    } else {
      stream.println("process(__act, " + quote(op.v) + ")")
      stream.println("__act")
    }
    stream.println(/*{*/"}")
    stream.println("def process(__act: " + actType + ", " + quotearg(op.v) + "): Unit = {"/*}*/)
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
          //stream.println("__act." + quote(sym) + ".insertAll(__act." +quote(sym) + ".length, rhs." + quote(sym) + ")")
          stream.println("__act." + quote(sym) + "_buf_appendAll(rhs." +quote(sym) + "_buf, rhs." + quote(sym) + "_size)")
          //stream.println("__act." + quote(sym) + "_size += rhs." +quote(sym) + "_size")
          stream.println("//__act." + quote(sym) + ".unsafeSetData(__act." +quote(sym) + ".length, rhs." + quote(sym) + ")")
        }
      case (sym, elem: DeliteForeachElem[_]) => // nothing needed
      case (sym, elem: DeliteReduceElem[_]) =>
        // if either value is zero, return the other instead of combining
        stream.println("val " + quote(elem.rV._1) + " = " + "__act." + quote(sym))
        stream.println("val " + quote(elem.rV._2) + " = " + "rhs." + quote(sym))
        stream.println("if (" + quote(elem.rV._1) + " == " + quote(elem.zero) + ") {")
        stream.println("__act." + quote(sym) + " = " + quote(elem.rV._2))
        stream.println("}")
        stream.println("else if (" + quote(elem.rV._2) + " != " + quote(elem.zero) + ") {")
        emitBlock(elem.rFunc)
        stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))
        stream.println("}")
    }
    stream.println(/*{*/"}")
    stream.println("def postprocess(__act: " + actType + ", " + quotearg(op.v) + "): Unit = {"/*}*/)
    stream.println(/*{*/"}")


    stream.println(/*{*/"}")
    deliteKernel = true
  }
  

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case op: AbstractFatLoop =>
      if (!deliteKernel) emitInlineAbstractFatLoop(op, symList)
      else emitKernelAbstractFatLoop(op, symList)
    case _ => super.emitFatNode(symList, rhs)
  }

  override def emitFatNodeKernelExtra(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter): Unit = rhs match {
    case op: AbstractFatLoop =>
      stream.println("//activation record for fat loop")
      emitAbstractFatLoopKernelExtra(op, symList)
    case ThinDef(op: AbstractLoop[_]) => 
      stream.println("//activation record for thin loop")
      emitAbstractFatLoopKernelExtra(SimpleFatLoop(op.size, op.v, List(op.body)), symList)
    case _ => 
      super.emitFatNodeKernelExtra(symList, rhs)
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
    case foreach:DeliteOpForeach2[_,_] => {
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
    case foreach:DeliteOpForeach2[_,_] => {
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
