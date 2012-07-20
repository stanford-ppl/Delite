package ppl.delite.framework.ops

import java.io.{FileWriter, File, PrintWriter}

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericCodegen, GenericFatCodegen, GenerationFailedException}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.Config
import ppl.delite.framework.extern.lib._

//trait DeliteOpsExp extends BaseFatExp with EffectExp with VariablesExp with LoopsFatExp {
trait DeliteOpsExp extends BaseFatExp with EffectExp with VariablesExp with LoopsFatExp with FunctionBlocksExp with IfThenElseFatExp
    with VariantsOpsExp with DeliteCollectionOpsExp
    with OrderingOpsExp with CastingOpsExp with ImplicitOpsExp with WhileExp with ArrayOpsExp with StaticDataExp {  
  

/*
  //may try this some time to wrap functions that are passed as case class args...
  case class FF[A,B](val x: Rep[A], val y: Rep[B])(val f: Rep[A]=>Rep[B])  
  type ===>[A,B] = FF[A,B]
  implicit def deliteFunc[A:Manifest,B:Manifest](f: Rep[A]=>Rep[B]): A ===> B = { val x = fresh[A]; val y = reifyEffects(f(x)); FF(x, y)(f) }
*/
  
  /* Markers to tell Delite op code generation what kind of strategy to use.
   * 
   * Where should these live?
   */
  trait DeliteParallelStrategy
  object ParFlat extends DeliteParallelStrategy
  object ParBuffer extends DeliteParallelStrategy
  
  /*
   * Useful for mirroring
   */
  abstract class DefWithManifest[A:Manifest,R:Manifest] extends Def[R] {
    val mA = manifest[A]
    val mR = manifest[R]
  }  
  
  /**
   * The base type of the DeliteOp hierarchy.
   */
  /*sealed*/ trait DeliteOp[A] extends Def[A] {
    type OpType <: DeliteOp[A]
    def original: Option[(Transformer,Def[_])] = None // we should have type OpType, really but then it needs to be specified in mirror (why?)
    def copyOrElse[B](f: OpType => B)(e: => B): B = original.map(p=>f(p._2.asInstanceOf[OpType])).getOrElse(e)
    def copyTransformedOrElse[B](f: OpType => Exp[B])(e: => Exp[B]): Exp[B] = original.map(p=>p._1(f(p._2.asInstanceOf[OpType]))).getOrElse(e)
    def copyTransformedBlockOrElse[B:Manifest](f: OpType => Block[B])(e: => Block[B]): Block[B] = original.map(p=>p._1(f(p._2.asInstanceOf[OpType]))).getOrElse(e)

/*
    consider z1 = VectorPlus(a,b), which could be something like z1 = Block(z2); z2 = loop(a.size) { i => a(i) + b(i) }
    we might want to mirror z1 because we have changed z2.
    but since a and b are the same, if we use case class equality: 
    1) we end up with the same object z1
    2) we created a third object that creates a new loop but is immediately discarded
*/
    // GROSS HACK ALERT
    // This is a hack to (among other things) enable objects with non-structural equality 
    // (e.g. functions) as case class parameters. Otherwise cse won't work and we mirror either not 
    // enough or the world...
    // However, we don't want to infringe on cse in the normal IR construction case. That is, 2 calls
    // to X.t (creating to MatrixTrans(x) instances) should result in the second MatrixTrans(x) node 
    // being cse'd, even though it has a new impl symbol. Thus we change the meaning of equality 
    // based on whether we're mirroring or not.
    override def equals(x: Any): Boolean = (this,x) match {
      case (a: Product,b: Product) => 
        if (a.productPrefix == b.productPrefix) {
          val r1 = a.productIterator.toList == b.productIterator.toList
          val r2 = syms(a) == syms(b)
          lazy val inMirror = Thread.currentThread.getStackTrace.exists(_.getMethodName == "mirror")
          //if (r1 != r2)
            //printdbg("?== "+this+","+x + " is "+r1+"/"+r2+" syms "+syms(a)+"/"+syms(b))
          r1 && (!inMirror || r2)
        } else false
      case _ => super.equals(x)
    }
  }



  //sealed trait DeliteFatOp extends FatDef

  /**
   * A sequential task - will execute block in a single thread and respect any free variable dependencies inside it.
   *
   * @param  block   the task to execute; must be reified if it contains effectful operations!
   */
  class DeliteOpSingleTask[A:Manifest](block0: => Block[A], val requireInputs: Boolean = false) extends DeliteOp[A] {
    type OpType <: DeliteOpSingleTask[A]
    final lazy val block: Block[A] = copyTransformedBlockOrElse(_.block)(block0)
  }
  
  class DeliteOpSingleWithManifest[A:Manifest,R:Manifest](block0: => Block[R], requireInputs: Boolean = false) extends DeliteOpSingleTask[R](block0,requireInputs) {
    val mA = manifest[A]
    val mR = manifest[R]
  }
  
  class DeliteOpSingleWithManifest2[A:Manifest,B:Manifest,R:Manifest](block0: => Block[R], requireInputs: Boolean = false) extends DeliteOpSingleWithManifest[A,R](block0,requireInputs) {
    val mB = manifest[B]
  }
  
  /**
   * A method call to an external library.
   */
  abstract class DeliteOpExternal[A:Manifest] extends DeliteOp[A] {
    type OpType <: DeliteOpExternal[A]
    def alloc: Exp[A]
    val funcName: String
    final lazy val allocVal: Block[A] = copyTransformedBlockOrElse(_.allocVal)(reifyEffects(alloc))
  }

  /**
   * The base class for most data parallel Delite ops.
   */
  abstract class DeliteOpLoop[A:Manifest] extends AbstractLoop[A] with DeliteOp[A] {
    type OpType <: DeliteOpLoop[A]
    def copyBodyOrElse(e: => Def[A]): Def[A] = original.map(p=>mirrorLoopBody(p._2.asInstanceOf[OpType].body,p._1)).getOrElse(e)
    final lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(fresh[Int]).asInstanceOf[Sym[Int]]
  }

  //case class DeliteOpFatLoop(val size: Exp[Int], val v: Sym[Int], val body: List[Def[Any]]) extends AbstractFatLoop with DeliteFatOp
    
  // for use in loops:

  case class DeliteForeachElem[A:Manifest](
    func: Block[A],
    sync: Block[List[Any]] // FIXME: don't want to create lists at runtime...
    // TODO: this is sort of broken right now, re-enable when we figure out how to make this work without emitting dependencies twice
    //cond: List[Exp[Boolean]] = Nil
  ) extends Def[Unit] {
    val mA = manifest[A]
  }
  
  // used only for ParBuffer operations
  // dc_append, dc_set_logical_size, dc_alloc, and dc_copy only need to be
  // overridden if the DeliteParallelStrategy is ParBuffer
  case class DeliteBufferElem[A:Manifest, I <: DeliteCollection[A]:Manifest, CA <: DeliteCollection[A]:Manifest](
    // -- bound vars
    aV: Sym[I],
    iV: Sym[Int],
    iV2: Sym[Int],    
    
    // collection functions
    append: Block[Boolean],
    setSize: Block[Unit],
    allocRaw: Block[I],
    copyRaw: Block[Unit]    
  ) {
    val mA = manifest[A]
    val mI = manifest[I]
    val mCA = manifest[CA]
  }
  
  case class DeliteCollectElem[A:Manifest, I <: DeliteCollection[A]:Manifest, CA <: DeliteCollection[A]:Manifest]( 
    // -- bound vars
    eV: Sym[A],
    sV: Sym[Int],            
    allocVal: Sym[I],
    // -- collect functions
    allocN: Block[I],
    func: Block[A],
    update: Block[Unit],    
    finalizer: Block[CA],
    // -- misc
    cond: List[Block[Boolean]] = Nil,
    par: DeliteParallelStrategy,
    buf: DeliteBufferElem[A,I,CA]
  ) extends Def[CA] {
    val mA = manifest[A]
    val mI = manifest[I]
    val mCA = manifest[CA]
  }

  
  case class DeliteReduceElem[A:Manifest](
    func: Block[A],
    cond: List[Block[Boolean]] = Nil,
    zero: Block[A],
    rV: (Sym[A], Sym[A]),
    rFunc: Block[A],
    stripFirst: Boolean
  ) extends Def[A] {
    val mA = manifest[A]
  }

  case class DeliteReduceTupleElem[A:Manifest,B:Manifest](
    func: (Block[A],Block[B]),
    cond: List[Block[Boolean]] = Nil,
    zero: (Block[A],Block[B]),
    rVPar: ((Sym[A], Sym[B]),(Sym[A], Sym[B])),
    rVSeq: ((Sym[A], Sym[B]),(Sym[A], Sym[B])),
    rFuncPar: (Block[A],Block[B]),
    rFuncSeq: (Block[A],Block[B]),
    stripFirst: Boolean
  ) extends Def[A] {
    val mA = manifest[A]
    val mB = manifest[B]
  }



  def loopBodyNeedsStripFirst[A](e: Def[A]) = e match {
    case e:DeliteReduceElem[_] => e.stripFirst
    case e:DeliteReduceTupleElem[_,_] => e.stripFirst
    case _ => false
  }
  
  def loopBodyNeedsCombine[A](e: Def[A]) = e match {
    case e:DeliteReduceElem[_] => true
    case e:DeliteReduceTupleElem[_,_] => true
    case e:DeliteCollectElem[_,_,_] => e.par == ParBuffer //e.cond.nonEmpty
    case _ => false
  }  

  def loopBodyNeedsPostProcess[A](e: Def[A]) = e match {
    case e:DeliteCollectElem[_,_,_] => e.par == ParBuffer //e.cond.nonEmpty
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
    type OpType <: DeliteOpCondition[A]
    val cond: Exp[Boolean]
    val thenp: Block[A]
    val elsep: Block[A]
  }

  /**
   * An while loop - will emit an while loop DEG node as well as a kernel for the body
   *
   * @param  cond  condition expression, will be emitted as a kernel
   * @param  body   the body of the loop
   */
  trait DeliteOpWhileLoop extends DeliteOp[Unit] {
    type OpType <: DeliteOpWhileLoop
    val cond: Block[Boolean]
    val body: Block[Unit]
  }

  /**
   *  Delite parallel ops - represents common parallel execution patterns, most of which
   *  are represented by an underlying 'loop' abstraction. All Delite parallel ops must
   *  not mutate their inputs or any global state, and produce a single output, with the
   *  exception of DeliteOpForeach. DeliteOpForeach can only mutate shared state protected 
   *  by the 'sync' method; all other side-effects and writes must be disjoint (i.e., not
   *  have inter-iteration dependencies). In all cases, there is no ordering guarantee.
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
    * DeliteOpMapLike is the base type for all Delite ops with collect elem bodies.
    * 
    * It now supports allocating a result of type I (which will be modified during construction) and
    * returning a result of type CA by invoking the 'finalizer' method.
    */   
   abstract class DeliteOpMapLike[A:Manifest, I <: DeliteCollection[A]:Manifest, CA <: DeliteCollection[A]:Manifest] extends DeliteOpLoop[CA] {
     type OpType <: DeliteOpMapLike[A,I,CA]

     // ideally we would leave these abstract: how can we specify that at least one of the two should be supplied,
     // while keeping collect a unified abstraction?
     
     // this version should be overridden if par == ParFlat and the other version is not suitable (e.g. DenseMatrix)
     def alloc: Exp[I] = throw new IllegalArgumentException("alloc in DeliteOpMapLike should have been overridden")
     // this version should be overridden if par == ParBuffer
     def alloc(i: Exp[Int]): Exp[I] = alloc 
     def finalizer(x: Exp[I]): Exp[CA] 
     
     // bound vars
     final lazy val eV: Sym[A] = copyTransformedOrElse(_.eV)(fresh[A]).asInstanceOf[Sym[A]]
     final lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]     
     final lazy val allocVal: Sym[I] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[I])).asInstanceOf[Sym[I]]
     final lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
     final lazy val iV2: Sym[Int] = copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
     final lazy val aV: Sym[I] = copyTransformedOrElse(_.aV)(fresh[I]).asInstanceOf[Sym[I]]     
   }
   
   
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
    extends DeliteOpMapI[A,B,CB,CB] {
    type OpType <: DeliteOpMap[A,B,CB]
    
    def finalizer(x: Exp[CB]) = x
  }
  
  abstract class DeliteOpMapI[A:Manifest,B:Manifest,I <: DeliteCollection[B]:Manifest,CB <: DeliteCollection[B]:Manifest]
    extends DeliteOpMapLike[B,I,CB] {
    type OpType <: DeliteOpMapI[A,B,I,CB]
    
    // supplied by subclass
    val in: Exp[DeliteCollection[A]]
    //val size: Exp[Int] // could be dc_size(in), but we want type-specific pattern matching to work
    def func: Exp[A] => Exp[B]
    
    // bound var for map function, may be required by transformers
    lazy val fin: Exp[A] = copyTransformedOrElse(_.fin)(dc_apply(in,v))    
    
    // loop
    lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[B,I,CB](  
      eV = this.eV,     
      sV = this.sV,      
      allocVal = this.allocVal,      
      allocN = reifyEffects(this.alloc(sV)),
      func = reifyEffects(this.func(fin)),
      update = reifyEffects(dc_update(allocVal,v,eV)),
      finalizer = reifyEffects(this.finalizer(allocVal)),
      par = dc_parallelization(allocVal, false),
      buf = DeliteBufferElem(
        iV = this.iV,
        iV2 = this.iV2,
        aV = this.aV,
        append = reifyEffects(dc_append(allocVal,v,eV)),
        setSize = reifyEffects(dc_set_logical_size(allocVal,sV)),
        allocRaw = reifyEffects(dc_alloc[B,I](allocVal,sV)),
        copyRaw = reifyEffects(dc_copy(aV,iV,allocVal,iV2,sV))        
      )
    ))
    
    val dmA = manifest[A]
    val dmB = manifest[B]
    val dmI = manifest[I]
    val dmCB = manifest[CB]
  }
    
  /**
   *  Currently conditionally appends values to buffers, which are concatenated in the combine stage.
   *  Note that it is also implicitly a Map-Filter (it accepts a mapping function). Should this be renamed? 
   */
  abstract class DeliteOpFilter[A:Manifest,
                                B:Manifest, CB <: DeliteCollection[B]:Manifest]
    extends DeliteOpFilterI[A,B,CB,CB] {
    type OpType <: DeliteOpFilter[A,B,CB]
    
    def finalizer(x: Exp[CB]) = x
  }  
  
  abstract class DeliteOpFilterI[A:Manifest,
                                B:Manifest, I <: DeliteCollection[B]:Manifest, CB <: DeliteCollection[B]:Manifest]
    extends DeliteOpMapLike[B,I,CB] {
    type OpType <: DeliteOpFilterI[A,B,I,CB]

    // supplied by subclass
    val in: Exp[DeliteCollection[A]]
    //val size: Exp[Int] // could be dc_size(in), but we want type-specific pattern matching to work
    def func: Exp[A] => Exp[B]
    def cond: Exp[A] => Exp[Boolean] // does this need to be more general (i.e. a List?)

    // loop
    lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[B,I,CB](
      eV = this.eV,   
      sV = this.sV,   
      allocVal = this.allocVal,      
      allocN = reifyEffects(this.alloc(sV)),
      func = reifyEffects(this.func(dc_apply(in,v))),
      update = reifyEffects(dc_update(allocVal,v,eV)),
      finalizer = reifyEffects(this.finalizer(allocVal)),
      cond = reifyEffects(this.cond(dc_apply(in,v)))::Nil,
      par = dc_parallelization(allocVal, true),
      buf = DeliteBufferElem(
        iV = this.iV,
        iV2 = this.iV2,
        aV = this.aV,
        append = reifyEffects(dc_append(allocVal,v,eV)),
        setSize = reifyEffects(dc_set_logical_size(allocVal,sV)),
        allocRaw = reifyEffects(dc_alloc[B,I](allocVal,sV)),
        copyRaw = reifyEffects(dc_copy(aV,iV,allocVal,iV2,sV))        
      )      
    ))    
  }  
  
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
    extends DeliteOpZipWithI[A,B,R,CR,CR] {
    type OpType <: DeliteOpZipWith[A,B,R,CR]
    
    def finalizer(x: Exp[CR]) = x
  }
  
  abstract class DeliteOpZipWithI[A:Manifest,
                                 B:Manifest,
                                 R:Manifest, 
                                 I <: DeliteCollection[R]:Manifest, CR <: DeliteCollection[R]:Manifest]
    extends DeliteOpMapLike[R,I,CR] {
    type OpType <: DeliteOpZipWithI[A,B,R,I,CR]
      
    // supplied by subclass   
    val inA: Exp[DeliteCollection[A]]
    val inB: Exp[DeliteCollection[B]]
    def func: (Exp[A], Exp[B]) => Exp[R]
    
    // bound var for map function, may be required by transformers
    lazy val fin: (Exp[A],Exp[B]) = (copyTransformedOrElse(_.fin._1)(dc_apply(inA,v)),copyTransformedOrElse(_.fin._2)(dc_apply(inB,v)))
    
    // loop
    lazy val body: Def[CR] = copyBodyOrElse(DeliteCollectElem[R,I,CR](
      eV = this.eV,    
      sV = this.sV,      
      allocVal = this.allocVal,
      allocN = reifyEffects(this.alloc(sV)),
      func = reifyEffects(this.func(fin._1,fin._2)),
      update = reifyEffects(dc_update(allocVal,v,eV)),
      finalizer = reifyEffects(this.finalizer(allocVal)),
      par = dc_parallelization(allocVal, false),
      buf = DeliteBufferElem(
        iV = this.iV,
        iV2 = this.iV2,
        aV = this.aV,
        append = reifyEffects(dc_append(allocVal,v,eV)),
        setSize = reifyEffects(dc_set_logical_size(allocVal,sV)),
        allocRaw = reifyEffects(dc_alloc[R,I](allocVal,sV)),
        copyRaw = reifyEffects(dc_copy(aV,iV,allocVal,iV2,sV))        
      )      
    ))
    
    val dmA = manifest[A]
    val dmB = manifest[B]
    val dmR = manifest[R]
    val dmI = manifest[I]
    val dmCR = manifest[CR]        
  }
  
    
  abstract class DeliteOpReduceLike[A:Manifest] extends DeliteOpLoop[A] {
    type OpType <: DeliteOpReduceLike[A]
    final lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)((reflectMutableSym(fresh[A]), fresh[A])) // TODO: transform vars??
    val mutable: Boolean = false
    // TODO: should reflectMutableSym only be called if we're actually mutating the accumulator?
  }
  
  /**
   * Parallel reduction of a DeliteCollection[A]. Reducing function must be associative.
   *
   * @param  in    the input collection
   * @param  size  the size of the input collection 
   * @param  zero  the "empty" value - must have value equality 
   * @param  func  the reduction function; ([Exp[A],Exp[A]) => Exp[A]. Must be associative.
   */
  abstract class DeliteOpReduce[A:Manifest] extends DeliteOpReduceLike[A] {
    type OpType <: DeliteOpReduce[A]
      
    // supplied by subclass   
    val in: Exp[DeliteCollection[A]]
    def zero: Exp[A] 
    def func: (Exp[A], Exp[A]) => Exp[A]
    
    // loop    
    lazy val body: Def[A] = copyBodyOrElse(DeliteReduceElem[A](
      func = reifyEffects(dc_apply(in,v)),
      zero = reifyEffects(this.zero),
      rV = this.rV,
      rFunc = reifyEffects(this.func(rV._1, rV._2)),
      stripFirst = !isPrimitiveType(manifest[A]) && !this.mutable
    ))
    
    val dmA = manifest[A]
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
    extends DeliteOpReduceLike[R] {
    type OpType <: DeliteOpMapReduce[A,R]
    
    // supplied by subclass   
    val in: Exp[DeliteCollection[A]]
    def zero: Exp[R] 
    def map: Exp[A] => Exp[R]
    def reduce: (Exp[R], Exp[R]) => Exp[R]
    
    // loop
    lazy val body: Def[R] = copyBodyOrElse(DeliteReduceElem[R](
      func = reifyEffects(map(dc_apply(in,v))),
      zero = reifyEffects(this.zero),
      rV = this.rV,
      rFunc = reifyEffects(reduce(rV._1, rV._2)),
      stripFirst = !isPrimitiveType(manifest[R]) && !this.mutable
    ))
  }
  
  // should this be folded into DeliteOpMapReduce (or into DeliteOpFilter)?
  abstract class DeliteOpFilterReduce[A:Manifest,R:Manifest]
    extends DeliteOpReduceLike[R] {
    type OpType <: DeliteOpFilterReduce[A,R]
    
    // supplied by subclass   
    val in: Exp[DeliteCollection[A]]
    def zero: Exp[R] 
    def func: Exp[A] => Exp[R]        
    def reduce: (Exp[R], Exp[R]) => Exp[R]
    def cond: Exp[A] => Exp[Boolean] // does this need to be more general (i.e. a List?)
    
    // loop
    lazy val body: Def[R] = copyBodyOrElse(DeliteReduceElem[R](
      func = reifyEffects(this.func(dc_apply(in,v))),
      cond = reifyEffects(this.cond(dc_apply(in,v)))::Nil,
      zero = reifyEffects(this.zero),
      rV = this.rV,
      rFunc = reifyEffects(reduce(rV._1, rV._2)),
      stripFirst = !isPrimitiveType(manifest[R]) && !this.mutable
    ))
  }  

  // for sumIf ...
/*
  abstract class DeliteOpFilterReduceFold[A:Manifest,R:Manifest]
    extends DeliteOpLoop[R] {
    type OpType <: DeliteOpFilterReduceFold[A,R]

    // supplied by subclass
    val in: Exp[DeliteCollection[A]]
    val zero: Exp[R]
    def func: Exp[A] => Exp[R]
    def reduce: (Exp[R], Exp[R]) => Exp[R]
    def cond: Exp[A] => Exp[Boolean] // does this need to be more general (i.e. a List?)

    def accum: Exp[A] => Exp[R] //= (elem) => zero // given an element, return appropriate accum (i.e. vector of size)
    def step: (Exp[R], Exp[A]) => Exp[Unit] = (acc,elem) => ifThenElse(dc_apply(in,v))reduce(acc, func(elem))

      val z = dc_apply(in,v)
      if (cond(z)) {
        if (fresh)
          reduce(accum(z), func(z))
        else
          reduce(acc, func(z))
      }



    def step: (Exp[R], Exp[A]) => Exp[Unit] = (acc,elem) => ifThenElse(dc_apply(in,v))reduce(acc, func(elem))

    final lazy protected val aV: Sym[A] = copyOrElse(_.aV)(fresh[A]) // TODO: transform vars??
    final lazy protected val fV: (Sym[A],Sym[A]) = copyOrElse(_.fV)((reflectMutableSym(fresh[A]), fresh[A])) // TODO: transform vars??
    final lazy protected val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)((reflectMutableSym(fresh[A]), fresh[A])) // TODO: transform vars??

    // loop
    lazy val body: Def[R] = copyBodyOrElse(DeliteReduceStepElem[R](
      func = reifyEffects(this.func(dc_apply(in,v))),
      cond = reifyEffects(this.cond(dc_apply(in,v)))::Nil,
      zero = this.zero,
      aV =
      sFunc = reifyEffects(fold(fV._1, fV._2)),
      rV = this.rV,
      rFunc = reifyEffects(reduce(rV._1, rV._2)),
      stripFirst = false
    ))
  }
*/


  // reduce tuple in parallel, return first component
  abstract class DeliteOpFilterReduceFold[R:Manifest]
    extends DeliteOpLoop[R] {
    type OpType <: DeliteOpFilterReduceFold[R]

    // supplied by subclass
    val in: Exp[DeliteCollection[Int]]
    val zero: (Block[R], Block[Int])
    def func: Exp[Int] => (Block[R],Block[Int])
    def reducePar: ((Exp[R],Exp[Int]), (Exp[R],Exp[Int])) => (Block[R],Block[Int])
    def reduceSeq: ((Exp[R],Exp[Int]), (Exp[R],Exp[Int])) => (Block[R],Block[Int]) // = reduce
  
    val mutable: Boolean = false
    final lazy protected val rVPar: ((Sym[R],Sym[Int]),(Sym[R],Sym[Int])) = copyOrElse(_.rVPar)(((reflectMutableSym(fresh[R]),reflectMutableSym(fresh[Int])), (fresh[R],fresh[Int])))
    final lazy protected val rVSeq: ((Sym[R],Sym[Int]),(Sym[R],Sym[Int])) = copyOrElse(_.rVSeq)(((reflectMutableSym(fresh[R]),reflectMutableSym(fresh[Int])), (fresh[R],fresh[Int])))
    
    // loop
    lazy val body: Def[R] = copyBodyOrElse(DeliteReduceTupleElem[R,Int](
      func = /*reifyEffects*/(func(dc_apply(in,v))), //FIXME: tupled reify
      zero = this.zero,
      rVPar = this.rVPar,
      rVSeq = this.rVSeq,
      rFuncPar = /*reifyEffects*/(reducePar(rVPar._1, rVPar._2)),  //FIXME: tupled reify
      rFuncSeq = /*reifyEffects*/(reduceSeq(rVSeq._1, rVSeq._2)),  //FIXME: tupled reify
      stripFirst = false //(!isPrimitiveType(manifest[R]) || !isPrimitiveType(manifest[R])) && !this.mutable
    ))
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
    extends DeliteOpReduceLike[R] {
    type OpType <: DeliteOpZipWithReduce[A,B,R]
      
    // supplied by subclass   
    val inA: Exp[DeliteCollection[A]]
    val inB: Exp[DeliteCollection[B]]
    def zero: Exp[R]
    def zip: (Exp[A], Exp[B]) => Exp[R]
    def reduce: (Exp[R], Exp[R]) => Exp[R]
    
    // loop
    lazy val body: Def[R] = copyBodyOrElse(DeliteReduceElem[R](
      func = reifyEffects(zip(dc_apply(inA,v), dc_apply(inB,v))),
      zero = reifyEffects(this.zero),
      rV = this.rV,
      rFunc = reifyEffects(reduce(rV._1, rV._2)),
      stripFirst = !isPrimitiveType(manifest[R]) && !this.mutable
    ))
  }
  
  // reduce tuple in parallel, return first component
  abstract class DeliteOpZipWithReduceTuple[A:Manifest,B:Manifest,R:Manifest,Q:Manifest]
    extends DeliteOpLoop[R] {
    type OpType <: DeliteOpZipWithReduceTuple[A,B,R,Q]

    // supplied by subclass
    val inA: Exp[DeliteCollection[A]]
    val inB: Exp[DeliteCollection[B]]
    val zero: (Block[R], Block[Q])
    def zip: (Exp[A], Exp[B]) => (Block[R],Block[Q])
    def reduce: ((Exp[R],Exp[Q]), (Exp[R],Exp[Q])) => (Block[R],Block[Q])
    
    val mutable: Boolean = false
    final lazy protected val rV: ((Sym[R],Sym[Q]),(Sym[R],Sym[Q])) = copyOrElse(_.rV)(((reflectMutableSym(fresh[R]),reflectMutableSym(fresh[Q])), (fresh[R],fresh[Q]))) // TODO: transform vars??
    // loop
    lazy val body: Def[R] = copyBodyOrElse(DeliteReduceTupleElem[R,Q](
      func = /*reifyEffects*/(zip(dc_apply(inA,v), dc_apply(inB,v))), //FIXME: tupled reify
      zero = this.zero,
      rVPar = this.rV,
      rVSeq = this.rV,
      rFuncPar = /*reifyEffects*/(reduce(rV._1, rV._2)),  //FIXME: tupled reify
      rFuncSeq = /*reifyEffects*/(reduce(rV._1, rV._2)),  //FIXME: tupled reify
      stripFirst = (!isPrimitiveType(manifest[R]) || !isPrimitiveType(manifest[R])) && !this.mutable
    ))
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
    type OpType <: DeliteOpForeach[A]
    val in: Exp[DeliteCollection[A]]
    val size: Exp[Int]
    def func: Exp[A] => Exp[Unit]
    def sync: Exp[Int] => Exp[List[Any]] // TODO: need to extend runtime to do something with sync in multiloop
    
    final lazy val i: Sym[Int] = copyOrElse(_.i)(fresh[Int])
    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = reifyEffects(this.func(dc_apply(in,v))),
      sync = reifyEffects(this.sync(i))
    ))
  }

  abstract class DeliteOpIndexedLoop extends DeliteOpLoop[Unit] {
    type OpType <: DeliteOpIndexedLoop
    val size: Exp[Int]
    def func: Exp[Int] => Exp[Unit]
    
    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = reifyEffects(this.func(v)),
      sync = reifyEffects(unit(List()))
    ))
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
    val func: Block[Unit]
    val i: Sym[Int]
    val sync: Block[List[Any]]

    lazy val alloc = Const()
    /*lazy val variant = {
      implicit val mA: Manifest[A] = v.tp.asInstanceOf[Manifest[A]]
      reifyEffects {
        var index = var_new(unit(0))
        var vs = var_new(unit(null).AsInstanceOf[A])
        while (index < in.size) {
          vs = in(index)
          rebind(v.asInstanceOf[Sym[A]], ReadVar(vs))
          //reflectEffect(findDefinition(func.asInstanceOf[Sym[Unit]]).get.rhs)
          var x = var_new(func)
          index += 1
        }
        alloc
      }
    }*/
  }

  // TODO: should we make all DeliteOps be boundable? This is probably not the right way to do this anyways.
  @deprecated("DeliteOpForeachBounded should only be used if sync is required. It will be removed as soon as sync works with DeliteOpForeach", "")
  abstract class DeliteOpForeachBounded[B,A <: B,C[X <: B] <: DeliteCollection[X]] extends DeliteOp[Unit] {
    val in: Exp[C[A]]
    val v: Sym[A]
    val func: Block[Unit]
    val i: Sym[Int]
    val sync: Block[List[Any]]
  }


  

  ///////////////////////////
  // effects + other helpers

  // used by delite code generators to handle nested delite ops TR: shouldn't this be part of the codegen hierarchy?
  var deliteKernel: Boolean = false
  var deliteResult: Option[List[Exp[Any]]] = None
  var deliteInputs: List[Sym[Any]] = Nil

  var simpleCodegen: Boolean = false// try to generate more readable code

  // TODO: move to lms? TR: will that actually work? it looks pretty unsafe to rebind syms
  //def rebind(sym: Sym[Any], rhs: Def[Any]) = createDefinition(sym, rhs).rhs

  def summarizeBody[A](d: Def[A]) = d match {
    case e: DeliteForeachElem[_] => summarizeEffects(e.func).star
    case e: DeliteCollectElem[_,_,_] => 
      val ec = if (e.cond.nonEmpty) e.cond.map(summarizeEffects).reduce((s1,s2) => s1 andThen s2) else Pure()      
      val ef = summarizeEffects(e.func)
      // ef.star
      ef.star andAlso ec // should be (ef andAlso ec).star? there is an issue with orElse setting resAlloc to false
    //case e: DeliteReduceElem[_] => (summarizeEffects(e.func) andThen summarizeEffects(e.rFunc)).star
    case e: DeliteReduceElem[_] => 
      // explicitly remove writes to the accumulator -- can we generalize this somehow?
      def clean(xs: List[Sym[Any]]) = xs.filterNot(_ == e.rV._1)
      val ef = summarizeEffects(e.func)
      val er = summarizeEffects(e.rFunc)
      val ec = if (e.cond.nonEmpty) e.cond.map(summarizeEffects).reduce((s1,s2) => s1 andThen s2) else Pure()      
      val er2 = er.copy(mayRead = clean(er.mayRead), mstRead = clean(er.mstRead), 
                        mayWrite = clean(er.mayWrite), mstWrite = clean(er.mstWrite))
      // (ef andThen er2).star // not 100% correct
      (ef andThen er2).star andAlso ec // not 100% correct
    case e: DeliteReduceTupleElem[_,_] =>
      // explicitly remove writes to the accumulator -- can we generalize this somehow?
      def cleanP(xs: List[Sym[Any]]) = xs.filterNot(x => x == e.rVPar._1._1 || x == e.rVPar._1._2)
      def cleanS(xs: List[Sym[Any]]) = xs.filterNot(x => x == e.rVSeq._1._1 || x == e.rVSeq._1._2)
      val ef = summarizeEffects(e.func._1) andAlso summarizeEffects(e.func._2)
      val erp = summarizeEffects(e.rFuncPar._1) andAlso summarizeEffects(e.rFuncPar._2)
      val ers = summarizeEffects(e.rFuncSeq._1) andAlso summarizeEffects(e.rFuncSeq._2)
      val erp2 = erp.copy(mayRead = cleanP(erp.mayRead), mstRead = cleanP(erp.mstRead),
                        mayWrite = cleanP(erp.mayWrite), mstWrite = cleanP(erp.mstWrite))
      val ers2 = ers.copy(mayRead = cleanS(ers.mayRead), mstRead = cleanS(ers.mstRead),
                        mayWrite = cleanS(ers.mayWrite), mstWrite = cleanS(ers.mstWrite))
      (ef andAlso erp2 andAlso ers2).star
  }
  
  // TODO: just to make refactoring easier in case we want to change to reflectSomething
  // def reflectPure[A:Manifest](x: Def[A]): Exp[A] = toAtom(x)

  // alternative: leave reflectPure as above and override toAtom...

  def reflectPure[A:Manifest](d: Def[A])(implicit ctx: SourceContext): Exp[A] = d match {
    case x: DeliteOpLoop[_] =>
      val mutableInputs = readMutableData(d) //TODO: necessary or not??
      //val mutableInputs = Nil // readMutableData(d) TODO: necessary or not??
      val re = Read(mutableInputs)
      val be = summarizeBody(x.body)
      reflectEffect(d, re andAlso be)
    case x: DeliteOpSingleTask[_] =>      
      val mutableInputs = readMutableData(d) //TODO: necessary or not??
      //val mutableInputs = Nil // readMutableData(d) TODO: necessary or not??
      val re = Read(mutableInputs)
      val be = summarizeEffects(x.block)
      reflectEffect(d, re andAlso be)
    case _ => 
      toAtom(d)
  }

  // TBD: move logic from reflectPure (above) into reflectEffect?

  // HACK lazy val bites again: must make sure that block is evaluated!
  override def reflectEffect[A:Manifest](d: Def[A], u: Summary)(implicit ctx: SourceContext): Exp[A] = d match {
    case x: DeliteOpSingleTask[_] =>
      x.block
      super.reflectEffect(d,u)
    case x: DeliteOpLoop[_] => 
      val z = x.body  //  <-- not lazy
      super.reflectEffect(d,u)
    case _ =>
      super.reflectEffect(d,u)
  }
  
  // HACK lazy val bites again: must make sure that block is evaluated!
  override def reflectMirrored[A:Manifest](zd: Reflect[A]): Exp[A] = zd match {
    case Reflect(x:DeliteOpSingleTask[_], u, es) =>
      x.block
      super.reflectMirrored(zd)
    case Reflect(x: DeliteOpLoop[_], u, es) => 
      val z = x.body  //  <-- somehow not always evaluated? lazy val extends a strict val, what are the semantics?
      super.reflectMirrored(zd)
    case _ =>
      super.reflectMirrored(zd)
  }
  

  // what about this: enable?
  // override def reflectMutable[A:Manifest](d: Def[A]): Exp[A] = d match {
  //   case x: DeliteOpLoop[_] => 
  //     val mutableInputs = readMutableData(d)    
  //     val allocAndRead = Alloc() andAlso Read(mutableInputs)
  //     val be = summarizeBody(x.body)
  //     val z = reflectEffect(d, allocAndRead andAlso be)
  //     
  //     val mutableAliases = mutableTransitiveAliases(d)
  //     checkIllegalSharing(z, mutableAliases)      
  //     z
  //   case _ => 
  //     super.reflectMutable(d)
  // }
      

  //////////////
  // mirroring

  override def mirrorFatDef[A:Manifest](d: Def[A], f: Transformer)(implicit ctx: SourceContext): Def[A] = mirrorLoopBody(d,f) // TODO: cleanup

  def mirrorLoopBody[A:Manifest](d: Def[A], f: Transformer): Def[A] = {
    // should this be the default apply in Transforming? note we need a manifest! but the default in Transforming seems wrong otherwise... better to catch early
    // def fb[B:Manifest](b: Block[B]) = if (f.hasContext) reifyEffects(f.reflectBlock(b)) else f(b)
    def fb[B:Manifest](b: Block[B]) = f(b)
    
    d match {
      case e: DeliteCollectElem[a,i,ca] => 
        (DeliteCollectElem[a,i,ca]( // need to be a case class for equality (do we rely on equality?)          
          eV = f(e.eV).asInstanceOf[Sym[a]],        
          sV = f(e.sV).asInstanceOf[Sym[Int]],
          allocVal = f(e.allocVal).asInstanceOf[Sym[i]],
          allocN = fb(e.allocN)(e.mI),
          func = fb(e.func)(e.mA),
          update = fb(e.update)(manifest[Unit]),
          finalizer = fb(e.finalizer)(e.mCA),
          cond = e.cond.map(fb(_)(manifest[Boolean])), //f(e.cond)
          par = e.par,
          buf = DeliteBufferElem[a,i,ca](
            iV = f(e.buf.iV).asInstanceOf[Sym[Int]],
            iV2 = f(e.buf.iV2).asInstanceOf[Sym[Int]],
            aV = f(e.buf.aV).asInstanceOf[Sym[i]],
            append = fb(e.buf.append)(manifest[Boolean]),
            setSize = fb(e.buf.setSize)(manifest[Unit]),
            allocRaw = fb(e.buf.allocRaw)(e.mI),
            copyRaw = fb(e.buf.copyRaw)(manifest[Unit])          
          )(e.mA,e.mI,e.mCA)
        )(e.mA,e.mI,e.mCA)).asInstanceOf[Def[A]]
      case e: DeliteForeachElem[a] => 
        (DeliteForeachElem[a](
          func = fb(e.func)(e.mA),
          sync = f(e.sync)
//          cond = f(e.cond)
        )(e.mA)).asInstanceOf[Def[A]] // reasonable?
      case e: DeliteReduceElem[a] => 
        (DeliteReduceElem[a](
          func = fb(e.func)(e.mA),
          cond = e.cond.map(fb(_)(manifest[Boolean])),//f(e.cond),
          zero = fb(e.zero)(e.mA),
          rV = (f(e.rV._1).asInstanceOf[Sym[a]], f(e.rV._2).asInstanceOf[Sym[a]]), // need to transform bound vars ??
          rFunc = fb(e.rFunc)(e.mA),
          stripFirst = e.stripFirst
        )(e.mA)).asInstanceOf[Def[A]]
      case e: DeliteReduceTupleElem[a,b] =>
        (DeliteReduceTupleElem[a,b](
          func = (fb(e.func._1)(e.mA),fb(e.func._2)(e.mB)),
          cond = e.cond.map(fb(_)(manifest[Boolean])),//f(e.cond),
          zero = (fb(e.zero._1)(e.mA),fb(e.zero._2)(e.mB)),
          rVPar = ((f(e.rVPar._1._1).asInstanceOf[Sym[a]], f(e.rVPar._1._2).asInstanceOf[Sym[b]]),(f(e.rVPar._2._1).asInstanceOf[Sym[a]], f(e.rVPar._2._2).asInstanceOf[Sym[b]])), // need to transform bound vars ??
          rVSeq = ((f(e.rVSeq._1._1).asInstanceOf[Sym[a]], f(e.rVSeq._1._2).asInstanceOf[Sym[b]]),(f(e.rVSeq._2._1).asInstanceOf[Sym[a]], f(e.rVSeq._2._2).asInstanceOf[Sym[b]])), // need to transform bound vars ??
          rFuncPar = (fb(e.rFuncPar._1)(e.mA),fb(e.rFuncPar._2)(e.mB)),
          rFuncSeq = (fb(e.rFuncSeq._1)(e.mA),fb(e.rFuncSeq._2)(e.mB)),
          stripFirst = e.stripFirst
        )(e.mA,e.mB)).asInstanceOf[Def[A]]
    }
  }
  

  //////////////
  // dependencies

  /* is this necessary or not? does not appear to effect anything yet. */
  override def blocks(e: Any): List[Block[Any]] = e match {
    case s: DeliteOpSingleTask[_] => blocks(s.block)
    case e: DeliteOpExternal[_] =>  super.blocks(e) ::: blocks(e.allocVal) 
    case op: DeliteCollectElem[_,_,_] => blocks(op.func) ::: blocks(op.update) ::: blocks(op.cond) ::: blocks(op.allocN) ::: blocks(op.finalizer) ::: blocks(op.buf) 
    case op: DeliteBufferElem[_,_,_] => blocks(op.append) ::: blocks(op.setSize) ::: blocks(op.allocRaw) ::: blocks(op.copyRaw)
//    case op: DeliteForeachElem[_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.sync)
    case op: DeliteForeachElem[_] => blocks(op.func) ::: blocks(op.sync)
    case op: DeliteReduceElem[_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFunc)
    case op: DeliteReduceTupleElem[_,_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFuncSeq) ::: blocks(op.rFuncPar) // should be ok for tuples...
    case foreach: DeliteOpForeach2[_,_] => blocks(foreach.func) ::: blocks(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => blocks(foreach.func) ::: blocks(foreach.sync)
    case _ => super.blocks(e)
  }  
  
  override def syms(e: Any): List[Sym[Any]] = e match { //TR TODO: question -- is alloc a dependency (should be part of result) or a definition (should not)???
                                                        // aks: answer -- we changed it to be internal to the op to make things easier for CUDA. not sure if that still needs
                                                        // to be the case. similar question arises for sync
    case s: DeliteOpSingleTask[_] if s.requireInputs => super.syms(e) ::: syms(s.block) // super call: add case class syms (iff flag is set)
    case s: DeliteOpSingleTask[_] => syms(s.block)
    case e: DeliteOpExternal[_] =>  super.syms(e) ::: syms(e.allocVal) 
    case op: DeliteCollectElem[_,_,_] => syms(op.func) ::: syms(op.update) ::: syms(op.cond) ::: syms(op.allocN) ::: syms(op.finalizer) ::: syms(op.buf) 
    case op: DeliteBufferElem[_,_,_] => syms(op.append) ::: syms(op.setSize) ::: syms(op.allocRaw) ::: syms(op.copyRaw)
//    case op: DeliteForeachElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.sync)
    case op: DeliteForeachElem[_] => syms(op.func) ::: syms(op.sync)
    case op: DeliteReduceElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFunc)
    case op: DeliteReduceTupleElem[_,_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFuncSeq) ::: syms(op.rFuncPar) // should be ok for tuples...
    case foreach: DeliteOpForeach2[_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ::: syms(foreach.func) ::: syms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ::: syms(foreach.func) ::: syms(foreach.sync)
    case _ => super.syms(e)
  }
    
  override def readSyms(e: Any): List[Sym[Any]] = e match { //TR FIXME: check this is actually correct
    case s: DeliteOpSingleTask[_] if s.requireInputs => super.syms(e) ::: syms(s.block) // super call: add case class syms (iff flag is set)
    case s: DeliteOpSingleTask[_] => syms(s.block)
    case e: DeliteOpExternal[_] => super.syms(e) ::: syms(e.allocVal)
    case op: DeliteCollectElem[_,_,_] => syms(op.func) ::: syms(op.cond) ::: syms(op.allocN) ::: syms(op.finalizer)
//    case op: DeliteForeachElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.sync)
    case op: DeliteForeachElem[_] => syms(op.func) ::: syms(op.sync)
    case op: DeliteReduceElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFunc)
    case op: DeliteReduceTupleElem[_,_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFuncSeq) ::: syms(op.rFuncPar)
    case foreach: DeliteOpForeach2[_,_] => syms(foreach.in)
    case foreach: DeliteOpForeachBounded[_,_,_] => syms(foreach.in) 
    case _ => super.readSyms(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => effectSyms(s.block)
    case e: DeliteOpExternal[_] => effectSyms(e.allocVal) /*::: super.effectSyms(e) */
    case op: DeliteCollectElem[_,_,_] => List(op.eV, op.sV) ::: effectSyms(op.func)  ::: effectSyms(op.cond) ::: effectSyms(op.allocN) ::: effectSyms(op.finalizer) ::: syms(op.allocVal) ::: boundSyms(op.buf)
    case op: DeliteBufferElem[_,_,_] => List(op.aV, op.iV, op.iV2) ::: effectSyms(op.append) ::: effectSyms(op.setSize) ::: effectSyms(op.allocRaw) ::: effectSyms(op.copyRaw) 
//    case op: DeliteForeachElem[_] => effectSyms(op.func) ::: effectSyms(op.cond) ::: effectSyms(op.sync)
    case op: DeliteForeachElem[_] => effectSyms(op.func) ::: effectSyms(op.sync)
    case op: DeliteReduceElem[_] => List(op.rV._1, op.rV._2) ::: effectSyms(op.func) ::: effectSyms(op.cond) ::: effectSyms(op.zero) ::: effectSyms(op.rFunc)
    case op: DeliteReduceTupleElem[_,_] => syms(op.rVPar) ::: syms(op.rVSeq) ::: effectSyms(op.func._1) ::: effectSyms(op.cond) ::: effectSyms(op.zero) ::: effectSyms(op.rFuncPar) ::: effectSyms(op.rFuncSeq)
    case foreach: DeliteOpForeach2[_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case _ => super.boundSyms(e)
  }

  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: DeliteOpSingleTask[_] if s.requireInputs => super.symsFreq(e) ::: freqNormal(s.block)  // super call: add case class syms (iff flag is set)
    case s: DeliteOpSingleTask[_] => freqNormal(s.block)
    case e: DeliteOpExternal[_] => super.symsFreq(e) ::: freqNormal(e.allocVal)
    case op: DeliteCollectElem[_,_,_] => freqNormal(op.allocN) ::: freqNormal(op.finalizer) ::: freqHot(op.cond) ::: freqHot(op.func) ::: freqHot(op.update) ::: symsFreq(op.buf)
    case op: DeliteBufferElem[_,_,_] => freqHot(op.append) ::: freqNormal(op.setSize) ::: freqNormal(op.allocRaw) ::: freqNormal(op.copyRaw)
//    case op: DeliteForeachElem[_] => freqNormal(op.sync) ::: freqHot(op.cond) ::: freqHot(op.func)
    case op: DeliteForeachElem[_] => freqNormal(op.sync) ::: freqHot(op.func)
    case op: DeliteReduceElem[_] => freqHot(op.cond) ::: freqHot(op.func) ::: freqNormal(op.zero) ::: freqHot(op.rFunc)
    case op: DeliteReduceTupleElem[_,_] => freqHot(op.cond) ::: freqHot(op.func) ::: freqNormal(op.zero) ::: freqHot(op.rFuncSeq) ::: freqHot(op.rFuncPar)
    case foreach: DeliteOpForeach2[_,_] => freqNormal(foreach.in) ::: freqHot(foreach.func) ::: freqHot(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => freqNormal(foreach.in) ::: freqHot(foreach.func) ::: freqHot(foreach.sync)
    case _ => super.symsFreq(e)
  }
  
	/////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => syms(s.block)
    case e: DeliteOpExternal[_] => Nil
    case op: DeliteCollectElem[_,_,_] => Nil // in particular not op.alloc !
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case op: DeliteReduceTupleElem[_,_] => Nil
    case foreach: DeliteOpForeach2[_,_] => Nil
    case foreach: DeliteOpForeachBounded[_,_,_] => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => Nil
    case e: DeliteOpExternal[_] => Nil
    case op: DeliteCollectElem[_,_,_] => syms(op.func) 
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case op: DeliteReduceTupleElem[_,_] => Nil
    case foreach: DeliteOpForeach2[_,_] => Nil
    case foreach: DeliteOpForeachBounded[_,_,_] => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => Nil
    case e: DeliteOpExternal[_] => Nil
    case op: DeliteCollectElem[_,_,_] => Nil
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case op: DeliteReduceTupleElem[_,_] => Nil
    case foreach: DeliteOpForeach2[_,_] => Nil
    case foreach: DeliteOpForeachBounded[_,_,_] => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => Nil
    case e: DeliteOpExternal[_] => syms(e.allocVal)
    case op: DeliteCollectElem[_,_,_] => syms(op.allocN)
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case op: DeliteReduceTupleElem[_,_] => Nil
    case foreach: DeliteOpForeach2[_,_] => Nil
    case foreach: DeliteOpForeachBounded[_,_,_] => Nil
    case _ => super.copySyms(e)
  }
}


trait BaseDeliteOpsTraversalFat extends BaseLoopsTraversalFat {
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
    case e: DeliteCollectElem[_,_,_] if e.cond.isEmpty => Some(e.func.res)
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case e: DeliteCollectElem[_,_,_] => Some((e.func.res, e.cond.map(_.res)))
  //    case e: DeliteReduceElem[_] => Some((e.func, e.cond)) // TODO: aks -- testing fusing conditionals for reduce elems
    case _ => super.unapplySimpleCollectIf(e)
  }

  override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match {
    case e: DeliteCollectElem[_,_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))
    case e: DeliteReduceElem[_] => e.copy(cond = e.cond ++ c.map(Block(_)))
    case _ => super.applyAddCondition(e,c)
  }

  override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]) = Config.opfusionEnabled  
}


trait BaseGenDeliteOps extends BaseDeliteOpsTraversalFat with BaseGenLoopsFat with LoopFusionOpt with BaseGenStaticData {
  val IR: DeliteOpsExp
  import IR._

  //abstract override def emitValDef(sym: Sym[Any], rhs: String): Unit =
  //  if (!simpleCodegen) super.emitValDef(sym,rhs) else stream.print(quote(sym)+";")
  //def emitVarDef(sym: Sym[Any], rhs: String): Unit
  //def quote(x: Exp[Any]) : String = 

  // TODO: what about deliteResult and deliteInput??

  // CAVEAT: DeliteCodegen does not inherit from this trait, so this is called
  // only within kernels

  override def focusBlock[A](result: Block[Any])(body: => A): A = {
    var saveKernel = deliteKernel
    deliteKernel = false
    val ret = super.focusBlock(result)(body)
    deliteKernel = saveKernel
    ret
  }
}



trait ScalaGenStaticDataDelite extends ScalaGenStaticData {
  val IR: StaticDataExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StaticData(x) => 
      // TODO: should call super if we're generating a single big scala file
      emitValDef(sym, "ppl.delite.runtime.graph.ops.Arguments.staticData["+remap(sym.tp)+"](\""+quote(sym)+"\") // static data: " + x)
    case _ => super.emitNode(sym, rhs)
  }
}


trait ScalaGenDeliteOps extends ScalaGenLoopsFat with ScalaGenStaticDataDelite with BaseGenDeliteOps { // not sure where to mix in ScalaGenStaticData
  import IR._

  def quotearg(x: Sym[Any]) = quote(x) + ": " + quotetp(x)
  def quotetp(x: Sym[Any]) = remap(x.tp)
/*
  def quoteZero(x: Sym[Any]) = x.tp.toString match {
    case "Int" | "Long" | "Float" | "Double" => "0" 
    case "Boolean" => "false"
    case _ => "null" 
  }
*/

  //TODO: modularize code generators even more

  /**
   * MultiLoop components
   */
  def emitCollectElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteCollectElem[_,_,_], prefixSym: String = "") {
    emitValDef(elem.eV, quote(getBlockResult(elem.func)))      
    elem.par match {
      case ParBuffer =>
        emitValDef(elem.allocVal, prefixSym + quote(sym) + "_buf")                
        if (elem.cond.nonEmpty) stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")        
        // should append be called insert? it is a physical append of element e, at logical index v (it's a tail-insert)        
        emitBlock(elem.buf.append)
        stream.println("if (" + quote(getBlockResult(elem.buf.append)) + ")")
        stream.println(prefixSym + quote(sym) + "_size += 1")
        if (elem.cond.nonEmpty) { 
          stream.println(prefixSym + quote(sym) + "_conditionals += 1")
          stream.println("}") 
        } 
        
      case ParFlat =>
        emitValDef(elem.allocVal, prefixSym + quote(sym) + "_data")                
        emitBlock(elem.update)
    }
  }
    
  def emitForeachElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteForeachElem[_]) {
    // if (elem.cond.nonEmpty)
    //   stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
    stream.println(quote(getBlockResult(elem.func)))
    // if (elem.cond.nonEmpty) {
    //   stream.println("}")                         
    // }
  }
  
  // -- begin emit reduce
  
  def emitFirstReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    if (elem.cond.nonEmpty) {
      // if we have conditionals, we have to delay the the initialization of the accumulator to the
      // first element where the condition is true
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
      stream.println(quote(getBlockResult(elem.func)))
      stream.println("} else {")
      stream.println(prefixSym + quote(sym) + "_zero")
      stream.println("}")
    }
    else {
      stream.println(quote(getBlockResult(elem.func)))        
    }
  }

  def emitReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
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

  def emitReduceTupleElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceTupleElem[_,_], prefixSym: String = "") {
    if (elem.cond.nonEmpty){
      sys.error("tuple reduce with external conditions not implemented!")
    }
    else {
      emitReductionTuple(op, sym, elem, prefixSym)
    }
  }

  def emitInitializeOrReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    stream.println("// TODO: we could optimize this check away with more convoluted runtime support if necessary")          
    stream.println("if (" + prefixSym + quote(sym) + " == " + prefixSym + quote(sym) + "_zero" + ") " + prefixSym + quote(sym) + " = {")
    
    // initialize
    stream.println(quote(getBlockResult(elem.func)))
    stream.println("}")
    // or reduce
    stream.println("else {")
    emitReduction(op, sym, elem, prefixSym)
    stream.println("}")
  } 
        
  def emitReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    stream.println("val " + quote(elem.rV._1) + " = " + prefixSym + quote(sym))
    stream.println("val " + quote(elem.rV._2) + " = " + quote(getBlockResult(elem.func)))
    emitBlock(elem.rFunc)
    stream.println(prefixSym + quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))    
  }

  def emitReductionTuple(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceTupleElem[_,_], prefixSym: String) {
    val rV = elem.rVSeq
    val rFunc = elem.rFuncSeq
    stream.println("val " + quote(rV._1._1) + " = " + prefixSym + quote(sym) + "  ")
    stream.println("val " + quote(rV._1._2) + " = " + prefixSym + quote(sym) + "_2")
    stream.println("val " + quote(rV._2._1) + " = " + quote(getBlockResult(elem.func._1)))
    stream.println("val " + quote(rV._2._2) + " = " + quote(getBlockResult(elem.func._2)))
    emitFatBlock(List(rFunc._1, rFunc._2))
    stream.println(prefixSym + quote(sym) + "   = " + quote(getBlockResult(rFunc._1)))
    stream.println(prefixSym + quote(sym) + "_2 = " + quote(getBlockResult(rFunc._2)))
  }

  // -- end emit reduce emit
  
  def emitMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    val elemFuncs = op.body flatMap { // don't emit dependencies twice!
      case elem: DeliteCollectElem[_,_,_] => elem.func :: elem.cond
//      case elem: DeliteForeachElem[_] => elem.cond // only emit func inside condition! TODO: how to avoid emitting deps twice? // elem.func :: elem.cond
      case elem: DeliteForeachElem[_] => List(elem.func) 
      case elem: DeliteReduceElem[_] => elem.func :: elem.cond
      case elem: DeliteReduceTupleElem[_,_] => elem.func._1 :: elem.func._2 :: elem.cond
    }
    emitFatBlock(elemFuncs)
  }

  def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    // initialization             
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) => elem.par match {
        case ParBuffer => 
          stream.println("val " + quote(sym) + "_buf = {")      
          emitValDef(elem.sV, "0")
        case ParFlat => 
          stream.println("val " + quote(sym) + "_data = {")      
          emitValDef(elem.sV, quote(op.size))
        }
        emitBlock(elem.allocN)
        stream.println(quote(getBlockResult(elem.allocN)))        
        stream.println("}")        
        stream.println("var " + quote(sym) + "_size = 0")
        if (elem.cond.nonEmpty) stream.println("var " + quote(sym) + "_conditionals = 0")
      case (sym, elem: DeliteForeachElem[_]) => 
        stream.println("var " + quotearg(sym) + " = ()") // must be type Unit
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println("val " + quote(sym) + "_zero = {"/*}*/)
        emitBlock(elem.zero)
        stream.println(quote(getBlockResult(elem.zero)))
        stream.println(/*{*/"}")
        /*stream.println("val " + quote(sym) + "_zero = " + elem.zero)*/
        stream.println("var " + quotearg(sym) + " = " + quote(sym) + "_zero")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        stream.println("val " + quote(sym) + "_zero   = {"/*}*/) // better use emitFatBlock?
        emitBlock(elem.zero._1)
        stream.println(quote(getBlockResult(elem.zero._1)))
        stream.println(/*{*/"}")
        stream.println("val " + quote(sym) + "_zero_2 = {"/*}*/)
        emitBlock(elem.zero._2)
        stream.println(quote(getBlockResult(elem.zero._2)))
        stream.println(/*{*/"}")
        /*stream.println("val " + quote(sym) + "_zero   = " + elem.zero._1)
        stream.println("val " + quote(sym) + "_zero_2 = " + elem.zero._2)*/
        stream.println("var " + quote(sym) + "  " + " = " + quote(sym) + "_zero  ") // should have types...
        stream.println("var " + quote(sym) + "_2" + " = " + quote(sym) + "_zero_2")
    }
    stream.println("var " + quote(op.v) + " = 0")
    //if (true) { //op.body exists (loopBodyNeedsStripFirst _)) { preserve line count as indicator for succesful fusing
    if (op.body exists (loopBodyNeedsStripFirst _)) {
      stream.println("if (" + quote(op.size) + " > 0) { // prerun fat loop " + symList.map(quote).mkString(",")/*}*/)
      /* strip first iteration */     
      emitMultiLoopFuncs(op, symList)
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitCollectElem(op, sym, elem)
        case (sym, elem: DeliteForeachElem[_]) => 
          stream.println(quote(sym) + " = {"/*}*/)
          emitForeachElem(op, sym, elem)
          stream.println(/*{*/"}")
        case (sym, elem: DeliteReduceElem[_]) =>
          if (elem.stripFirst) {
            stream.println(quote(sym) + " = {"/*}*/)
            emitFirstReduceElem(op, sym, elem)
            stream.println(/*{*/"}")
          }
          else {
            emitReduceElem(op, sym, elem)
          }
      }
      stream.println(/*{*/"}")
      stream.println(quote(op.v) + " = 1")
    }
    stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(",")/*}*/)
    // body
    emitMultiLoopFuncs(op, symList)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitCollectElem(op, sym, elem)
      case (sym, elem: DeliteForeachElem[_]) => 
        stream.println(quote(sym) + " = {"/*}*/)                             
        emitForeachElem(op, sym, elem)
        stream.println(/*{*/"}")
      case (sym, elem: DeliteReduceElem[_]) =>
        emitReduceElem(op, sym, elem)
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        emitReduceTupleElem(op, sym, elem)
    }
    stream.println(quote(op.v) + " += 1")
    stream.println(/*{*/"} // end fat loop " + symList.map(quote).mkString(","))
    // finalizer
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        // if we are using parallel buffers, set the logical size of the output since it
        // might be different than the physically appended size for some representations
        if (elem.par == ParBuffer) {      
          emitValDef(elem.allocVal, quote(sym) + "_buf")     
          if (elem.cond.nonEmpty) 
            emitValDef(elem.sV, quote(sym) + "_conditionals")
          else
            emitValDef(elem.sV, quote(op.size))
          emitBlock(elem.buf.setSize)            
        }
        else {
          emitValDef(elem.allocVal, quote(sym) + "_data")
        }
        emitBlock(elem.finalizer)
        stream.println("val " + quote(sym) + " = " + quote(getBlockResult(elem.finalizer)))
      case (sym, elem: DeliteForeachElem[_]) => 
      case (sym, elem: DeliteReduceElem[_]) =>
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
    }
  }

  def emitKernelAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    // kernel mode
    val kernelName = symList.map(quote).mkString("")
    val actType = "activation_"+kernelName
    //deliteKernel = false
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
      case (sym, elem: DeliteCollectElem[_,_,_]) => elem.par match {
        case ParBuffer =>
          stream.println("// __act." + quote(sym) + " stays null for now")
        case ParFlat =>
          stream.println("__act." + quote(sym) + "_data = {")                        
          emitValDef(elem.sV, quote(op.size))
          emitBlock(elem.allocN)
          stream.println(quote(getBlockResult(elem.allocN)))                  
          stream.println("}")
      }
      case (sym, elem: DeliteForeachElem[_]) => 
        stream.println("__act." + quote(sym) + " = ()") // must be type Unit, initialized in init below
      case (sym, elem: DeliteReduceElem[_]) => 
        stream.println("__act." + quote(sym) + "_zero = {"/*}*/)
        emitBlock(elem.zero)
        stream.println(quote(getBlockResult(elem.zero)))
        stream.println(/*{*/"}")
        /*stream.println("__act." + quote(sym) + "_zero = " + elem.zero)
        stream.println("__act." + quote(sym) + " = __act." + quote(sym) + "_zero")*/
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        stream.println("__act." + quote(sym) + "_zero   = {"/*}*/) // better use emitFatBlock?
        emitBlock(elem.zero._1)
        stream.println(quote(getBlockResult(elem.zero._1)))
        stream.println(/*{*/"}")
        stream.println("__act." + quote(sym) + "_zero_2 = {"/*}*/)
        emitBlock(elem.zero._2)
        stream.println(quote(getBlockResult(elem.zero._2)))
        stream.println(/*{*/"}")
        /*stream.println("__act." + quote(sym) + "_zero   = " + elem.zero._1)
        stream.println("__act." + quote(sym) + "_zero_2 = " + elem.zero._2)
        stream.println("__act." + quote(sym) + "  " + " = __act." + quote(sym) + "_zero  ")
        stream.println("__act." + quote(sym) + "_2" + " = __act." + quote(sym) + "_zero_2")*/
    }
    stream.println("__act")
    stream.println(/*{*/"}")
    // processRange
    stream.println("def processRange(__act: " + actType + ", start: Int, end: Int): " + actType + " = {"/*}*/)
    stream.println("var idx = start")
    stream.println("val __act2 = init(__act,idx)")
    stream.println("idx += 1")
    stream.println("while (idx < end) {"/*}*/)
    stream.println("process(__act2, idx)")
    stream.println("idx += 1")
    stream.println("/*{*/}")
    stream.println("__act2")
    stream.println(/*{*/"}")

/*
    out.append("val acc = head.closure.init(out, idx)\n") // copy of out per chunk
                out.append("idx += 1\n")
    out.append("val hc = head.closure\n")
    out.append("while (idx < end) {\n")
    out.append("hc.process(acc, idx)\n")
    out.append("idx += 1\n")
    out.append("}\n")
*/
    //out.append("val acc = head.closure.processRange(out,idx,end)\n")


    stream.println("def init(__act: " + actType + ", " + quotearg(op.v) + "): " + actType + " = {"/*}*/)
    if (op.body exists (loopBodyNeedsCombine _)) {
      emitMultiLoopFuncs(op, symList)                               
      stream.println("val __act2 = new " + actType)
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) => elem.par match {
          case ParBuffer =>
            stream.println("__act2." + quote(sym) + "_buf = {")
            emitValDef(elem.sV, "0")
            emitBlock(elem.allocN)
            stream.println(quote(getBlockResult(elem.allocN)))                  
            stream.println("}")                                
          case ParFlat =>
            stream.println("__act2." + quote(sym) + "_data = __act." + quote(sym) + "_data")
          }
          stream.println("if (" + quote(op.size) + " > 0) {")
          emitCollectElem(op, sym, elem, "__act2.")
          stream.println("}")
        case (sym, elem: DeliteForeachElem[_]) => 
          stream.println("__act2." + quote(sym) + " = {"/*}*/)
          stream.println("if (" + quote(op.size) + " > 0) {")
          emitForeachElem(op, sym, elem)
          stream.println("}")
          stream.println(/*{*/"}")               
        case (sym, elem: DeliteReduceElem[_]) =>
          if (elem.stripFirst) {
            stream.println("__act2." + quote(sym) + "_zero = " + "__act." + quote(sym) + "_zero") // do we need zero here? yes, for comparing against...
            stream.println("__act2." + quote(sym) + " = {"/*}*/)
            stream.println("if (" + quote(op.size) + " > 0) {")
            emitFirstReduceElem(op, sym, elem, "__act2.")
            stream.println("} else __act2." + quote(sym) + "_zero")
            stream.println(/*{*/"}")
          } else { 
            stream.println("__act2." + quote(sym) + "_zero = " + "__act." + quote(sym) + "_zero")
            if (isPrimitiveType(sym.tp)) {
              stream.println("__act2." + quote(sym) + " = " + "__act2." + quote(sym) + "_zero")
            } else {
              stream.println("__act2." + quote(sym) + " = " + "__act2." + quote(sym) + "_zero.Clone") // separate zero buffer
            }
            stream.println("if (" + quote(op.size) + " > 0) {")
            emitReduceElem(op, sym, elem, "__act2.")
            stream.println("}")
          }
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          // no strip first here ... stream.println("assert(false, \"TODO: tuple reduce\")")
          stream.println("__act2." + quote(sym) + "_zero   = " + "__act." + quote(sym) + "_zero  ")
          stream.println("__act2." + quote(sym) + "_zero_2 = " + "__act." + quote(sym) + "_zero_2")
          stream.println("__act2." + quote(sym) + "   = " + "__act2." + quote(sym) + "_zero  ")
          stream.println("__act2." + quote(sym) + "_2 = " + "__act2." + quote(sym) + "_zero_2")
          stream.println("if (" + quote(op.size) + " > 0) {")
          emitReduceTupleElem(op, sym, elem, "__act2.")
          stream.println("}")
      }
      stream.println("__act2")
    } else {
      stream.println("if (" + quote(op.size) + " > 0) {")
      stream.println("process(__act, " + quote(op.v) + ")")
      stream.println("}")
      stream.println("__act")
    }
    stream.println(/*{*/"}")
    stream.println("def process(__act: " + actType + ", " + quotearg(op.v) + "): Unit = {"/*}*/)
    emitMultiLoopFuncs(op, symList)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitCollectElem(op, sym, elem, "__act.")
      case (sym, elem: DeliteForeachElem[_]) =>
        stream.println("val " + quote(sym) + " = {")
        emitForeachElem(op, sym, elem)
        stream.println("}")
      case (sym, elem: DeliteReduceElem[_]) =>
        emitReduceElem(op, sym, elem, "__act.")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        emitReduceTupleElem(op, sym, elem, "__act.")
    }
    stream.println(/*{*/"}")
    stream.println("def combine(__act: " + actType + ", rhs: " + actType + "): Unit = {"/*}*/)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
      case (sym, elem: DeliteForeachElem[_]) => // nothing needed
      case (sym, elem: DeliteReduceElem[_]) =>
        // if either value is zero, return the other instead of combining
        stream.println("val " + quote(elem.rV._1) + " = " + "__act." + quote(sym))
        stream.println("val " + quote(elem.rV._2) + " = " + "rhs." + quote(sym))
        stream.println("if (" + quote(elem.rV._1) + " == " + "__act." + quote(sym) + "_zero) {"/*}*/) //TODO: what if zero is an accumulator (SumIf)?
        stream.println("__act." + quote(sym) + " = " + quote(elem.rV._2))
        stream.println(/*{*/"}")
        stream.println("else if (" + quote(elem.rV._2) + " != " + "__act." + quote(sym) + "_zero) {"/*}*/) //TODO: see above
        emitBlock(elem.rFunc)
        stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))
        stream.println(/*{*/"}")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        // stream.println("assert(false, \"TODO: tuple reduce\")")
        val rV = elem.rVPar
        val rFunc = elem.rFuncPar
        stream.println("val " + quote(rV._1._1) + " = " + "__act." + quote(sym) + "  ")
        stream.println("val " + quote(rV._1._2) + " = " + "__act." + quote(sym) + "_2")
        stream.println("val " + quote(rV._2._1) + " = " + "rhs." + quote(sym) + "  ")
        stream.println("val " + quote(rV._2._2) + " = " + "rhs." + quote(sym) + "_2")
        emitFatBlock(List(rFunc._1, rFunc._2))
        stream.println("__act." + quote(sym) + "   = " + quote(getBlockResult(rFunc._1)))
        stream.println("__act." + quote(sym) + "_2 = " + quote(getBlockResult(rFunc._2)))
    }
    stream.println(/*{*/"}")
    // scan/postprocess follows
    stream.println("def postCombine(__act: " + actType + ", rhs: " + actType + "): Unit = {"/*}*/)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        if (elem.par == ParBuffer) {
          stream.println("__act." + quote(sym) + "_offset = rhs." + quote(sym) + "_offset + rhs." + quote(sym) + "_size")
        }
      case (sym, elem: DeliteForeachElem[_]) =>
      case (sym, elem: DeliteReduceElem[_]) =>
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
    }
    //XX link act frames so we can set data later
    stream.println("__act.left_act = rhs")
    stream.println(/*{*/"}")
    stream.println("def postProcInit(__act: " + actType + "): Unit = {"/*}*/) // only called for last chunk!!
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        if (elem.par == ParBuffer) {          
          stream.println("if (__act." + quote(sym) + "_offset > 0) {") 
          stream.println("val xs = {")
          emitValDef(elem.sV, "__act." + quote(sym) + "_offset + __act." + quote(sym) + "_size")
          emitValDef(elem.allocVal, "__act." + quote(sym) + "_buf")
          emitBlock(elem.buf.allocRaw)
          stream.println(quote(getBlockResult(elem.buf.allocRaw)))                 
          stream.println("}")
          stream.println("__act." + quote(sym) + "_data_set(xs)")
          stream.println("} else {")
          stream.println("__act." + quote(sym) + "_data = __act." + quote(sym) + "_buf")
          stream.println("}")
        }
      case (sym, elem: DeliteForeachElem[_]) =>
      case (sym, elem: DeliteReduceElem[_]) =>
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
    }
    stream.println(/*{*/"}")
    stream.println("def postProcess(__act: " + actType + "): Unit = {"/*}*/)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) => //FIXME: get rid of .data and adapt to new alloc style
        if (elem.par == ParBuffer) {
          // write size results from buf into data at offset
          stream.println("if (__act." + quote(sym) + "_data ne __act." + quote(sym) + "_buf) {")
          emitValDef(elem.sV, "__act." + quote(sym) + "_size")          
          emitValDef(elem.buf.aV, "__act." + quote(sym) + "_buf")
          emitValDef(elem.allocVal, "__act." + quote(sym) + "_data")
          emitValDef(elem.buf.iV, "0")
          emitValDef(elem.buf.iV2, "__act." + quote(sym) + "_offset")
          emitBlock(elem.buf.copyRaw)
          stream.println("}")
          stream.println("__act." + quote(sym) + "_buf = null")
        }
      case (sym, elem: DeliteForeachElem[_]) =>
      case (sym, elem: DeliteReduceElem[_]) =>
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
    }
    stream.println(/*{*/"}")
    stream.println("def finalize(__act: " + actType + "): Unit = {"/*}*/)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitValDef(elem.allocVal, "__act." + quote(sym) + "_data")                    
        if (elem.par == ParBuffer) {          
          if (elem.cond.nonEmpty) {
            emitValDef(elem.sV, "__act." + quote(sym) + "_conditionals")
          }
          else {
            emitValDef(elem.sV, quote(op.size))
          }          
          emitBlock(elem.buf.setSize)
        }        
        emitBlock(elem.finalizer)
        stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.finalizer)))
      case (sym, elem: DeliteForeachElem[_]) =>
      case (sym, elem: DeliteReduceElem[_]) =>
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
    }
    stream.println(/*{*/"}")


    stream.println(/*{*/"}")
    //deliteKernel = true
  }
  
  def emitAbstractFatLoopKernelExtra(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    val kernelName = symList.map(quote).mkString("")        
    stream.println("final class activation_" + kernelName + " {"/*}*/)
    stream.println("var left_act: activation_" + kernelName + " = _") // XX need to link frames
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) => 
        stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = _")
        stream.println("var " + quote(sym) + "_data: " + remap(elem.allocVal.tp) + " = _")
        if (elem.par == ParBuffer) {
          stream.println("var " + quote(sym) + "_buf: " + remap(elem.allocVal.tp) + " = _")
          stream.println("var " + quote(sym) + "_size = 0")
          stream.println("var " + quote(sym) + "_offset = 0")
          if (elem.cond.nonEmpty)
            stream.println("var " + quote(sym) + "_conditionals= 0")
          stream.println("def " + quote(sym) + "_data_set(xs: " + remap(elem.allocVal.tp) + "): Unit = {"/*}*/)
          stream.println(quote(sym) + "_data = xs")
          stream.println("if (left_act ne null) left_act." + quote(sym) + "_data_set(xs)") // XX linked frame             
          stream.println("}")                              
        }
      case (sym, elem: DeliteForeachElem[_]) =>
        stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = _")
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = _")
        stream.println("var " + quote(sym) + "_zero: " + remap(sym.tp) + " = _")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        stream.println("var " + quote(sym) + "  : " + remap(sym.tp) + " = _")
        stream.println("var " + quote(sym) + "_2: " + remap(elem.func._2.tp) + " = _")
        stream.println("var " + quote(sym) + "_zero  : " + remap(sym.tp) + " = _")
        stream.println("var " + quote(sym) + "_zero_2" + ": " + remap(elem.func._2.tp) + " = _")
    }
    stream.println(/*{*/"}")
  }
  
  override def emitFatNodeKernelExtra(sym: List[Sym[Any]], rhs: FatDef): Unit = rhs match {
    case op: AbstractFatLoop =>
      stream.println("//activation record for fat loop")
      emitAbstractFatLoopKernelExtra(op, sym)
    case _ =>
      super.emitFatNodeKernelExtra(sym, rhs)
  }

  override def emitNodeKernelExtra(sym: List[Sym[Any]], rhs: Def[Any]): Unit = rhs match {
    case op: AbstractLoop[_] =>
      stream.println("//activation record for thin loop")
      emitAbstractFatLoopKernelExtra(SimpleFatLoop(op.size, op.v, List(op.body)), sym)
    case _ =>
      super.emitNodeKernelExtra(sym, rhs)
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop =>
      if (!deliteKernel) emitInlineAbstractFatLoop(op, symList)
      else emitKernelAbstractFatLoop(op, symList)
    case _ => super.emitFatNode(symList, rhs)
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      //printlog("EMIT single "+s)
      // always wrap single tasks in methods to reduce JIT compilation unit size 
      val b = s.block
      stream.println("def " + quote(sym) + "_block = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
    }
    case op: AbstractLoop[_] => 
      // TODO: we'd like to always have fat loops but currently they are not allowed to have effects
      // if inline, wrap thin loops in methods to reduce JIT compilation unit size 
      if (!deliteKernel) {
        stream.println("def " + quote(sym) + "_thin = {")
      }
      stream.println("// a *thin* loop follows: " + quote(sym))
      emitFatNode(List(sym), SimpleFatLoop(op.size, op.v, List(op.body)))
      if (!deliteKernel) {
        stream.println(quote(sym))
        stream.println("}")
        stream.println("val " + quote(sym) + " = " + quote(sym) + "_thin")
      }
    case foreach:DeliteOpForeach2[_,_] => {
      if (deliteKernel == false){
        //stream.println("def " + quote(sym) + "_block = {")
        stream.println("val " + quote(sym) + " = {")
        stream.println("var forIdx = 0")
        stream.println("while (forIdx < " + quote(foreach.in) + ".size) { // begin foreach loop " + quote(sym))
        stream.println("val " + quote(foreach.v) + " = " + quote(foreach.in) + ".dcApply(forIdx)")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("forIdx += 1")
        stream.println("} // end foreach loop " + quote(sym))
        stream.println("}")
        //stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        //deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpForeach[" + remap(foreach.v.tp) + "] {")
        stream.println("def in = " + quote(foreach.in))
        stream.println("def sync(" + quote(foreach.i) + ": " + remap(foreach.i.tp) + ") = {")
        emitBlock(foreach.sync)
        stream.println(quote(getBlockResult(foreach.sync)))
        stream.println("}")
        stream.println("def foreach(" + quote(foreach.v) + ": " + remap(foreach.v.tp) + ") = {")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("}}")
        //deliteKernel = true
      }
    }
    case foreach:DeliteOpForeachBounded[_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("var forIdx = 0")
        stream.println("while (forIdx < " + quote(foreach.in) + ".size) { // begin foreachBounded loop " + quote(sym))
        stream.println("val " + quote(foreach.v) + " = " + quote(foreach.in) + ".dcApply(forIdx)")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("forIdx += 1")
        stream.println("} // end foreachBounded loop " + quote(sym))
        stream.println("}")
        stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        //deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpForeach[" + remap(foreach.v.tp) + "] {")
        stream.println("def in = " + quote(foreach.in))
        stream.println("def sync(" + quote(foreach.i) + ": " + remap(foreach.i.tp) + ") = {")
        emitBlock(foreach.sync)
        stream.println(quote(getBlockResult(foreach.sync)))
        stream.println("}")
        stream.println("def foreach(" + quote(foreach.v) + ": " + remap(foreach.v.tp) + ") = {")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("}}")
        //deliteKernel = true
      }
    }
    case _ => super.emitNode(sym,rhs)
  }
  
}

trait GPUGenDeliteOps extends GPUGenLoopsFat with BaseGenDeliteOps {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop =>
      if (deliteKernel) emitKernelAbstractFatLoop(op, symList)
      else emitInlineAbstractFatLoop(op, symList)
    case _ => super.emitFatNode(symList, rhs)
  }

  def emitMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    val elemFuncs = op.body flatMap { // don't emit dependencies twice!
      case elem: DeliteCollectElem[_,_,_] => elem.func :: elem.cond
      case elem: DeliteForeachElem[_] => List(elem.func)
      case elem: DeliteReduceElem[_] => elem.func :: elem.cond
      case elem: DeliteReduceTupleElem[_,_] => elem.func._1 :: elem.func._2 :: elem.cond
      case _ => throw new GenerationFailedException("GPUGen: Unsupported Elem Type!")
    }
    emitFatBlock(elemFuncs)
  }

  /*
  def emitFirstReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
      if (elem.cond.nonEmpty) {
        stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
        stream.println(quote(getBlockResult(elem.func)))
        stream.println("} else {")
        stream.println(prefixSym + quote(sym) + "_zero")
        stream.println("}")
      }
      else {
        stream.println(quote(getBlockResult(elem.func)))
      }
  }
  */

  def emitReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    if (elem.cond.nonEmpty){
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
      emitReduction(op, sym, elem, prefixSym)
      stream.println("}")
    }
    else {
      emitReduction(op, sym, elem, prefixSym)
    }
  }

 def emitReduceTupleElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceTupleElem[_,_], prefixSym: String = "") {
    if (elem.cond.nonEmpty){
      sys.error("tuple reduce with external conditions not implemented!")
    }
    else {
      emitReductionTuple(op, sym, elem, prefixSym)
    }
  }

  def emitReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    stream.println("%s %s = %s;".format(remap(elem.rV._1.tp),quote(elem.rV._1),prefixSym+quote(sym)))
    stream.println("%s %s = %s;".format(remap(elem.rV._2.tp),quote(elem.rV._2),quote(getBlockResult(elem.func))))
    emitBlock(elem.rFunc)
    stream.println(prefixSym + quote(sym) + " = " + quote(getBlockResult(elem.rFunc)) + ";")
  }

  def emitReductionTuple(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceTupleElem[_,_], prefixSym: String) {
    val rV = elem.rVSeq
    val rFunc = elem.rFuncSeq

    stream.println("%s %s = %s;".format(remap(rV._1._1.tp),quote(rV._1._1),prefixSym+quote(sym)))
    stream.println("%s %s = %s_2;".format(remap(rV._1._2.tp),quote(rV._1._2),prefixSym+quote(sym)))
    stream.println("%s %s = %s;".format(remap(rV._2._1.tp),quote(rV._2._1),quote(getBlockResult(elem.func._1))))
    stream.println("%s %s = %s;".format(remap(rV._2._2.tp),quote(rV._2._2),quote(getBlockResult(elem.func._2))))
    emitFatBlock(List(rFunc._1, rFunc._2))
    stream.println(prefixSym + quote(sym) + "   = " + quote(getBlockResult(rFunc._1)) + ";")
    stream.println(prefixSym + quote(sym) + "_2 = " + quote(getBlockResult(rFunc._2)) + ";")
  }

  def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    (symList zip op.body) foreach {
      //TODO: Check if primitive type operations
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitBlock(elem.allocN) //This will generate alloc failure exception
        throw new GenerationFailedException("GPUGen: Inlined DeliteCollectElem is not supported yet due to memory allocations.\n" + quotePos(sym))
      case (sym, elem: DeliteReduceElem[_]) =>
        emitBlock(elem.zero)
        stream.println("%s %s = %s;".format(remap(elem.zero.tp),quote(sym),quote(getBlockResult(elem.zero))))
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        emitFatBlock(List(elem.zero._1,elem.zero._2))
        stream.println("%s %s = %s;".format(remap(elem.zero._1.tp),quote(sym),quote(getBlockResult(elem.zero._1))))
        stream.println("%s %s_2 = %s;".format(remap(elem.zero._2.tp),quote(sym),quote(getBlockResult(elem.zero._2))))
      case _ =>
        throw new GenerationFailedException("GPUGen: Unsupported Elem Type!")
    }
    stream.println("int " + quote(op.v) + " = 0;")
    stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
    // body
    emitMultiLoopFuncs(op, symList)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println("//start emitReduceElem")
        emitReduceElem(op, sym, elem)
        stream.println("//end emitReduceElem")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        stream.println("//start emitReduceTupleElem")
        emitReduceTupleElem(op, sym, elem)
        stream.println("//end emitReduceTupleElem")
      case _ =>
        throw new GenerationFailedException("GPUGen: Unsupported Elem Type!")
    }
    stream.println(quote(op.v) + " += 1;")
    stream.println(/*{*/"} // end fat loop " + symList.map(quote).mkString(","))
  }

  def emitKernelAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    tabWidth += 1
    def funcNameSuffix(sym: Sym[Any]) = {
      symList.map(quote).mkString("")+"_"+quote(sym)
    }

    (symList zip op.body) foreach {
        case (sym, elem:DeliteCollectElem[_,_,_]) =>
          val lf = metaData.loopFuncs.getOrElse(sym,new LoopFunc)
          metaData.loopFuncs.put(sym,lf)
          lf.tpe = "COLLECT"

          if (elem.par == ParBuffer) throw new GenerationFailedException("GPU DeliteOps: ParBuffer is not implemented yet!")

          if(elem.cond.nonEmpty) {
            emitAllocFunc(sym,Block(Combine(List(elem.allocN,elem.finalizer).map(getBlockResultFull))),elem.allocVal,null)
            //emitAllocFunc(sym,elem.allocN,elem.allocVal,null)
            lf.loopFuncInputs = emitMultiLoopFunc(elem.func, "collect_"+funcNameSuffix(sym), List(op.v), stream)
            emitMultiLoopCond(sym, elem.cond, op.v, "cond_"+funcNameSuffix(sym), stream)
          }
          else {
            emitAllocFunc(sym,elem.allocN,elem.allocVal,op.size)
            lf.loopFuncInputs = emitMultiLoopFunc(elem.func, "collect_"+funcNameSuffix(sym), List(op.v), stream)
          }
          lf.loopFuncOutputType = remap(getBlockResult(elem.func).tp)

        //TODO: Handle when synclist exists!
        case (sym, elem:DeliteForeachElem[_]) =>
          metaData.outputs.put(sym,new TransferFunc)
          val lf = metaData.loopFuncs.getOrElse(sym,new LoopFunc)
          metaData.loopFuncs.put(sym,lf)
          lf.tpe = "FOREACH"
          lf.loopFuncInputs = emitMultiLoopFunc(elem.func, "foreach_"+funcNameSuffix(sym), List(op.v), stream)
          lf.loopFuncOutputType = remap(getBlockResult(elem.func).tp)

        case (sym, elem: DeliteReduceElem[_]) =>
          val lf = metaData.loopFuncs.getOrElse(sym,new LoopFunc)
          metaData.loopFuncs.put(sym,lf)
          lf.tpe = "REDUCE"

          lf.loopFuncInputs = emitMultiLoopFunc(elem.func, "collect_"+funcNameSuffix(sym), List(op.v), stream)
          lf.loopReduceInputs = emitMultiLoopFunc(elem.rFunc, "reduce_"+funcNameSuffix(sym), List(elem.rV._1, elem.rV._2, op.v), stream)
          lf.loopZeroInputs = emitMultiLoopFunc(elem.zero,"zero_"+funcNameSuffix(sym), Nil, stream)
          if(!isPrimitiveType(sym.tp)) {
            printDebug(sym, "DeliteReduceElem with non-primitive types is not supported.")
          } else {
            emitAllocFuncPrimitive(sym)
          }
          lf.loopFuncOutputType = remap(getBlockResult(elem.func).tp)
          if(elem.cond.nonEmpty) emitMultiLoopCond(sym, elem.cond, op.v, "cond_"+funcNameSuffix(sym), stream)

        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          val lf = metaData.loopFuncs.getOrElse(sym,new LoopFunc)
          metaData.loopFuncs.put(sym,lf)
          lf.tpe = "REDUCE_TUPLE"

          lf.loopFuncInputs = emitMultiLoopFunc(elem.func._1, "collect_1_"+funcNameSuffix(sym), List(op.v), stream)
          lf.loopFuncInputs_2 = emitMultiLoopFunc(elem.func._2, "collect_2_"+funcNameSuffix(sym), List(op.v), stream)
          lf.loopReduceInputs = emitMultiLoopFunc(elem.rFuncSeq._1, "reduce_seq_1_"+funcNameSuffix(sym), List(elem.rVSeq._1._1, elem.rVSeq._1._2, elem.rVSeq._2._1, elem.rVSeq._2._2, op.v), stream)
          lf.loopReduceInputs_2 = emitMultiLoopFunc(elem.rFuncSeq._2, "reduce_seq_2_"+funcNameSuffix(sym), List(elem.rVSeq._1._1, elem.rVSeq._1._2, elem.rVSeq._2._1, elem.rVSeq._2._2, op.v), stream)
          lf.loopReduceParInputs = emitMultiLoopFunc(elem.rFuncPar._1, "reduce_par_1_"+funcNameSuffix(sym), List(elem.rVPar._1._1, elem.rVPar._1._2, elem.rVPar._2._1, elem.rVPar._2._2, op.v), stream)
          lf.loopReduceParInputs_2 = emitMultiLoopFunc(elem.rFuncPar._2, "reduce_par_2_"+funcNameSuffix(sym), List(elem.rVPar._1._1, elem.rVPar._1._2, elem.rVPar._2._1, elem.rVPar._2._2, op.v), stream)
          lf.loopZeroInputs = emitMultiLoopFunc(elem.zero._1,"zero_1_"+funcNameSuffix(sym), Nil, stream)
          lf.loopZeroInputs_2 = emitMultiLoopFunc(elem.zero._2,"zero_2_"+funcNameSuffix(sym), Nil, stream)
          if(!isPrimitiveType(sym.tp)) {
            printDebug(sym, "DeliteReduceTupleElem with non-primitive types is not supported.")
          } else {
            emitAllocFuncPrimitive(sym)
          }
          lf.loopFuncOutputType = remap(getBlockResult(elem.func._1).tp)
          lf.loopFuncOutputType_2 = remap(getBlockResult(elem.func._2).tp)
          if(elem.cond.nonEmpty) emitMultiLoopCond(sym, elem.cond, op.v, "cond_"+funcNameSuffix(sym), stream)

          /*
        //TODO: Factor out the same key functions and indicate in DEG
        case (sym, elem: DeliteHashReduceElem[_,_,_]) =>
          val lf = metaData.loopFuncs.getOrElse(sym,new LoopFunc)
          metaData.loopFuncs.put(sym,lf)
          lf.tpe = "HASH_REDUCE"

          lf.loopFuncInputs = emitMultiLoopFunc(elem.valFunc, "valFunc_"+funcNameSuffix(sym), List(op.v), stream)
          lf.loopFuncInputs_2 = emitMultiLoopFunc(elem.keyFunc, "keyFunc_"+funcNameSuffix(sym), List(op.v), stream)
          lf.loopReduceInputs = emitMultiLoopFunc(elem.rFunc, "reduce_"+funcNameSuffix(sym), List(elem.rV._1, elem.rV._2, op.v), stream)
          lf.loopZeroInputs = emitMultiLoopFunc(elem.zero,"zero_"+funcNameSuffix(sym), Nil, stream)

          if(elem.cond.nonEmpty) emitMultiLoopCond(sym, elem.cond, op.v, "cond_"+funcNameSuffix(sym), stream)
          emitAllocFunc(sym,null,sym,op.size)
          lf.loopFuncOutputType = remap(getBlockResult(elem.valFunc).Type)
          lf.loopFuncOutputType_2 = remap(getBlockResult(elem.keyFunc).Type)
        */

        case _ =>
          throw new GenerationFailedException("GPUGen: Unsupported Elem type!")
    }
    tabWidth -= 1
    isGPUable = true
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      val b = s.block
      if (!deliteKernel) {  //In the process of generating operations for deliteKernel type kernels (allow SingleTask to be inlined)
        emitBlock(b)
        if(!isVoidType(sym.tp)) stream.println(addTab() + "%s %s = %s;".format(remap(sym.tp),quote(sym),quote(getBlockResult(b))))
      }
      else {
    	  throw new GenerationFailedException("GPUGen: DeliteOpSingleTask is not GPUable")
      }
    }

    case op: AbstractLoop[_] =>
      // TODO: we'd like to always have fat loops but currently they are not allowed to have effects
      stream.println("// a *thin* loop follows: " + quote(sym))
      emitFatNode(List(sym), SimpleFatLoop(op.size, op.v, List(op.body)))

    case _ => super.emitNode(sym,rhs)
  }

}

trait CudaGenDeliteOps extends CudaGenLoopsFat with GPUGenDeliteOps

trait OpenCLGenDeliteOps extends OpenCLGenLoopsFat with GPUGenDeliteOps

trait CGenDeliteOps extends CGenEffect with BaseGenDeliteOps
