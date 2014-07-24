package ppl.delite.framework.ops

import java.io.{FileWriter, File, PrintWriter, StringWriter}

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericCodegen, GenericFatCodegen, GenerationFailedException, CCodegen}
import ppl.delite.framework.{Config, Util, DeliteApplication}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.transform.LoopSoAOpt
import ppl.delite.framework.analysis.{StencilExp,NestedLoopMappingExp}
import scala.collection.mutable.{HashSet,HashMap}

//trait DeliteOpsExp extends BaseFatExp with EffectExp with VariablesExp with LoopsFatExp {
trait DeliteOpsExp extends BaseFatExp with EffectExp with VariablesExp with LoopsFatExp with FunctionBlocksExp with IfThenElseFatExp
    with PrimitiveOpsExp with DeliteCollectionOpsExp with DeliteReductionOpsExp with DeliteMapOpsExp with DeliteArrayFatExp with StencilExp
    with OrderingOpsExp with EqualExpBridge with CastingOpsExp with ImplicitOpsExp with WhileExp with StaticDataExp with NestedLoopMappingExp {

    val encounteredZipWith = new scala.collection.mutable.HashMap[Exp[Any], DeliteOpZipWith[_,_,_,_]]()

/*
  //may try this some time to wrap functions that are passed as case class args...
  case class FF[A,B](val x: Rep[A], val y: Rep[B])(val f: Rep[A]=>Rep[B])
  type ===>[A,B] = FF[A,B]
  implicit def deliteFunc[A:Manifest,B:Manifest](f: Rep[A]=>Rep[B]): A ===> B = { val x = fresh[A]; val y = reifyEffects(f(x)); FF(x, y)(f) }
*/


  // hack: need to pass explicit type class parameters during mirroring, similar to mtype
  def ntype[A,B](n:Numeric[A]): Numeric[B] = n.asInstanceOf[Numeric[B]]
  def otype[A,B](o:Ordering[A]): Ordering[B] = o.asInstanceOf[Ordering[B]]
  def frtype[A,B](o:Fractional[A]): Fractional[B] = o.asInstanceOf[Fractional[B]]

  /* Markers to tell Delite op code generation what kind of strategy to use.
   *
   * Where should these live?
   */
  trait DeliteParallelStrategy
  object ParFlat extends DeliteParallelStrategy { override def toString = "ParFlat" }
  object ParBuffer extends DeliteParallelStrategy { override def toString = "ParBuffer" }
  object ParSimpleBuffer extends DeliteParallelStrategy { override def toString = "ParSimpleBuffer" }

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
  class DeliteOpSingleTask[R:Manifest](block0: => Block[R], val requireInputs: Boolean = false) extends DeliteOp[R] {
    type OpType <: DeliteOpSingleTask[R]
    final lazy val block: Block[R] = copyTransformedBlockOrElse(_.block)(block0)
    val mR = manifest[R]
  }

  class DeliteOpSingleWithManifest[A:Manifest,R:Manifest](block0: => Block[R], requireInputs: Boolean = false) extends DeliteOpSingleTask[R](block0,requireInputs) {
    val mA = manifest[A]
  }

  class DeliteOpSingleWithManifest2[A:Manifest,B:Manifest,R:Manifest](block0: => Block[R], requireInputs: Boolean = false) extends DeliteOpSingleWithManifest[A,R](block0,requireInputs) {
    val mB = manifest[B]
  }

  trait DeliteOpInput[A] extends DeliteOp[A]


  /**
   * A method call to an external library.
   */
  abstract class DeliteOpExternal[A:Manifest] extends DeliteOp[A] {
    type OpType <: DeliteOpExternal[A]
    def alloc: Exp[A]
    def inputs: List[Exp[Any]] = Nil
    val funcName: String
    final lazy val allocVal: Block[A] = copyTransformedBlockOrElse(_.allocVal)(reifyEffects(alloc))
  }

  /**
   * The base class for most data parallel Delite ops.
   */
  abstract class DeliteOpLoop[A:Manifest](implicit ctx: SourceContext) extends AbstractLoop[A] with DeliteOp[A] {
    type OpType <: DeliteOpLoop[A]
    def copyBodyOrElse(e: => Def[A]): Def[A] = original.map(p=>mirrorLoopBody(p._2.asInstanceOf[OpType].body,p._1)).getOrElse(e)
    final lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(fresh[Int]).asInstanceOf[Sym[Int]]
    val numDynamicChunks:Int = 0
  }
  
  trait DeliteLoopElem {
    val numDynamicChunks:Int
  }

  //case class DeliteOpFatLoop(val size: Exp[Int], val v: Sym[Int], val body: List[Def[Any]]) extends AbstractFatLoop with DeliteFatOp

  // for use in loops:

  case class DeliteForeachElem[A:Manifest] (
    func: Block[A],
    numDynamicChunks: Int
    //sync: Block[List[Any]] // FIXME: don't want to create lists at runtime...
    // TODO: this is sort of broken right now, re-enable when we figure out how to make this work without emitting dependencies twice
    //cond: List[Exp[Boolean]] = Nil
  ) extends Def[Unit] with DeliteLoopElem {
    val mA = manifest[A]
  }

  // used only for ParBuffer operations
  // dc_append, dc_set_logical_size, dc_alloc, and dc_copy only need to be
  // overridden if the DeliteParallelStrategy is ParBuffer
  case class DeliteBufferElem[A:Manifest, I:Manifest, CA:Manifest](
    // -- bound vars
    eV: Sym[A], //element to be added
    sV: Sym[Int], //size
    allocVal: Sym[I], //primary allocated collection
    aV2: Sym[I], //secondary allocated collection
    iV: Sym[Int], //start index
    iV2: Sym[Int], //end index

    //collection functions
    alloc: Block[I],
    apply: Block[A],
    update: Block[Unit],
    appendable: Block[Boolean],
    append: Block[Unit],
    setSize: Block[Unit],
    allocRaw: Block[I],
    copyRaw: Block[Unit],
    finalizer: Block[CA]
  ) {
    val mA = manifest[A]
    val mI = manifest[I]
    val mCA = manifest[CA]
  }

  case class DeliteCollectElem[A:Manifest, I <: DeliteCollection[A]:Manifest, CA <: DeliteCollection[A]:Manifest](
    func: Block[A],
    cond: List[Block[Boolean]] = Nil,
    par: DeliteParallelStrategy,
    buf: DeliteBufferElem[A,I,CA],
    iFunc: Option[Block[DeliteCollection[A]]] = None, //TODO: is there a cleaner way to merge flatMap functionality with Collect?
    iF: Option[Sym[Int]] = None,
    sF: Option[Block[Int]] = None,
    eF: Option[Sym[DeliteCollection[A]]] = None,
    numDynamicChunks: Int
  ) extends Def[CA] with DeliteLoopElem {
    val mA = manifest[A]
    val mI = manifest[I]
    val mCA = manifest[CA]
    val mDCA = manifest[DeliteCollection[A]]
  }

  case class DeliteReduceElem[A:Manifest](
    func: Block[A],
    cond: List[Block[Boolean]] = Nil,
    zero: Block[A],
    accInit: Block[A],
    rV: (Sym[A], Sym[A]),
    rFunc: Block[A],
    stripFirst: Boolean,
    numDynamicChunks: Int
  ) extends Def[A] with DeliteLoopElem {
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
    stripFirst: Boolean,
    numDynamicChunks: Int
  ) extends Def[A] with DeliteLoopElem {
    val mA = manifest[A]
    val mB = manifest[B]
  }

  abstract class DeliteHashElem[K,CV] extends Def[CV] {
    def keyFunc: Block[K]
    def cond: List[Block[Boolean]]
  }

  case class DeliteHashCollectElem[K:Manifest,V:Manifest,I:Manifest,CV:Manifest,CI:Manifest,CCV:Manifest](
    keyFunc: Block[K],
    valFunc: Block[V],
    cond: List[Block[Boolean]] = Nil,
    buf: DeliteBufferElem[I,CI,CCV],
    iBuf: DeliteBufferElem[V,I,CV],
    iBufSize: Block[Int],
    numDynamicChunks: Int
  ) extends DeliteHashElem[K,CCV] with DeliteLoopElem {
    val mK = manifest[K]
    val mV = manifest[V]
    val mI = manifest[I]
    val mCV = manifest[CV]
    val mCI = manifest[CI]
    val mCCV = manifest[CCV]
  }

  case class DeliteHashReduceElem[K:Manifest,V:Manifest,I:Manifest,CV:Manifest](
    keyFunc: Block[K],
    valFunc: Block[V],
    cond: List[Block[Boolean]] = Nil,
    zero: Block[V], //TODO: necessary?
    rV: (Sym[V], Sym[V]),
    rFunc: Block[V],
    buf: DeliteBufferElem[V,I,CV],
    numDynamicChunks: Int
  ) extends DeliteHashElem[K,CV] with DeliteLoopElem {
    val mK = manifest[K]
    val mV = manifest[V]
    val mI = manifest[I]
    val mCV = manifest[CV]
  }

  case class DeliteHashIndexElem[K:Manifest,CV:Manifest](
    keyFunc: Block[K],
    cond: List[Block[Boolean]] = Nil,
    numDynamicChunks: Int
  ) extends DeliteHashElem[K,CV] with DeliteLoopElem {
    val mK = manifest[K]
    val mCV = manifest[CV]
  }

  def unusedBlock() = reifyEffectsHere(fatal(unit("emitted unused block in Multiloop")))
  def unusedSym() = Sym(-10)


  def loopBodyNeedsStripFirst[A](e: Def[A]) = e match {
    case e:DeliteReduceElem[_] => e.stripFirst
    case e:DeliteReduceTupleElem[_,_] => e.stripFirst
    case _ => false
  }

  def loopBodyNeedsCombine[A](e: Def[A]) = e match {
    case e:DeliteReduceElem[_] => true
    case e:DeliteReduceTupleElem[_,_] => true
    case e:DeliteHashReduceElem[_,_,_,_] => true
    case e:DeliteHashCollectElem[_,_,_,_,_,_] => true
    case e:DeliteHashIndexElem[_,_] => true
    case e:DeliteCollectElem[_,_,_] => false //e.par == ParBuffer //e.cond.nonEmpty
    case _ => false
  }
  def loopBodyNumDynamicChunks[A](e: Def[A]) = e match {
    case e:DeliteLoopElem => e.numDynamicChunks
    case _ => 0
  }

  def loopBodyNeedsPostProcess[A](e: Def[A]) = e match {
    case e:DeliteCollectElem[_,_,_] => e.par == ParBuffer || e.par == ParSimpleBuffer //e.cond.nonEmpty
    case e:DeliteHashCollectElem[_,_,_,_,_,_] => true
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
   *    if stripFirst is set to false, i.e. for a mutable reduction, then the accInit value is used
   *    to allocate the accumulator, and it IS used in the initial reduction.
   */

   /**
    * DeliteOpMapLike is the base type for all Delite ops with collect elem bodies.
    *
    * It now supports allocating a result of type I (which will be modified during construction) and
    * returning a result of type CA by invoking the 'finalizer' method.
    */
    abstract class DeliteOpMapLike[A:Manifest, I <: DeliteCollection[A]:Manifest, CA <: DeliteCollection[A]:Manifest](implicit ctx: SourceContext) extends DeliteOpLoop[CA] {
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
      final lazy val aV2: Sym[I] = copyTransformedOrElse(_.aV2)(fresh[I]).asInstanceOf[Sym[I]]

      lazy val buf = DeliteBufferElem(
        eV = this.eV,
        sV = this.sV,
        iV = this.iV,
        iV2 = this.iV2,
        allocVal = this.allocVal,
        aV2 = this.aV2,
        alloc = reifyEffects(this.alloc(sV)),
        apply = unusedBlock, //reifyEffects(dc_apply(allocVal,v)),
        update = reifyEffects(dc_update(allocVal,v,eV)),
        append = reifyEffects(dc_append(allocVal,v,eV)),
        appendable = reifyEffects(dc_appendable(allocVal,v,eV)),
        setSize = reifyEffects(dc_set_logical_size(allocVal,sV)),
        allocRaw = reifyEffects(dc_alloc[A,I](allocVal,sV)),
        copyRaw = reifyEffects(dc_copy(aV2,iV,allocVal,iV2,sV)),
        finalizer = reifyEffects(this.finalizer(allocVal))
      )
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
                             B:Manifest, CB <: DeliteCollection[B]:Manifest](implicit ctx: SourceContext)
    extends DeliteOpMapI[A,B,CB,CB] {
    type OpType <: DeliteOpMap[A,B,CB]

    def finalizer(x: Exp[CB]) = x
  }

  abstract class DeliteOpMapI[A:Manifest,B:Manifest,I <: DeliteCollection[B]:Manifest,CB <: DeliteCollection[B]:Manifest](implicit ctx: SourceContext)
    extends DeliteOpMapLike[B,I,CB] {
    type OpType <: DeliteOpMapI[A,B,I,CB]

    // supplied by subclass
    val in: Exp[DeliteCollection[A]]
    //val size: Exp[Int] // could be dc_size(in), but we want type-specific pattern matching to work
    def func: Exp[A] => Exp[B]
    //val numChunks: Int

    // bound var for map function, may be required by transformers
    lazy val fin: Exp[A] = copyTransformedOrElse(_.fin)(dc_apply(in,v))

    // loop
    lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[B,I,CB](
      func = reifyEffects(this.func(fin)),
      //numChunks = this.numChunks,
      par = dc_parallelization(allocVal, false),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmB = manifest[B]
    val dmI = manifest[I]
    val dmCB = manifest[CB]
  }

  abstract class DeliteOpFlatMap[A:Manifest, B:Manifest, CB<:DeliteCollection[B]:Manifest](implicit ctx: SourceContext)
    extends DeliteOpFlatMapI[A,B,CB,CB] {
    type OpType <: DeliteOpFlatMap[A,B,CB]

    def finalizer(x: Exp[CB]) = x
  }

  abstract class DeliteOpFlatMapI[A:Manifest, B:Manifest, I<:DeliteCollection[B]:Manifest, CB<:DeliteCollection[B]:Manifest](implicit ctx: SourceContext)
    extends DeliteOpMapLike[B,I,CB] {
    type OpType <: DeliteOpFlatMapI[A,B,I,CB]

    // supplied by subclass
    val in: Exp[DeliteCollection[A]]
    def func: Exp[A] => Exp[DeliteCollection[B]]

    final lazy val iFunc: Exp[DeliteCollection[B]] = copyTransformedOrElse(_.iFunc)(func(dc_apply(in,v)))
    final lazy val iF: Sym[Int] = copyTransformedOrElse(_.iF)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val eF: Sym[DeliteCollection[B]] = copyTransformedOrElse(_.eF)(fresh[DeliteCollection[B]](iFunc.tp)).asInstanceOf[Sym[DeliteCollection[B]]]

    // loop
    lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[B,I,CB](
      iFunc = Some(reifyEffects(this.iFunc)),
      iF = Some(this.iF),
      sF = Some(reifyEffects(dc_size(eF))), //note: applying dc_size directly to iFunc can lead to iFunc being duplicated (during mirroring?)
      eF = Some(this.eF),
      func = reifyEffects(dc_apply(eF,iF)),
      par = dc_parallelization(allocVal, true),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmB = manifest[B]
    val dmI = manifest[I]
    val dmCB = manifest[CB]
  }

  abstract class DeliteOpMapIndices[A:Manifest,CA <: DeliteCollection[A]:Manifest](implicit ctx: SourceContext) extends DeliteOpMapIndicesI[A,CA,CA] {
    type OpType <: DeliteOpMapIndices[A,CA]
    def finalizer(x: Exp[CA]) = x
  }

  abstract class DeliteOpMapIndicesI[A:Manifest,I <: DeliteCollection[A]:Manifest,CA <: DeliteCollection[A]:Manifest](implicit ctx: SourceContext)
    extends DeliteOpMapLike[A,I,CA] {
    type OpType <: DeliteOpMapIndicesI[A,I,CA]

    def func: Exp[Int] => Exp[A]

    lazy val body: Def[CA] = copyBodyOrElse(DeliteCollectElem[A,I,CA](
      func = reifyEffects(this.func(v)),
      par = dc_parallelization(allocVal, false),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmI = manifest[I]
    val dmCA = manifest[CA]
  }

  /**
   *  Currently conditionally appends values to buffers, which are concatenated in the combine stage.
   *  Note that it is also implicitly a Map-Filter (it accepts a mapping function). Should this be renamed?
   */
  abstract class DeliteOpFilter[A:Manifest,
                                B:Manifest, CB <: DeliteCollection[B]:Manifest](implicit ctx: SourceContext)
    extends DeliteOpFilterI[A,B,CB,CB] {
    type OpType <: DeliteOpFilter[A,B,CB]

    def finalizer(x: Exp[CB]) = x
  }

  abstract class DeliteOpFilterI[A:Manifest,
                                B:Manifest, I <: DeliteCollection[B]:Manifest, CB <: DeliteCollection[B]:Manifest](implicit ctx: SourceContext)
    extends DeliteOpMapLike[B,I,CB] {
    type OpType <: DeliteOpFilterI[A,B,I,CB]

    // supplied by subclass
    val in: Exp[DeliteCollection[A]]
    //val size: Exp[Int] // could be dc_size(in), but we want type-specific pattern matching to work
    def func: Exp[A] => Exp[B]
    def cond: Exp[A] => Exp[Boolean] // does this need to be more general (i.e. a List?)

    // loop
    lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[B,I,CB](
      func = reifyEffects(this.func(dc_apply(in,v))),
      cond = reifyEffects(this.cond(dc_apply(in,v)))::Nil,
      par = dc_parallelization(allocVal, true),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmB = manifest[B]
    val dmI = manifest[I]
    val dmCB = manifest[CB]
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
                                 R:Manifest, CR <: DeliteCollection[R]:Manifest](implicit ctx: SourceContext)
    extends DeliteOpZipWithI[A,B,R,CR,CR] {
    type OpType <: DeliteOpZipWith[A,B,R,CR]

    def finalizer(x: Exp[CR]) = x
  }

  abstract class DeliteOpZipWithI[A:Manifest,
                                 B:Manifest,
                                 R:Manifest,
                                 I <: DeliteCollection[R]:Manifest, CR <: DeliteCollection[R]:Manifest](implicit ctx: SourceContext)
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
      func = reifyEffects(this.func(fin._1,fin._2)),
      par = dc_parallelization(allocVal, false),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmB = manifest[B]
    val dmR = manifest[R]
    val dmI = manifest[I]
    val dmCR = manifest[CR]
  }


  abstract class DeliteOpReduceLike[A:Manifest](implicit ctx: SourceContext) extends DeliteOpLoop[A] {
    type OpType <: DeliteOpReduceLike[A]
    final lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)((if (mutable) reflectMutableSym(fresh[A]) else fresh[A], fresh[A])) // TODO: transform vars??
    val mutable: Boolean = false
    val stripFirst = !isPrimitiveType(manifest[A]) && !this.mutable

    def accInit: Exp[A] = fatal(unit("DeliteOpReduce accInit called without any implementation on " + manifest[A].toString))
  }

  /**
   * Parallel reduction of a DeliteCollection[A]. Reducing function must be associative.
   *
   * @param  in    the input collection
   * @param  size  the size of the input collection
   * @param  zero  the "empty" value - must have value equality
   * @param  func  the reduction function; ([Exp[A],Exp[A]) => Exp[A]. Must be associative.
   */
  abstract class DeliteOpReduce[A:Manifest](implicit ctx: SourceContext) extends DeliteOpReduceLike[A] {
    type OpType <: DeliteOpReduce[A]

    // supplied by subclass
    val in: Exp[DeliteCollection[A]]
    def zero: Exp[A]
    def func: (Exp[A], Exp[A]) => Exp[A]
    
    // loop
    lazy val body: Def[A] = copyBodyOrElse(DeliteReduceElem[A](
      func = reifyEffects(dc_apply(in,v)),
      zero = reifyEffects(this.zero),
      accInit = if (isPrimitiveType(manifest[A])) reifyEffects(zero) else reifyEffects(this.accInit),
      rV = this.rV,
      rFunc = reifyEffects(this.func(rV._1, rV._2)),
      stripFirst = this.stripFirst,
      numDynamicChunks = this.numDynamicChunks
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
  abstract class DeliteOpMapReduce[A:Manifest,R:Manifest](implicit ctx: SourceContext)
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
      accInit = if (isPrimitiveType(manifest[R])) reifyEffects(zero) else reifyEffects(this.accInit),
      rV = this.rV,
      rFunc = reifyEffects(reduce(rV._1, rV._2)),
      stripFirst = this.stripFirst,
      numDynamicChunks = this.numDynamicChunks
    ))
  }

  // should this be folded into DeliteOpMapReduce (or into DeliteOpFilter)?
  abstract class DeliteOpFilterReduce[A:Manifest,R:Manifest](implicit ctx: SourceContext)
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
      accInit = if (isPrimitiveType(manifest[R])) reifyEffects(zero) else reifyEffects(this.accInit),
      rV = this.rV,
      rFunc = reifyEffects(reduce(rV._1, rV._2)),
      stripFirst = this.stripFirst,
      numDynamicChunks = this.numDynamicChunks
    ))
  }


  // reduce tuple in parallel, return first component
  abstract class DeliteOpFilterReduceFold[R:Manifest](implicit ctx: SourceContext)
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
      stripFirst = false, //(!isPrimitiveType(manifest[R]) || !isPrimitiveType(manifest[R])) && !this.mutable
      numDynamicChunks = this.numDynamicChunks
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
  abstract class DeliteOpZipWithReduce[A:Manifest,B:Manifest,R:Manifest](implicit ctx: SourceContext)
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
      accInit = if (isPrimitiveType(manifest[R])) reifyEffects(zero) else reifyEffects(this.accInit),
      rV = this.rV,
      rFunc = reifyEffects(reduce(rV._1, rV._2)),
      stripFirst = this.stripFirst,
      numDynamicChunks = this.numDynamicChunks
    ))
  }

  // reduce tuple in parallel, return first component
  abstract class DeliteOpZipWithReduceTuple[A:Manifest,B:Manifest,R:Manifest,Q:Manifest](implicit ctx: SourceContext)
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
      stripFirst = (!isPrimitiveType(manifest[R]) || !isPrimitiveType(manifest[R])) && !this.mutable,
      numDynamicChunks = this.numDynamicChunks
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
  abstract class DeliteOpForeach[A:Manifest](implicit ctx: SourceContext) extends DeliteOpLoop[Unit] { //DeliteOp[Unit] {
    type OpType <: DeliteOpForeach[A]
    val in: Exp[DeliteCollection[A]]
    val size: Exp[Int]
    def func: Exp[A] => Exp[Unit]
    def sync: Exp[Int] => Exp[List[Any]] // TODO: need to extend runtime to do something with sync in multiloop

    final lazy val i: Sym[Int] = copyOrElse(_.i)(fresh[Int])
    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = reifyEffects(this.func(dc_apply(in,v))),
      numDynamicChunks = this.numDynamicChunks
      //sync = reifyEffects(this.sync(i))
    ))
  }

  /**
   * Parallel foreach with possible global reductions (instances of DeliteReduction) in the body.
   * DeliteReductions are factored out into separate ReduceElem bodies, and the result of the reduction
   * is written to their input vars after the foreach completes.
   *
   * @param  in     the input collection
   * @param  size   the size of the input collection
   * @param  func   the foreach function Exp[A] => Exp[Unit]
   */
  abstract class DeliteOpForeachReduce[A:Manifest](implicit ctx: SourceContext) extends DeliteOp[Unit]  { //DeliteOpLoop[Unit] {
    type OpType <: DeliteOpForeachReduce[A]
    val in: Exp[DeliteCollection[A]]
    val size: Exp[Int]
    def func: Exp[A] => Exp[Unit]

    final lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(fresh[Int]).asInstanceOf[Sym[Int]]

    /* expand func body for proper effect tracking */
    lazy val funcBody: Block[Unit] = copyTransformedBlockOrElse(_.funcBody)(reifyEffects(this.func(dc_apply(in,v))))

    /**
     * DeliteOpForeachReduce instances eventually get transformed into a composite of foreach and reduce elements.
     * See ForeachReduceTransformer.scala. Here we grab the reductions to be used later.
     */
    lazy val (xFunc, deliteReductions) = funcBody match {
      case Block(Def(Reify(x,u,es))) =>
        val reductions = es.collect(e => e match { case Def(Reflect(r:DeliteReduction[_,_],_,_)) => r })
        if (reductions.length == 0) {
          (funcBody, Nil)
        }
        else {
          (Block(Reify(Const(),u,es)), reductions)
        }
     }
  }

  abstract class DeliteOpIndexedLoop(implicit ctx: SourceContext) extends DeliteOpLoop[Unit] {
    type OpType <: DeliteOpIndexedLoop
    val size: Exp[Int]
    def func: Exp[Int] => Exp[Unit]

    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = reifyEffects(this.func(v)),
      numDynamicChunks = this.numDynamicChunks
      //sync = reifyEffects(unit(List()))
    ))
  }

  abstract class DeliteOpHashCollectLike[K:Manifest, V:Manifest, I:Manifest, CV:Manifest, CI:Manifest, CCV: Manifest](implicit ctx: SourceContext) extends DeliteOpLoop[CCV] {
    type OpType <: DeliteOpHashCollectLike[K,V,I,CV,CI,CCV]
    final lazy val allocVal: Sym[CI] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[CI])).asInstanceOf[Sym[CI]]
    final lazy val aV2: Sym[CI] = copyTransformedOrElse(_.aV2)(fresh[CI]).asInstanceOf[Sym[CI]]
    final lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val iV2: Sym[Int] = copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val iiV: Sym[Int] = copyTransformedOrElse(_.iiV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val iiV2: Sym[Int] = copyTransformedOrElse(_.iiV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val eV: Sym[V] = copyTransformedOrElse(_.eV)(fresh[V]).asInstanceOf[Sym[V]]

    def alloc(i: Exp[Int]): Exp[CI]
    def allocI(i: Exp[Int]): Exp[I]
    def finalizer(x: Exp[CI]): Exp[CCV]
    def finalizerI(x: Exp[I]): Exp[CV]
  }

  abstract class DeliteOpBuildIndex[A:Manifest, K:Manifest, CV:Manifest](implicit ctx: SourceContext) extends DeliteOpLoop[CV] {
    type OpType <: DeliteOpBuildIndex[A,K,CV]

    val in: Exp[DeliteCollection[A]]
    def keyFunc: Exp[A] => Exp[K]
    def cond: Exp[A] => Exp[Boolean]

    lazy val body: Def[CV] = copyBodyOrElse(DeliteHashIndexElem[K,CV](
      keyFunc = reifyEffects(this.keyFunc(dc_apply(in,v))),
      cond = if (this.cond == null) Nil else reifyEffects(this.cond(dc_apply(in,v)))::Nil,
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmK = manifest[K]
    val dmCV = manifest[CV]
  }

  abstract class DeliteOpGroupBy[K:Manifest, V: Manifest, CV<:DeliteCollection[V]:Manifest, CCV<:DeliteCollection[CV]:Manifest](implicit ctx: SourceContext) extends DeliteOpMappedGroupBy[V,K,V,CV,CCV] {
    type OpType <: DeliteOpGroupBy[K,V,CV,CCV]
    val in: Exp[DeliteCollection[V]]
    def valFunc: Exp[V] => Exp[V] = v => v
  }

  abstract class DeliteOpMappedGroupBy[A:Manifest, K:Manifest, V:Manifest, CV<:DeliteCollection[V]:Manifest, CCV<:DeliteCollection[CV]:Manifest](implicit ctx: SourceContext) extends DeliteOpFilteredGroupBy[A,K,V,CV,CCV] {
    type OpType <: DeliteOpMappedGroupBy[A,K,V,CV,CCV]
    val in: Exp[DeliteCollection[A]]
    def cond: Exp[A] => Exp[Boolean] = null
  }

  abstract class DeliteOpFilteredGroupBy[A:Manifest, K:Manifest, V:Manifest, CV<:DeliteCollection[V]:Manifest, CCV<:DeliteCollection[CV]:Manifest](implicit ctx: SourceContext) extends DeliteOpFilteredGroupByI[A,K,V,CV,CV,CCV,CCV] {
    type OpType <: DeliteOpFilteredGroupBy[A,K,V,CV,CCV]
    def finalizer(x: Exp[CCV]) = x
    def finalizerI(x: Exp[CV]) = x
  }

  abstract class DeliteOpFilteredGroupByI[A:Manifest, K:Manifest, V:Manifest, I<:DeliteCollection[V]:Manifest, CV<:DeliteCollection[V]:Manifest, CI<:DeliteCollection[I]:Manifest, CCV<:DeliteCollection[CV]:Manifest](implicit ctx: SourceContext) extends DeliteOpHashCollectLike[K,V,I,CV,CI,CCV] {
    type OpType <: DeliteOpFilteredGroupByI[A,K,V,I,CV,CI,CCV]
    val in: Exp[DeliteCollection[A]]

    def keyFunc: Exp[A] => Exp[K]
    def valFunc: Exp[A] => Exp[V]
    def cond: Exp[A] => Exp[Boolean]

    final lazy val ibufVal: Exp[I] = 
      if (Config.soaEnabled) {
        copyTransformedOrElse(_.ibufVal)(dc_apply(allocVal,iV)) // this is necessary to make nested collections with SoA work properly (rewrites can't kick in on a bound sym)
      }
      else {
        copyTransformedOrElse(_.ibufVal)(reflectMutableSym(fresh[I]).asInstanceOf[Sym[I]])
      }
    final lazy val ibufVal2: Exp[I] = 
      if (Config.soaEnabled) {
        copyTransformedOrElse(_.ibufVal2)(dc_apply(aV2,iV2))
      }
      else {
        copyTransformedOrElse(_.ibufVal2)(reflectMutableSym(fresh[I]).asInstanceOf[Sym[I]]) 
      }

    lazy val body: Def[CCV] = copyBodyOrElse(DeliteHashCollectElem[K,V,I,CV,CI,CCV](
      keyFunc = reifyEffects(this.keyFunc(dc_apply(in,v))),
      valFunc = reifyEffects(this.valFunc(dc_apply(in,v))),
      cond = if (this.cond == null) Nil else reifyEffects(this.cond(dc_apply(in,v)))::Nil,
      iBufSize = reifyEffects(dc_size(ibufVal2)),
      iBuf = DeliteBufferElem(
        eV = this.eV,
        sV = this.sV,
        iV = this.iiV,
        iV2 = this.iiV2,
        allocVal = if (Config.soaEnabled) unusedSym else this.ibufVal.asInstanceOf[Sym[I]],
        aV2 = if (Config.soaEnabled) unusedSym else this.ibufVal2.asInstanceOf[Sym[I]],
        alloc = reifyEffects(this.allocI(sV)),
        apply = unusedBlock, 
        update = unusedBlock,
        appendable = unusedBlock,
        append = reifyEffects(dc_append(ibufVal,v,eV)), // with SoA enabled, this short-circuits directly to contents of allocVal
        setSize = reifyEffects(dc_set_logical_size(ibufVal,sV)),
        allocRaw = if (Config.soaEnabled) unusedBlock else reifyEffects(dc_apply(this.aV2,this.iV2)), // co-opting this field to tunnel the bound apply through
        copyRaw = reifyEffects(dc_copy(ibufVal2,iiV2,ibufVal,iiV,sV)),
        finalizer = reifyEffects(this.finalizerI(ibufVal))
      ),
      buf = DeliteBufferElem(
        eV = unusedSym,
        sV = this.sV,
        iV = this.iV,
        iV2 = this.iV2,
        allocVal = this.allocVal,
        aV2 = this.aV2,
        alloc = reifyEffects(this.alloc(sV)),
        apply = if (Config.soaEnabled) unusedBlock else reifyEffects(dc_apply(allocVal,iV)),
        // update = reifyEffects(dc_update(this.allocVal,iV,dc_alloc[V,I](ibufVal,sV))), // why does this use dc_alloc on iBufVal, while append below uses allocI? this one is apparently used only in postProcess, while append is used in process.
        update = if (Config.soaEnabled) reifyEffects(dc_update(this.allocVal,iV,dc_alloc[V,I](ibufVal,sV))) else reifyEffects(dc_update(this.allocVal,iV,ibufVal.unsafeImmutable)), 
        appendable = unusedBlock,
        // append = reifyEffects(dc_append(this.allocVal,v,this.allocI(sV))), // without SoA, we lose the mutable allocI here. why is the allocI (iBuf allocation) hidden down here instead of using iBuf.alloc above? must have something to do with the rewrites on iBufVal..
        append = if (Config.soaEnabled) reifyEffects(dc_append(this.allocVal,v,this.allocI(sV))) else reifyEffects(dc_append(this.allocVal,v,ibufVal.unsafeImmutable)), 
        setSize = reifyEffects(dc_set_logical_size(this.allocVal,sV)),
        allocRaw = reifyEffects(dc_alloc[I,CI](this.allocVal,sV)),
        copyRaw = unusedBlock,
        finalizer = reifyEffects(this.finalizer(this.allocVal))
      ),
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmK = manifest[K]
    val dmV = manifest[V]
    val dmI = manifest[I]
    val dmCV = manifest[CV]
    val dmCI = manifest[CI]
    val dmCCV = manifest[CCV]
  }

  abstract class DeliteOpHashReduceLike[K:Manifest, V:Manifest, I<:DeliteCollection[V]:Manifest, CV<:DeliteCollection[V]:Manifest](implicit ctx: SourceContext) extends DeliteOpLoop[CV] {
    type OpType <: DeliteOpHashReduceLike[K,V,I,CV]
    final lazy val rV: (Sym[V],Sym[V]) = copyOrElse(_.rV)((fresh[V], fresh[V]))
    final lazy val allocVal: Sym[I] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[I])).asInstanceOf[Sym[I]]
    final lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val eV: Sym[V] = copyTransformedOrElse(_.eV)(fresh[V]).asInstanceOf[Sym[V]]
    def alloc(i: Exp[Int]): Exp[I]
    def finalizer(x: Exp[I]): Exp[CV]
  }

  abstract class DeliteOpGroupByReduce[K:Manifest, V:Manifest, CV <: DeliteCollection[V]:Manifest](implicit ctx: SourceContext) extends DeliteOpMappedGroupByReduce[V,K,V,CV] {
    type OpType <: DeliteOpGroupByReduce[K,V,CV]
    def valFunc: Exp[V] => Exp[V] = v => v
  }

  abstract class DeliteOpMappedGroupByReduce[A:Manifest, K:Manifest, V:Manifest, CV <: DeliteCollection[V]:Manifest](implicit ctx: SourceContext) extends DeliteOpFilteredGroupByReduce[A,K,V,CV] {
    type OpType <: DeliteOpMappedGroupByReduce[A,K,V,CV]
    def cond: Exp[A] => Exp[Boolean] = null
  }

  abstract class DeliteOpFilteredGroupByReduce[A:Manifest, K:Manifest, V:Manifest, CV<:DeliteCollection[V]:Manifest](implicit ctx: SourceContext) extends DeliteOpFilteredGroupByReduceI[A,K,V,CV,CV] {
    type OpType <: DeliteOpFilteredGroupByReduce[A,K,V,CV]
    def finalizer(x: Exp[CV]) = x
  }

  abstract class DeliteOpFilteredGroupByReduceI[A:Manifest, K:Manifest, V:Manifest, I<:DeliteCollection[V]:Manifest, CV<:DeliteCollection[V]:Manifest](implicit ctx: SourceContext) extends DeliteOpHashReduceLike[K,V,I,CV] {
    type OpType <: DeliteOpFilteredGroupByReduceI[A,K,V,I,CV]
    val in: Exp[DeliteCollection[A]]

    def keyFunc: Exp[A] => Exp[K]
    def valFunc: Exp[A] => Exp[V]
    def reduceFunc: (Exp[V], Exp[V]) => Exp[V]
    def zero: Exp[V]
    def cond: Exp[A] => Exp[Boolean]

    lazy val body: Def[CV] = copyBodyOrElse(DeliteHashReduceElem[K,V,I,CV](
      keyFunc = reifyEffects(this.keyFunc(dc_apply(in,v))),
      valFunc = reifyEffects(this.valFunc(dc_apply(in,v))),
      cond = if (this.cond == null) Nil else reifyEffects(this.cond(dc_apply(in,v)))::Nil,
      zero = reifyEffects(this.zero),
      rV = this.rV,
      rFunc = reifyEffects(reduceFunc(rV._1, rV._2)),
      buf = DeliteBufferElem(
        eV = this.eV,
        sV = this.sV,
        iV = this.iV,
        iV2 = unusedSym,
        allocVal = this.allocVal,
        aV2 = unusedSym,
        alloc = reifyEffects(this.alloc(sV)),
        apply = reifyEffects(dc_apply(allocVal,iV)),
        update = reifyEffects(dc_update(allocVal,iV,eV)),
        appendable = unusedBlock, //reifyEffects(dc_appendable(allocVal,v,eV)),
        append = reifyEffects(dc_append(allocVal,v,eV)),
        setSize = reifyEffects(dc_set_logical_size(allocVal,sV)),
        allocRaw = unusedBlock, //reifyEffects(dc_alloc[V,I](allocVal,sV)),
        copyRaw = unusedBlock, //reifyEffects(dc_copy(aV2,iV,allocVal,iV2,sV)),
        finalizer = reifyEffects(this.finalizer(allocVal))
      ),
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmK = manifest[K]
    val dmV = manifest[V]
    val dmI = manifest[I]
    val dmCV = manifest[CV]
  }


  /**
   * Deprecated Delite ops
   */

  /*
   * We temporarily need these two versions of 'foreach' until we get 'sync' working with the new version.
   */

  @deprecated("DeliteOpForeach2 should only be used if sync is required. It will be removed as soon as sync works with DeliteOpForeach", "") // TODO: swap names with DeliteOpForeach
  abstract class DeliteOpForeach2[A,C[X] <: DeliteCollection[X]]() extends DeliteOp[Unit] {
    val in: Exp[C[A]]
    val v: Sym[A]
    val func: Block[Unit]
    val i: Sym[Int]
    val sync: Block[List[Any]]

    lazy val alloc = Const()    
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

  def summarizeBody[A](d: Def[A]) = d match {
    case e: DeliteForeachElem[_] => summarizeEffects(e.func).star
    case e: DeliteHashCollectElem[_,_,_,_,_,_] => (summarizeEffects(e.keyFunc) andAlso summarizeEffects(e.valFunc)).star
    case e: DeliteHashReduceElem[_,_,_,_] => (summarizeEffects(e.keyFunc) andAlso summarizeEffects(e.valFunc) andAlso summarizeEffects(e.rFunc)).star // TODO writable reduce
    case e: DeliteHashIndexElem[_,_] => summarizeEffects(e.keyFunc).star
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
    case x: DeliteOpExternal[_] =>
      x.allocVal
      super.reflectEffect(d,u)
    case _ =>
      super.reflectEffect(d,u)
  }

  // HACK lazy val bites again: must make sure that block is evaluated!
  override def reflectMirrored[A:Manifest](zd: Reflect[A])(implicit pos: SourceContext): Exp[A] = zd match {
    case Reflect(x:DeliteOpSingleTask[_], u, es) =>
      x.block
      super.reflectMirrored(zd)
    case Reflect(x: DeliteOpLoop[_], u, es) =>
      val z = x.body  //  <-- somehow not always evaluated? lazy val extends a strict val, what are the semantics?
      super.reflectMirrored(zd)
    case Reflect(x: DeliteOpExternal[_], u, es) =>
      x.allocVal
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

    def mirrorBuffer[A,I,CA](e: DeliteBufferElem[A,I,CA]) =
      DeliteBufferElem[A,I,CA](
        eV = f(e.eV).asInstanceOf[Sym[A]],
        sV = f(e.sV).asInstanceOf[Sym[Int]],
        allocVal = f(e.allocVal).asInstanceOf[Sym[I]],
        aV2 = f(e.aV2).asInstanceOf[Sym[I]],
        iV = f(e.iV).asInstanceOf[Sym[Int]],
        iV2 = f(e.iV2).asInstanceOf[Sym[Int]],
        alloc = fb(e.alloc)(e.mI),
        apply = fb(e.apply)(e.mA),
        update = fb(e.update)(manifest[Unit]),
        appendable = fb(e.appendable)(manifest[Boolean]),
        append = fb(e.append)(manifest[Unit]),
        setSize = fb(e.setSize)(manifest[Unit]),
        allocRaw = fb(e.allocRaw)(e.mI),
        copyRaw = fb(e.copyRaw)(manifest[Unit]),
        finalizer = fb(e.finalizer)(e.mCA)
      )(e.mA,e.mI,e.mCA)

    d match {
      case e: DeliteHashCollectElem[k,v,i,cv,ci,ccv] =>
        (DeliteHashCollectElem[k,v,i,cv,ci,ccv](
          keyFunc = fb(e.keyFunc)(e.mK),
          valFunc = fb(e.valFunc)(e.mV),
          cond = e.cond.map(fb(_)(manifest[Boolean])),
          buf = mirrorBuffer(e.buf),
          iBuf = mirrorBuffer(e.iBuf),
          iBufSize = fb(e.iBufSize)(manifest[Int]),
          numDynamicChunks = e.numDynamicChunks
        )(e.mK,e.mV,e.mI,e.mCV,e.mCI,e.mCCV)).asInstanceOf[Def[A]]
      case e: DeliteHashReduceElem[k,v,i,cv] =>
        (DeliteHashReduceElem[k,v,i,cv](
          keyFunc = fb(e.keyFunc)(e.mK),
          valFunc = fb(e.valFunc)(e.mV),
          cond = e.cond.map(fb(_)(manifest[Boolean])),
          zero = fb(e.zero)(e.mV),
          rV = (f(e.rV._1).asInstanceOf[Sym[v]], f(e.rV._2).asInstanceOf[Sym[v]]), // need to transform bound vars ??
          rFunc = fb(e.rFunc)(e.mV),
          buf = mirrorBuffer(e.buf),
          numDynamicChunks = e.numDynamicChunks
        )(e.mK,e.mV,e.mI,e.mCV)).asInstanceOf[Def[A]]
      case e: DeliteHashIndexElem[k,cv] =>
        (DeliteHashIndexElem[k,cv](
          keyFunc = fb(e.keyFunc)(e.mK),
          cond = e.cond.map(fb(_)(manifest[Boolean])),
          numDynamicChunks = e.numDynamicChunks
        )(e.mK,e.mCV)).asInstanceOf[Def[A]]
      case e: DeliteCollectElem[a,i,ca] =>
        (DeliteCollectElem[a,i,ca]( // need to be a case class for equality (do we rely on equality?)
          func = fb(e.func)(e.mA),
          cond = e.cond.map(fb(_)(manifest[Boolean])), //f(e.cond)
          par = e.par,
          buf = mirrorBuffer(e.buf),
          iFunc = e.iFunc.map(fb(_)(e.mDCA)),
          iF = e.iF.map(f(_).asInstanceOf[Sym[Int]]),
          sF = e.sF.map(fb(_)(manifest[Int])),
          eF = e.eF.map(f(_).asInstanceOf[Sym[i]]),
          numDynamicChunks = e.numDynamicChunks
        )(e.mA,e.mI,e.mCA)).asInstanceOf[Def[A]]
      case e: DeliteForeachElem[a] =>
        (DeliteForeachElem[a](
          func = fb(e.func)(e.mA),
          numDynamicChunks = e.numDynamicChunks
          //sync = f(e.sync)
//          cond = f(e.cond)
        )(e.mA)).asInstanceOf[Def[A]] // reasonable?
      case e: DeliteReduceElem[a] =>
        (DeliteReduceElem[a](
          func = fb(e.func)(e.mA),
          cond = e.cond.map(fb(_)(manifest[Boolean])),//f(e.cond),
          zero = fb(e.zero)(e.mA),
          accInit = fb(e.accInit)(e.mA),
          rV = (f(e.rV._1).asInstanceOf[Sym[a]], f(e.rV._2).asInstanceOf[Sym[a]]), // need to transform bound vars ??
          rFunc = fb(e.rFunc)(e.mA),
          stripFirst = e.stripFirst,
          numDynamicChunks = e.numDynamicChunks
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
          stripFirst = e.stripFirst,
          numDynamicChunks = e.numDynamicChunks
        )(e.mA,e.mB)).asInstanceOf[Def[A]]
    }
  }


  //////////////
  // dependencies


  /* is this necessary or not? does not appear to effect anything yet. */
  override def blocks(e: Any): List[Block[Any]] = e match {
    case s: DeliteOpSingleTask[_] => blocks(s.block)
    case e: DeliteOpExternal[_] =>  super.blocks(e) ::: blocks(e.allocVal)
    case fr: DeliteOpForeachReduce[_] => blocks(fr.funcBody)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc) ::: blocks(op.cond) ::: blocks(op.buf) ::: blocks(op.iBuf) ::: blocks(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFunc) ::: blocks(op.buf)
    case op: DeliteHashIndexElem[_,_] => blocks(op.keyFunc) ::: blocks(op.cond)
    case op: DeliteCollectElem[_,_,_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.buf)
    case op: DeliteBufferElem[_,_,_] => blocks(op.alloc) ::: blocks(op.apply) ::: blocks(op.update) ::: blocks(op.appendable) ::: blocks(op.append) ::: blocks(op.setSize) ::: blocks(op.allocRaw) ::: blocks(op.copyRaw) ::: blocks(op.finalizer)
//    case op: DeliteForeachElem[_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.sync)
    case op: DeliteForeachElem[_] => blocks(op.func) //::: blocks(op.sync)
    case op: DeliteReduceElem[_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFunc) ::: blocks(op.accInit)
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
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => syms(op.keyFunc) ++ syms(op.valFunc) ++ syms(op.cond) ++ syms(op.buf) ++ syms(op.iBuf) ++ syms(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => syms(op.keyFunc) ++ syms(op.valFunc) ++ syms(op.cond) ++ syms(op.zero) ++ syms(op.rFunc) ++ syms(op.buf)
    case op: DeliteHashIndexElem[_,_] => syms(op.keyFunc) ++ syms(op.cond)
    case e: DeliteOpExternal[_] =>  super.syms(e) ::: syms(e.allocVal)
    case fr: DeliteOpForeachReduce[_] => syms(fr.funcBody)
    case op: DeliteCollectElem[_,_,_] => syms(op.func) ::: syms(op.cond) ::: syms(op.buf) ::: syms(op.iFunc) ::: syms(op.sF)
    case op: DeliteBufferElem[_,_,_] => syms(op.alloc) ::: syms(op.apply) ::: syms(op.update) ::: syms(op.appendable) ::: syms(op.append) ::: syms(op.setSize) ::: syms(op.allocRaw) ::: syms(op.copyRaw) ::: syms(op.finalizer)
//    case op: DeliteForeachElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.sync)
    case op: DeliteForeachElem[_] => syms(op.func) //::: syms(op.sync)
    case op: DeliteReduceElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFunc) ::: syms(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFuncSeq) ::: syms(op.rFuncPar) // should be ok for tuples...
    case foreach: DeliteOpForeach2[_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ::: syms(foreach.func) ::: syms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ::: syms(foreach.func) ::: syms(foreach.sync)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match { //TR FIXME: check this is actually correct
    case s: DeliteOpSingleTask[_] if s.requireInputs => super.readSyms(e) ::: readSyms(s.block) // super call: add case class syms (iff flag is set)
    case s: DeliteOpSingleTask[_] => readSyms(s.block)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => readSyms(op.keyFunc) ++ readSyms(op.valFunc) ++ readSyms(op.cond) ++ readSyms(op.buf) ++ readSyms(op.iBuf) ++ readSyms(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => readSyms(op.keyFunc) ++ readSyms(op.valFunc) ++ readSyms(op.cond) ++ readSyms(op.zero) ++ readSyms(op.rFunc) ++ readSyms(op.buf)
    case op: DeliteHashIndexElem[_,_] => readSyms(op.keyFunc) ++ readSyms(op.cond)
    case e: DeliteOpExternal[_] => super.readSyms(e) ::: readSyms(e.allocVal)
    case fr: DeliteOpForeachReduce[_] => readSyms(fr.funcBody)
    case op: DeliteCollectElem[_,_,_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.buf) ::: readSyms(op.iFunc) ::: readSyms(op.sF)
    case op: DeliteBufferElem[_,_,_] => readSyms(op.alloc) ::: readSyms(op.apply) ::: readSyms(op.update) ::: readSyms(op.appendable) ::: readSyms(op.append) ::: readSyms(op.setSize) ::: readSyms(op.allocRaw) ::: readSyms(op.copyRaw) ::: readSyms(op.finalizer)
//    case op: DeliteForeachElem[_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.sync)
    case op: DeliteForeachElem[_] => readSyms(op.func) //::: readSyms(op.sync)
    case op: DeliteReduceElem[_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.zero) ::: readSyms(op.rFunc) ::: readSyms(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.zero) ::: readSyms(op.rFuncSeq) ::: readSyms(op.rFuncPar)
    case foreach: DeliteOpForeach2[_,_] => readSyms(foreach.in)
    case foreach: DeliteOpForeachBounded[_,_,_] => readSyms(foreach.in)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => effectSyms(s.block)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => effectSyms(op.keyFunc) ++ effectSyms(op.valFunc) ++ effectSyms(op.cond) ++ boundSyms(op.buf) ++ boundSyms(op.iBuf) ++ effectSyms(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => List(op.rV._1, op.rV._2) ++ effectSyms(op.keyFunc) ++ effectSyms(op.valFunc) ++ effectSyms(op.cond) ++ effectSyms(op.zero) ++ effectSyms(op.rFunc) ++ boundSyms(op.buf)
    case op: DeliteHashIndexElem[_,_] => effectSyms(op.keyFunc) ++ effectSyms(op.cond)
    case e: DeliteOpExternal[_] => effectSyms(e.allocVal) /*::: super.effectSyms(e) */
    case fr: DeliteOpForeachReduce[_] => List(fr.v) ::: effectSyms(fr.funcBody)
    case op: DeliteCollectElem[_,_,_] => effectSyms(op.func)  ::: effectSyms(op.cond) ::: boundSyms(op.buf) ::: effectSyms(op.iFunc) ::: effectSyms(op.sF) ::: op.iF.toList ::: op.eF.toList
    case op: DeliteBufferElem[_,_,_] => List(op.eV, op.sV, op.allocVal, op.aV2, op.iV, op.iV2) ::: effectSyms(op.alloc) ::: effectSyms(op.apply) ::: effectSyms(op.update) ::: effectSyms(op.appendable) ::: effectSyms(op.append) ::: effectSyms(op.setSize) ::: effectSyms(op.allocRaw) ::: effectSyms(op.copyRaw) ::: effectSyms(op.finalizer)
//    case op: DeliteForeachElem[_] => effectSyms(op.func) ::: effectSyms(op.cond) ::: effectSyms(op.sync)
    case op: DeliteForeachElem[_] => effectSyms(op.func) //::: effectSyms(op.sync)
    case op: DeliteReduceElem[_] => List(op.rV._1, op.rV._2) ::: effectSyms(op.func) ::: effectSyms(op.cond) ::: effectSyms(op.zero) ::: effectSyms(op.rFunc) ::: effectSyms(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => syms(op.rVPar) ::: syms(op.rVSeq) ::: effectSyms(op.func._1) ::: effectSyms(op.cond) ::: effectSyms(op.zero) ::: effectSyms(op.rFuncPar) ::: effectSyms(op.rFuncSeq)
    case foreach: DeliteOpForeach2[_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case _ => super.boundSyms(e)
  }


  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: DeliteOpSingleTask[_] if s.requireInputs => super.symsFreq(e) ::: freqNormal(s.block)  // super call: add case class syms (iff flag is set)
    case s: DeliteOpSingleTask[_] => freqNormal(s.block)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => freqHot(op.keyFunc) ++ freqHot(op.valFunc) ++ freqHot(op.cond) ++ symsFreq(op.buf) ++ symsFreq(op.iBuf) ++ freqNormal(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => freqHot(op.keyFunc) ++ freqHot(op.valFunc) ++ freqHot(op.cond) ++ freqNormal(op.zero) ++ freqHot(op.rFunc) ++ symsFreq(op.buf)
    case op: DeliteHashIndexElem[_,_] => freqHot(op.keyFunc) ++ freqHot(op.cond)
    case e: DeliteOpExternal[_] => super.symsFreq(e) ::: freqNormal(e.allocVal)
    case fr: DeliteOpForeachReduce[_] => freqHot(fr.funcBody)
    case op: DeliteCollectElem[_,_,_] => freqHot(op.cond) ::: freqHot(op.func) ::: symsFreq(op.buf) ::: freqHot(op.iFunc) ::: freqHot(op.sF)
    case op: DeliteBufferElem[_,_,_] => freqNormal(op.alloc) ::: freqHot(op.apply) ::: freqHot(op.update) ::: freqHot(op.appendable) ::: freqHot(op.append) ::: freqNormal(op.setSize) ::: freqNormal(op.allocRaw) ::: freqNormal(op.copyRaw) ::: freqNormal(op.finalizer)
//    case op: DeliteForeachElem[_] => freqNormal(op.sync) ::: freqHot(op.cond) ::: freqHot(op.func)
    case op: DeliteForeachElem[_] => /*freqNormal(op.sync) :::*/ freqHot(op.func)
    case op: DeliteReduceElem[_] => freqHot(op.cond) ::: freqHot(op.func) ::: freqNormal(op.zero) ::: freqHot(op.rFunc) ::: freqNormal(op.accInit)
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
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => Nil
    case op: DeliteHashReduceElem[_,_,_,_] => Nil
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
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => Nil
    case op: DeliteHashReduceElem[_,_,_,_] => Nil
    case foreach: DeliteOpForeach2[_,_] => Nil
    case foreach: DeliteOpForeachBounded[_,_,_] => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => Nil
    case e: DeliteOpExternal[_] => syms(e.allocVal)
    case op: DeliteCollectElem[_,_,_] => syms(op.buf.alloc)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => syms(op.buf.alloc)
    case op: DeliteHashReduceElem[_,_,_,_] => syms(op.buf.alloc)
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
  //    case e: DeliteReduceElem[_] => Some((e.func, e.cond)) // TODO: aks -- testing fusing conditionals for reduce elems
    case e: DeliteHashReduceElem[_,_,_,_] => Some((e.valFunc.res, e.cond.map(_.res))) // FIXME: HACK!!
    case e: DeliteCollectElem[_,_,_] => Some((e.func.res, e.cond.map(_.res)))
    case _ => super.unapplySimpleCollectIf(e)
  }

  // FIXME: need to modify .par from ParPlat to ParBuf accordingly
  override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match {
    case e: DeliteHashCollectElem[_,_,_,_,_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mK,e.mV,e.mI,e.mCV,e.mCI,e.mCCV)
    case e: DeliteHashReduceElem[_,_,_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mK,e.mV,e.mI,e.mCV)
    case e: DeliteHashIndexElem[_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mK,e.mCV)
    case e: DeliteCollectElem[_,_,_] => e.copy(par = ParBuffer, cond = e.cond ++ c.map(Block(_)))(e.mA, e.mI, e.mCA)
    case e: DeliteReduceElem[_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mA)
    case e: DeliteReduceTupleElem[_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mA,e.mB)
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

/* CPU-like target code generation for DeliteOps */
trait GenericGenDeliteOps extends BaseGenLoopsFat with BaseGenStaticData with BaseGenDeliteOps {
  import IR._

  def quotearg(x: Sym[Any])
  def quotetp(x: Sym[Any])
  def methodCall(name:String, inputs: List[String] = Nil): String
  def emitMethodCall(name:String, inputs: List[String]): Unit
  def emitMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit): Unit
  def createInstance(typeName:String, args: List[String] = Nil): String
  def fieldAccess(className: String, varName: String): String
  def releaseRef(varName: String): Unit
  def emitReturn(rhs: String)
  def emitFieldDecl(name: String, tpe: String)
  def emitClass(name: String)(body: => Unit)
  def emitObject(name: String)(body: => Unit)
  def emitValDef(name: String, tpe: String, init: String): Unit
  def emitVarDef(name: String, tpe: String, init: String): Unit
  def emitAssignment(name: String, tpe: String, rhs: String): Unit
  def emitAssignment(lhs: String, rhs: String): Unit
  def emitAbstractFatLoopHeader(className: String, actType: String): Unit
  def emitAbstractFatLoopFooter(): Unit
  def refNotEq: String
  def nullRef: String
  def arrayType(argType: String): String
  def arrayApply(arr: String, idx: String): String
  def newArray(argType: String, size: String): String
  def hashmapType(argType: String): String
  def typeCast(sym: String, to: String): String
  def withBlock(name: String)(block: => Unit): Unit

  // variable for loop nest level (used for adding openmp pragma)
  var loopLevel: Int = 0

  /**
   * MultiLoop components
   */

  /* (grouped) hash support follows */
  def emitInlineMultiHashInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = "") {
    for ((cond,cps) <- ps.groupBy(_._2.cond)) {
      for ((key,kps) <- cps.groupBy(_._2.keyFunc)) { //TODO: to properly abstract this over multiple code generators we need a hashmap impl for each!
        emitVarDef(kps.map(p=>quote(p._1)).mkString("") + "_hash_pos", hashmapType(remap(getBlockResult(key).tp)), createInstance(hashmapType(remap(getBlockResult(key).tp)), List("512","128")))
        kps foreach {
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
            emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "128")
            emitBlock(elem.buf.alloc)
            emitVarDef(quote(sym) + "_hash_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) => 
            emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "128") 
            emitBlock(elem.buf.alloc)
            emitVarDef(quote(sym) + "_hash_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
        }
      }
    }
  }

  def emitKernelMultiHashDecl(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], actType: String) {
    if (ps.length > 0) {
      emitFieldDecl("tid", remap(Manifest.Int))
      emitFieldDecl("num_threads", remap(Manifest.Int))
      emitFieldDecl("all_acts", arrayType(actType))
      for ((cond,cps) <- ps.groupBy(_._2.cond)) {
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          emitFieldDecl(quotedGroup + "_hash_pos", hashmapType(remap(getBlockResult(key).tp)))
          emitFieldDecl(quotedGroup + "_size", remap(Manifest.Int))
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              emitFieldDecl(quote(sym), remap(sym.tp))
              emitFieldDecl(quote(sym) + "_hash_data", remap(getBlockResult(elem.buf.alloc).tp))
              emitFieldDecl(quote(sym) + "_data", remap(getBlockResult(elem.buf.alloc).tp))
              emitMethod(quote(sym)+"_data_set", remap(Manifest.Unit), List(("xs",remap(elem.buf.allocVal.tp)))) {
                emitAssignment(quote(sym) + "_data", "xs")
                stream.println("if (left_act " + refNotEq + " " + nullRef + ")")
                emitMethodCall(fieldAccess("left_act",quote(sym)+"_data_set"),List("xs")) // XX linked frame
              }
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              emitFieldDecl(quote(sym), remap(sym.tp))
              emitFieldDecl(quote(sym) + "_hash_data", remap(getBlockResult(elem.buf.alloc).tp))
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
              emitFieldDecl(quote(sym), remap(sym.tp))
          }
        }
      }
    }
  }

  def emitKernelMultiHashInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = ""){
    for ((cond,cps) <- ps.groupBy(_._2.cond)) {
      for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
        val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
        emitAssignment(fieldAccess(prefixSym, quotedGroup + "_hash_pos"), createInstance(hashmapType(remap(getBlockResult(key).tp)),List("512","128")))
        emitAssignment(fieldAccess(prefixSym, quotedGroup + "_size"), "-1")
        kps foreach { 
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) => 
            emitValDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "128")
            emitBlock(elem.buf.alloc)
            emitAssignment(fieldAccess(prefixSym, quote(sym) + "_hash_data"), quote(getBlockResult(elem.buf.alloc)))
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) => 
            emitValDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "128") 
            emitBlock(elem.buf.alloc)
            emitAssignment(fieldAccess(prefixSym, quote(sym) + "_hash_data"), quote(getBlockResult(elem.buf.alloc)))
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
        }
      }
    }
  }

  def emitMultiHashElem(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = "") {
    for ((cond,cps) <- ps.groupBy(_._2.cond)) { // group by cond
      if (cond.nonEmpty) stream.println("if (" + cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) { // group by key
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          stream.println("// common key "+key+" for "+quotedGroup)
          emitValDef(quotedGroup + "_sze", remap(manifest[Int]), fieldAccess(fieldAccess(prefixSym, quotedGroup + "_hash_pos"), methodCall("size")))
          emitValDef(quotedGroup + "_idx", remap(manifest[Int]), fieldAccess(fieldAccess(prefixSym, quotedGroup + "_hash_pos"), "put(" + quote(getBlockResult(key)) + ")"))

          stream.println("if (" + quotedGroup + "_idx == " + quotedGroup + "_sze) { // new key")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              //allocate inner buffer & append to outer buffer
              emitValDef(quote(elem.iBuf.sV), remap(elem.buf.sV.tp), "128")
              emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              getActBuffer = List(fieldAccess(prefixSym, quote(sym) + "_hash_data"), quote(elem.buf.allocVal))
              getActSize = quotedGroup + "_sze"
              if (!Config.soaEnabled) {
                emitBlock(elem.iBuf.alloc)
                emitValDef(quote(elem.iBuf.allocVal), remap(getBlockResult(elem.iBuf.alloc).tp), quote(getBlockResult(elem.iBuf.alloc)))
              }
              emitBlock(elem.buf.append)
              //append elem to inner buffer
              emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), getActSize)
              emitValDef(quote(elem.iBuf.eV), remap(elem.iBuf.eV.tp), quote(getBlockResult(elem.valFunc)))
              emitBlock(elem.iBuf.append)
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.valFunc)))
              getActBuffer = List(fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              getActSize = quotedGroup + "_sze"
              emitBlock(elem.buf.append)
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }

          stream.println("} else { // existing key")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "_idx")
              emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              emitValDef(quote(elem.iBuf.eV), remap(elem.iBuf.eV.tp), quote(getBlockResult(elem.valFunc)))
              if (!Config.soaEnabled) {
                emitBlock(elem.buf.apply)
                emitValDef(quote(elem.iBuf.allocVal), remap(getBlockResult(elem.buf.apply).tp), quote(getBlockResult(elem.buf.apply)))
              }
              emitBlock(elem.iBuf.append) //append to inner buffer
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              if (elem.rFunc != Block(elem.rV._1)) { //else drop
                emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "_idx")
                emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
                if (elem.rFunc == Block(elem.rV._2)) { //overwrite
                  emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.valFunc)))
                  emitBlock(elem.buf.update)
                } else { //reduce
                  emitBlock(elem.buf.apply)
                  emitValDef(quote(elem.rV._1), remap(elem.rV._1.tp), quote(getBlockResult(elem.buf.apply)))
                  emitValDef(quote(elem.rV._2), remap(elem.rV._2.tp), quote(getBlockResult(elem.valFunc)))
                  emitBlock(elem.rFunc)
                  emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.rFunc)))
                  emitBlock(elem.buf.update)
                }
              }
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }
          stream.println("}")
        }
      if (cond.nonEmpty) stream.println("}")
    }
  }

  def emitMultiHashCombine(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String){
    for ((cond,cps) <- ps.groupBy(_._2.cond)) {
      for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
        val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
        stream.println("// common key "+key+" for "+quotedGroup)
        stream.print("if (" + fieldAccess(prefixSym, quotedGroup+"_size") + " == -1) ") //store pre-merge sizes for later
        emitAssignment(fieldAccess(prefixSym, quotedGroup + "_size"), fieldAccess(prefixSym, fieldAccess(quotedGroup+"_hash_pos",methodCall("size"))))
        stream.print("if (" + fieldAccess("rhs", quotedGroup+"_size") + " == -1) ")
        emitAssignment(fieldAccess("rhs", quotedGroup+"_size"), fieldAccess("rhs", fieldAccess(quotedGroup+"_hash_pos",methodCall("size"))))
        emitVarDef(quotedGroup + "rhs_idx", remap(manifest[Int]), "0")
        stream.println("while (" + quotedGroup + "rhs_idx < " + fieldAccess("rhs", fieldAccess(quotedGroup+"_hash_pos",methodCall("size"))) + ") {")
        emitValDef(quotedGroup + "_k", remap(key.tp), fieldAccess("rhs", fieldAccess(quotedGroup + "_hash_pos",arrayApply(methodCall("unsafeKeys"),quotedGroup + "rhs_idx"))))
        emitValDef(quotedGroup + "_sze", remap(manifest[Int]), fieldAccess(prefixSym, fieldAccess(quotedGroup + "_hash_pos",methodCall("size"))))
        emitValDef(quotedGroup + "_idx", remap(manifest[Int]), fieldAccess(prefixSym, fieldAccess(quotedGroup + "_hash_pos","put("+quotedGroup+"_k)")))
        stream.println("if (" + quotedGroup + "_idx == " + quotedGroup + "_sze) { // new key")
        kps foreach {
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess("rhs", quote(sym) + "_hash_data"))
            emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "rhs_idx")
            emitBlock(elem.buf.apply)
            emitAssignment(quote(elem.buf.allocVal), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
            emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.buf.apply)))
            getActBuffer = List(fieldAccess(prefixSym, quote(sym) + "_hash_data"))
            getActSize = quotedGroup + "_sze"
            emitBlock(elem.buf.append)
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
        }
        stream.println("} else { // existing key")
        kps foreach {
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            if (elem.rFunc != Block(elem.rV._1)) { //else drop
              emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess("rhs", quote(sym) + "_hash_data"))
              emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "rhs_idx")
              emitBlock(elem.buf.apply)
              emitValDef(quote(sym) + "_v", remap(getBlockResult(elem.buf.apply).tp), quote(getBlockResult(elem.buf.apply)))
              withBlock(quote(sym) + "_reduce_block") {
                emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
                emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "_idx")
                if (elem.rFunc == Block(elem.rV._2)) { //overwrite
                  emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(sym) + "_v")
                  emitBlock(elem.buf.update)
                } else { //reduce
                  emitBlock(elem.buf.apply)
                  emitValDef(quote(elem.rV._1), remap(elem.rV._1.tp), quote(getBlockResult(elem.buf.apply)))
                  emitValDef(quote(elem.rV._2), remap(elem.rV._2.tp), quote(sym)+"_v")
                  emitBlock(elem.rFunc)
                  emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.rFunc)))
                  emitBlock(elem.buf.update)
                }
              }
            }
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
        }
        stream.println("}")
        emitAssignment(quotedGroup + "rhs_idx", quotedGroup + "rhs_idx+1")
        stream.println("}") // end loop
      }
    }
  }

  def emitMultiHashPostCombine(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String){
    if (ps.length > 0) 
      emitAssignment(fieldAccess(prefixSym, "tid"),fieldAccess("lhs", "tid") + " + 1")
  }

  def emitMultiHashPostProcInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], actType: String, prefixSym: String){
    if (ps.length > 0) {
      stream.println("if (" + fieldAccess(prefixSym,"tid") + " > 0) {")
      emitValDef("all_acts", arrayType(actType), newArray(actType, fieldAccess(prefixSym,"tid")+"+1"))
      emitVarDef("currentAct", actType, prefixSym)
      emitVarDef("i", remap(Manifest.Int), fieldAccess(prefixSym,"tid"))
      stream.println("while(i >= 0) {")
      emitAssignment(fieldAccess("currentAct","num_threads"), fieldAccess(prefixSym,"tid")+"+1")
      emitAssignment(arrayApply("all_acts","i"), "currentAct")
      emitAssignment(fieldAccess("currentAct","all_acts"), "all_acts")
      emitAssignment("currentAct", fieldAccess("currentAct","left_act"))
      emitAssignment("i", "i-1")
      stream.println("}")

      for ((cond,cps) <- ps.groupBy(_._2.cond)) {
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              emitValDef(elem.buf.sV, fieldAccess(fieldAccess(fieldAccess(prefixSym, arrayApply("all_acts","0")), quotedGroup+"_hash_pos"),methodCall("size")))
              emitValDef(elem.buf.allocVal, fieldAccess(prefixSym, quote(sym)+"_hash_data"))
              emitBlock(elem.buf.allocRaw)
              emitMethodCall(fieldAccess(prefixSym,quote(sym)+"_data_set"),List(quote(getBlockResult(elem.buf.allocRaw))))
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }
        }
      }
      stream.println("} else {")
      for ((cond,cps) <- ps.groupBy(_._2.cond)) {
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              emitMethodCall(fieldAccess(prefixSym,quote(sym)+"_data_set"),List(fieldAccess(prefixSym,quote(sym)+"_hash_data")))
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }
        }
      }
      stream.println("}")
    }
  }

  def emitMultiHashPostProcess(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], actType: String, prefixSym: String){
    if (ps.length > 0) {
      stream.println("if (" + fieldAccess(prefixSym,"all_acts") + " " + refNotEq + " " + nullRef + ") {")
      emitValDef("all_acts", arrayType(actType), fieldAccess(prefixSym,"all_acts"))
      for ((cond,cps) <- ps.groupBy(_._2.cond)) {
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          emitValDef(quotedGroup+"_globalKeys", arrayType(remap(key.tp)), fieldAccess(fieldAccess(fieldAccess(prefixSym, arrayApply("all_acts","0")), quotedGroup+"_hash_pos"), methodCall("unsafeKeys")))
          emitVarDef(quotedGroup+"_idx", remap(Manifest.Int), typeCast(typeCast(fieldAccess(fieldAccess(fieldAccess(prefixSym, arrayApply("all_acts","0")), quotedGroup+"_hash_pos"), methodCall("size")), remap(Manifest.Long)) + " * " + fieldAccess(prefixSym,"tid") + " / " + fieldAccess(prefixSym,"num_threads"),remap(Manifest.Int)))
          emitValDef(quotedGroup+"_end", remap(Manifest.Int), typeCast(typeCast(fieldAccess(fieldAccess(fieldAccess(prefixSym, arrayApply("all_acts","0")), quotedGroup+"_hash_pos"), methodCall("size")), remap(Manifest.Long)) + " * (" + fieldAccess(prefixSym,"tid") + "+1) / " + fieldAccess(prefixSym,"num_threads"),remap(Manifest.Int)))
          stream.println("while (" + quotedGroup+"_idx < " + quotedGroup + "_end) {")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
                emitValDef(elem.buf.iV, quotedGroup+"_idx")
                emitValDef(elem.buf.allocVal, fieldAccess(prefixSym,quote(sym)+"_data"))
                emitVarDef(quote(sym)+"_act_idx", remap(Manifest.Int), "0")
                emitVarDef(quote(sym)+"_values_size", remap(Manifest.Int), "0")
                stream.println("while (" + quote(sym)+"_act_idx < " + fieldAccess(prefixSym,"num_threads") + ") {")
                  emitValDef("currentAct", actType, arrayApply("all_acts",quote(sym)+"_act_idx"))
                  emitValDef("pos", remap(Manifest.Int), fieldAccess(fieldAccess("currentAct", quotedGroup+"_hash_pos"), "get("+arrayApply(quotedGroup+"_globalKeys",quotedGroup+"_idx")+")"))
                  stream.println("if (pos != -1 && pos < " + fieldAccess("currentAct",quotedGroup+"_size") + ") {")
                    emitValDef(elem.buf.iV2, "pos")
                    emitValDef(elem.buf.aV2, fieldAccess("currentAct",quote(sym)+"_hash_data"))
                    if (!Config.soaEnabled) {
                      emitBlock(elem.iBuf.allocRaw)
                      emitValDef(quote(elem.iBuf.aV2), remap(getBlockResult(elem.iBuf.allocRaw).tp), quote(getBlockResult(elem.iBuf.allocRaw)))  
                    }
                    emitBlock(elem.iBufSize)
                    emitAssignment(quote(sym)+"_values_size", quote(sym)+"_values_size + " + quote(getBlockResult(elem.iBufSize)))
                  stream.println("}")
                  emitAssignment(quote(sym)+"_act_idx", quote(sym)+"_act_idx + 1")
                stream.println("}")

                emitValDef(elem.buf.sV, quote(sym)+"_values_size")
                if (!Config.soaEnabled) {
                  emitBlock(elem.iBuf.alloc)
                  emitValDef(quote(elem.iBuf.allocVal), remap(getBlockResult(elem.iBuf.alloc).tp), quote(getBlockResult(elem.iBuf.alloc)))
                }
                emitBlock(elem.buf.update)

                emitVarDef(quote(sym)+"_offset", remap(Manifest.Int), "0")
                emitAssignment(quote(sym)+"_act_idx", "0")
                stream.println("while (" + quote(sym)+"_act_idx < " + fieldAccess(prefixSym,"num_threads") + ") {")
                  emitValDef("currentAct", actType, arrayApply("all_acts",quote(sym)+"_act_idx"))
                  emitValDef("pos", remap(Manifest.Int), fieldAccess(fieldAccess("currentAct", quotedGroup+"_hash_pos"), "get("+arrayApply(quotedGroup+"_globalKeys",quotedGroup+"_idx")+")"))
                  stream.println("if (pos != -1 && pos < " + fieldAccess("currentAct",quotedGroup+"_size") + ") {")
                    emitValDef(elem.buf.iV2, "pos")
                    emitValDef(elem.buf.aV2, fieldAccess("currentAct",quote(sym)+"_hash_data"))
                    if (!Config.soaEnabled) {
                      emitBlock(elem.iBuf.allocRaw)
                      emitValDef(quote(elem.iBuf.aV2), remap(getBlockResult(elem.iBuf.allocRaw).tp), quote(getBlockResult(elem.iBuf.allocRaw)))  
                    }
                    emitBlock(elem.iBufSize)
                    emitValDef(elem.iBuf.sV, quote(getBlockResult(elem.iBufSize)))
                    emitValDef(elem.iBuf.iV, quote(sym)+"_offset")
                    emitValDef(elem.iBuf.iV2, "0")
                    emitBlock(elem.iBuf.copyRaw)
                    emitAssignment(quote(sym)+"_offset", quote(sym)+"_offset + " + quote(getBlockResult(elem.iBufSize)))
                  stream.println("}")
                  emitAssignment(quote(sym)+"_act_idx", quote(sym)+"_act_idx + 1")
                stream.println("}")
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }
          emitAssignment(quotedGroup+"_idx", quotedGroup+"_idx + 1")
          stream.println("}")
        }
      }
      stream.println("}")
    }
  }

  def emitMultiHashFinalize(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = "") {
    for ((cond,cps) <- ps.groupBy(_._2.cond)) {
      for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
        val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
        emitValDef(quotedGroup + "_sze", remap(manifest[Int]), fieldAccess(fieldAccess(prefixSym, quotedGroup + "_hash_pos"), methodCall("size")))
        kps foreach {
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) => //TODO: finalizer
            if (prefixSym == "") {
              emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              getActBuffer = List(quote(elem.buf.allocVal))
              emitAssignment(quote(elem.buf.sV), quotedGroup + "_sze")
              emitBlock(elem.buf.setSize)
              emitValDef(quote(sym), remap(sym.tp), quote(elem.buf.allocVal))
            }
            else {
              emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_data")) 
              getActBuffer = List(quote(elem.buf.allocVal))
              emitValDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quotedGroup + "_sze")
              emitBlock(elem.buf.setSize)
              emitAssignment(fieldAccess(prefixSym, quote(sym)), quote(elem.buf.allocVal))
            }
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) => //TODO: finalizer
            emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
            getActBuffer = List(quote(elem.buf.allocVal))
            if (prefixSym == "") {
              emitAssignment(quote(elem.buf.sV), quotedGroup + "_sze")
              emitBlock(elem.buf.setSize)
              emitValDef(quote(sym), remap(sym.tp), quote(elem.buf.allocVal))
            }
            else {
              emitValDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quotedGroup + "_sze")
              emitBlock(elem.buf.setSize)
              emitAssignment(fieldAccess(prefixSym, quote(sym)), quote(elem.buf.allocVal))
            }
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
            if (prefixSym == "")
              emitValDef(quote(sym), remap(sym.tp), quotedGroup + "_hash_pos")
            else
              emitAssignment(fieldAccess(prefixSym, quote(sym)), fieldAccess(prefixSym, quotedGroup + "_hash_pos"))
        }
      }
    }
  }

  // --- end hash reduce

  def emitCollectElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteCollectElem[_,_,_], prefixSym: String = "") {
    elem.par match {
      case ParBuffer | ParSimpleBuffer =>
        if (elem.cond.nonEmpty) stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
        if (elem.iFunc.nonEmpty) {
          emitValDef(elem.eF.get, quote(getBlockResult(elem.iFunc.get)))
          emitBlock(elem.sF.get)
          emitVarDef(quote(elem.iF.get), remap(elem.iF.get.tp), "0")
          stream.println("while (" + quote(elem.iF.get) + " < " + quote(getBlockResult(elem.sF.get)) + ") { //flatMap loop")
          emitBlock(elem.func)
        }
        emitValDef(elem.buf.allocVal, fieldAccess(prefixSym,quote(sym)+"_buf"))
        emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
        getActBuffer = List(fieldAccess(prefixSym, quote(sym) + "_buf"))
        getActSize = fieldAccess(prefixSym, quote(sym) + "_size")
        emitBlock(elem.buf.appendable)
        stream.println("if (" + quote(getBlockResult(elem.buf.appendable)) + ") {")
        emitBlock(elem.buf.append)
        emitAssignment(fieldAccess(prefixSym,quote(sym)+"_size"), fieldAccess(prefixSym,quote(sym)+"_size") + " + 1")
        stream.println("}")
        emitAssignment(fieldAccess(prefixSym,quote(sym)+"_conditionals"), fieldAccess(prefixSym,quote(sym)+"_conditionals") + " + 1")
        if (elem.iFunc.nonEmpty) {
          emitAssignment(quote(elem.iF.get), quote(elem.iF.get) + " + 1")
          stream.println("}") //close flatmap loop
        }
        if (elem.cond.nonEmpty) stream.println("}")
      case ParFlat =>
        emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
        emitValDef(elem.buf.allocVal, fieldAccess(prefixSym,quote(sym)+"_data"))
        if (elem.cond.nonEmpty) {
          stream.println("//ERROR: need to test for conds " + elem.cond)
          println("ERROR: need to test for conds " + elem.cond)
        }
        emitBlock(elem.buf.update)
    }
  }

  var getActSize = ""
  var getActBuffer: List[String] = Nil

  def emitForeachElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteForeachElem[_]) {
    emitAssignment(quote(sym), remap(sym.tp), quote(getBlockResult(elem.func)))
    //stream.println(quote(getBlockResult(elem.func)))
  }

  // -- begin emit reduce

  // Assigns the first reduce element to the result symbol
  def emitFirstReduceElemAssign(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    val resultSym = fieldAccess(prefixSym,quote(sym))
    if (elem.cond.nonEmpty) {
      // if we have conditionals, we have to delay the the initialization of the accumulator to the
      // first element where the condition is true
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
      emitAssignment(resultSym,quote(getBlockResult(elem.func)))
      stream.println("} else {")
      emitAssignment(resultSym,fieldAccess(prefixSym,quote(sym)+"_zero"))
      stream.println("}")
    }
    else {
      emitAssignment(resultSym,quote(getBlockResult(elem.func)))
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
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {"/*}*/)
      assert(!elem.stripFirst, "tuple reduce with condition + stripFirst not implemented")
      emitReductionTuple(op, sym, elem, prefixSym)
      stream.println("}")
    }
    else {
      emitReductionTuple(op, sym, elem, prefixSym)
    }
  }

  def emitInitializeOrReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    stream.println("// TODO: we could optimize this check away with more convoluted runtime support if necessary")
    stream.println("if (" + fieldAccess(prefixSym,quote(sym)) + " == " + fieldAccess(prefixSym,quote(sym)+"_zero") + ") ")

    // initialize
    emitAssignment(fieldAccess(prefixSym,quote(sym)),quote(getBlockResult(elem.func)))

    // or reduce
    stream.println("else {")
    emitReduction(op, sym, elem, prefixSym)
    stream.println("}")
  }

  def emitReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    emitValDef(elem.rV._1, fieldAccess(prefixSym,quote(sym)))
    emitValDef(elem.rV._2, quote(getBlockResult(elem.func)))
    emitBlock(elem.rFunc)
    emitAssignment(fieldAccess(prefixSym,quote(sym)), quote(getBlockResult(elem.rFunc)))
  }

  def emitReductionTuple(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceTupleElem[_,_], prefixSym: String) {
    val rV = elem.rVSeq
    val rFunc = elem.rFuncSeq
    emitValDef(rV._1._1, fieldAccess(prefixSym,quote(sym)))
    emitValDef(rV._1._2, fieldAccess(prefixSym,quote(sym)+"_2"))
    emitValDef(rV._2._1, quote(getBlockResult(elem.func._1)))
    emitValDef(rV._2._2, quote(getBlockResult(elem.func._2)))
    emitFatBlock(List(rFunc._1, rFunc._2))
    emitAssignment(fieldAccess(prefixSym,quote(sym)), quote(getBlockResult(rFunc._1)))
    emitAssignment(fieldAccess(prefixSym,quote(sym)+"_2"), quote(getBlockResult(rFunc._2)))
  }

  // -- end emit reduce emit

  def getMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]]) = op.body flatMap { // don't emit dependencies twice!
    case elem: DeliteHashCollectElem[_,_,_,_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
    case elem: DeliteHashReduceElem[_,_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
    case elem: DeliteHashIndexElem[_,_] => elem.keyFunc :: elem.cond
    case elem: DeliteCollectElem[_,_,_] if elem.iFunc.nonEmpty => elem.iFunc.get :: elem.cond
    case elem: DeliteCollectElem[_,_,_] => elem.func :: elem.cond
    //case elem: DeliteForeachElem[_] => elem.cond // only emit func inside condition! TODO: how to avoid emitting deps twice? // elem.func :: elem.cond
    case elem: DeliteForeachElem[_] => List(elem.func)
    case elem: DeliteReduceElem[_] => elem.func :: elem.cond
    case elem: DeliteReduceTupleElem[_,_] => elem.func._1 :: elem.func._2 :: elem.cond
  }

  def emitMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    // FIXME: without .distinct TPCHQ2 has duplicate definitions. this should be fixed in emitFatBlock.
    emitFatBlock(getMultiLoopFuncs(op, symList).distinct)
  }

  def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    emitInlineMultiHashInit(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) })
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        elem.par match {
          case ParBuffer | ParSimpleBuffer =>
            emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "0")
          case ParFlat =>
            emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quote(op.size))
        }
        emitBlock(elem.buf.alloc)
        elem.par match {
          case ParBuffer | ParSimpleBuffer =>
            emitVarDef(quote(sym) + "_buf", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
          case ParFlat =>
            emitValDef(quote(sym) + "_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
        }
        emitVarDef(quote(sym) + "_size", remap(Manifest.Int), "0")
        emitVarDef(quote(sym) + "_conditionals", remap(Manifest.Int), "0")
      case (sym, elem: DeliteHashElem[_,_]) => //done above
      case (sym, elem: DeliteForeachElem[_]) =>
        emitVarDef(quote(sym), remap(sym.tp), "()")  //TODO: Need this for other targets? (Currently, other targets just don't generate unit types)
      case (sym, elem: DeliteReduceElem[_]) =>
        emitBlock(elem.zero)
        emitValDef(quote(sym) + "_zero", remap(sym.tp), quote(getBlockResult(elem.zero)))
        emitVarDef(quote(sym), remap(sym.tp), quote(sym) + "_zero")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        emitBlock(elem.zero._1)
        emitValDef(quote(sym) + "_zero", remap(elem.zero._1.tp), quote(getBlockResult(elem.zero._1)))
        emitBlock(elem.zero._2)
        emitValDef(quote(sym) + "_zero_2", remap(elem.zero._2.tp), quote(getBlockResult(elem.zero._2)))
        /*stream.println("val " + quote(sym) + "_zero   = " + elem.zero._1)
        stream.println("val " + quote(sym) + "_zero_2 = " + elem.zero._2)*/
        emitVarDef(quote(sym), remap(elem.zero._1.tp), quote(sym) + "_zero")
        emitVarDef(quote(sym) + "_2", remap(elem.zero._2.tp), quote(sym) + "_zero_2")
    }
    emitVarDef(quote(op.v), remap(op.v.tp), "0")
    //if (true) { //op.body exists (loopBodyNeedsStripFirst _)) { preserve line count as indicator for succesful fusing
    if (op.body exists (loopBodyNeedsStripFirst _)) {
      stream.println("if (" + quote(op.size) + " > 0) { // prerun fat loop " + symList.map(quote).mkString(",")/*}*/)
      /* strip first iteration */
      emitMultiLoopFuncs(op, symList)
      emitMultiHashElem(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) })
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitCollectElem(op, sym, elem)
        case (sym, elem: DeliteHashElem[_,_]) => // done above
        case (sym, elem: DeliteForeachElem[_]) =>
          emitForeachElem(op, sym, elem)
        case (sym, elem: DeliteReduceElem[_]) =>
          if (elem.stripFirst) {
            emitFirstReduceElemAssign(op, sym, elem)
          }
          else {
            emitReduceElem(op, sym, elem)
          }
      }
      stream.println(/*{*/"}")
      emitAssignment(quote(op.v), "1")
    }
    
    val freeVars = getFreeVarBlock(Block(Combine(getMultiLoopFuncs(op,symList).map(getBlockResultFull))),List(op.v)).filter(_ != op.size).distinct
    val streamVars = freeVars.filter(_.tp == manifest[DeliteFileStream])
    if (streamVars.length > 0) {
      assert(streamVars.length == 1, "ERROR: don't know how to handle multiple stream inputs at once")
      val streamSym = streamVars(0)
      emitVarDef("start_"+quote(op.v), remap(op.v.tp), "0")
      emitMethodCall(fieldAccess(quote(streamSym),"openAtNewLine"),List("start_"+quote(op.v)))
      stream.println("while (" + fieldAccess(quote(streamSym),"pos(start_"+quote(op.v)+")") + " < " + fieldAccess(quote(streamSym),"end(start_"+quote(op.v)+")") + ") {")
      emitAssignment(quote(op.v), "start_"+quote(op.v))
    }
    else {
      this match {
        case g: CCodegen if(loopLevel==1 && Config.debug && op.body.size==1) => 
          // only add openmp pragma to the outer-most loop in debug mode
          (symList zip op.body) foreach {
            case (sym, elem: DeliteCollectElem[_,_,_]) => 
              stream.println("#pragma omp parallel for")
            case (sym, elem: DeliteReduceElem[_]) => 
              //TODO: figure out the reduction operator.
              //stream.println("#pragma omp parallel for reduction(+:" + quote(sym) + ")")
            case _ => //
          }
          stream.println("for (int " + quote(op.v) + "=0; " + quote(op.v) + "<" + quote(op.size) + ";" + quote(op.v) + "++) {  // begin fat loop " + symList.map(quote).mkString(",")/*}*/)
        case _ => stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(",")/*}*/)
      } 
    }

    // body
    emitMultiLoopFuncs(op, symList)
    emitMultiHashElem(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) })
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitCollectElem(op, sym, elem)
      case (sym, elem: DeliteHashElem[_,_]) => // done above
      case (sym, elem: DeliteForeachElem[_]) =>
        emitForeachElem(op, sym, elem)
      case (sym, elem: DeliteReduceElem[_]) =>
        emitReduceElem(op, sym, elem)
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        emitReduceTupleElem(op, sym, elem)
    }

    this match {
      case g: CCodegen if(loopLevel==1 && Config.debug && op.body.size==1) => // 
      case _ => emitAssignment(quote(op.v), quote(op.v) + " + 1")  
    }
    stream.println(/*{*/"} // end fat loop " + symList.map(quote).mkString(","))
    
    if (streamVars.length > 0) {
      val streamSym = streamVars(0)
      emitMethodCall(fieldAccess(quote(streamSym),"close"),List("start_"+quote(op.v)))
    }

    // finalizer
    emitMultiHashFinalize(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) })
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        // if we are using parallel buffers, set the logical size of the output since it
        // might be different than the physically appended size for some representations
        if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
          emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), quote(sym) + "_buf")
          getActBuffer = List(quote(elem.buf.allocVal))
          emitAssignment(quote(elem.buf.sV), quote(sym) + "_conditionals")
          emitBlock(elem.buf.setSize)
        }
        else {
          emitValDef(elem.buf.allocVal, quote(sym) + "_data")
        }
        emitBlock(elem.buf.finalizer)
        emitValDef(sym, quote(getBlockResult(elem.buf.finalizer)))
      case (sym, elem: DeliteHashElem[_,_]) =>
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

    emitAbstractFatLoopHeader(kernelName, actType)

    emitMethod("size", remap(Manifest.Int), Nil) { emitReturn(quote(op.size)) }

    emitFieldDecl("loopStart", remap(Manifest.Int))
    emitFieldDecl("loopSize", remap(Manifest.Int))

    emitMethod("alloc", actType, Nil) {
      emitValDef("__act", actType, createInstance(actType))
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) => elem.par match {
          case ParBuffer | ParSimpleBuffer =>
            stream.println("// " + fieldAccess("__act",quote(sym)) + " stays null for now")
          case ParFlat =>
            emitValDef(elem.buf.sV, "loopSize")
            emitBlock(elem.buf.alloc)
            if (Config.generateSerializable) {
              val arraySym = if (!remap(elem.buf.alloc.tp).contains("DeliteArray")) fieldAccess(quote(getBlockResult(elem.buf.alloc)), dc_data_field(getBlockResult(elem.buf.alloc))(elem.mA)) else quote(getBlockResult(elem.buf.alloc))
              emitAssignment(fieldAccess(arraySym,"offset"), "loopStart") //FIXME: extremely hacky
            }
            emitAssignment(fieldAccess("__act",quote(sym)+"_data"),quote(getBlockResult(elem.buf.alloc)))
        }
        case (sym, elem: DeliteHashElem[_,_]) => //
        case (sym, elem: DeliteForeachElem[_]) =>
          emitAssignment(fieldAccess("__act",quote(sym)), remap(sym.tp), "()")  // must be type Unit, initialized in init below
        case (sym, elem: DeliteReduceElem[_]) =>
          emitBlock(elem.zero)
          emitAssignment(fieldAccess("__act",quote(sym)+"_zero"),quote(getBlockResult(elem.zero)))
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          emitBlock(elem.zero._1)
          emitAssignment(fieldAccess("__act",quote(sym)+"_zero"),quote(getBlockResult(elem.zero._1)))
          emitBlock(elem.zero._2)
          emitAssignment(fieldAccess("__act",quote(sym)+"_zero_2"),quote(getBlockResult(elem.zero._2)))
      }
      emitReturn("__act")
    }

    // processRange
    emitMethod("processRange", actType, List(("__act",actType),("start",remap(Manifest.Int)),("end",remap(Manifest.Int)))) {
      //GROSS HACK ALERT!
      val freeVars = getFreeVarBlock(Block(Combine(getMultiLoopFuncs(op,symList).map(getBlockResultFull))),List(op.v)).filter(_ != op.size).distinct

      val streamVars = freeVars.filter(_.tp == manifest[DeliteFileStream])
      if (streamVars.length > 0) {
        assert(streamVars.length == 1, "ERROR: don't know how to handle multiple stream inputs at once")
        val streamSym = streamVars(0)
        emitMethodCall(fieldAccess(quote(streamSym),"openAtNewLine"),List("start"))
        emitValDef("isEmpty",remap(Manifest.Boolean),fieldAccess(quote(streamSym),"end(start)") + " - " + fieldAccess(quote(streamSym),"pos(start)") + " <= 0")
        emitValDef("__act2",actType,"init(__act,start,isEmpty)")
        stream.println("while (" + fieldAccess(quote(streamSym),"pos(start)") + " < " + fieldAccess(quote(streamSym),"end(start)") + ") {")
        emitMethodCall("process",List("__act2","start"))
        stream.println("}")
        emitMethodCall(fieldAccess(quote(streamSym),"close"),List("start"))
      }
      else {
        emitValDef("isEmpty",remap(Manifest.Boolean),"end-start <= 0")
        emitValDef("__act2",actType,"init(__act,start,isEmpty)") // TODO: change to use method call
        emitVarDef("idx", remap(Manifest.Int), "start + 1")
        stream.println("while (idx < end) {")
        emitMethodCall("process",List("__act2","idx"))
        emitAssignment("idx","idx + 1")
        stream.println("}")
      }
      emitReturn("__act2")
    }

    // init and compute first element
    emitMethod("init", actType, List(("__act",actType),(quote(op.v),remap(op.v.tp)),("isEmpty",remap(Manifest.Boolean)))) {
      if (op.body exists (b => loopBodyNeedsCombine(b) || loopBodyNeedsPostProcess(b))) {
        emitValDef("__act2", actType, createInstance(actType))
        emitKernelMultiHashInit(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act2")
        (symList zip op.body) foreach {
          case (sym, elem: DeliteCollectElem[_,_,_]) => elem.par match {
            case ParBuffer | ParSimpleBuffer =>
              emitValDef(elem.buf.sV, "0")
              emitBlock(elem.buf.alloc)
              emitAssignment(fieldAccess("__act2",quote(sym)+"_buf"),quote(getBlockResult(elem.buf.alloc)))
            case ParFlat =>
              emitAssignment(fieldAccess("__act2",quote(sym)+"_data"),fieldAccess("__act",quote(sym)+"_data"))
            }
          case (sym, elem: DeliteHashElem[_,_]) =>
          case (sym, elem: DeliteForeachElem[_]) => // nothing needed - this case only happens if a ForeachElem is fused with something else that needs combine
          case (sym, elem: DeliteReduceElem[_]) =>
            emitAssignment(fieldAccess("__act2",quote(sym)+"_zero"),fieldAccess("__act",quote(sym)+"_zero"))
            // should we throw an exception instead on an empty reduce?
            if (elem.stripFirst) {
              stream.println("if (isEmpty) // stripping the first iter: only initialize to zero if empty")
              emitAssignment(fieldAccess("__act2",quote(sym)),fieldAccess("__act2",quote(sym)+"_zero"))
            } else {
              if (isPrimitiveType(sym.tp)) {
                emitAssignment(fieldAccess("__act2",quote(sym)),fieldAccess("__act2",quote(sym)+"_zero"))
              } else {
                emitBlock(elem.accInit)
                emitAssignment(fieldAccess("__act2",quote(sym)),quote(getBlockResult(elem.accInit))) // separate zero buffer
              }
            }
          case (sym, elem: DeliteReduceTupleElem[_,_]) =>
            // no strip first here ... stream.println("assert(false, \"TODO: tuple reduce\")")
            emitAssignment(fieldAccess("__act2",quote(sym)+"_zero"), fieldAccess("__act",quote(sym)+"_zero"))
            emitAssignment(fieldAccess("__act2",quote(sym)+"_zero_2"), fieldAccess("__act",quote(sym)+"_zero_2"))
            emitAssignment(fieldAccess("__act2",quote(sym)), fieldAccess("__act2",quote(sym)+"_zero"))
            emitAssignment(fieldAccess("__act2",quote(sym)+"_2"), fieldAccess("__act2",quote(sym)+"_zero_2"))
        }
        // then emit first element initializers, if size is non-zero
        stream.println("if (!isEmpty) {")
        emitMultiLoopFuncs(op, symList)
        emitMultiHashElem(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act2")
        (symList zip op.body) foreach {
          case (sym, elem: DeliteCollectElem[_,_,_]) =>
            emitCollectElem(op, sym, elem, "__act2")
          case (sym, elem: DeliteHashElem[_,_]) =>
          case (sym, elem: DeliteForeachElem[_]) =>
            emitForeachElem(op, sym, elem)
            emitAssignment(fieldAccess("__act2",quote(sym)),quote(sym))
          case (sym, elem: DeliteReduceElem[_]) =>
            if (elem.stripFirst) {
              emitFirstReduceElemAssign(op, sym, elem, "__act2")
            } else {
              emitReduceElem(op, sym, elem, "__act2")
            }
          case (sym, elem: DeliteReduceTupleElem[_,_]) =>
            emitReduceTupleElem(op, sym, elem, "__act2")
        }
        stream.println("}")
        emitReturn("__act2")
      } else {
        stream.println("if (!isEmpty) {")
        emitMethodCall("process", List("__act",quote(op.v)))
        stream.println("}")
        emitReturn("__act")
      }
    }

    emitMethod("process", remap(Manifest.Unit), List(("__act",actType),(quote(op.v),remap(op.v.tp)))) {
      emitMultiLoopFuncs(op, symList)
      emitMultiHashElem(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitCollectElem(op, sym, elem, "__act")
        case (sym, elem: DeliteHashElem[_,_]) => // done above
        case (sym, elem: DeliteForeachElem[_]) =>
          emitVarDef(quote(sym), remap(sym.tp), "()")
          emitForeachElem(op, sym, elem)
        case (sym, elem: DeliteReduceElem[_]) =>
          emitReduceElem(op, sym, elem, "__act")
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          emitReduceTupleElem(op, sym, elem, "__act")
      }
    }

    emitMethod("combine", remap(Manifest.Unit), List(("__act",actType),("rhs",actType))) {
      emitMultiHashCombine(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          if (Config.generateSerializable) {
            val tpe = remap(sym.tp)
            val obj = if (tpe.contains("DeliteArrayObject")) tpe.take(tpe.indexOf("[")) else tpe
            emitAssignment(fieldAccess("__act",quote(sym)), obj+".combine(" + fieldAccess("__act",quote(sym)) + "," + fieldAccess("rhs",quote(sym)) + ")")
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) => // nothing needed
        case (sym, elem: DeliteReduceElem[_]) =>
          // if either value is zero, return the other instead of combining
          emitValDef(elem.rV._1, fieldAccess("__act",quote(sym)))
          emitValDef(elem.rV._2, fieldAccess("rhs", quote(sym)))
          stream.println("if (" + quote(elem.rV._1) + " == " + fieldAccess("__act", quote(sym) + "_zero") + ") {"/*}*/) //TODO: what if zero is an accumulator (SumIf)?
          emitAssignment(fieldAccess("__act",quote(sym)), quote(elem.rV._2))
          stream.println(/*{*/"}")
          stream.println("else if (" + quote(elem.rV._2) + " != " + fieldAccess("__act", quote(sym) + "_zero") + ") {"/*}*/) //TODO: see above
          emitBlock(elem.rFunc)
          emitAssignment(fieldAccess("__act",quote(sym)), quote(getBlockResult(elem.rFunc)))
          stream.println(/*{*/"}")
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          // stream.println("assert(false, \"TODO: tuple reduce\")")
          val rV = elem.rVPar
          val rFunc = elem.rFuncPar
          emitValDef(rV._1._1, fieldAccess("__act",quote(sym)))
          emitValDef(rV._1._2, fieldAccess("__act",quote(sym)+"_2"))
          emitValDef(rV._2._1, fieldAccess("rhs",quote(sym)))
          emitValDef(rV._2._2, fieldAccess("rhs",quote(sym)+"_2"))
          emitFatBlock(List(rFunc._1, rFunc._2))
          emitAssignment(fieldAccess("__act",quote(sym)),quote(getBlockResult(rFunc._1)))
          emitAssignment(fieldAccess("__act",quote(sym)+"_2"), quote(getBlockResult(rFunc._2)))
      }
    }
    // scan/postprocess follows

    emitMethod("postCombine", remap(Manifest.Unit), List(("__act",actType),("lhs",actType))) {
      emitMultiHashPostCombine(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            emitAssignment(fieldAccess("__act", quote(sym) + "_offset"),fieldAccess("lhs", quote(sym) + "_offset") + "+" + fieldAccess("lhs", quote(sym) + "_size"))
            emitAssignment(fieldAccess("__act", quote(sym) + "_conditionals"),fieldAccess("__act", quote(sym) + "_conditionals") + "+" + fieldAccess("lhs", quote(sym) + "_conditionals"))
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
      //XX link act frames so we can set data later
      emitAssignment(fieldAccess("__act","left_act"), "lhs")
    }

    emitMethod("postProcInit", remap(Manifest.Unit), List(("__act",actType))) { // only called for last chunk!!
      emitMultiHashPostProcInit(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, actType, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            stream.println("if (" + fieldAccess("__act", quote(sym) + "_offset") + " > 0) {")
            emitValDef(elem.buf.sV, fieldAccess("__act", quote(sym) + "_offset") + " + " + fieldAccess("__act",quote(sym) + "_size"))
            emitValDef(elem.buf.allocVal, fieldAccess("__act", quote(sym) + "_buf"))
            emitBlock(elem.buf.allocRaw)
            emitMethodCall(fieldAccess("__act",quote(sym) + "_data_set"),List(quote(getBlockResult(elem.buf.allocRaw)),fieldAccess("__act", quote(sym) + "_conditionals")))
            stream.println("} else {")
            emitMethodCall(fieldAccess("__act",quote(sym) + "_data_set"),List(fieldAccess("__act", quote(sym) + "_buf"),fieldAccess("__act", quote(sym) + "_conditionals")))
            stream.println("}")
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
    }

    emitMethod("postProcess", remap(Manifest.Unit), List(("__act",actType))) {
      emitMultiHashPostProcess(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, actType, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            // write size results from buf into data at offset
            stream.println("if (" + fieldAccess("__act",quote(sym)+"_data") + " " + refNotEq + " " + fieldAccess("__act",quote(sym)+"_buf") + ") {")
            emitValDef(elem.buf.sV, fieldAccess("__act",quote(sym)+"_size"))
            emitValDef(elem.buf.aV2, fieldAccess("__act",quote(sym)+"_buf"))
            emitValDef(elem.buf.allocVal, fieldAccess("__act",quote(sym)+"_data"))
            emitValDef(elem.buf.iV, "0")
            emitValDef(elem.buf.iV2, fieldAccess("__act",quote(sym)+"_offset"))
            emitBlock(elem.buf.copyRaw)
            stream.println("}")
            releaseRef(fieldAccess("__act",quote(sym)+"_buf"))
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
    }

    emitMethod("finalize", remap(Manifest.Unit), List(("__act",actType))) {
      emitMultiHashFinalize(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess("__act",quote(sym) + "_data"))
          releaseRef(fieldAccess("__act",quote(sym)+"_data"))
          getActBuffer = List(quote(elem.buf.allocVal))
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            emitValDef(elem.buf.sV, fieldAccess("__act", quote(sym) + "_conditionals"))
            emitBlock(elem.buf.setSize)
          }
          emitBlock(elem.buf.finalizer)
          emitAssignment(fieldAccess("__act",quote(sym)), quote(getBlockResult(elem.buf.finalizer)))
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
    }

    //TODO: This would not be needed if other targets (CUDA, C, etc) properly creates activation records
    emitMethod("initAct", "activation_"+kernelName, List()) {
      emitValDef("act", "activation_"+kernelName, "new activation_"+kernelName)
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
          emitBlock(elem.zero)
          emitAssignment(fieldAccess("act",quote(sym)+"_zero"),quote(getBlockResult(elem.zero)))
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          emitBlock(elem.zero._1)
          emitAssignment(fieldAccess("act",quote(sym)+"_zero"),quote(getBlockResult(elem.zero._1)))
          emitBlock(elem.zero._2)
          emitAssignment(fieldAccess("act",quote(sym)+"_zero_2"),quote(getBlockResult(elem.zero._2)))
        case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
          //emitAssignment(fieldAccess("act",quote(sym)+"_hash_data"), "new Array(128)")
        case (sym, elem: DeliteHashElem[_,_]) =>
      }

      val hashElems = (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }
      for ((cond,cps) <- hashElems.groupBy(_._2.cond)) {
        for((key,kps) <- cps.groupBy(_._2.keyFunc))
          emitAssignment(fieldAccess("act",kps.map(p=>quote(p._1)).mkString("")+"_hash_pos"), createInstance(hashmapType(remap(getBlockResult(key).tp)), List("512","128")))
      }
      emitReturn("act")
    }

    emitAbstractFatLoopFooter()
  }

  def emitAbstractFatLoopKernelExtra(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    val kernelName = symList.map(quote).mkString("")
    val actType = "activation_" + kernelName
    emitClass(actType) {
      emitFieldDecl("left_act", actType)
      emitKernelMultiHashDecl(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, actType)
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitFieldDecl(quote(sym), remap(sym.tp))
          emitFieldDecl(quote(sym) + "_data", remap(elem.buf.allocVal.tp))
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            emitFieldDecl(quote(sym) + "_buf", remap(elem.buf.allocVal.tp))
            emitFieldDecl(quote(sym) + "_size", remap(Manifest.Int))
            emitFieldDecl(quote(sym) + "_offset", remap(Manifest.Int))
            emitFieldDecl(quote(sym) + "_conditionals", remap(Manifest.Int))
            emitMethod(quote(sym)+"_data_set", remap(Manifest.Unit), List(("xs",remap(elem.buf.allocVal.tp)),("cs",remap(Manifest.Int)))) {
              emitAssignment(quote(sym) + "_data", "xs")
              emitAssignment(quote(sym) + "_conditionals", "cs")
              stream.println("if (left_act " + refNotEq + " " + nullRef + ")")
              emitMethodCall(fieldAccess("left_act",quote(sym)+"_data_set"),List("xs","cs")) // XX linked frame
            }
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
          emitFieldDecl(quote(sym), remap(sym.tp))
        case (sym, elem: DeliteReduceElem[_]) =>
          emitFieldDecl(quote(sym), remap(sym.tp))
          emitFieldDecl(quote(sym)+"_zero", remap(sym.tp))
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          emitFieldDecl(quote(sym), remap(sym.tp))
          emitFieldDecl(quote(sym)+"_2", remap(elem.func._2.tp))
          emitFieldDecl(quote(sym)+"_zero", remap(sym.tp))
          emitFieldDecl(quote(sym)+"_zero_2", remap(elem.func._2.tp))
      }
      if (Config.generateSerializable) {
        emitMethod("serialize", "java.util.ArrayList[com.google.protobuf.ByteString]", List()) {
          def serializeRef(sym: String) = "ppl.delite.runtime.messages.Serialization.serialize(this." + sym + ", true, \"" + sym + "\")"
          def serializeVal(sym: String, size: String = "-1") = "ppl.delite.runtime.messages.Serialization.serialize(this." + sym + ", 0, " + size + ")"

          emitValDef("arr", "java.util.ArrayList[com.google.protobuf.ByteString]", "new java.util.ArrayList")
          def prefix = "arr.add"
          var firstHash = true
          (symList zip op.body) foreach {
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              if (firstHash) {
                emitValDef("size", remap(Manifest.Int), kernelName+"_hash_pos.size")
                emitMethodCall(prefix, List(serializeVal(kernelName+"_hash_pos.unsafeKeys", "size")))
                //emitMethodCall(prefix, List(serializeVal(kernelName+"_hash_pos.unsafeIndices"))) //TODO: remove this
                firstHash = false
              }
              emitMethodCall(prefix, List(serializeVal(quote(sym)+"_hash_data", "size")))
            case (sym, elem: DeliteCollectElem[_,_,_]) =>
              emitMethodCall(prefix, List(serializeRef(quote(sym))))
            case (sym, elem: DeliteForeachElem[_]) =>
            case (sym, elem: DeliteReduceElem[_]) =>
              emitMethodCall(prefix, List(serializeVal(quote(sym))))
              //emitMethodCall(prefix, List(serializeVal(quote(sym)+"_zero")))
            case (sym, elem: DeliteReduceTupleElem[_,_]) =>
              emitMethodCall(prefix, List(serializeVal(quote(sym))))
              emitMethodCall(prefix, List(serializeVal(quote(sym)+"_2")))
          }
          emitReturn("arr")
        }

        //TODO: This would not be needed if other targets (CUDA, C, etc) properly creates activation records
        //Target devices should send back the key array also, not just the data.
        //This unwrapping would only work for dense perfect hash cases.
        emitMethod("unwrap", remap(Manifest.Unit), List()) {
          val keyGroups = (symList zip op.body) collect { case (sym, elem: DeliteHashReduceElem[_,_,_,_]) => (sym,elem) } groupBy(_._2.keyFunc)
          for((key,kps) <- keyGroups) {
            val name = kps.map(p=>quote(p._1)).mkString("")
            emitVarDef("i_"+name, remap(Manifest.Int), "0")
            stream.println("while(i_"+name+" < " + fieldAccess(quote(kps(0)._1),"length") + ") {")
            emitMethodCall(fieldAccess(name+"_hash_pos","put"),List("i_"+name))
            emitAssignment("i_"+name,"i_"+name+"+1")
            stream.println("}")
          }
          (symList zip op.body) foreach {
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              emitAssignment(quote(sym)+"_hash_data",quote(sym))
              releaseRef(quote(sym))
            case _ =>
          }
        }

      }
    }

    emitObject("activation_" + kernelName) {
      if (Config.generateSerializable) {
        emitMethod("deserialize", "activation_"+kernelName, List(("bytes", "java.util.List[com.google.protobuf.ByteString]"))) {
          var idx = -1
          def deserialize(tp: String) = {
            idx += 1
            if (tp.contains("DeliteArrayObject")) //FIXME: need to handle this generically
              "ppl.delite.runtime.messages.Serialization.deserializeDeliteArrayObject["+tp.substring(tp.indexOf("[")+1,tp.lastIndexOf("]"))+"](ppl.delite.runtime.messages.Messages.ArrayMessage.parseFrom(bytes.get("+idx+")))"
            else
              "ppl.delite.runtime.messages.Serialization.deserialize(classOf["+tp+"], bytes.get(" + idx + "))"
          }
          emitValDef("act", "activation_"+kernelName, "new activation_"+kernelName)
          val prefix = "act."
          var firstHash = true
          (symList zip op.body) foreach {
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              if (firstHash) {
                val keyType = if (isPrimitiveType(elem.keyFunc.tp)) "ppl.delite.runtime.data.DeliteArray" + remap(elem.keyFunc.tp) else "ppl.delite.runtime.data.DeliteArrayObject[" + remap(elem.keyFunc.tp) + "]"
                emitValDef("keys", keyType, deserialize(keyType))
                //FIXME: kernelName is only correct if entire kernel is one hash_pos!
                emitAssignment(prefix+kernelName+"_hash_pos", "", "new generated.scala.container.HashMapImpl[" + remap(elem.keyFunc.tp) + "](512,128)")
                stream.println("for (i <- 0 until keys.length) " + prefix+kernelName+"_hash_pos.put(keys(i))") //FIXME!
                firstHash = false
              }
              emitAssignment(prefix+quote(sym)+"_hash_data", "", deserialize(remap(sym.tp)))
            case (sym, elem: DeliteCollectElem[_,_,_]) =>
              emitAssignment(prefix+quote(sym), "", deserialize(remap(sym.tp)))
            case (sym, elem: DeliteForeachElem[_]) =>
            case (sym, elem: DeliteReduceElem[_]) =>
              emitAssignment(prefix+quote(sym), "", deserialize(remap(sym.tp)))
              //emitAssignment(prefix+quote(sym)+"_zero", "", deserialize(remap(sym.tp)))
            case (sym, elem: DeliteReduceTupleElem[_,_]) =>
              emitAssignment(prefix+quote(sym), "", deserialize(remap(sym.tp)))
              emitAssignment(prefix+quote(sym)+"_2", "", deserialize(remap(elem.func._2.tp)))
          }
          emitReturn("act")
        }
      }
    }
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
      if (!deliteKernel) { loopLevel += 1; emitInlineAbstractFatLoop(op, symList); loopLevel -= 1; }
      else emitKernelAbstractFatLoop(op, symList)
    case _ => super.emitFatNode(symList, rhs)
  }

}

trait ScalaGenDeliteOps extends ScalaGenLoopsFat with ScalaGenStaticDataDelite with GenericGenDeliteOps { // not sure where to mix in ScalaGenStaticData
  import IR._

  def quotearg(x: Sym[Any]) = quote(x) + ": " + quotetp(x)
  def quotetp(x: Sym[Any]) = remap(x.tp)

  def methodCall(name:String, inputs: List[String] = Nil): String = {
    inputs match {
      case Nil => name
      case _ => name + "(" + inputs.mkString(",") + ")"
    }
  }

  def emitMethodCall(name:String, inputs: List[String]) {
    stream.println(methodCall(name, inputs))
  }

  def emitMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit) {
    stream.println("def " + name + "(" + inputs.map(i => i._1 + ":" + i._2).mkString(",") + "): " +  outputType + " = {")
    body
    stream.println("}\n")
  }

  def createInstance(typeName:String, args: List[String] = Nil): String = {
    "new " + typeName + args.mkString("(",",",")")
  }

  def fieldAccess(className: String, varName: String): String = {
    if (className == "") varName
    else className + "." + varName
  }

  def releaseRef(varName: String) {
    stream.println(varName + " = null")
  }

  def emitReturn(rhs: String) = stream.print(rhs)

  def emitFieldDecl(name: String, tpe: String) {
    emitVarDef(name, tpe, "_")
  }

  def emitClass(name: String)(body: => Unit) {
    stream.println("final class " + name + " {")
    body
    stream.println("}")
  }

  def emitObject(name: String)(body: => Unit) {
    stream.println("object " + name + " {")
    body
    stream.println("}")
  }

  def emitValDef(name: String, tpe: String, init: String) {
    stream.println("val " + name + ": " + tpe + " = " + init)
  }

  def emitVarDef(name: String, tpe: String, init: String) {
    stream.println("var " + name + ": " + tpe + " = " + init)
  }

  def emitAssignment(name: String, tpe: String, rhs: String) = emitAssignment(name, rhs)

  def emitAssignment(lhs: String, rhs: String) {
    stream.println(lhs + " = " + rhs)
  }

  def emitAbstractFatLoopHeader(className: String, actType: String) {
    stream.println("val " + className + " = new generated.scala.DeliteOpMultiLoop[" + actType + "] {"/*}*/)
  }

  def emitAbstractFatLoopFooter() {
    stream.println("}")
  }

  def refNotEq: String = "ne"
  def nullRef: String = "null"

  def arrayType(argType: String): String = "Array[" + argType + "]"
  def arrayApply(arr: String, idx: String): String = arr + "(" + idx + ")"
  def newArray(argType: String, size: String): String = createInstance(arrayType(argType),List(size))
  def hashmapType(argType: String): String = "generated.scala.container.HashMapImpl[" + argType + "]"
  def typeCast(sym: String, to: String): String = "(" + sym + ").asInstanceOf[" + to + "]"
  def withBlock(name: String)(block: => Unit): Unit = {
    emitValDef(name, remap(manifest[Unit]), "{")
    block
    stream.println("}")
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

trait GPUGenDeliteOpsOpt extends GPUGenDeliteOps {
  import IR._

  var multiDimMapping: Boolean = false
  var currentLoopLevel: Int = 0
  var maxLoopLevel: Int = 0

  private def getDimIdx(level: Int): String = {
    loopAnalysisResult.get(level) match {
      case Some((DimX,_,_)) => "threadIdx.x"
      case Some((DimY,_,_)) => "threadIdx.y"
      case Some((DimZ,_,_)) => "threadIdx.z"
      case _ => throw new GenerationFailedException("getCurrentDimIdx")
    }
  }

  private def getDimGlobalIdx(level: Int): String = {
    loopAnalysisResult.get(level) match {
      case Some((DimX,_,_)) => "blockIdx.x * blockDim.x + threadIdx.x"
      case Some((DimY,_,_)) => "blockIdx.y * blockDim.y + threadIdx.y"
      case Some((DimZ,_,_)) => "blockIdx.z * blockDim.z + threadIdx.z"
      case _ => throw new GenerationFailedException("getCurrentDimGlobalIdx")
    }
  }

  private def getDimStride(level: Int): String = {
    loopAnalysisResult.get(level) match {
      case Some((DimX,_,_)) => "gridDim.x * blockDim.x"
      case Some((DimY,_,_)) => "gridDim.y * blockDim.y"
      case Some((DimZ,_,_)) => "gridDim.z * blockDim.z"
      case _ => throw new GenerationFailedException("getCurrentDimGlobalIdx")
    }
  }

  private def getCurrentDimIdx: String = getDimIdx(currentLoopLevel)
  private def getCurrentDimGlobalIdx: String = getDimGlobalIdx(currentLoopLevel)
  private def getCurrentDimStride: String = getDimStride(currentLoopLevel)

  private def getDimSize(level: Int): String = {
    loopAnalysisResult.get(level) match {
    // Note: using blockDim.x to allocate shared memory is not allowed.
    // They should be constants
/*
      case Some((DimX,_,_)) => "blockDim.x"
      case Some((DimY,_,_)) => "blockDim.y"
      case Some((DimZ,_,_)) => "blockDim.z"
*/
      case Some((_,size,_)) => size.toString
      case _ => throw new GenerationFailedException("getCurrentDimSize")
    }
  }

  private def getCurrentDimSize: String = getDimSize(currentLoopLevel)

  private def getOuterLoopDimSizes: List[String] = {
    val indices = (0 until loopAnalysisResult.size).filter(_ < currentLoopLevel)
    indices.map(getDimSize).toList
  }

  private def getInnerLoopDimIndices: List[String] = {
    val indices = (0 until loopAnalysisResult.size).filter(_ > currentLoopLevel)
    indices.map(getDimIdx).toList
  }
  private def getOuterLoopDimIndices: List[String] = {
    val indices = (0 until loopAnalysisResult.size).filter(_ < currentLoopLevel)
    indices.map(getDimIdx).toList
  }
  def getInnerLoopGuard: String = {
    val indices = (0 until loopAnalysisResult.size).filter(_ > currentLoopLevel)
    "if(" + indices.map(i => "(" + getDimIdx(i) + "== 0)").mkString("&&") + ")"
  }


  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop if (loopAnalysisResult.size > 1) =>
      multiDimMapping = true
      maxLoopLevel = loopAnalysisResult.size - 1
      if (deliteKernel) {
        currentLoopLevel = 0
        emitKernelAbstractFatLoop(op, symList)
        metaData.auxMeta.append(("multiDim","[" + loopAnalysisResult.map(m => "{\"level\":\"" + m._1 + "\"," + m._2._1 + ",\"size\":\"" + m._2._2 + "\"," + m._2._3.toString(quote) + "}").mkString(",") + "]"))
      }
      else { currentLoopLevel += 1; emitInlineAbstractFatLoop(op, symList); currentLoopLevel -= 1; }
    case _ =>
      multiDimMapping = false
      super.emitFatNode(symList, rhs)
  }

  override def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    if (multiDimMapping == false) {
      super.emitInlineAbstractFatLoop(op, symList)
    }
    else {
      (symList zip op.body) foreach {
        //TODO: Check if primitive type operations
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          boundMap.put(elem.buf.sV,op.size)
          emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quote(op.size))
          emitBlock(elem.buf.alloc) //This will generate alloc failure exception
          emitValDef(quote(sym) + "_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
          emitVarDef(quote(sym) + "_size", remap(Manifest.Int), "0")
          //throw new GenerationFailedException("GPUGen: Inlined DeliteCollectElem is not supported yet due to memory allocations.\n" + quotePos(sym))
        case (sym, elem: DeliteForeachElem[_]) =>
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


      loopAnalysisResult.get(currentLoopLevel) match {
        case Some((_,_,SpanOne(_))) =>
          stream.println("int " + quote(op.v) + " = " + getCurrentDimGlobalIdx + ";")
          //stream.println("if (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
          stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))

        case _ =>
          stream.println("int " + quote(op.v) + " = " + getCurrentDimIdx + ";")
          stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
      }

      // body
      emitMultiLoopFuncs(op, symList)
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
          emitValDef(elem.buf.allocVal, quote(sym)+"_data")
          emitBlock(elem.buf.update)
        case (sym, elem: DeliteForeachElem[_]) =>
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

      loopAnalysisResult.get(currentLoopLevel) match {
        //case Some((_,_,SpanOne(_))) =>
        //  stream.println(quote(op.v) + " += " + getCurrentDimSize + ";")
        case Some((_,_,SpanAll)) =>
          stream.println(quote(op.v) + " += " + getCurrentDimSize + ";")
        case Some((_,_,SpanOne(_))) =>
          stream.println(quote(op.v) + " += " + getCurrentDimStride + ";")
        case _ => //
      }
      stream.println(/*{*/"} // end fat loop " + symList.map(quote).mkString(","))

      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitValDef(elem.buf.allocVal, quote(sym) + "_data")
          emitBlock(elem.buf.finalizer)
          emitValDef(sym, quote(getBlockResult(elem.buf.finalizer)))
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
          // reduce across threads in this dimension
          stream.println("__shared__ " + remap(sym.tp) + " shared_" + quote(sym) + getOuterLoopDimSizes.map("["+_+"]").mkString("") + "[" + getCurrentDimSize + "];")
          stream.println("shared_" + quote(sym) + getOuterLoopDimIndices.map("["+_+"]").mkString("") + "[" + getCurrentDimIdx + "] = " + quote(sym) + ";")
          stream.println("__syncthreads();")
          stream.println("if(" + getCurrentDimIdx + " == 0) { for(int i=1; i<" + getCurrentDimSize + "; i++) { " + quote(sym) + " += shared_" + quote(sym) + getOuterLoopDimIndices.map("["+_+"]").mkString("") + "[i]; } }")
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
    }
  }

  override def remapInputs(inputs: List[Sym[Any]], size: Exp[Int] = outerLoopSize, v: Sym[Int] = outerLoopSym.asInstanceOf[Sym[Int]]) : List[String] = {
    if (multiDimMapping == false) {
      super.remapInputs(inputs, size, v)
    }
    else {
      // last inputs always added to any device functions
      def lastInputs = (size match {
        case s@Sym(_) => List(size).map(i => remap(i.tp) + " " + quote(i))
        case _ => List("int size")
      }) ++ List("TEMP_"+kernelName+" size_t tempMemSize","char *tempMemPtr","int *tempMemUsage, activation_"+kernelName+" act")

      inputs.filter(_ != size).map(s =>
        if(inVars contains s)
          deviceTarget + "Ref" + remap(s.tp) + " " + quote(s)
        else
          remap(s.tp) + " " + quote(s)
      ) ++ lastInputs
    }
  }

  // emit process functions
  override def emitProcessMethods(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    if (multiDimMapping == false) {
      super.emitProcessMethods(op, symList)
    }
    else {
      emitHashReduceElemProcess(op, symList)
      (symList zip op.body) foreach {
        case (sym, elem:DeliteCollectElem[_,_,_]) =>
          val freeVars = (getFreeVarBlock(Block(Combine((List(elem.func,elem.buf.update,elem.buf.appendable)++elem.cond).map(getBlockResultFull))),List(elem.buf.eV,elem.buf.allocVal,op.v,sym))++List(sym)).filter(_ != op.size).distinct
          val inputs = remapInputs(freeVars)
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          stream.println("__device__ void dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          //emitBlock(elem.func)
          //emitValDef(elem.eV, quote(getBlockResult(elem.func)))
          //if(multiDimMapping)
          //emitValDef(op.v, getCurrentDimGlobalIdx)
          loopAnalysisResult.get(currentLoopLevel) match {
            case Some((_,_,SpanOne(_))) =>
              stream.println("int " + quote(op.v) + " = " + getCurrentDimGlobalIdx + ";")
              stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
            case _ =>
              stream.println("int " + quote(op.v) + " = " + getCurrentDimIdx + ";")
              stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
          }

          elem.par match {
            case ParSimpleBuffer =>
              //emitValDef(elem.allocVal, "act." + quote(sym) + "_buf")
              emitFatBlock(elem.cond)
              if (elem.cond.nonEmpty) stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
              emitBlock(elem.buf.appendable)
              stream.println("if (" + quote(getBlockResult(elem.buf.appendable)) + ") {")
              //emitValDef(elem.allocVal, "act." + quote(sym) + "_buf")
              //emitBlock(elem.update)
              stream.println("act." + quote(sym) + "_bitmap[" + quote(op.v) + "] = 1;")
              stream.println("}")
              if (elem.cond.nonEmpty) {
                // Need this for GPU?
                // stream.println(quote(sym) + "_conditionals[" + quote(op.v) + "] += 1;")
                stream.println("}")
              }
            case ParFlat =>
              emitBlock(elem.func)
              emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
              emitValDef(elem.buf.allocVal, quote(sym))
              //if(multiDimMapping)
              //  stream.println("if (" + getInnerLoopDimIndices.map(_ + "==0").mkString("&&") + ") {")
              emitBlock(elem.buf.update)
              //if(multiDimMapping)
              //  stream.println("}")
            case _ =>
          }
          loopAnalysisResult.get(currentLoopLevel) match {
            case Some((_,_,SpanAll)) =>
              stream.println(quote(op.v) + " += " + getCurrentDimSize + ";")
            case Some((_,_,SpanOne(_))) =>
              stream.println(quote(op.v) + " += " + getCurrentDimStride + ";")
            case _ => //
          }
          stream.println("}")
          stream.println("}")

        case (sym, elem:DeliteForeachElem[_]) =>
          val freeVars = getFreeVarBlock(elem.func,List(op.v)).filter(_ != op.size).distinct
          val inputs = remapInputs(freeVars)
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          stream.println("__device__ void dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          //emitValDef(op.v, getCurrentDimGlobalIdx)
          loopAnalysisResult.get(currentLoopLevel) match {
            case Some((_,_,SpanOne(_))) =>
              stream.println("int " + quote(op.v) + " = " + getCurrentDimGlobalIdx + ";")
              stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
            case _ =>
              stream.println("int " + quote(op.v) + " = " + getCurrentDimIdx + ";")
              stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
          }
          emitBlock(elem.func)
          loopAnalysisResult.get(currentLoopLevel) match {
            case Some((_,_,SpanAll)) =>
              stream.println(quote(op.v) + " += " + getCurrentDimSize + ";")
            case Some((_,_,SpanOne(_))) =>
              stream.println(quote(op.v) + " += " + getCurrentDimStride + ";")
            case _ => //
          }
          stream.println("}")
          stream.println("}")

        case (sym, elem: DeliteReduceElem[_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
          val freeVars = getFreeVarBlock(elem.func,List(op.v)).filter(_ != op.size).distinct
          val inputs = remapInputs(freeVars)
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          stream.println("__device__ " + remap(sym.tp) + " dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          emitValDef(op.v, getCurrentDimGlobalIdx)
          emitBlock(elem.func)
          stream.println("return " + quote(getBlockResult(elem.func)) + ";")
          stream.println("}")

        case (sym, elem:DeliteReduceElem[_]) =>
          val freeVars = getFreeVarBlock(Block(Combine((List(elem.func,elem.rFunc,elem.zero)++elem.cond).map(getBlockResultFull))),List(elem.rV._1,elem.rV._2,op.v)).filter(_ != op.size).distinct
          val inputs = remapInputs(freeVars ++ List(elem.rV._1))
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          stream.println("__device__ " + remap(sym.tp) + " dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          emitValDef(op.v, getCurrentDimGlobalIdx)
          emitBlock(elem.func)
          emitValDef(elem.rV._2, quote(getBlockResult(elem.func)))
          if(elem.cond.nonEmpty) {
            emitFatBlock(elem.cond)
            stream.println("if(" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
            if (elem.stripFirst) {
              emitBlock(elem.zero)
              stream.println(remap(sym.tp) + " " + quote(sym) + "_zero = " + quote(getBlockResult(elem.zero)) + ";")
              stream.println("if(" + quote(elem.rV._1) + " == " + quote(sym) + "_zero) {")
              stream.println("return " + quote(elem.rV._2) + ";")
              stream.println("}")
              stream.println("else {")
              emitBlock(elem.rFunc)
              stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
              stream.println("}")
            }
            else {
              emitBlock(elem.rFunc)
              stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
            }
            stream.println("}")
            stream.println("else {")
            stream.println("return " + quote(elem.rV._1) + ";")
            stream.println("}")
          }
          else {
            emitBlock(elem.rFunc)
            stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
          }
          stream.println("}")

        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          val freeVars = getFreeVarBlock(Block(Combine((List(elem.func._1,elem.func._2,elem.rFuncSeq._1,elem.rFuncSeq._2)++elem.cond).map(getBlockResultFull))),List(elem.rVSeq._1._1,elem.rVSeq._1._2,elem.rVSeq._2._1,elem.rVSeq._2._2,op.v)).filter(_ != op.size).distinct
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          for(i <- 1 until 3) {
            val (rVSeq1, rVSeq2, rFuncSeq) = if(i == 1) (elem.rVSeq._1._1, elem.rVSeq._2._1, elem.rFuncSeq._1)
                                             else (elem.rVSeq._1._2, elem.rVSeq._2._2, elem.rFuncSeq._2)
            val inputs = remapInputs(freeVars ++ List(elem.rVSeq._1._1,elem.rVSeq._1._2))
            stream.println("__device__ " + remap(sym.tp) + " dev_process" + i + "_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
            emitValDef(op.v, getCurrentDimGlobalIdx)
            emitFatBlock(List(elem.func._1,elem.func._2))
            emitValDef(elem.rVSeq._2._1, quote(getBlockResult(elem.func._1)))
            emitValDef(elem.rVSeq._2._2, quote(getBlockResult(elem.func._2)))
            if(elem.cond.nonEmpty) {
              emitFatBlock(elem.cond)
              stream.println("if(" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
              assert(!elem.stripFirst)
              emitBlock(rFuncSeq)
              stream.println("return " + quote(getBlockResult(rFuncSeq)) + ";")
              stream.println("}")
              stream.println("else {")
              stream.println("return " + quote(rVSeq1) + ";")
              stream.println("}")
            }
            else {
              emitBlock(rFuncSeq)
              stream.println("return " + quote(getBlockResult(rFuncSeq)) + ";")
            }
            stream.println("}")
          }

        case _ => //
      }
    }
  }


}

trait GPUGenDeliteOps extends GPUGenLoopsFat with BaseGenDeliteOps {
  import IR._

  var kernelName: String = _

  def emitVarDef(name: String, tpe: String, init: String) {
    tpe match {
      case "void" => //
      case _ =>
        stream.println(tpe + " " + name + " = " + init + ";")
    }
  }

  def emitValDef(name: String, tpe: String, init: String) {
    emitVarDef(name, tpe, init)
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop =>
      if (deliteKernel) emitKernelAbstractFatLoop(op, symList)
      else emitInlineAbstractFatLoop(op, symList)
    case _ => super.emitFatNode(symList, rhs)
  }

  def emitMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    val elemFuncs = op.body flatMap { // don't emit dependencies twice!
      //case elem: DeliteHashCollectElem[_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
      //case elem: DeliteHashReduceElem[_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
      //case elem: DeliteHashIndexElem[_,_] => elem.keyFunc :: elem.cond
      case elem: DeliteCollectElem[_,_,_] => elem.func :: elem.cond
      case elem: DeliteForeachElem[_] => List(elem.func)
      case elem: DeliteReduceElem[_] => elem.func :: elem.cond
      case elem: DeliteReduceTupleElem[_,_] => elem.func._1 :: elem.func._2 :: elem.cond
    }
    // FIXME: without .distinct TPCHQ2 has duplicate definitions. this should be fixed in emitFatBlock.
    emitFatBlock(elemFuncs.distinct)
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
        boundMap.put(elem.buf.sV,op.size)
        emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quote(op.size))
        emitBlock(elem.buf.alloc) //This will generate alloc failure exception
        emitValDef(quote(sym) + "_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
        emitVarDef(quote(sym) + "_size", remap(Manifest.Int), "0")
        //throw new GenerationFailedException("GPUGen: Inlined DeliteCollectElem is not supported yet due to memory allocations.\n" + quotePos(sym))
      case (sym, elem: DeliteForeachElem[_]) =>
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
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
        emitValDef(elem.buf.allocVal, quote(sym)+"_data")
        emitBlock(elem.buf.update)
      case (sym, elem: DeliteForeachElem[_]) =>
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

    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitValDef(elem.buf.allocVal, quote(sym) + "_data")
        emitBlock(elem.buf.finalizer)
        emitValDef(sym, quote(getBlockResult(elem.buf.finalizer)))
      case (sym, elem: DeliteForeachElem[_]) =>
      case (sym, elem: DeliteReduceElem[_]) =>
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
    }
  }

  def remapInputs(inputs: List[Sym[Any]], size: Exp[Int] = outerLoopSize, v: Sym[Int] = outerLoopSym.asInstanceOf[Sym[Int]]) : List[String] = {
      // last inputs always added to any device functions
      def lastInputs = (size match {
        case s@Sym(_) => List(v, size).map(i => remap(i.tp) + " " + quote(i))
        case _ => List(v).map(i => remap(i.tp) + " " + quote(i)) ++ List("int size")
      }) ++ List("TEMP_"+kernelName+" size_t tempMemSize","char *tempMemPtr","int *tempMemUsage, activation_"+kernelName+" act")

      inputs.filter(_ != size).map(s =>
        if(inVars contains s)
          deviceTarget + "Ref" + remap(s.tp) + " " + quote(s)
        else
          remap(s.tp) + " " + quote(s)
      ) ++ lastInputs
  }

    def emitHashReduceElemProcess(op: AbstractFatLoop, symList: List[Sym[Any]]) {
      val keyGroups = (symList zip op.body) collect { case (sym, elem: DeliteHashReduceElem[_,_,_,_]) => (sym,elem) } groupBy (_._2.keyFunc)
      for (k <- keyGroups.keySet) {
        val funcs = op.body flatMap { case e: DeliteHashReduceElem[_,_,_,_] if (e.keyFunc==k) => e.keyFunc :: e.valFunc :: e.cond }
        val freeVars = getFreeVarBlock(Block(Combine(funcs.map(getBlockResultFull))),List(op.v)).filter(_ != op.size).distinct
        val inputs = ((symList zip op.body) collect { case (s, e: DeliteHashReduceElem[_,_,_,_]) if (e.keyFunc==k) => remap(e.mK)+" *"+quote(s)+"_key,"+remap(e.mV)+" *"+quote(s)+"_val" } ) ++ remapInputs(freeVars)

        val syms = keyGroups.get(k).get.map(_._1)
        for(sym <- syms) {
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          e.funcs += "key" -> syms.map(quote)
        }
        stream.println("__device__ void dev_process_" + funcNameSuffixSyms(syms) + "(" + inputs.mkString(",") + ") {")
        emitFatBlock(funcs.distinct)
        (symList zip op.body) foreach {
          case (s,e: DeliteHashReduceElem[_,_,_,_]) if (e.keyFunc==k) =>
            stream.println(quote(s) + "_key[" + quote(op.v) + "] = " + quote(getBlockResult(e.keyFunc)) + ";")
            stream.println(quote(s) + "_val[" + quote(op.v) + "] = " + quote(getBlockResult(e.valFunc)) + ";")
          case _ =>
        }
        stream.println("}")

      }
    }

  def funcNameSuffixSyms(syms: List[Sym[Any]]): String = {
    kernelName+"_"+syms.map(quote(_)).mkString("")
  }

  def funcNameSuffix(sym: Sym[Any]): String = funcNameSuffixSyms(List(sym))

    // emit process functions
  def emitProcessMethods(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    emitHashReduceElemProcess(op, symList)
    (symList zip op.body) foreach {
      case (sym, elem:DeliteCollectElem[_,_,_]) =>
        val freeVars = (getFreeVarBlock(Block(Combine((List(elem.func,elem.buf.update,elem.buf.appendable)++elem.cond).map(getBlockResultFull))),List(elem.buf.eV,elem.buf.allocVal,op.v,sym))++List(sym)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        stream.println("__device__ void dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        //emitBlock(elem.func)
        //emitValDef(elem.eV, quote(getBlockResult(elem.func)))
        elem.par match {
          case ParSimpleBuffer =>
            //emitValDef(elem.allocVal, "act." + quote(sym) + "_buf")
            emitFatBlock(elem.cond)
            if (elem.cond.nonEmpty) stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
            emitBlock(elem.buf.appendable)
            stream.println("if (" + quote(getBlockResult(elem.buf.appendable)) + ") {")
            //emitValDef(elem.allocVal, "act." + quote(sym) + "_buf")
            //emitBlock(elem.update)
            stream.println("act." + quote(sym) + "_bitmap[" + quote(op.v) + "] = 1;")
            stream.println("}")
            if (elem.cond.nonEmpty) {
              // Need this for GPU?
              // stream.println(quote(sym) + "_conditionals[" + quote(op.v) + "] += 1;")
              stream.println("}")
            }
          case ParFlat =>
            emitBlock(elem.func)
            emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
            emitValDef(elem.buf.allocVal, quote(sym))
            emitBlock(elem.buf.update)
          case _ =>
        }
        stream.println("}")

      case (sym, elem:DeliteForeachElem[_]) =>
        val freeVars = getFreeVarBlock(elem.func,List(op.v)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        stream.println("__device__ void dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.func)
        stream.println("}")

      case (sym, elem: DeliteReduceElem[_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
        val freeVars = getFreeVarBlock(elem.func,List(op.v)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.func)
        stream.println("return " + quote(getBlockResult(elem.func)) + ";")
        stream.println("}")

      case (sym, elem:DeliteReduceElem[_]) =>
        val freeVars = getFreeVarBlock(Block(Combine((List(elem.func,elem.rFunc,elem.zero)++elem.cond).map(getBlockResultFull))),List(elem.rV._1,elem.rV._2,op.v)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(elem.rV._1))
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.func)
        emitValDef(elem.rV._2, quote(getBlockResult(elem.func)))
        if(elem.cond.nonEmpty) {
          emitFatBlock(elem.cond)
          stream.println("if(" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
          if (elem.stripFirst) {
            emitBlock(elem.zero)
            stream.println(remap(sym.tp) + " " + quote(sym) + "_zero = " + quote(getBlockResult(elem.zero)) + ";")
            stream.println("if(" + quote(elem.rV._1) + " == " + quote(sym) + "_zero) {")
            stream.println("return " + quote(elem.rV._2) + ";")
            stream.println("}")
            stream.println("else {")
            emitBlock(elem.rFunc)
            stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
            stream.println("}")
          }
          else {
            emitBlock(elem.rFunc)
            stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
          }
          stream.println("}")
          stream.println("else {")
          stream.println("return " + quote(elem.rV._1) + ";")
          stream.println("}")
        }
        else {
          emitBlock(elem.rFunc)
          stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
        }
        stream.println("}")

      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        val freeVars = getFreeVarBlock(Block(Combine((List(elem.func._1,elem.func._2,elem.rFuncSeq._1,elem.rFuncSeq._2)++elem.cond).map(getBlockResultFull))),List(elem.rVSeq._1._1,elem.rVSeq._1._2,elem.rVSeq._2._1,elem.rVSeq._2._2,op.v)).filter(_ != op.size).distinct
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        for(i <- 1 until 3) {
          val (rVSeq1, rVSeq2, rFuncSeq) = if(i == 1) (elem.rVSeq._1._1, elem.rVSeq._2._1, elem.rFuncSeq._1)
                                           else (elem.rVSeq._1._2, elem.rVSeq._2._2, elem.rFuncSeq._2)
          val inputs = remapInputs(freeVars ++ List(elem.rVSeq._1._1,elem.rVSeq._1._2))
          stream.println("__device__ " + remap(sym.tp) + " dev_process" + i + "_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          emitFatBlock(List(elem.func._1,elem.func._2))
          emitValDef(elem.rVSeq._2._1, quote(getBlockResult(elem.func._1)))
          emitValDef(elem.rVSeq._2._2, quote(getBlockResult(elem.func._2)))
          if(elem.cond.nonEmpty) {
            emitFatBlock(elem.cond)
            stream.println("if(" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
            assert(!elem.stripFirst)
            emitBlock(rFuncSeq)
            stream.println("return " + quote(getBlockResult(rFuncSeq)) + ";")
            stream.println("}")
            stream.println("else {")
            stream.println("return " + quote(rVSeq1) + ";")
            stream.println("}")
          }
          else {
            emitBlock(rFuncSeq)
            stream.println("return " + quote(getBlockResult(rFuncSeq)) + ";")
          }
          stream.println("}")
        }

      case _ => //
    }
  }

  def emitKernelAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    outerLoopSize = op.size
    outerLoopSym = op.v
    tabWidth += 1
    kernelName = symList.map(quote).mkString("")

    // register metadata for each elem and check GenerationFailedException conditions
    (symList zip op.body) foreach { s =>
      s match {
        case (sym, elem:DeliteCollectElem[_,_,_]) =>
          if (elem.par == ParBuffer) throw new GenerationFailedException("GPUGen DeliteOps: ParBuffer is not supported.")
          if (!isPrimitiveType(elem.mA)) throw new GenerationFailedException("GPUGen DeliteOps: output of collect elem is non-primitive type.")
          if (elem.iFunc.isDefined) throw new GenerationFailedException("GPUGen DeliteOps: Flatmap is now allowed for GPU")
          if(elem.par == ParFlat)
            metaData.outputs.put(sym, new LoopElem("COLLECT",Map("mA"->remap(elem.mA),"mI"->remap(elem.mI),"mCA"->remap(elem.mCA))))
          else
            metaData.outputs.put(sym, new LoopElem("COLLECT_BUF",Map("mA"->remap(elem.mA),"mI"->remap(elem.mI),"mCA"->remap(elem.mCA))))
        case (sym, elem:DeliteForeachElem[_]) =>
          metaData.outputs.put(sym, new LoopElem("FOREACH",Map("mA"->remap(elem.mA))))
          /*throw new GenerationFailedException("GPUGen DeliteOps: GPU ForEachElem is temporarily disabled..")
          metaData.outputs.put(sym,new TransferFunc)
          lf.tpe = "FOREACH"*/
        case (sym, elem: DeliteReduceElem[_]) =>
          if(!isPrimitiveType(sym.tp)) {
            if(encounteredZipWith contains getBlockResult(elem.rFunc)) {
              val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
              if(isPrimitiveType(z.dmR)) metaData.outputs.put(sym, new LoopElem("REDUCE_SPEC",Map("mA"->remap(elem.mA),"dmR"->remap(z.dmR))))
              else throw new GenerationFailedException("GPUGen DeliteOps: DeliteReduceElem with non-primitive types is not supported.")
            }
            else {
              throw new GenerationFailedException("GPUGen DeliteOps: DeliteReduceElem with non-primitive types is not supported.")
            }
          }
          else {
            metaData.outputs.put(sym, new LoopElem("REDUCE",Map("mA"->remap(elem.mA))))
          }
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          if(!isPrimitiveType(sym.tp)) throw new GenerationFailedException("GPUGen DeliteOps: DeliteReduceTupleElem with non-primitive types is not supported.")
          if(elem.cond.nonEmpty && elem.stripFirst) throw new GenerationFailedException("GPUGen DeliteOps: DeliteReduceTupleElem with condition + stripFirst is not supported.")
          metaData.outputs.put(sym, new LoopElem("REDUCE_TUPLE",Map("mA"->remap(elem.mA),"mB"->remap(elem.mB))))
        case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>

          // Currently only support limited types of hash-reduce on GPU
          // keys should be dense perfect hash (0 ~ N-1 for N keys)
          // reduction needs to be primitive type reduction
          //if(elem.cond.nonEmpty) throw new GenerationFailedException("GPUGen DeliteOps: DeliteHashReduceElem with condition is not supported.")
          //if(!isPrimitiveType(elem.mV)) throw new GenerationFailedException("GPUGen DeliteOPs: DeliteHashReduceElem only supports primitve type reduction.")
          if(remap(elem.mK) != "int") throw new GenerationFailedException("GPUGen DeliteOps: DeliteHashReduceElem only supports perfect hash.")

          if(!isPrimitiveType(elem.mV)) {
            if(encounteredZipWith contains getBlockResult(elem.rFunc)) {
              val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
              if(isPrimitiveType(z.dmR)) metaData.outputs.put(sym, new LoopElem("HASH_REDUCE_SPEC",Map("mK"->remap(elem.mK),"mV"->remap(elem.mV),"mCV"->remap(elem.mCV),"dmR"->remap(z.dmR))))
              else throw new GenerationFailedException("GPUGen DeliteOps: DeliteHashReduceElem with non-primitive types is not supported.")
            }
            else {
              throw new GenerationFailedException("GPUGen DeliteOps: DeliteHashReduceElem with non-primitive types is not supported.")
            }
          }
          else {
            metaData.outputs.put(sym, new LoopElem("HASH_REDUCE",Map("mK"->remap(elem.mK),"mV"->remap(elem.mV),"mCV"->remap(elem.mCV))))
          }
        case (sym, _) =>
          throw new GenerationFailedException("GPUGen DeliteOps: Unsupported Elem type for " + quote(sym))
      }
    }

    isNestedNode = true;

    // emit init functions
    (symList zip op.body) foreach {
      case (sym, elem:DeliteReduceElem[_]) =>
        val initFunc = if(elem.stripFirst || isPrimitiveType(sym.tp)) elem.zero else elem.accInit
        val freeVars = getFreeVarBlock(initFunc,Nil).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "init" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_init_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(initFunc)
        stream.println("return " + quote(getBlockResult(initFunc)) + ";")
        stream.println("}")
        if(e.elemType == "REDUCE_SPEC") {
          val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
          stream.println("__device__ " + remap(z.dmR) + " dev_spcinit_" + funcNameSuffix(sym) + "(void) { return 0; }")
        }
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        //TODO: would it affect the performance to have separate inputs for zero1 and zero2?
        val freeVars = getFreeVarBlock(Block(Combine(List(elem.zero._1,elem.zero._2).map(getBlockResultFull))),Nil).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "init" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_init1_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.zero._1)
        stream.println("return " + quote(getBlockResult(elem.zero._1)) + ";")
        stream.println("}")
        stream.println("__device__ " + remap(sym.tp) + " dev_init2_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.zero._2)
        stream.println("return " + quote(getBlockResult(elem.zero._2)) + ";")
        stream.println("}")
      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
        val freeVars = getFreeVarBlock(elem.zero,Nil).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "init" -> freeVars.map(quote)
        stream.println("__device__ " + remap(elem.mV) + " dev_init_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.zero)
        stream.println("return " + quote(getBlockResult(elem.zero)) + ";")
        stream.println("}")
        if(e.elemType == "HASH_REDUCE_SPEC") {
          val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
          stream.println("__device__ " + remap(z.dmR) + " dev_spcinit_" + funcNameSuffix(sym) + "(void) { return 0; }")
        }
      case _ => //
    }

    emitProcessMethods(op, symList)

    // emit post-process functions
    (symList zip op.body) foreach {
      case (sym, elem:DeliteCollectElem[_,_,_]) =>
        val freeVars = (getFreeVarBlock(Block(Combine((List(elem.func,elem.buf.update)++elem.cond).map(getBlockResultFull))),List(elem.buf.eV,elem.buf.allocVal,op.v,sym))++List(sym)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "postprocess" -> freeVars.map(quote)
        stream.println("__device__ void dev_postprocess_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        elem.par match {
          case ParSimpleBuffer =>
            emitValDef(elem.buf.allocVal, quote(sym))
            if (elem.cond.nonEmpty) stream.println("if (act."+quote(sym)+"_bitmap[" + quote(op.v) + "] == 1) {")
            emitBlock(elem.func)
            emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
            stream.println(quote(op.v) + " = act." + quote(sym) + "_scanmap[" + quote(op.v) + "];")
            emitBlock(elem.buf.update)
            if (elem.cond.nonEmpty) stream.println("}")
          case _ =>
        }
        stream.println("}")

      case _ =>
    }

    // emit combine functions
    (symList zip op.body) foreach {
      case (sym, elem: DeliteReduceElem[_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
        /*
        val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
        val zbody = z.body.asInstanceOf[DeliteCollectElem[_,_,_]]
        val freeVars = getFreeVarBlock(Block(Combine(List(zbody.func).map(getBlockResultFull))),List(z.inA.asInstanceOf[Sym[_]],z.inB.asInstanceOf[Sym[_]],z.v)).distinct
        val inputs = remapInputs(freeVars ++ List(z.inA.asInstanceOf[Sym[_]],z.inB.asInstanceOf[Sym[_]]),z.size,z.v) //Hack : Set size to be a const!
        val e = metaData.outputs.getOrElse(sym,new LoopElem)
        e.loopReduceInputs = freeVars.map(quote)
        stream.println("__device__ " + remap(z.dmR) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(zbody.func)
        stream.println("return " + quote(getBlockResult(zbody.func)) + ";")
        stream.println("}")
        */
        // FIXIT: Hacky way of generating zip function
        val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
        val zbody = z.body.asInstanceOf[DeliteCollectElem[_,_,_]]
        val prevInnerScope = innerScope
        val result = zbody.func
        result.res match {
          case r:Sym[_] if(innerScope==null) => innerScope = List(findDefinition(r).get)
          case r:Sym[_] => innerScope = findDefinition(r).get :: innerScope
          case _ => //
        }
        val freeVars = getFreeVarBlock(Block(Combine(List(zbody.func).map(getBlockResultFull))),List(z.fin._1.asInstanceOf[Sym[_]],z.fin._2.asInstanceOf[Sym[_]])).filter(_ != op.size).distinct
        //val inputs = (freeVars ++ List(z.fin._1,z.fin._2)).map(i => remap(i.tp) + " " + quote(i))
        val inputs = remapInputs(freeVars ++ List(z.fin._1.asInstanceOf[Sym[_]],z.fin._2.asInstanceOf[Sym[_]]))
        stream.println("__device__ " + remap(z.dmR) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(result)
        stream.println("return " + quote(getBlockResult(result)) + ";")
        stream.println("}")
        innerScope = prevInnerScope
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)

      case (sym, elem:DeliteReduceElem[_]) =>
        val freeVars = getFreeVarBlock(Block(Combine(List(elem.rFunc,elem.zero).map(getBlockResultFull))),List(elem.rV._1,elem.rV._2,op.v)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(elem.rV._1,elem.rV._2))
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.zero)
        stream.println(remap(sym.tp) + " " + quote(sym) + "_zero = " + quote(getBlockResult(elem.zero)) + ";")
        if(elem.cond.nonEmpty) {
          stream.println("if (" + quote(elem.rV._1) + " == " + quote(sym) + "_zero) {")
          stream.println("return " + quote(elem.rV._2) + ";")
          stream.println("}")
          stream.println("else if(" + quote(elem.rV._2) + " != " + quote(sym) + "_zero) {")
          emitBlock(elem.rFunc)
          stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
          stream.println("}")
          stream.println("else {")
          stream.println("return " + quote(elem.rV._1) + ";")
          stream.println("}")
        }
        else {
          emitBlock(elem.rFunc)
          stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
        }
        stream.println("}")

      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        val freeVars = getFreeVarBlock(Block(Combine(List(elem.rFuncPar._1,elem.rFuncPar._2).map(getBlockResultFull))),List(elem.rVPar._1._1,elem.rVPar._1._2,elem.rVPar._2._1,elem.rVPar._2._2,op.v)).filter(_ != op.size).distinct
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)
        for(i <- 1 until 3) {
          val (func, rFuncPar) = if(i == 1) (elem.func._1, elem.rFuncPar._1)
                                 else (elem.func._2, elem.rFuncPar._2)
          val inputs = remapInputs(freeVars ++ List(elem.rVPar._1._1,elem.rVPar._1._2,elem.rVPar._2._1,elem.rVPar._2._2))
          stream.println("__device__ " + remap(sym.tp) + " dev_combine" + i + "_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          emitBlock(rFuncPar)
          stream.println("return " + quote(getBlockResult(rFuncPar)) + ";")
          stream.println("}")
        }

      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
        // FIXIT: Hacky way of generating zip function
        val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
        val zbody = z.body.asInstanceOf[DeliteCollectElem[_,_,_]]
        val prevInnerScope = innerScope
        val result = zbody.func
        result.res match {
          case r:Sym[_] if(innerScope==null) => innerScope = List(findDefinition(r).get)
          case r:Sym[_] => innerScope = findDefinition(r).get :: innerScope
          case _ => //
        }
        val freeVars = getFreeVarBlock(Block(Combine(List(zbody.func).map(getBlockResultFull))),List(z.fin._1.asInstanceOf[Sym[_]],z.fin._2.asInstanceOf[Sym[_]])).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(z.fin._1.asInstanceOf[Sym[_]],z.fin._2.asInstanceOf[Sym[_]]))
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)
        stream.println("__device__ " + remap(z.dmR) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(result)
        stream.println("return " + quote(getBlockResult(result)) + ";")
        stream.println("}")
        innerScope = prevInnerScope

      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
        val freeVars = getFreeVarBlock(elem.rFunc,List(elem.rV._1,elem.rV._2)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(elem.rV._1,elem.rV._2))
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)
        stream.println("__device__ " + remap(elem.mV) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.rFunc)
        stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
        stream.println("}")


      case _ => //
    }

    isNestedNode = false;

    // emit output allocations
    val actName = symList.map(quote).mkString("")
    (symList zip op.body) foreach {
      case (sym, elem:DeliteCollectElem[_,_,_]) =>
        val e = metaData.outputs.get(sym).get
        val allocInputs = elem.par match {
          case ParSimpleBuffer =>
            //TODO: enable below by generating both alloc and allocRaw
            if (getFreeVarBlock(elem.buf.allocRaw,List(elem.buf.sV)).isEmpty)
              emitMultiLoopAllocFunc(elem.buf.allocRaw,"alloc_"+quote(sym),actName,quote(sym)+"_data",Map(elem.buf.sV->("act."+quote(sym)+"_conditionals")))
            else
              throw new GenerationFailedException("allocRaw block refers to alloc block")
          case _ => emitMultiLoopAllocFunc(elem.buf.alloc,"alloc_"+quote(sym),actName,quote(sym)+"_data",Map(elem.buf.sV->("act.size")))
        }
        e.funcs += "alloc" -> allocInputs.map(quote)
        val finalizerInputs = emitMultiLoopAllocFunc(elem.buf.finalizer,"finalizer_"+quote(sym),actName,quote(sym),Map(elem.buf.allocVal->("act."+quote(sym)+"_data")))
        e.funcs += "finalizer" -> finalizerInputs.map(quote)

      case (sym, elem:DeliteForeachElem[_]) =>
        val e = metaData.outputs.get(sym).get

      //TODO: Fix below alloc func to use a correct one.
      case (sym, elem: DeliteReduceElem[_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
        val e = metaData.outputs.get(sym).get
        val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
        val zbody = z.body.asInstanceOf[DeliteCollectElem[_,_,_]]
        val allocInputs = emitMultiLoopAllocFunc(elem.zero, "alloc_"+quote(sym), actName, quote(sym), Map())
        e.funcs += "alloc" -> allocInputs.map(quote)

      case (sym, elem: DeliteReduceElem[_]) =>
        val e = metaData.outputs.get(sym).get
        val allocInputs = emitMultiLoopAllocFunc(elem.zero, "alloc_"+quote(sym), actName, quote(sym), Map())
        e.funcs += "alloc" -> allocInputs.map(quote)

      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        val e = metaData.outputs.get(sym).get
        assert(isPrimitiveType(sym.tp))
        val allocInputs = emitMultiLoopAllocFunc(elem.zero._1, "alloc_"+quote(sym), actName, quote(sym), Map())
        e.funcs += "alloc" -> allocInputs.map(quote)

      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
        val e = metaData.outputs.get(sym).get
        val allocInputs = emitMultiLoopAllocFunc(elem.buf.alloc, "alloc_"+quote(sym), actName, quote(sym), Map(elem.buf.sV->("act."+quote(sym)+"_numKeys")))
        e.funcs += "alloc" -> allocInputs.map(quote)

        // Generate update function
        val freeVars = getFreeVarBlock(elem.buf.update,List(elem.buf.allocVal,elem.buf.iV,elem.buf.eV)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(elem.buf.allocVal,elem.buf.iV,elem.buf.eV))
        e.funcs += "update" -> freeVars.map(quote)
        stream.println("__device__ void " + quote(sym) + "_update(" + inputs.mkString(",") + ") {")
        emitBlock(elem.buf.update)
        stream.println("}")

      case _ =>
    }

    tabWidth -= 1
    isGPUable = true
  }

  // Emit Activation Record
  def emitAbstractFatLoopKernelExtra(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    val stream = actRecordStream
    val kernelName = symList.map(quote).mkString("")
    stream.println("#ifndef __ACT_" + kernelName + "__")
    stream.println("#define __ACT_" + kernelName + "__")
    stream.println("typedef struct {")
    stream.println("unsigned int size;")
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        stream.println(remap(sym.tp) + " *" + quote(sym) + ";")
        stream.println(remap(elem.buf.allocVal.tp) + " *" + quote(sym) + "_data;")
        if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
          //stream.println(remap(elem.eV) + " *" + quote(sym) + "_buf;")
          stream.println("unsigned int *" + quote(sym) + "_bitmap;")
          stream.println("unsigned int *" + quote(sym) + "_scanmap;")
          stream.println("unsigned int " + quote(sym) + "_conditionals;")
        }
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println(remap(sym.tp) + " *" + quote(sym) + ";")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        stream.println(remap(sym.tp) + " *" + quote(sym) + ";")
      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
        stream.println(remap(sym.tp) + " *" + quote(sym) + ";")
        stream.println("unsigned int " + quote(sym) + "_numKeys;")
      case _ => //
    }
    stream.println("} activation_" + kernelName + ";")
    stream.println("#endif")
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      val b = s.block
      if (!deliteKernel) {  //In the process of generating operations for deliteKernel type kernels (allow SingleTask to be inlined)
        emitBlock(b)
        if(!isVoidType(sym.tp)) {
          emitValDef(sym, quote(getBlockResult(b)))
          emitPtrDef(sym, getBlockResult(b))
        }
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

  def emitMultiLoopAllocFunc(block: Block[Any], funcName: String, actType: String, resultActField: String, boundVals:Map[Sym[Any],String]): List[Sym[Any]] = {
    processingHelperFunc = true

    val out = new StringBuilder
    val blockString = new StringWriter
    val blockStream = new PrintWriter(blockString,true)

    val inputs = getFreeVarBlock(block,boundVals.keySet.toList)

    if(isPrimitiveType(block.tp)) { // primitive type allocation
      if(actType == "") throw new GenerationFailedException("Non-DeliteOp with primitive type output.")
      blockString.append("\tDeliteCudaMalloc((void**)&(act." + resultActField + "), sizeof(%s));\n".format(remap(block.tp)))
    }
    else {
      // emit bouding symbols
      for (b <- boundVals) {
        withStream(blockStream) {
          if(isPrimitiveType(b._1.tp)) emitValDef(b._1,b._2)
          else {
            stream.println("\t%s *%s_ptr = %s;".format(remap(b._1.tp),quote(b._1),b._2))
            stream.println("\t%s %s = *(%s_ptr);".format(remap(b._1.tp),quote(b._1),quote(b._1)))
          }
        }
      }

      // emit block body
      withStream(blockStream) {
        emitBlock(block)
        if(actType!="") stream.println("\tact." + resultActField + " = " + quote(getBlockResult(block)) + "_ptr;")
        else stream.println("\t*" + resultActField + " = " + quote(getBlockResult(block)) + "_ptr;")
      }
    }

    val act = if(actType=="") remap(block.tp) + " **" + resultActField else "activation_" + actType + " &act"
    val paramStr = (inputs.map(s =>
      if(isPrimitiveType(s.tp)) remap(s.tp) + " " + quote(s)
      else remap(s.tp) + " *" + quote(s) + "_ptr"
    ) :+ act).mkString(",")
    val derefParams = inputs.map(s =>
      if(isPrimitiveType(s.tp)) ""
      else "\t%s %s = *(%s_ptr);\n".format(remap(s.tp),quote(s),quote(s))
    ).mkString("")

    // emit header and complete host function
    out.append("void %s(%s)".format(funcName, paramStr))
    headerStream.append(out.toString + ";\n")
    out.append("{\n")
    out.append(derefParams)
    out.append(blockString)
    out.append("}\n")
    if(!helperFuncList.contains(funcName+actType)) helperFuncStream.println(out.toString)
    helperFuncList += funcName+actType

    processingHelperFunc = false
    inputs
  }

  def emitMultiLoopFunc(func:Block[Any], postfix: String, lastInputs: List[Sym[Any]], stream:PrintWriter): List[String] = {
    isNestedNode = true
    val tempString = new StringWriter
    val tempStream = new PrintWriter(tempString, true)
    val header = new StringWriter
    val footer = new StringWriter

    val currentTab = tabWidth
    tabWidth = 1
    withStream(tempStream) {
      emitBlock(func)
    }
    tabWidth = currentTab

    def wrapRef(sym: Exp[Any]):String = {
      if(inVars contains sym)
        deviceTarget + "Ref" + remap(sym.tp) + addRef(sym.tp)
      else
        remap(sym.tp)
    }

    val inputs = getFreeVarBlock(func,lastInputs).distinct
    val paramStr = ((inputs++lastInputs).map(ele => wrapRef(ele) + " " + quote(ele)) ++ metaData.temps.map(t=>t.tp + " *" + t.sym) ++ List("size_t tempMemSize","char *tempMemPtr","int *tempMemUsage")).mkString(",")

    header.append(devFuncPrefix + " %s dev_%s(%s) {\n".format(remap(getBlockResult(func).tp),postfix,paramStr))
    if(remap(getBlockResult(func).tp) != "void")
      footer.append("\treturn %s;\n".format(quote(getBlockResult(func))))
    footer.append("}\n")
    stream.print(header)
    stream.print(tempString)
    stream.print(footer)

    isNestedNode = false
    inputs.map(quote(_))
  }

}

trait CudaGenDeliteOps extends CudaGenLoopsFat with GPUGenDeliteOpsOpt

trait OpenCLGenDeliteOps extends OpenCLGenLoopsFat with GPUGenDeliteOpsOpt

trait CGenDeliteOps extends CGenLoopsFat with GenericGenDeliteOps {

  import IR._

  override def addRef(tpe: String): String = {
    if (tpe startsWith "activation") " *"
    else super.addRef(tpe)
  }
  
  def quotearg(x: Sym[Any]) = quotetp(x) + " " + quote(x)
  def quotetp(x: Sym[Any]) = remap(x.tp)

  def methodCall(name: String, inputs: List[String] = Nil): String = {
    name + "(" + inputs.mkString(",") + ")"
  }

  def emitMethodCall(name:String, inputs: List[String]) {
    stream.println(methodCall(name, inputs) + ";")
  }

  def emitMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit) {
    stream.println(remapWithRef(outputType) + name + "(" + inputs.map(i => remapWithRef(i._2) + i._1).mkString(",") + ") {")
    body
    stream.println("}\n")
  }

  def createInstance(typeName:String, args: List[String] = Nil): String = {
    "new " + typeName + args.mkString("(",",",");")
  }

  def fieldAccess(className: String, varName: String): String = {
    if (className == "") varName
    else className + "->" + varName
  }

  def releaseRef(varName: String) {
    //TODO: Change this to decrement the reference count?
    //stream.println("free(" + varName + ");")
    //stream.println(varName + " = NULL;")
    //stream.println(varName + ".reset();")
  }

  def emitReturn(rhs: String) = {
    stream.print("return " + rhs + ";")
  }

  def emitFieldDecl(name: String, tpe: String) {
    tpe match {
      case "void" => //
      case _ =>
        stream.println(remapWithRef(tpe) + name + ";")
    }
  }

  def emitClass(name: String)(body: => Unit) {
    stream.println("#ifndef __" + name + "__")
    stream.println("#define __" + name + "__")
    stream.println("#include \"" + deviceTarget + "helperFuncs.h\"")
    stream.println("class " + name + " {")
    stream.println("public:")
    body
    stream.println("};")
    stream.println("#endif")
  }

  def emitObject(name: String)(body: => Unit) {

  }

  def emitValDef(name: String, tpe: String, init: String) {
    emitVarDef(name, tpe, init)
  }

  def emitVarDef(name: String, tpe: String, init: String) {
    tpe match {
      case "void" => //
      case _ if tpe.contains("*") && isPrimitiveType(tpe.replaceAll("\\*","").trim) => // primitive type pointer (not cppDeliteArray)
        stream.println(tpe + " " + name + " = " + init + ";")
      case _ =>
        stream.println(remapWithRef(tpe) + " " + name + " = " + init + ";")
    }
  }

  def emitAssignment(name: String, tpe: String, rhs: String) {
    tpe match {
      case "void" => //
      case _ => emitAssignment(name, rhs)
    }
  }

  def emitAssignment(lhs: String, rhs: String) {
    stream.println(lhs + " = " + rhs + ";")
  }

  def emitAbstractFatLoopHeader(className: String, actType: String) {
    stream.println("#ifndef __" + kernelName + "__")
    stream.println("#define __" + kernelName + "__")
    stream.println("class " + kernelName + "{")
    stream.println("public:")
    emitFieldsAndConstructor()
  }

  def emitAbstractFatLoopFooter() {
    stream.println("};")
    stream.println("#endif")
  }

  def refNotEq: String = "!="
  def nullRef: String = "NULL"

  def arrayType(argType: String): String = argType + "*"
  def arrayApply(arr: String, idx: String): String = arr + "[" + idx + "]"
  def newArray(argType: String, size: String): String = "new " + remapWithRef(argType) + "[" + size + "]"
  def hashmapType(argType: String): String = "cppHashMap<" + remapWithRef(argType) + ">"
  def typeCast(sym: String, to: String): String = "(" + to + ")(" + sym + ")"
  def withBlock(name: String)(block: => Unit): Unit = {
    stream.println("//block:" + name)
    stream.println("{")
    block
    stream.println("}")
  }

  private def emitFieldsAndConstructor() {
    val fields = if (cppMemMgr == "refcnt") kernelInputVals.map(i => remapWithRef(i.tp) + quote(i)) ++ kernelInputVars.map(i => wrapSharedPtr(deviceTarget.toString + "Ref" + unwrapSharedPtr(remap(i.tp))) + " " + quote(i))
                 else kernelInputVals.map(i => remapWithRef(i.tp) + quote(i)) ++ kernelInputVars.map(i => remapWithRef(deviceTarget.toString + "Ref" + remap(i.tp)) + quote(i))
    val constructorInputs = if (cppMemMgr == "refcnt") kernelInputVals.map(i => remapWithRef(i.tp) + " _" + quote(i)) ++ kernelInputVars.map(i => wrapSharedPtr(deviceTarget.toString + "Ref" + unwrapSharedPtr(remap(i.tp))) + " _" + quote(i))
                            else kernelInputVals.map(i => remapWithRef(i.tp) + " _" + quote(i)) ++ kernelInputVars.map(i => remapWithRef(deviceTarget.toString + "Ref" + remap(i.tp)) + " _" + quote(i))

    //print fields
    stream.println(fields.map(_ + ";\n").mkString(""))

    //print constructor
    stream.println(kernelName + "(" + constructorInputs.mkString(",") + ") {")
    stream.print((kernelInputVals++kernelInputVars).map(i => quote(i) + " = _" + quote(i) + ";\n").mkString(""))
    stream.println("}")
    stream.println
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case s:DeliteOpSingleTask[_] =>
      //printlog("EMIT single "+s)
      // always wrap single tasks in methods to reduce JIT compilation unit size
      val b = s.block
      emitBlock(b)
      if (!isVoidType(sym.tp)) 
        stream.println(remap(sym.tp) + addRef(sym.tp) + quote(sym) + " = " + quote(getBlockResult(b)) + ";")

    case op: AbstractLoop[_] =>
      // TODO: we'd like to always have fat loops but currently they are not allowed to have effects
      stream.println("// a *thin* loop follows: " + quote(sym))
      emitFatNode(List(sym), SimpleFatLoop(op.size, op.v, List(op.body)))

    case _ => super.emitNode(sym,rhs)
  }

  /*
  // Prevent C++ kernel generation for HashElems. Not supported yet.
  override def emitKernelMultiHashInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = ""){
    if (ps.length > 0)
      throw new GenerationFailedException("CGen: DeliteHashElems are not yet supported for C++ target.")
  }
  override def emitInlineMultiHashInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = "") {
    if (ps.length > 0)
      throw new GenerationFailedException("CGen: DeliteHashElems are not yet supported for C++ target.")
  }
  */

}
