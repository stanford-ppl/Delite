package ppl.delite.framework.ops

import ppl.delite.framework.analysis.{StencilExp,NestedLoopMappingExp}
import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._
import scala.collection.mutable.HashMap
import scala.reflect.SourceContext
import scala.virtualization.lms.common._


// This file contains the low-level Delite constructs, for the user-facing
// operations check DeliteOps.scala.

trait DeliteOpsExpIR extends DeliteReductionOpsExp with StencilExp with NestedLoopMappingExp {

  // hack: need to pass explicit type class parameters during mirroring, similar to mtype
  def ntype[A,B](n:Numeric[A]): Numeric[B] = n.asInstanceOf[Numeric[B]]
  def otype[A,B](o:Ordering[A]): Ordering[B] = o.asInstanceOf[Ordering[B]]
  def frtype[A,B](o:Fractional[A]): Fractional[B] = o.asInstanceOf[Fractional[B]]


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

  /**
   * The base class for most data parallel Delite ops.
   */
  abstract class DeliteOpLoop[A:Manifest](implicit ctx: SourceContext) extends AbstractLoop[A] with DeliteOp[A] {
    type OpType <: DeliteOpLoop[A]
    def copyBodyOrElse(e: => Def[A]): Def[A] = original.map(p=>mirrorLoopBody(p._2.asInstanceOf[OpType].body,p._1)).getOrElse(e)
    final lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(fresh[Int]).asInstanceOf[Sym[Int]]
    val numDynamicChunks:Int = 0
  }

  abstract class DeliteOpAbstractSingleTask[R:Manifest](block0: => Block[R], val requireInputs: Boolean) extends DeliteOp[R] {
    type OpType <: DeliteOpAbstractSingleTask[R]
    final lazy val block: Block[R] = copyTransformedBlockOrElse(_.block)(block0)
  }

  abstract class DeliteOpAbstractExternal[A:Manifest] extends DeliteOp[A] {
    type OpType <: DeliteOpAbstractExternal[A]
    def alloc: Exp[A]
    final lazy val allocVal: Block[A] = copyTransformedBlockOrElse(_.allocVal)(reifyEffects(alloc))
  }

  abstract class DeliteOpAbstractForeachReduce[A:Manifest](implicit ctx: SourceContext) extends DeliteOp[Unit]  { //DeliteOpLoop[Unit] {
    type OpType <: DeliteOpAbstractForeachReduce[A]
    def funcBody: Block[Unit]
    def func: Exp[A] => Exp[Unit]
    final lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(fresh[Int]).asInstanceOf[Sym[Int]]
  }

  trait DeliteLoopElem {
    val numDynamicChunks:Int
  }

  case class DeliteForeachElem[A:Manifest] (
    func: Block[A],
    numDynamicChunks: Int
    //sync: Block[List[Any]] // FIXME: don't want to create lists at runtime...
    // TODO: this is sort of broken right now, re-enable when we figure out how to make this work without emitting dependencies twice
    //cond: List[Exp[Boolean]] = Nil
  ) extends Def[Unit] with DeliteLoopElem {
    val mA = manifest[A]
  }

  /** This LoopElem is used for all flatMap-type operations (loops that produce
    * an output collection of type CA). The special cases of map and filter
    * operations are treated at the codegen level, see DeliteCollectElemType.
    * There are also two different output strategies, see
    * DeliteCollectOutputStrategy. */
  case class DeliteCollectElem[A:Manifest, I <: DeliteCollection[A]:Manifest, CA <: DeliteCollection[A]:Manifest](
    // flatmap function, produces an intermediate collection for each
    // iteration,  which is appended one by one to the output collection
    // through an inner loop
    iFunc: Block[DeliteCollection[A]],
    // true in the general flatMap case, false for a fixed-size map
    unknownOutputSize: Boolean,
    // The output collection/buffer to be used
    buf: DeliteCollectOutput[A,I,CA],
    // The number of dynamic chunks
    numDynamicChunks: Int,
    // bound symbol to hold the intermediate collection computed by iFunc
    eF: Sym[DeliteCollection[A]],
    // inner loop index symbol to iterate over the intermediate collection
    iF: Sym[Int],
    // size of the intermediate collection (range of inner loop)
    sF: Block[Int],
    // element of the intermediate collection at the current inner loop index
    aF: Block[A]
  ) extends Def[CA] with DeliteLoopElem {
    val mA = manifest[A]
    val mI = manifest[I]
    val mCA = manifest[CA]
    val mDCA = manifest[DeliteCollection[A]]
  }

  /** Depending on the function, a DeliteCollectElem can represent a map,
   * filter or a general flatMap. Code generation can be specialized to avoid
   * the unnecessary allocation of an inner collection in the map and filter
   * cases. */
  abstract class DeliteCollectElemType
  /** Extracts the inner map elem, always with OutputFlat strategy. */
  case class CollectMap(elem: Block[Any]) extends DeliteCollectElemType
  /** Extracts the inner map elem. This is the special case of a map where the
   * size will only be known at runtime, can also use OutputBuffer strategy. */
  case class CollectDynamicMap(elem: Block[Any]) extends DeliteCollectElemType
  /** Matches CollectMap as well as CollectDynamicMap. */
  object CollectAnyMap {
    def unapply(collect: DeliteCollectElemType): Option[Block[Any]] = collect match {
      case CollectMap(elem) => Some(elem)
      case CollectDynamicMap(elem) => Some(elem)
      case _ => None
    } 
  }
  /** Extracts the condition and the elem for when the condition is true. */
  case class CollectFilter(cond: Exp[Boolean], elem: Block[Any]) extends DeliteCollectElemType
  /** The general flatMap case. */
  case class CollectFlatMap extends DeliteCollectElemType
  /** Determines whether the function represents a special case. */
  def getCollectElemType(collect: DeliteCollectElem[_,_,_]): DeliteCollectElemType

  /** CollectElem nodes can have two different types of output collections,
    * flat or buffer, which defines what class of DeliteCollectOutput is used
    * in the CollectElem. */
  trait DeliteCollectOutputStrategy
  /** Used for collect operations when the result DeliteCollection is a linear
    * buffer to which elements are appended (implements the buffer methods from
    * DeliteCollectionOpsExp). */
  object OutputBuffer extends DeliteCollectOutputStrategy
  /** Used for collect operations when the result DeliteCollection can either
    * be allocated to the correct size from the start (e.g. map with known
    * output size) or does not conform to a linear append model (eg. matrices).
    * Need to implement the flat methods from DeliteCollectionOpsExp. */
  object OutputFlat extends DeliteCollectOutputStrategy
  /** Computes the strategy for a new DeliteCollectElem. */
  def getOutputStrategy(unknownOutputSize: Boolean, linearOutputCollection: Boolean) = 
    if (unknownOutputSize && linearOutputCollection) OutputBuffer else OutputFlat
  /** Convenience function to get the type of output strategy used. */
  def getOutputStrategy(elem: DeliteCollectElem[_,_,_]) = elem.buf match {
    case _: DeliteCollectBufferOutput[_,_,_] => OutputBuffer
    case _: DeliteCollectFlatOutput[_,_,_] => OutputFlat
  }

  /** Represents the output collection of a CollectElem loop body. */
  abstract class DeliteCollectOutput[A:Manifest, I:Manifest, CA:Manifest](
    // bound vars
    /** Element to be added to the output. Element computation finishes with
      * eV = result. */
    val eV: Sym[A],  
    /** Size of the output collection. */
    val sV: Sym[Int],
    /** Allocated output collection. */
    val allocVal: Sym[I],

    // collection functions
    /** Allocates the output collection, usually using sV as argument. */
    val alloc: Block[I],
    /** Updates allocVal with the newest output element eV, possibly using the
      * loop index v (e.g. update at index of linear output collection). */
    val update: Block[Unit],
    /** Transforms from DC[I] to DC[CA], identity if no intermediate collection
      * type is used, or e.g. creating the result collection from a builder. */
    val finalizer: Block[CA]
  ) {
    val mA = manifest[A]
    val mI = manifest[I]
    val mCA = manifest[CA]
  }

  /** Used in DeliteCollectElems when the output strategy is OutputFlat. */
  case class DeliteCollectFlatOutput[A:Manifest, I:Manifest, CA:Manifest](
    override val eV: Sym[A], override val sV: Sym[Int], override val allocVal: Sym[I],
    override val alloc: Block[I], override val update: Block[Unit],
    override val finalizer: Block[CA]
  ) extends DeliteCollectOutput[A,I,CA](eV, sV, allocVal, alloc, update, finalizer)

  /** Used in DeliteCollectElems when the output strategy is OutputBuffer. */
  case class DeliteCollectBufferOutput[A:Manifest, I:Manifest, CA:Manifest](
    override val eV: Sym[A], override val sV: Sym[Int], override val allocVal: Sym[I],
    // additional bound vars
    aV2: Sym[I],      // secondary allocated collection
    iV: Sym[Int],     // start index
    iV2: Sym[Int],    // end index

    // additional collection functions, typically use any of allocVal (output),
    // the loop index v, the output element to be added eV, the size sV.
    override val alloc: Block[I],
    override val update: Block[Unit],
    appendable: Block[Boolean],
    append: Block[Unit],
    setSize: Block[Unit],
    allocRaw: Block[I],
    copyRaw: Block[Unit],
    override val finalizer: Block[CA]
  ) extends DeliteCollectOutput[A,I,CA](eV, sV, allocVal, alloc, update, finalizer)


  // -- end of DeliteCollectElem-related classes

  // Represents a general output collection of a DeliteOp
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

  @deprecated("DeliteReduceTupleElem will be removed as soon as there's a foldElem with rFuncPar and rFuncSeq. Tuples don't need special support anymore.", "")
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


  ///////////////////////////
  // effects + other helpers

  // used by delite code generators to handle nested delite ops TR: shouldn't this be part of the codegen hierarchy?
  var deliteKernel: Boolean = false
  var deliteResult: Option[List[Exp[Any]]] = None
  var deliteInputs: List[Sym[Any]] = Nil

  var simpleCodegen: Boolean = false // try to generate more readable code

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
    case e:DeliteCollectElem[_,_,_] => false //e.strategy == OutputBuffer //e.cond.nonEmpty
    case _ => false
  }
  def loopBodyNumDynamicChunks[A](e: Def[A]) = e match {
    case e:DeliteLoopElem => e.numDynamicChunks
    case _ => 0
  }

  def loopBodyNeedsPostProcess[A](e: Def[A]) = e match {
    case e:DeliteCollectElem[_,_,_] => getOutputStrategy(e) == OutputBuffer
    case e:DeliteHashCollectElem[_,_,_,_,_,_] => true
    case _ => false
  }

  def summarizeBody[A](d: Def[A]) = d match {
    case e: DeliteForeachElem[_] => summarizeEffects(e.func).star
    case e: DeliteHashCollectElem[_,_,_,_,_,_] => (summarizeEffects(e.keyFunc) andAlso summarizeEffects(e.valFunc)).star
    case e: DeliteHashReduceElem[_,_,_,_] => (summarizeEffects(e.keyFunc) andAlso summarizeEffects(e.valFunc) andAlso summarizeEffects(e.rFunc)).star // TODO writable reduce
    case e: DeliteHashIndexElem[_,_] => summarizeEffects(e.keyFunc).star
    case e: DeliteCollectElem[_,_,_] => summarizeEffects(e.iFunc).star
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
    case x: DeliteOpAbstractSingleTask[_] =>
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
    case x: DeliteOpAbstractSingleTask[_] =>
      x.block
      super.reflectEffect(d,u)
    case x: DeliteOpLoop[_] =>
      val z = x.body  //  <-- not lazy
      super.reflectEffect(d,u)
    case x: DeliteOpAbstractExternal[_] =>
      x.allocVal
      super.reflectEffect(d,u)
    case _ =>
      super.reflectEffect(d,u)
  }

  // HACK lazy val bites again: must make sure that block is evaluated!
  override def reflectMirrored[A:Manifest](zd: Reflect[A])(implicit pos: SourceContext): Exp[A] = zd match {
    case Reflect(x:DeliteOpAbstractSingleTask[_], u, es) =>
      x.block
      super.reflectMirrored(zd)
    case Reflect(x: DeliteOpLoop[_], u, es) =>
      val z = x.body  //  <-- somehow not always evaluated? lazy val extends a strict val, what are the semantics?
      super.reflectMirrored(zd)
    case Reflect(x: DeliteOpAbstractExternal[_], u, es) =>
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

    def mirrorCollectOutput[A,I,CA](e: DeliteCollectOutput[A,I,CA]) = e match {
      case e: DeliteCollectFlatOutput[_,_,_] =>
        DeliteCollectFlatOutput[A,I,CA](
          eV = f(e.eV).asInstanceOf[Sym[A]],
          sV = f(e.sV).asInstanceOf[Sym[Int]],
          allocVal = f(e.allocVal).asInstanceOf[Sym[I]],
          alloc = fb(e.alloc)(e.mI),
          update = fb(e.update)(manifest[Unit]),
          finalizer = fb(e.finalizer)(e.mCA)
        )(e.mA,e.mI,e.mCA)
      case e: DeliteCollectBufferOutput[_,_,_] => 
        DeliteCollectBufferOutput[A,I,CA](
          eV = f(e.eV).asInstanceOf[Sym[A]],
          sV = f(e.sV).asInstanceOf[Sym[Int]],
          allocVal = f(e.allocVal).asInstanceOf[Sym[I]],
          aV2 = f(e.aV2).asInstanceOf[Sym[I]],
          iV = f(e.iV).asInstanceOf[Sym[Int]],
          iV2 = f(e.iV2).asInstanceOf[Sym[Int]],
          alloc = fb(e.alloc)(e.mI),
          update = fb(e.update)(manifest[Unit]),
          appendable = fb(e.appendable)(manifest[Boolean]),
          append = fb(e.append)(manifest[Unit]),
          setSize = fb(e.setSize)(manifest[Unit]),
          allocRaw = fb(e.allocRaw)(e.mI),
          copyRaw = fb(e.copyRaw)(manifest[Unit]),
          finalizer = fb(e.finalizer)(e.mCA)
        )(e.mA,e.mI,e.mCA)
    }

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
          iFunc = fb(e.iFunc)(e.mDCA),
          unknownOutputSize = e.unknownOutputSize,
          buf = mirrorCollectOutput(e.buf),
          numDynamicChunks = e.numDynamicChunks,
          eF = f(e.eF).asInstanceOf[Sym[DeliteCollection[a]]],
          iF = f(e.iF).asInstanceOf[Sym[Int]],
          sF = fb(e.sF)(manifest[Int]),
          aF = fb(e.aF)(e.mA)
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

  override def blocks(e: Any): List[Block[Any]] = e match {
    case s: DeliteOpAbstractSingleTask[_] => blocks(s.block)
    case e: DeliteOpAbstractExternal[_] =>  super.blocks(e) ::: blocks(e.allocVal)
    case fr: DeliteOpAbstractForeachReduce[_] => blocks(fr.funcBody)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc) ::: blocks(op.cond) ::: blocks(op.buf) ::: blocks(op.iBuf) ::: blocks(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFunc) ::: blocks(op.buf)
    case op: DeliteHashIndexElem[_,_] => blocks(op.keyFunc) ::: blocks(op.cond)
    case op: DeliteCollectElem[_,_,_] => blocks(op.aF) ::: blocks(op.buf) ::: blocks(op.iFunc) ::: blocks(op.sF)
    case op: DeliteCollectBufferOutput[_,_,_] => blocks(op.alloc) ::: blocks(op.update) ::: blocks(op.appendable) ::: blocks(op.append) ::: blocks(op.setSize) ::: blocks(op.allocRaw) ::: blocks(op.copyRaw) ::: blocks(op.finalizer)
    case op: DeliteCollectFlatOutput[_,_,_] => blocks(op.alloc) ::: blocks(op.update) ::: blocks(op.finalizer)
    case op: DeliteBufferElem[_,_,_] => blocks(op.alloc) ::: blocks(op.apply) ::: blocks(op.update) ::: blocks(op.appendable) ::: blocks(op.append) ::: blocks(op.setSize) ::: blocks(op.allocRaw) ::: blocks(op.copyRaw) ::: blocks(op.finalizer)
//    case op: DeliteForeachElem[_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.sync)
    case op: DeliteForeachElem[_] => blocks(op.func) //::: blocks(op.sync)
    case op: DeliteReduceElem[_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFunc) ::: blocks(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFuncSeq) ::: blocks(op.rFuncPar) // should be ok for tuples...
    case _ => super.blocks(e)
  }

  override def syms(e: Any): List[Sym[Any]] = e match { //TR TODO: question -- is alloc a dependency (should be part of result) or a definition (should not)???
                                                        // aks: answer -- we changed it to be internal to the op to make things easier for CUDA. not sure if that still needs
                                                        // to be the case. similar question arises for sync
    case s: DeliteOpAbstractSingleTask[_] if s.requireInputs => super.syms(e) ::: syms(s.block) // super call: add case class syms (iff flag is set)
    case s: DeliteOpAbstractSingleTask[_] => syms(s.block)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => syms(op.keyFunc) ++ syms(op.valFunc) ++ syms(op.cond) ++ syms(op.buf) ++ syms(op.iBuf) ++ syms(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => syms(op.keyFunc) ++ syms(op.valFunc) ++ syms(op.cond) ++ syms(op.zero) ++ syms(op.rFunc) ++ syms(op.buf)
    case op: DeliteHashIndexElem[_,_] => syms(op.keyFunc) ++ syms(op.cond)
    case e: DeliteOpAbstractExternal[_] =>  super.syms(e) ::: syms(e.allocVal)
    case fr: DeliteOpAbstractForeachReduce[_] => syms(fr.funcBody)
    case op: DeliteCollectElem[_,_,_] => syms(op.aF) ::: syms(op.buf) ::: syms(op.iFunc) ::: syms(op.sF)
    case op: DeliteCollectBufferOutput[_,_,_] => syms(op.alloc) ::: syms(op.update) ::: syms(op.appendable) ::: syms(op.append) ::: syms(op.setSize) ::: syms(op.allocRaw) ::: syms(op.copyRaw) ::: syms(op.finalizer)
    case op: DeliteCollectFlatOutput[_,_,_] => syms(op.alloc) ::: syms(op.update) ::: syms(op.finalizer)
    case op: DeliteBufferElem[_,_,_] => syms(op.alloc) ::: syms(op.apply) ::: syms(op.update) ::: syms(op.appendable) ::: syms(op.append) ::: syms(op.setSize) ::: syms(op.allocRaw) ::: syms(op.copyRaw) ::: syms(op.finalizer)
//    case op: DeliteForeachElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.sync)
    case op: DeliteForeachElem[_] => syms(op.func) //::: syms(op.sync)
    case op: DeliteReduceElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFunc) ::: syms(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFuncSeq) ::: syms(op.rFuncPar) // should be ok for tuples...
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match { //TR FIXME: check this is actually correct
    case s: DeliteOpAbstractSingleTask[_] if s.requireInputs => super.readSyms(e) ::: readSyms(s.block) // super call: add case class syms (iff flag is set)
    case s: DeliteOpAbstractSingleTask[_] => readSyms(s.block)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => readSyms(op.keyFunc) ++ readSyms(op.valFunc) ++ readSyms(op.cond) ++ readSyms(op.buf) ++ readSyms(op.iBuf) ++ readSyms(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => readSyms(op.keyFunc) ++ readSyms(op.valFunc) ++ readSyms(op.cond) ++ readSyms(op.zero) ++ readSyms(op.rFunc) ++ readSyms(op.buf)
    case op: DeliteHashIndexElem[_,_] => readSyms(op.keyFunc) ++ readSyms(op.cond)
    case e: DeliteOpAbstractExternal[_] => super.readSyms(e) ::: readSyms(e.allocVal)
    case fr: DeliteOpAbstractForeachReduce[_] => readSyms(fr.funcBody)
    case op: DeliteCollectElem[_,_,_] => readSyms(op.aF) ::: readSyms(op.buf) ::: readSyms(op.iFunc) ::: readSyms(op.sF)
    case op: DeliteCollectBufferOutput[_,_,_] => readSyms(op.alloc) ::: readSyms(op.update) ::: readSyms(op.appendable) ::: readSyms(op.append) ::: readSyms(op.setSize) ::: readSyms(op.allocRaw) ::: readSyms(op.copyRaw) ::: readSyms(op.finalizer)
    case op: DeliteCollectFlatOutput[_,_,_] => readSyms(op.alloc) ::: readSyms(op.update) ::: readSyms(op.finalizer)
    case op: DeliteBufferElem[_,_,_] => readSyms(op.alloc) ::: readSyms(op.apply) ::: readSyms(op.update) ::: readSyms(op.appendable) ::: readSyms(op.append) ::: readSyms(op.setSize) ::: readSyms(op.allocRaw) ::: readSyms(op.copyRaw) ::: readSyms(op.finalizer)
//    case op: DeliteForeachElem[_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.sync)
    case op: DeliteForeachElem[_] => readSyms(op.func) //::: readSyms(op.sync)
    case op: DeliteReduceElem[_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.zero) ::: readSyms(op.rFunc) ::: readSyms(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.zero) ::: readSyms(op.rFuncSeq) ::: readSyms(op.rFuncPar)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpAbstractSingleTask[_] => effectSyms(s.block)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => effectSyms(op.keyFunc) ++ effectSyms(op.valFunc) ++ effectSyms(op.cond) ++ boundSyms(op.buf) ++ boundSyms(op.iBuf) ++ effectSyms(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => List(op.rV._1, op.rV._2) ++ effectSyms(op.keyFunc) ++ effectSyms(op.valFunc) ++ effectSyms(op.cond) ++ effectSyms(op.zero) ++ effectSyms(op.rFunc) ++ boundSyms(op.buf)
    case op: DeliteHashIndexElem[_,_] => effectSyms(op.keyFunc) ++ effectSyms(op.cond)
    case e: DeliteOpAbstractExternal[_] => effectSyms(e.allocVal) /*::: super.effectSyms(e) */
    case fr: DeliteOpAbstractForeachReduce[_] => List(fr.v) ::: effectSyms(fr.funcBody)
    case op: DeliteCollectElem[_,_,_] => op.iF :: op.eF :: effectSyms(op.aF)  ::: boundSyms(op.buf) ::: effectSyms(op.iFunc) ::: effectSyms(op.sF)
    case op: DeliteCollectBufferOutput[_,_,_] => List(op.eV, op.sV, op.allocVal, op.aV2, op.iV, op.iV2) ::: effectSyms(op.alloc) ::: effectSyms(op.update) ::: effectSyms(op.appendable) ::: effectSyms(op.append) ::: effectSyms(op.setSize) ::: effectSyms(op.allocRaw) ::: effectSyms(op.copyRaw) ::: effectSyms(op.finalizer)
    case op: DeliteCollectFlatOutput[_,_,_] => List(op.eV, op.sV, op.allocVal) ::: effectSyms(op.alloc) ::: effectSyms(op.update) ::: effectSyms(op.finalizer)
    case op: DeliteBufferElem[_,_,_] => List(op.eV, op.sV, op.allocVal, op.aV2, op.iV, op.iV2) ::: effectSyms(op.alloc) ::: effectSyms(op.apply) ::: effectSyms(op.update) ::: effectSyms(op.appendable) ::: effectSyms(op.append) ::: effectSyms(op.setSize) ::: effectSyms(op.allocRaw) ::: effectSyms(op.copyRaw) ::: effectSyms(op.finalizer)
//    case op: DeliteForeachElem[_] => effectSyms(op.func) ::: effectSyms(op.cond) ::: effectSyms(op.sync)
    case op: DeliteForeachElem[_] => effectSyms(op.func) //::: effectSyms(op.sync)
    case op: DeliteReduceElem[_] => List(op.rV._1, op.rV._2) ::: effectSyms(op.func) ::: effectSyms(op.cond) ::: effectSyms(op.zero) ::: effectSyms(op.rFunc) ::: effectSyms(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => syms(op.rVPar) ::: syms(op.rVSeq) ::: effectSyms(op.func._1) ::: effectSyms(op.cond) ::: effectSyms(op.zero) ::: effectSyms(op.rFuncPar) ::: effectSyms(op.rFuncSeq)
    case _ => super.boundSyms(e)
  }


  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: DeliteOpAbstractSingleTask[_] if s.requireInputs => super.symsFreq(e) ::: freqNormal(s.block)  // super call: add case class syms (iff flag is set)
    case s: DeliteOpAbstractSingleTask[_] => freqNormal(s.block)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => freqHot(op.keyFunc) ++ freqHot(op.valFunc) ++ freqHot(op.cond) ++ symsFreq(op.buf) ++ symsFreq(op.iBuf) ++ freqNormal(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => freqHot(op.keyFunc) ++ freqHot(op.valFunc) ++ freqHot(op.cond) ++ freqNormal(op.zero) ++ freqHot(op.rFunc) ++ symsFreq(op.buf)
    case op: DeliteHashIndexElem[_,_] => freqHot(op.keyFunc) ++ freqHot(op.cond)
    case e: DeliteOpAbstractExternal[_] => super.symsFreq(e) ::: freqNormal(e.allocVal)
    case fr: DeliteOpAbstractForeachReduce[_] => freqHot(fr.funcBody)
    case op: DeliteCollectElem[_,_,_] => freqHot(op.aF) ::: symsFreq(op.buf) ::: freqHot(op.iFunc) ::: freqHot(op.sF)
    case op: DeliteCollectBufferOutput[_,_,_] => freqNormal(op.alloc) ::: freqHot(op.update) ::: freqHot(op.appendable) ::: freqHot(op.append) ::: freqNormal(op.setSize) ::: freqNormal(op.allocRaw) ::: freqNormal(op.copyRaw) ::: freqNormal(op.finalizer)
    case op: DeliteCollectFlatOutput[_,_,_] => freqNormal(op.alloc) ::: freqHot(op.update) ::: freqNormal(op.finalizer)
    case op: DeliteBufferElem[_,_,_] => freqNormal(op.alloc) ::: freqHot(op.apply) ::: freqHot(op.update) ::: freqHot(op.appendable) ::: freqHot(op.append) ::: freqNormal(op.setSize) ::: freqNormal(op.allocRaw) ::: freqNormal(op.copyRaw) ::: freqNormal(op.finalizer)
//    case op: DeliteForeachElem[_] => freqNormal(op.sync) ::: freqHot(op.cond) ::: freqHot(op.func)
    case op: DeliteForeachElem[_] => /*freqNormal(op.sync) :::*/ freqHot(op.func)
    case op: DeliteReduceElem[_] => freqHot(op.cond) ::: freqHot(op.func) ::: freqNormal(op.zero) ::: freqHot(op.rFunc) ::: freqNormal(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => freqHot(op.cond) ::: freqHot(op.func) ::: freqNormal(op.zero) ::: freqHot(op.rFuncSeq) ::: freqHot(op.rFuncPar)
    case _ => super.symsFreq(e)
  }

  /////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpAbstractSingleTask[_] => syms(s.block)
    case e: DeliteOpAbstractExternal[_] => Nil
    case op: DeliteCollectElem[_,_,_] => Nil // in particular not op.alloc !
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case op: DeliteReduceTupleElem[_,_] => Nil
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => Nil
    case op: DeliteHashReduceElem[_,_,_,_] => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpAbstractSingleTask[_] => Nil
    case e: DeliteOpAbstractExternal[_] => Nil
    case op: DeliteCollectElem[_,_,_] => syms(op.aF)
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case op: DeliteReduceTupleElem[_,_] => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpAbstractSingleTask[_] => Nil
    case e: DeliteOpAbstractExternal[_] => Nil
    case op: DeliteCollectElem[_,_,_] => Nil
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case op: DeliteReduceTupleElem[_,_] => Nil
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => Nil
    case op: DeliteHashReduceElem[_,_,_,_] => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpAbstractSingleTask[_] => Nil
    case e: DeliteOpAbstractExternal[_] => Nil //syms(e.allocVal)
    case op: DeliteCollectElem[_,_,_] => Nil //syms(op.buf.alloc)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => Nil //syms(op.buf.alloc)
    case op: DeliteHashReduceElem[_,_,_,_] => Nil //syms(op.buf.alloc)
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case op: DeliteReduceTupleElem[_,_] => Nil
    case _ => super.copySyms(e)
  }

}
