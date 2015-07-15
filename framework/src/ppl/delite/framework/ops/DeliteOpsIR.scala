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


  /* 
   * Markers to tell Delite op code generation what kind of strategy to use.
   */
  trait DeliteParallelStrategy
  object ParFlat extends DeliteParallelStrategy { override def toString = "ParFlat" }
  object ParBuffer extends DeliteParallelStrategy { override def toString = "ParBuffer" }
  object ParSimpleBuffer extends DeliteParallelStrategy { override def toString = "ParSimpleBuffer" }


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
    
    def copyTransformedSymListOrElse[B](f: OpType => List[Exp[B]])(e: => List[Exp[B]]): List[Exp[B]] = original.map(p => f(p._2.asInstanceOf[OpType]).map(p._1(_))).getOrElse(e)
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
   * Future base class for nested data parallel Delite ops.
   */
  abstract class DeliteOpLoopNest[A:Manifest](implicit ctx: SourceContext) extends AbstractLoopNest[A] with DeliteOp[A] {
    type OpType <: DeliteOpLoopNest[A]
    def copyBodyOrElse(e: => Def[A]): Def[A] = original.map(p=>mirrorLoopBody(p._2.asInstanceOf[OpType].body,p._1)).getOrElse(e)
    val numDynamicChunks:Int = 0
  }

  /**
   * The base class for most data parallel Delite ops.
   */
  abstract class DeliteOpLoop[A:Manifest](implicit ctx: SourceContext) extends AbstractLoop[A] with DeliteOp[A] {
    type OpType <: DeliteOpLoop[A]
    def copyBodyOrElse(e: => Def[A]): Def[A] = original.map(p=>mirrorLoopBody(p._2.asInstanceOf[OpType].body,p._1)).getOrElse(e)
    // DK - removed 'final' tag on iterator to allow override in DeliteSimpleOps
    lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(fresh[Int]).asInstanceOf[Sym[Int]]
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

  // --- Tiling Elems
  case class DeliteTileBuffer[A:Manifest,TA:Manifest,CA:Manifest](
    // -- bound vars
    bS: List[Sym[RangeVector]],   // buffer offsets
    bV: List[Sym[Int]],   // buffer indices
    tV: List[Sym[Int]],   // tile indices
    tD: List[Sym[Int]],   // tile dimensions
    buffVal: Sym[CA],     // primary buffer allocation
    tileVal: Sym[TA],     // tile function result
    partVal: Sym[TA],     // partial result
    bE: Sym[A],           // Buffer element (during copying out)
    tE: Sym[A],           // Tile element (during copying in)

    // collection functions
    bApply: Block[A],     // buffer apply
    tApply: Block[A],     // tile apply function
    bUpdate: Block[Unit], // buffer update function
    tUpdate: Block[Unit], // tile update function
    allocBuff: Block[CA], // buffer allocate function
    allocTile: Block[TA]  // tile allocate function
  ) {
    val mA = manifest[A]
    val mTA = manifest[TA]
    val mCA = manifest[CA]
  }

  case class DeliteTileElem[A:Manifest,TA:Manifest,CA:Manifest](
    keys: List[Block[RangeVector]],
    cond: List[Block[Boolean]] = Nil,
    tile: Block[TA],
    rV: (Sym[TA],Sym[TA]),
    rFunc: Option[Block[TA]],
    buf: DeliteTileBuffer[A,TA,CA],
    numDynamicChunks: Int
  ) extends Def[CA] with DeliteLoopElem {
    val mA = manifest[A]
    val mTA = manifest[TA]
    val mCA = manifest[CA]
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
    case e:DeliteCollectElem[_,_,_] => false //e.par == ParBuffer //e.cond.nonEmpty
    case _ => false
  }
  def loopBodyNumDynamicChunks[A](e: Def[A]) = e match {
    case e:DeliteLoopElem => e.numDynamicChunks
    case _ => 0
  }

  def loopBodyAverageDynamicChunks[A](e: List[Def[A]]) = {
    val chunks = e.map(loopBodyNumDynamicChunks)
    val specified = chunks.filter(i => i != 0 && i != -1)
    if (specified.length > 0) specified.sum / specified.length //use average of hints for the loops
    else if (chunks.contains(-1)) -1 //dynamic chunking
    else 0 //static chunking
  }

  def loopBodyNeedsPostProcess[A](e: Def[A]) = e match {
    case e:DeliteCollectElem[_,_,_] => e.par == ParBuffer || e.par == ParSimpleBuffer //e.cond.nonEmpty
    case e:DeliteHashCollectElem[_,_,_,_,_,_] => true
    case _ => false
  }

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

    case e: DeliteTileElem[_,_,_] => 
      // TODO: This probably isn't right
      val kf = e.keys.map(summarizeEffects).reduce((s1,s2) => s1 andAlso s2)
      val cf = if (e.cond.nonEmpty) e.cond.map(summarizeEffects).reduce((s1,s2) => s1 andThen s2) else Pure()
      val tf = summarizeEffects(e.tile)
      val rf = if (e.rFunc.isDefined) summarizeEffects(e.rFunc.get) else Pure()
      (kf andAlso cf andAlso tf andAlso rf).star
  }

  // TODO: just to make refactoring easier in case we want to change to reflectSomething
  // def reflectPure[A:Manifest](x: Def[A]): Exp[A] = toAtom(x)

  // alternative: leave reflectPure as above and override toAtom...

  def reflectPure[A:Manifest](d: Def[A])(implicit ctx: SourceContext): Exp[A] = d match {
    case x: DeliteOpLoopNest[_] => 
      val mutableInputs = readMutableData(d) 
      val re = Read(mutableInputs)
      val be = summarizeBody(x.body)
      reflectEffect(d, re andAlso be)

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
    case x: DeliteOpLoopNest[_] => 
      val z = x.body
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
    case Reflect(x: DeliteOpLoopNest[_], u, es) => 
      val z = x.body
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

    def mirrorTileBuffer[A,TA,CA](e: DeliteTileBuffer[A,TA,CA]) = 
      DeliteTileBuffer[A,TA,CA](
        bS = f(e.bS).asInstanceOf[List[Sym[RangeVector]]],
        bV = f(e.bV).asInstanceOf[List[Sym[Int]]],
        tV = f(e.tV).asInstanceOf[List[Sym[Int]]],
        tD = f(e.tD).asInstanceOf[List[Sym[Int]]],
        buffVal = f(e.buffVal).asInstanceOf[Sym[CA]],
        tileVal = f(e.tileVal).asInstanceOf[Sym[TA]],
        partVal = f(e.partVal).asInstanceOf[Sym[TA]],
        bE = f(e.bE).asInstanceOf[Sym[A]],
        tE = f(e.tE).asInstanceOf[Sym[A]],
        bApply = fb(e.bApply)(e.mA),
        tApply = fb(e.tApply)(e.mA),
        bUpdate = fb(e.bUpdate)(manifest[Unit]),
        tUpdate = fb(e.tUpdate)(manifest[Unit]),
        allocBuff = fb(e.allocBuff)(e.mCA),
        allocTile = fb(e.allocTile)(e.mTA)
      )(e.mA,e.mTA,e.mCA)

    d match {
      case e: DeliteTileElem[a,ta,ca] => 
        (DeliteTileElem[a,ta,ca](
          keys = e.keys.map(fb(_)(manifest[RangeVector])),
          cond = e.cond.map(fb(_)(manifest[Boolean])),
          tile = fb(e.tile)(e.mTA),
          rV = (f(e.rV._1).asInstanceOf[Sym[ta]], f(e.rV._2).asInstanceOf[Sym[ta]]), // need to transform bound vars? 
          rFunc = e.rFunc.map(fb(_)(e.mTA)),
          buf = mirrorTileBuffer(e.buf),
          numDynamicChunks = e.numDynamicChunks
        )(e.mA,e.mTA,e.mCA)).asInstanceOf[Def[A]]
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
    case s: DeliteOpAbstractSingleTask[_] => blocks(s.block)
    case e: DeliteOpAbstractExternal[_] =>  super.blocks(e) ::: blocks(e.allocVal)
    case fr: DeliteOpAbstractForeachReduce[_] => blocks(fr.funcBody)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc) ::: blocks(op.cond) ::: blocks(op.buf) ::: blocks(op.iBuf) ::: blocks(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFunc) ::: blocks(op.buf)
    case op: DeliteHashIndexElem[_,_] => blocks(op.keyFunc) ::: blocks(op.cond)
    case op: DeliteCollectElem[_,_,_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.buf)
    case op: DeliteBufferElem[_,_,_] => blocks(op.alloc) ::: blocks(op.apply) ::: blocks(op.update) ::: blocks(op.appendable) ::: blocks(op.append) ::: blocks(op.setSize) ::: blocks(op.allocRaw) ::: blocks(op.copyRaw) ::: blocks(op.finalizer)
//    case op: DeliteForeachElem[_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.sync)
    case op: DeliteForeachElem[_] => blocks(op.func) //::: blocks(op.sync)
    case op: DeliteReduceElem[_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFunc) ::: blocks(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.zero) ::: blocks(op.rFuncSeq) ::: blocks(op.rFuncPar) // should be ok for tuples...
    
    case op: DeliteTileBuffer[_,_,_] => blocks(op.bApply) ::: blocks(op.tApply) ::: blocks(op.bUpdate) ::: blocks(op.tUpdate) ::: blocks(op.allocBuff) ::: blocks(op.allocTile)
    case op: DeliteTileElem[_,_,_] => blocks(op.keys) ::: blocks(op.cond) ::: blocks(op.tile) ::: blocks(op.rFunc)

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
    case op: DeliteCollectElem[_,_,_] => syms(op.func) ::: syms(op.cond) ::: syms(op.buf) ::: syms(op.iFunc) ::: syms(op.sF)
    case op: DeliteBufferElem[_,_,_] => syms(op.alloc) ::: syms(op.apply) ::: syms(op.update) ::: syms(op.appendable) ::: syms(op.append) ::: syms(op.setSize) ::: syms(op.allocRaw) ::: syms(op.copyRaw) ::: syms(op.finalizer)
//    case op: DeliteForeachElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.sync)
    case op: DeliteForeachElem[_] => syms(op.func) //::: syms(op.sync)
    case op: DeliteReduceElem[_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFunc) ::: syms(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => syms(op.func) ::: syms(op.cond) ::: syms(op.zero) ::: syms(op.rFuncSeq) ::: syms(op.rFuncPar) // should be ok for tuples...
    
    case op: DeliteTileBuffer[_,_,_] => syms(op.bApply) ::: syms(op.tApply) ::: syms(op.bUpdate) ::: syms(op.tUpdate) ::: syms(op.allocBuff) ::: syms(op.allocTile)
    case op: DeliteTileElem[_,_,_] => syms(op.keys) ::: syms(op.cond) ::: syms(op.tile) ::: syms(op.rFunc) ::: syms(op.buf)
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
    case op: DeliteCollectElem[_,_,_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.buf) ::: readSyms(op.iFunc) ::: readSyms(op.sF)
    case op: DeliteBufferElem[_,_,_] => readSyms(op.alloc) ::: readSyms(op.apply) ::: readSyms(op.update) ::: readSyms(op.appendable) ::: readSyms(op.append) ::: readSyms(op.setSize) ::: readSyms(op.allocRaw) ::: readSyms(op.copyRaw) ::: readSyms(op.finalizer)
//    case op: DeliteForeachElem[_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.sync)
    case op: DeliteForeachElem[_] => readSyms(op.func) //::: readSyms(op.sync)
    case op: DeliteReduceElem[_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.zero) ::: readSyms(op.rFunc) ::: readSyms(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => readSyms(op.func) ::: readSyms(op.cond) ::: readSyms(op.zero) ::: readSyms(op.rFuncSeq) ::: readSyms(op.rFuncPar)
    
    case op: DeliteTileBuffer[_,_,_] => readSyms(op.bApply) ::: readSyms(op.tApply) ::: readSyms(op.bUpdate) ::: readSyms(op.tUpdate) ::: readSyms(op.allocBuff) ::: readSyms(op.allocTile)
    case op: DeliteTileElem[_,_,_] => readSyms(op.keys) ::: readSyms(op.cond) ::: readSyms(op.tile) ::: readSyms(op.rFunc) ::: readSyms(op.buf)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpAbstractSingleTask[_] => effectSyms(s.block)
    case op: DeliteHashCollectElem[_,_,_,_,_,_] => effectSyms(op.keyFunc) ++ effectSyms(op.valFunc) ++ effectSyms(op.cond) ++ boundSyms(op.buf) ++ boundSyms(op.iBuf) ++ effectSyms(op.iBufSize)
    case op: DeliteHashReduceElem[_,_,_,_] => List(op.rV._1, op.rV._2) ++ effectSyms(op.keyFunc) ++ effectSyms(op.valFunc) ++ effectSyms(op.cond) ++ effectSyms(op.zero) ++ effectSyms(op.rFunc) ++ boundSyms(op.buf)
    case op: DeliteHashIndexElem[_,_] => effectSyms(op.keyFunc) ++ effectSyms(op.cond)
    case e: DeliteOpAbstractExternal[_] => effectSyms(e.allocVal) /*::: super.effectSyms(e) */
    case fr: DeliteOpAbstractForeachReduce[_] => List(fr.v) ::: effectSyms(fr.funcBody)
    case op: DeliteCollectElem[_,_,_] => effectSyms(op.func)  ::: effectSyms(op.cond) ::: boundSyms(op.buf) ::: effectSyms(op.iFunc) ::: effectSyms(op.sF) ::: op.iF.toList ::: op.eF.toList
    case op: DeliteBufferElem[_,_,_] => List(op.eV, op.sV, op.allocVal, op.aV2, op.iV, op.iV2) ::: effectSyms(op.alloc) ::: effectSyms(op.apply) ::: effectSyms(op.update) ::: effectSyms(op.appendable) ::: effectSyms(op.append) ::: effectSyms(op.setSize) ::: effectSyms(op.allocRaw) ::: effectSyms(op.copyRaw) ::: effectSyms(op.finalizer)
//    case op: DeliteForeachElem[_] => effectSyms(op.func) ::: effectSyms(op.cond) ::: effectSyms(op.sync)
    case op: DeliteForeachElem[_] => effectSyms(op.func) //::: effectSyms(op.sync)
    case op: DeliteReduceElem[_] => List(op.rV._1, op.rV._2) ::: effectSyms(op.func) ::: effectSyms(op.cond) ::: effectSyms(op.zero) ::: effectSyms(op.rFunc) ::: effectSyms(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => syms(op.rVPar) ::: syms(op.rVSeq) ::: effectSyms(op.func._1) ::: effectSyms(op.cond) ::: effectSyms(op.zero) ::: effectSyms(op.rFuncPar) ::: effectSyms(op.rFuncSeq)
    
    case op: DeliteTileBuffer[_,_,_] => op.bS ::: op.bV ::: op.tV ::: op.tD ::: List(op.bE, op.tE, op.buffVal, op.tileVal, op.partVal) ::: effectSyms(op.bApply) ::: effectSyms(op.tApply) ::: effectSyms(op.bUpdate) ::: effectSyms(op.tUpdate) ::: effectSyms(op.allocBuff) ::: effectSyms(op.allocTile)
    case op: DeliteTileElem[_,_,_] => List(op.rV._1, op.rV._2) ::: effectSyms(op.keys) ::: effectSyms(op.cond) ::: effectSyms(op.tile) ::: effectSyms(op.rFunc) ::: boundSyms(op.buf)

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
    case op: DeliteCollectElem[_,_,_] => freqHot(op.cond) ::: freqHot(op.func) ::: symsFreq(op.buf) ::: freqHot(op.iFunc) ::: freqHot(op.sF)
    case op: DeliteBufferElem[_,_,_] => freqNormal(op.alloc) ::: freqHot(op.apply) ::: freqHot(op.update) ::: freqHot(op.appendable) ::: freqHot(op.append) ::: freqNormal(op.setSize) ::: freqNormal(op.allocRaw) ::: freqNormal(op.copyRaw) ::: freqNormal(op.finalizer)
//    case op: DeliteForeachElem[_] => freqNormal(op.sync) ::: freqHot(op.cond) ::: freqHot(op.func)
    case op: DeliteForeachElem[_] => /*freqNormal(op.sync) :::*/ freqHot(op.func)
    case op: DeliteReduceElem[_] => freqHot(op.cond) ::: freqHot(op.func) ::: freqNormal(op.zero) ::: freqHot(op.rFunc) ::: freqNormal(op.accInit)
    case op: DeliteReduceTupleElem[_,_] => freqHot(op.cond) ::: freqHot(op.func) ::: freqNormal(op.zero) ::: freqHot(op.rFuncSeq) ::: freqHot(op.rFuncPar)
    
    case op: DeliteTileBuffer[_,_,_] => freqNormal(op.allocBuff) ::: freqNormal(op.allocTile) ::: freqHot(op.bApply) ::: freqHot(op.tApply) ::: freqHot(op.bUpdate) ::: freqHot(op.tUpdate)
    case op: DeliteTileElem[_,_,_] => freqHot(op.keys) ::: freqHot(op.cond) ::: freqHot(op.tile) ::: freqHot(op.rFunc) ::: symsFreq(op.buf)

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

    case op: DeliteTileBuffer[_,_,_] => Nil
    case op: DeliteTileElem[_,_,_] => Nil

    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpAbstractSingleTask[_] => Nil
    case e: DeliteOpAbstractExternal[_] => Nil
    case op: DeliteCollectElem[_,_,_] => syms(op.func)
    case op: DeliteForeachElem[_] => Nil
    case op: DeliteReduceElem[_] => Nil
    case op: DeliteReduceTupleElem[_,_] => Nil

    case op: DeliteTileBuffer[_,_,_] => Nil
    case op: DeliteTileElem[_,_,_] => Nil

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

    case op: DeliteTileBuffer[_,_,_] => Nil
    case op: DeliteTileElem[_,_,_] => Nil

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

    case op: DeliteTileBuffer[_,_,_] => Nil
    case op: DeliteTileElem[_,_,_] => Nil

    case _ => super.copySyms(e)
  }

}
