package ppl.delite.framework.ops

import scala.reflect.SourceContext
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

/*
 * Simplified versions of DeliteOps which accept Block inputs rather than lambda functions
 * Useful for transforming loops after lambda functions have been staged
 *
 * Note that DeliteFileReader brings in pretty much everything,
 * including Ops, CollectionOps, Array, ArrayBuffer, and Map
 */
trait DeliteSimpleOpsExp extends DeliteFileReaderOpsExp {

  /**
   * Parallel, effectful foreach
   * @param ov       - symbol for loop iterator
   * @param loopSize - the size of the collection/dimensions to loop over
   * @param func     - the foreach function
   */
  case class SimpleForeach(ov: Sym[Int], loopSize: Exp[Int], func: Block[Unit])(implicit ctx: SourceContext) extends DeliteOpLoop[Unit] {
    type OpType <: SimpleForeach

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(ov).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = this.func,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleForeach(" + size + ")(" + v + " => " + body + ")"
  }
  object SimpleForeach {
    def mirror(op: SimpleForeach, f: Transformer)(implicit pos: SourceContext) = op match {
      case SimpleForeach(v,s,b) => new { override val original = Some(f,op) } with SimpleForeach(v,f(s),b)(pos)
    }
  }



  /**
   * Parallel reduction. Reducing function must be associative.
   * TODO: This will need to be changed with Vera's fold/reduce distinction
   * @param ov       - symbol for loop iterator
   * @param orV      - pair of symbols representing two elements (Sym[A], Sym[A])
   * @param loopSize - number of elements to reduce (usually equal to size of input collection)
   * @param mFunc    - map function to obtain elements to reduce
   * @param rFunc    - the reduction function; (Block[A]) Must be associative.
   * @param init     - accumulator initialization - used only if mutable is true
   * @param zero     - zero value (not actually used to compute output result)
   * @param filter   - optional filter function
   * @param mutable  - mutable reduce (reduction is done by directly updating the accumulator)
   */
  case class SimpleReduce[A:Manifest](ov: Sym[Int], orV: (Sym[A],Sym[A]), loopSize: Exp[Int], mFunc: Block[A], rFunc: Block[A], init: Block[A], zero: Block[A], fFunc: List[Block[Boolean]] = Nil, mutable: Boolean = false)(implicit ctx: SourceContext) extends DeliteOpLoop[A] {
    type OpType <: SimpleReduce[A]
    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(ov).asInstanceOf[Sym[Int]]
    lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)(orV)

    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    // If stripFirst is false, accInit is used to allocate the accumulator, and is used in the reduction
    // Zero is only used if the collection is empty or if the first iteration of the filter function is false
    lazy val body: Def[A] = copyBodyOrElse(DeliteReduceElem[A](
      func = this.mFunc,
      cond = this.fFunc,
      zero = this.zero,
      accInit = this.init,
      rV = this.rV,
      rFunc = this.rFunc,
      stripFirst = !mutable,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleReduce(" + size + ")(" + v + ", (" + rV._1 + ", " + rV._2 + ") => " + body + ")"
    val mA = manifest[A]
  }
  object SimpleReduce {
    def apply[A:Manifest](ov: Sym[Int], orV: (Sym[A],Sym[A]), size: Exp[Int], mFunc: Block[A], rFunc: Block[A], zero: Exp[A])(implicit ctx: SourceContext)
      = new SimpleReduce[A](ov,orV,size,mFunc,rFunc,reifyEffects(zero),reifyEffects(zero),Nil,false)

    def mirror[A:Manifest](op: SimpleReduce[A], f: Transformer)(implicit pos: SourceContext): SimpleReduce[A] = op match {
      case SimpleReduce(v,r,s,l,b,i,z,c,m) => new { override val original = Some(f,op) } with SimpleReduce(v,r,s,l,b,i,z,c,m)(op.mA, pos)
    }
    def unerase[A:Manifest](op: SimpleReduce[_]): SimpleReduce[A] = op.asInstanceOf[SimpleReduce[A]]
  }

  abstract class SimpleCollect[R:Manifest,C<:DeliteCollection[R]:Manifest] extends DeliteOpLoop[C] {
    type OpType <: SimpleCollect[R,C]

    lazy val eV: Sym[R] = copyTransformedOrElse(_.eV)(fresh[R]).asInstanceOf[Sym[R]]
    lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val iV2: Sym[Int] = copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val aV2: Sym[C] = copyTransformedOrElse(_.aV2)(fresh[C]).asInstanceOf[Sym[C]]
    lazy val allocVal: Sym[C] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[C])).asInstanceOf[Sym[C]]

    def alloc(i: Exp[Int]): Exp[C] = dc_alloc[R,C](allocVal, i)
    def finalize(av: Exp[C]): Exp[C] = av

    lazy val buf = DeliteBufferElem(
      eV = this.eV,     // current element
      sV = this.sV,     // intermediate size
      iV = this.iV,     // copy destination start
      iV2 = this.iV2,
      allocVal = this.allocVal,
      aV2 = this.aV2,
      alloc = reifyEffects(this.alloc(this.sV)),
      apply = unusedBlock,
      update = reifyEffects(dc_update(this.allocVal,v,this.eV)),
      append = reifyEffects(dc_append(this.allocVal,v,this.eV)),
      appendable = reifyEffects(dc_appendable(this.allocVal,v,this.eV)),
      setSize = reifyEffects(dc_set_logical_size(this.allocVal,this.sV)),
      allocRaw = reifyEffects(dc_alloc[R,C](this.allocVal,this.sV)),
      copyRaw = reifyEffects(dc_copy(this.aV2,this.iV,this.allocVal,this.iV2,this.sV)),
      finalizer = reifyEffects(this.finalize(this.allocVal))
    )
  }


  /**
   * Parallel map (includes map, zipWith, mapIndices)
   * @param ov        - symbol for loop iterator
   * @param loopSize  - size of loop (usually size of input collection)
   * @param func      - map function  (anything that productes Exp[R])
   */
  case class SimpleMap[R:Manifest,C<:DeliteCollection[R]:Manifest](ov: Sym[Int], loopSize: Exp[Int], func: Block[R])(implicit ctx: SourceContext) extends SimpleCollect[R,C] {
    type OpType <: SimpleMap[R,C]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(ov).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    lazy val body: Def[C] = copyBodyOrElse(DeliteCollectElem[R,C,C](
      func = this.func,
      cond = Nil,
      par = dc_parallelization(allocVal, false),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleMap(" + size + ")(" + v + " => " + body + ")"
    val mR = manifest[R]
    val mC = manifest[C]
  }
  object SimpleMap {
    def array[R:Manifest](ov: Sym[Int], size: Exp[Int], func: Block[R])(implicit ctx: SourceContext) = {
      new SimpleMap[R,DeliteArray[R]](ov, size, func)
    }

    def mirror[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleMap[R,C], f: Transformer)(implicit pos: SourceContext): SimpleMap[R,C] = op match {
      case SimpleMap(v,s,b) => new { override val original = Some(f,op) } with SimpleMap(v,s,b)(op.mR,op.mC,pos)
    }
    def unerase[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleMap[_,_]): SimpleMap[R,C] = op.asInstanceOf[SimpleMap[R,C]]
  }


  // TODO: Unify filter and flatMap? Slightly different type signatures (one returns element, other returns collection...)
  // Possible to make primitive types a form of DeliteCollection?
  /**
   * Parallel filter
   * @param ov       - symbol for loop iterator
   * @param loopSize - size of loop (usually size of input collection)
   * @param cond     - filter condition
   * @param func     - map function
   */
  case class SimpleFilter[R:Manifest,C<:DeliteCollection[R]:Manifest](ov: Sym[Int], loopSize: Exp[Int], cond: Block[Boolean], func: Block[R])(implicit ctx: SourceContext) extends SimpleCollect[R,C] {
    type OpType <: SimpleFilter[R,C]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(ov).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    lazy val body: Def[C] = copyBodyOrElse(DeliteCollectElem[R,C,C](
      func = this.func,
      cond = List(this.cond),
      par = dc_parallelization(allocVal, true),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleFilter(" + size + ")(" + v + " => " + body + ")"
    val mR = manifest[R]
    val mC = manifest[C]
  }
  object SimpleFilter {
    def array[R:Manifest](ov: Sym[Int], size: Exp[Int], cond: Block[Boolean], func: Block[R])(implicit ctx: SourceContext) = {
      new SimpleFilter[R,DeliteArray[R]](ov, size, cond, func)
    }

    def mirror[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleFilter[R,C], f: Transformer)(implicit pos: SourceContext): SimpleFilter[R,C] = op match {
      case SimpleFilter(v,s,c,b) => new { override val original = Some(f,op) } with SimpleFilter(v,s,c,b)(op.mR,op.mC,pos)
    }
    def unerase[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleFilter[_,_]): SimpleFilter[R,C] = op.asInstanceOf[SimpleFilter[R,C]]
  }



  /**
   * Parallel flat map
   * @param ov       - symbol for loop iterator
   * @param loopSize - size of loop (usually size of input collection)
   * @param func     - flatmap function - produces Exp[DeliteCollection[R]]
  */
  case class SimpleFlatMap[R:Manifest,C<:DeliteCollection[R]:Manifest](ov: Sym[Int], loopSize: Exp[Int], func: Block[DeliteCollection[R]])(implicit ctx: SourceContext) extends SimpleCollect[R,C] {
    type OpType <: SimpleFlatMap[R,C]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(ov).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    lazy val iF: Sym[Int] = copyTransformedOrElse(_.iF)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val eF: Sym[DeliteCollection[R]] = copyTransformedOrElse(_.eF)(fresh[DeliteCollection[R]](func.tp)).asInstanceOf[Sym[DeliteCollection[R]]]

    lazy val body: Def[C] = copyBodyOrElse(DeliteCollectElem[R,C,C](
      iFunc = Some(this.func),
      iF = Some(this.iF),
      sF = Some(reifyEffects(dc_size(this.eF))), //note: applying dc_size directly to iFunc can lead to iFunc being duplicated during mirroring
      eF = Some(this.eF),
      func = reifyEffects(dc_apply(this.eF,this.iF)),
      par = dc_parallelization(this.allocVal, true),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleFlatMap(" + size + ")(" + v + " => " + body + ")"
    val mR = manifest[R]
    val mC = manifest[C]
  }
  object SimpleFlatMap {
    def array[R:Manifest](ov: Sym[Int], size: Exp[Int], func: Block[DeliteCollection[R]])(implicit ctx: SourceContext) = {
      new SimpleFlatMap[R,DeliteArray[R]](ov,size,func)
    }
    def mirror[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleFlatMap[R,C], f: Transformer)(implicit pos: SourceContext): SimpleFlatMap[R,C] = op match {
      case SimpleFlatMap(v,s,b) => new {override val original = Some(f,op)} with SimpleFlatMap(v,s,b)(op.mR,op.mC,pos)
    }
    def unerase[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleFlatMap[_,_]): SimpleFlatMap[R,C] = op.asInstanceOf[SimpleFlatMap[R,C]]
  }



  /**
   * Extremely simple parallel file read - no string splitting, just returns collection of lines
   * @param ov    - symbol for loop iterator
   * @param paths - list of file paths
  */
  case class SimpleRead[C<:DeliteCollection[String]:Manifest](ov: Sym[Int], paths: Seq[Exp[String]])(implicit ctx: SourceContext) extends SimpleCollect[String,C] {
    type OpType <: SimpleRead[C]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(ov).asInstanceOf[Sym[Int]]

    val inputStream = dfis_new(paths, unit(null))
    val size: Exp[Int] = copyTransformedOrElse(_.size)(dfis_size(inputStream).asInstanceOf[Exp[Int]])

    lazy val body: Def[C] = copyBodyOrElse(DeliteCollectElem[String,C,C](
      func = reifyEffects(dfis_readLine(inputStream, v)),
      par = dc_parallelization(allocVal, true),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleRead(" + paths.mkString(",") + ")(" + v + " => " + body + ")"
    val mC = manifest[C]
  }

  object SimpleRead {
    def array(ov: Sym[Int], paths: Seq[Exp[String]])(implicit ctx: SourceContext) = {
      new SimpleRead[DeliteArray[String]](ov, paths)
    }
    def mirror[C<:DeliteCollection[String]:Manifest](op: SimpleRead[C], f: Transformer)(implicit ctx: SourceContext): SimpleRead[C] = op match {
      case SimpleRead(v,p) => new {override val original = Some(f,op)} with SimpleRead(v,f(p))(op.mC,ctx)
    }
    def unerase[C<:DeliteCollection[String]:Manifest](op: SimpleRead[_]): SimpleRead[C] = op.asInstanceOf[SimpleRead[C]]
  }



  /**
   * Parallel groupBy
   * @param ov       - symbol for loop iterator
   * @param loopSize - size of loop (usually size of input collection)
   * @param keyFunc  - key function
   * @param valFunc  - value function
   */
  case class SimpleGroupBy[K:Manifest,V:Manifest,C<:DeliteCollection[V]:Manifest,H<:DeliteCollection[C]:Manifest](oV: Sym[Int], loopSize: Exp[Int], keyFunc: Block[K], valFunc: Block[V])(implicit ctx: SourceContext) extends DeliteOpLoop[H] {
    type OpType <: SimpleGroupBy[K,V,C,H]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    lazy val allocVal: Sym[H] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[H])).asInstanceOf[Sym[H]]
    lazy val aV2: Sym[H] = copyTransformedOrElse(_.aV2)(fresh[H]).asInstanceOf[Sym[H]]
    lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val iV2: Sym[Int] = copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val iiV: Sym[Int] = copyTransformedOrElse(_.iiV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val iiV2: Sym[Int] = copyTransformedOrElse(_.iiV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val eV: Sym[V] = copyTransformedOrElse(_.eV)(fresh[V]).asInstanceOf[Sym[V]]

    def alloc(i: Exp[Int]): Exp[H] = dc_alloc[C,H](allocVal, i)
    def allocI(i: Exp[Int]): Exp[C] = dc_alloc[V,C](fresh[C], i)

    def finalize(x: Exp[H]): Exp[H] = x
    def finalizeI(x: Exp[C]): Exp[C] = x

    // this is necessary to make nested collections with SoA work properly (rewrites can't kick in on a bound sym)
    final lazy val ibufVal: Exp[C] = Config.soaEnabled match {
      case true => copyTransformedOrElse(_.ibufVal)(dc_apply(allocVal,iV))
      case false => copyTransformedOrElse(_.ibufVal)(reflectMutableSym(fresh[C]).asInstanceOf[Sym[C]])
    }
    final lazy val ibufVal2: Exp[C] = Config.soaEnabled match {
      case true => copyTransformedOrElse(_.ibufVal2)(dc_apply(aV2,iV2))
      case false => copyTransformedOrElse(_.ibufVal2)(reflectMutableSym(fresh[C]).asInstanceOf[Sym[C]])
    }
    lazy val body: Def[H] = copyBodyOrElse(DeliteHashCollectElem[K,V,C,C,H,H](
      keyFunc = this.keyFunc,
      valFunc = this.valFunc,
      cond = Nil,
      iBufSize = reifyEffects(dc_size(ibufVal2)),
      iBuf = DeliteBufferElem(
        eV = this.eV,
        sV = this.sV,
        iV = this.iiV,
        iV2 = this.iiV2,
        allocVal = if (Config.soaEnabled) unusedSym else this.ibufVal.asInstanceOf[Sym[C]],
        aV2 = if (Config.soaEnabled) unusedSym else this.ibufVal2.asInstanceOf[Sym[C]],
        alloc = reifyEffects(this.allocI(sV)),
        apply = unusedBlock,
        update = unusedBlock,
        appendable = unusedBlock,
        append = reifyEffects(dc_append(ibufVal,v,eV)),
        setSize = reifyEffects(dc_set_logical_size(ibufVal,sV)),
        allocRaw = if (Config.soaEnabled) unusedBlock else reifyEffects(dc_apply(this.aV2,this.iV2)), // co-opting this field to tunnel the bound apply through
        copyRaw = reifyEffects(dc_copy(ibufVal2,iiV2,ibufVal,iiV,sV)),
        finalizer = reifyEffects(this.finalizeI(ibufVal))
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
        update = if (Config.soaEnabled) reifyEffects(dc_update(this.allocVal,iV,dc_alloc[V,C](ibufVal,sV))) else reifyEffects(dc_update(this.allocVal,iV,delite_unsafe_immutable(ibufVal))),
        appendable = unusedBlock,
        // append = reifyEffects(dc_append(this.allocVal,v,this.allocI(sV))), // without SoA, we lose the mutable allocI here. why is the allocI (iBuf allocation) hidden down here instead of using iBuf.alloc above? must have something to do with the rewrites on iBufVal..
        append = if (Config.soaEnabled) reifyEffects(dc_append(this.allocVal,v,this.allocI(sV))) else reifyEffects(dc_append(this.allocVal,v,delite_unsafe_immutable(ibufVal))),
        setSize = reifyEffects(dc_set_logical_size(this.allocVal,sV)),
        allocRaw = reifyEffects(dc_alloc[C,H](this.allocVal,sV)),
        copyRaw = unusedBlock,
        finalizer = reifyEffects(this.finalize(this.allocVal))
      ),
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleGroupBy(" + size + ")(" + v + " => " + body + ")"
    val mK = manifest[K]
    val mV = manifest[V]
    val mC = manifest[C]
    val mH = manifest[H]
  }
  object SimpleGroupBy {
    def mirror[K:Manifest,V:Manifest,C<:DeliteCollection[V]:Manifest,H<:DeliteCollection[C]:Manifest](op: SimpleGroupBy[K,V,C,H], f: Transformer)(implicit pos: SourceContext): SimpleGroupBy[K,V,C,H] = op match {
      case SimpleGroupBy(v,s,k,b) => new {override val original = Some(f,op)} with SimpleGroupBy(v,s,k,b)(op.mK,op.mV,op.mC,op.mH,pos)
    }
    def unerase[K:Manifest,V:Manifest,C<:DeliteCollection[V]:Manifest,H<:DeliteCollection[C]:Manifest](op: SimpleGroupBy[_,_,_,_]): SimpleGroupBy[K,V,C,H] = op.asInstanceOf[SimpleGroupBy[K,V,C,H]]
  }



  /**
   * Parallel groupBy-reduce
   * @param ov       - symbol for loop iterator
   * @param orV      - symbols used to reify reduction block
   * @param loopSize - size of loop (usually size of input collection)
   * @param keyFunc  - key function
   * @param valFunc  - value function
   * @param rFunc    - reduction function
   * @param zero     - zero for bins
   * @param cond     - optional list of filter conditions
   */
  case class SimpleGroupByReduce[K:Manifest,V:Manifest,C<:DeliteCollection[V]:Manifest](ov: Sym[Int], orV: (Sym[V], Sym[V]), loopSize: Exp[Int], keyFunc: Block[K], valFunc: Block[V], rFunc: Block[V], zero: Block[V], cond: List[Block[Boolean]] = Nil)(implicit ctx: SourceContext) extends DeliteOpLoop[C] {
    type OpType <: SimpleGroupByReduce[K,V,C]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(ov).asInstanceOf[Sym[Int]]
    lazy val rV: (Sym[V], Sym[V]) = copyOrElse(_.rV)(orV)
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    lazy val allocVal: Sym[C] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[C])).asInstanceOf[Sym[C]]
    lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val eV: Sym[V] = copyTransformedOrElse(_.eV)(fresh[V]).asInstanceOf[Sym[V]]

    def alloc(i: Exp[Int]) = dc_alloc[V,C](allocVal, i)
    def finalize(av: Exp[C]): Exp[C] = av

    lazy val body: Def[C] = copyBodyOrElse(DeliteHashReduceElem[K,V,C,C](
      keyFunc = this.keyFunc,
      valFunc = this.valFunc,
      cond = this.cond,
      zero = this.zero,
      rV = this.rV,
      rFunc = this.rFunc,
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
        append = reifyEffects(dc_append(allocVal,ov,eV)),
        setSize = reifyEffects(dc_set_logical_size(allocVal,sV)),
        allocRaw = unusedBlock, //reifyEffects(dc_alloc[V,I](allocVal,sV)),
        copyRaw = unusedBlock, //reifyEffects(dc_copy(aV2,iV,allocVal,iV2,sV)),
        finalizer = reifyEffects(this.finalize(allocVal))
      ),
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleGroupByReduce(" + size + ")(" + v + ", (" + rV._1 + ", " + rV._2 + ") => " + body + ")"
    val mK = manifest[K]
    val mV = manifest[V]
    val mC = manifest[C]
  }
  object SimpleGroupByReduce {
    def mirror[K:Manifest,V:Manifest,C<:DeliteCollection[V]:Manifest](op: SimpleGroupByReduce[K,V,C], f: Transformer)(implicit pos: SourceContext): SimpleGroupByReduce[K,V,C] = op match {
      case SimpleGroupByReduce(v,rv,s,k,b,r,z,c) => new { override val original = Some(f,op) } with SimpleGroupByReduce(v,rv,s,k,b,r,z,c)(op.mK,op.mV,op.mC,pos)
    }
    def unerase[K:Manifest,V:Manifest,C<:DeliteCollection[V]:Manifest](op: SimpleGroupByReduce[_,_,_]): SimpleGroupByReduce[K,V,C] = op.asInstanceOf[SimpleGroupByReduce[K,V,C]]
  }


  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e: SimpleForeach =>
      reflectPure(SimpleForeach.mirror(e,f)(pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleForeach, u, es) =>
      reflectMirrored(Reflect(SimpleForeach.mirror(e,f)(pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleReduce[_] =>
      val op = SimpleReduce.unerase(e)(e.mA)
      reflectPure( SimpleReduce.mirror(op,f)(e.mA,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleReduce[_], u, es) =>
      val op = SimpleReduce.unerase(e)(e.mA)
      reflectMirrored(Reflect(SimpleReduce.mirror(op,f)(e.mA,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleMap[_,_] =>
      val op = SimpleMap.unerase(e)(e.mR,e.mC)
      reflectPure(SimpleMap.mirror(op,f)(e.mR,e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleMap[_,_], u, es) =>
      val op = SimpleMap.unerase(e)(e.mR,e.mC)
      reflectMirrored(Reflect(SimpleMap.mirror(op,f)(e.mR,e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleFlatMap[_,_] =>
      val op = SimpleFlatMap.unerase(e)(e.mR,e.mC)
      reflectPure(SimpleFlatMap.mirror(op,f)(e.mR,e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleFlatMap[_,_], u, es) =>
      val op = SimpleFlatMap.unerase(e)(e.mR,e.mC)
      reflectMirrored(Reflect(SimpleFlatMap.mirror(op,f)(e.mR,e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleRead[_] =>
      val op = SimpleRead.unerase(e)(e.mC)
      reflectPure(SimpleRead.mirror(op,f)(e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleRead[_], u, es) =>
      val op = SimpleRead.unerase(e)(e.mC)
      reflectMirrored(Reflect(SimpleRead.mirror(op,f)(e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleGroupBy[_,_,_,_] =>
      val op = SimpleGroupBy.unerase(e)(e.mK,e.mV,e.mC,e.mH)
      reflectPure(SimpleGroupBy.mirror(op,f)(e.mK,e.mV,e.mC,e.mH,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleGroupBy[_,_,_,_], u, es) =>
      val op = SimpleGroupBy.unerase(e)(e.mK,e.mV,e.mC,e.mH)
      reflectMirrored(Reflect(SimpleGroupBy.mirror(op,f)(e.mK,e.mV,e.mC,e.mH,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleGroupByReduce[_,_,_] =>
      val op = SimpleGroupByReduce.unerase(e)(e.mK,e.mV,e.mC)
      reflectPure(SimpleGroupByReduce.mirror(op,f)(e.mK,e.mV,e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleGroupByReduce[_,_,_], u, es) =>
      val op = SimpleGroupByReduce.unerase(e)(e.mK,e.mV,e.mC)
      reflectMirrored(Reflect(SimpleGroupByReduce.mirror(op,f)(e.mK,e.mV,e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }

  // Override to block view of blocks in Product iterator
  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: SimpleForeach => blocks(op.body)
    case op: SimpleReduce[_] => blocks(op.body)
    case op: SimpleMap[_,_] => blocks(op.body)
    case op: SimpleFilter[_,_] => blocks(op.body)
    case op: SimpleFlatMap[_,_] => blocks(op.body)
    case op: SimpleRead[_] => blocks(op.body)
    case op: SimpleGroupBy[_,_,_,_] => blocks(op.body)
    case op: SimpleGroupByReduce[_,_,_] => blocks(op.body)
    case _ => super.blocks(e)
  }

}