package ppl.delite.framework.ops

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

import ppl.delite.framework.datastructures._

trait DeliteSimpleOps extends RangeVectorOps with Base {
  def reduce[A:Manifest](size: Rep[Int], zero: Rep[A])(mFunc: Rep[Int] => Rep[A])(rFunc: (Rep[A], Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A] 
  def filterReduce[A:Manifest](size: Rep[Int], zero: Rep[A])(fFunc: Rep[Int] => Rep[Boolean])(mFunc: Rep[Int] => Rep[A])(rFunc: (Rep[A], Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A]
}

trait DeliteSimpleOpsExp extends DeliteSimpleOps with RangeVectorOpsExp with DeliteOpsExpIR with DeliteCollectionOpsExp with DeliteArrayOpsExp { 
  this: DeliteOpsExp with DeliteFileReaderOpsExp => 
  
  /**
   * Parallel, effectful foreach
   * @param size - the size of the collection/dimensions to loop over
   * @param func - the foreach function
   */
  case class SimpleForeach(oV: Sym[Int], loopSize: Exp[Int], func: Block[Unit])(implicit ctx: SourceContext) extends DeliteOpLoop[Unit] {
    type OpType <: SimpleForeach

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = this.func,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleForeach(" + size + ", iter: " + v + ", " + body.asInstanceOf[DeliteForeachElem[Unit]].func + ")"
  }
  object ForeachFactory {
    def apply(oV: Sym[Int], size: Exp[Int], func: Block[Unit])(implicit pos: SourceContext) = new SimpleForeach(oV, size, func)
    
    def mirror(op: SimpleForeach, f: Transformer)(implicit pos: SourceContext) = op match {
      case SimpleForeach(v,s,b) => new { override val original = Some(f,op) } with SimpleForeach(v,f(s),b)(pos)
    }
  }

  /**  
   * (Copied from DeliteOps.scala)
   * NOTE ABOUT ZERO:
   *   The supplied zero parameter is required to have value equality *in the generated code*
   *   it will not be used unless the collection is empty or in a conditional reduce where the
   *   first (or more) conditions fail. In both cases, we never try to actually reduce a zero
   *   element - we only return it or use it as an initialization check.
   *
   *   if stripFirst is set to false, i.e. for a mutable reduction, then the accInit value is used
   *   to allocate the accumulator, and it IS used in the initial reduction.
   */

  /**
   * Parallel fold. Reducing function must be associative.
   * FIXME: Not actually a true fold right now!! Do not use yet!
   * TODO: This will need to be changed with Vera's fold/reduce distinction
   * TODO: Mutable reduce?
   * NOTE: Makes no distinction between zero value and accumulator initialization value (both are 'zero')
   * This means, for example, that product( DeliteArray[Double] ) will return 1 for an empty array
   * @param oV       - symbol for loop iterator
   * @param orV      - pair of symbols representing two elements (Sym[A], Sym[A])
   * @param loopSize - the size of the input collection
   * @param lookup   - the apply method for the collection being reduced
   * @param func     - the reduction function; (Block[A]) Must be associative.
   * @param zero     - accumulator initialization
   * @param filter   - optional filter function
   */
  case class SimpleFold[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), loopSize: Exp[Int], lookup: Block[A], func: Block[A], zero: Block[A], filter: Option[Block[Boolean]])(implicit ctx: SourceContext) extends DeliteOpLoop[A] {
    type OpType <: SimpleFold[A]
    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)(orV) // TODO: transform vars?? -- what does this mean?

    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)
    //val mutable = filter.isDefined

    // If stripFirst is false, accInit is used to allocate the accumulator, and is used in the reduction
    // Zero is only used if the collection is empty or if the first iteration of the filter function is false
    lazy val body: Def[A] = copyBodyOrElse(DeliteReduceElem[A](
      func = this.lookup,
      cond = filter.toList,
      zero = this.zero,
      accInit = this.zero,
      rV = this.rV,
      rFunc = this.func,
      stripFirst = false, //&& !this.mutable, 
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleFold(" + size + ", iter: " + v + ", rV: (" + rV._1 + ", " + rV._2 + "), " + body.asInstanceOf[DeliteReduceElem[A]].rFunc + ")"
    val mA = manifest[A]
  }
  object FoldFactory {
    def apply[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), size: Exp[Int], lookup: Block[A], func: Block[A], zero: Exp[A], filter: Option[Block[Boolean]] = None)(implicit ctx: SourceContext) 
      = new SimpleFold[A](oV,orV,size,lookup,func,reifyEffects(zero),filter)

    def mirror[A:Manifest](op: SimpleFold[A], f: Transformer)(implicit pos: SourceContext): SimpleFold[A] = op match {
      case SimpleFold(v,r,s,l,b,z,c) => new { override val original = Some(f, op) } with SimpleFold(v,r,s,l,b,z,c)(op.mA, pos)
    }
    def unerase[A:Manifest](op: SimpleFold[_]): SimpleFold[A] = op.asInstanceOf[SimpleFold[A]]
  }

  /**
   * Parallel reduction. Reducing function must be associative.
   * TODO: This will need to be changed with Vera's fold/reduce distinction
   * @param oV       - symbol for loop iterator
   * @param orV      - pair of symbols representing two elements (Sym[A], Sym[A])
   * @param loopSize - the size of the input collection
   * @param lookup   - the apply method for the collection being reduced
   * @param func     - the reduction function; (Block[A]) Must be associative.
   * @param zero     - zero value (not actually used to compute output result)
   * @param filter   - optional filter function
   */
  case class SimpleReduce[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), loopSize: Exp[Int], mFunc: Block[A], rFunc: Block[A], zero: Block[A], fFunc: List[Block[Boolean]])(implicit ctx: SourceContext) extends DeliteOpLoop[A] {
    type OpType <: SimpleReduce[A]
    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)(orV) // TODO: transform vars?? -- what does this mean?

    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    // If stripFirst is false, accInit is used to allocate the accumulator, and is used in the reduction
    // Zero is only used if the collection is empty or if the first iteration of the filter function is false
    lazy val body: Def[A] = copyBodyOrElse(DeliteReduceElem[A](
      func = this.mFunc,
      cond = this.fFunc,
      zero = this.zero,
      accInit = this.zero,
      rV = this.rV,
      rFunc = this.rFunc,
      stripFirst = true, 
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleReduce(" + size + ", iter: " + v + ", rV: (" + rV._1 + ", " + rV._2 + "), " + body.asInstanceOf[DeliteReduceElem[A]].rFunc + ")"
    val mA = manifest[A]
  }
  object ReduceFactory {
    def regen[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), size: Exp[Int], mFunc: Block[A], rFunc: Block[A], zero: Block[A], fFunc: List[Block[Boolean]] = Nil)(implicit ctx: SourceContext) 
      = new SimpleReduce[A](oV,orV,size,mFunc,rFunc,zero,fFunc)

    def apply[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), size: Exp[Int], mFunc: Block[A], rFunc: Block[A], zero: Exp[A], fFunc: Option[Block[Boolean]] = None)(implicit ctx: SourceContext) 
      = new SimpleReduce[A](oV,orV,size,mFunc,rFunc,reifyEffects(zero),fFunc.toList)

    def mirror[A:Manifest](op: SimpleReduce[A], f: Transformer)(implicit pos: SourceContext): SimpleReduce[A] = op match {
      case SimpleReduce(v,r,s,l,b,z,c) => new { override val original = Some(f,op) } with SimpleReduce(v,r,s,l,b,z,c)(op.mA, pos)
    }
    def unerase[A:Manifest](op: SimpleReduce[_]): SimpleReduce[A] = op.asInstanceOf[SimpleReduce[A]]
  }

  def reduce[A:Manifest](size: Exp[Int], zero: Exp[A])(mFunc: Exp[Int] => Exp[A])(rFunc: (Exp[A], Exp[A]) => Exp[A])(implicit ctx: SourceContext): Exp[A] = {
    val v = fresh[Int]
    val rV = (fresh[A], fresh[A])
    reflectPure( ReduceFactory(v, rV, size, reifyEffects(mFunc(v)), reifyEffects(rFunc(rV._1,rV._2)), zero) )
  }
  def filterReduce[A:Manifest](size: Exp[Int], zero: Exp[A])(fFunc: Exp[Int] => Exp[Boolean])(mFunc: Exp[Int] => Exp[A])(rFunc: (Exp[A], Exp[A]) => Exp[A])(implicit ctx: SourceContext): Exp[A] = {
    val v = fresh[Int]
    val rV = (fresh[A], fresh[A])
    reflectPure( ReduceFactory(v, rV, size, reifyEffects(mFunc(v)), reifyEffects(rFunc(rV._1,rV._2)), zero, Some(reifyEffects(fFunc(v)))) )
  }


  abstract class SimpleMapLike[R:Manifest,C<:DeliteCollection[R]:Manifest] extends DeliteOpLoop[C] {
    type OpType <: SimpleMapLike[R,C]

    lazy val eV: Sym[R] = copyTransformedOrElse(_.eV)(fresh[R]).asInstanceOf[Sym[R]]
    lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val iV2: Sym[Int] = copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val aV2: Sym[C] = copyTransformedOrElse(_.aV2)(fresh[C]).asInstanceOf[Sym[C]]
    lazy val allocVal: Sym[C] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[C])).asInstanceOf[Sym[C]]

    def allocI(i: Exp[Int]): Exp[C]
    def finalizeI(av: Exp[C]): Exp[C] = av

    lazy val buf = DeliteBufferElem(
      eV = this.eV,     // current update position?
      sV = this.sV,     // intermediate size?
      iV = this.iV,     // copy destination start?
      iV2 = this.iV2,   // copy source start?
      allocVal = this.allocVal,
      aV2 = this.aV2,
      alloc = reifyEffects(this.allocI(this.sV)),
      apply = unusedBlock,
      update = reifyEffects(dc_update(this.allocVal,v,this.eV)),
      append = reifyEffects(dc_append(this.allocVal,v,this.eV)),
      appendable = reifyEffects(dc_appendable(this.allocVal,v,this.eV)),
      setSize = reifyEffects(dc_set_logical_size(this.allocVal,this.sV)),
      allocRaw = reifyEffects(dc_alloc[R,C](this.allocVal,this.sV)),
      copyRaw = reifyEffects(dc_copy(this.aV2,this.iV,this.allocVal,this.iV2,this.sV)),
      finalizer = reifyEffects(this.finalizeI(this.allocVal))
    )
  }

  /**
   * Parallel collect (includes map, zipWith, mapIndices)
   * @param oV        - symbol for loop iterator
   * @param loopSize  - size of loop (usually size of input collection)
   * @param func      - map/collect function  (anything that productes Exp[R])
   * @param alloc     - allocation rule for output type (Exp[Int] => Exp[C])
   * @param cond      - optional filter condition
   */
  case class SimpleCollect[R:Manifest,C<:DeliteCollection[R]:Manifest](oV: Sym[Int], loopSize: Exp[Int], func: Block[R], alloc: Exp[Int] => Exp[C], cond: Option[Block[Boolean]])(implicit ctx: SourceContext) extends SimpleMapLike[R,C] {
    type OpType <: SimpleCollect[R,C]
 
    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    def allocI(i: Exp[Int]) = alloc(i)

    lazy val body: Def[C] = copyBodyOrElse(DeliteCollectElem[R,C,C](
      func = this.func,
      cond = this.cond.toList,
      par = dc_parallelization(allocVal, !cond.isEmpty),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = {
      //val condLs = body.asInstanceOf[DeliteCollectElem[R,C,C]].cond
      //val condStr = if (condLs.isEmpty) "" else  ", cond: " + condLs.mkString(",")
      "SimpleCollect(" + size + ", iter: " + v + ", " + body + ")"
    }
    val mR = manifest[R]
    val mC = manifest[C]
  }
  object CollectFactory {
    def array[R:Manifest](oV: Sym[Int], size: Exp[Int], func: Block[R], cond: Option[Block[Boolean]] = None)(implicit ctx: SourceContext) = {
      val alloc = (z: Exp[Int]) => DeliteArray[R](z)
      new SimpleCollect[R,DeliteArray[R]](oV, size, func, alloc, cond)
    }

    def mirror[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleCollect[R,C], f: Transformer)(implicit pos: SourceContext): SimpleCollect[R,C] = op match {
      case SimpleCollect(v,s,b,a,c) => new { override val original = Some(f,op) } with SimpleCollect(v,s,b,a,c)(op.mR,op.mC,pos)
    }

    def unerase[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleCollect[_,_]): SimpleCollect[R,C] = op.asInstanceOf[SimpleCollect[R,C]]
  }

  /**
   * Parallel flat map (more general form of collect)
   * @param oV        - symbol for loop iterator
   * @param loopSize  - size of loop (usually size of input collection)
   * @param func      - flatmap function - produces Exp[DeliteCollection[R]]
   * @param alloc     - allocation rule for output type (Exp[Int] => Exp[C])
  */
  case class SimpleFlatMap[R:Manifest,C<:DeliteCollection[R]:Manifest](oV: Sym[Int], loopSize: Exp[Int], func: Block[DeliteCollection[R]], alloc: Exp[Int] => Exp[C])(implicit ctx: SourceContext) extends SimpleMapLike[R,C] {
    type OpType <: SimpleFlatMap[R,C]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)
    
    def allocI(i: Exp[Int]) = alloc(i)

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

    override def toString = "SimpleFlatMap(" + size + ", iter: " + v + ", " + body + ")" 
    val mR = manifest[R]
    val mC = manifest[C]
  }
  object FlatMapFactory {
    def array[R:Manifest](oV: Sym[Int], size: Exp[Int], func: Block[DeliteCollection[R]])(implicit ctx: SourceContext) = {
      val alloc = (z: Exp[Int]) => DeliteArray[R](z)
      new SimpleFlatMap[R,DeliteArray[R]](oV,size,func,alloc)
    }
    def mirror[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleFlatMap[R,C], f: Transformer)(implicit pos: SourceContext): SimpleFlatMap[R,C] = op match {
      case SimpleFlatMap(v,s,b,a) => new {override val original = Some(f,op)} with SimpleFlatMap(v,s,b,a)(op.mR,op.mC,pos)
    }
    def unerase[R:Manifest,C<:DeliteCollection[R]:Manifest](op: SimpleFlatMap[_,_]): SimpleFlatMap[R,C] = op.asInstanceOf[SimpleFlatMap[R,C]]
  }

  /**
   * Extremely simple parallel file read - no string splitting, just returns collection of lines
   * @param oV    - symbol for loop iterator
   * @param paths - list of file paths
   * @param alloc - allocation rule for output type (Exp[Int] => Exp[C])
  */
  case class SimpleRead[C<:DeliteCollection[String]:Manifest](oV: Sym[Int], paths: Seq[Exp[String]], alloc: Exp[Int] => Exp[C])(implicit ctx: SourceContext) extends SimpleMapLike[String,C] {
    type OpType <: SimpleRead[C]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    
    val inputStream = dfis_new(paths, unit(null))
    val size: Exp[Int] = copyTransformedOrElse(_.size)(dfis_size(inputStream).asInstanceOf[Exp[Int]])

    def allocI(i: Exp[Int]) = alloc(i)

    lazy val body: Def[C] = copyBodyOrElse(DeliteCollectElem[String,C,C](
      func = reifyEffects(dfis_readLine(inputStream, v)),
      par = dc_parallelization(allocVal, true),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))
    val mC = manifest[C]
  }

  object SimpleReadFactory {
    def array(oV: Sym[Int], paths: Seq[Exp[String]])(implicit ctx: SourceContext) = {
      val alloc = (z: Exp[Int]) => DeliteArray[String](z)
      new SimpleRead[DeliteArray[String]](oV, paths, alloc)
    }
    def mirror[C<:DeliteCollection[String]:Manifest](op: SimpleRead[C], f: Transformer)(implicit ctx: SourceContext): SimpleRead[C] = op match {
      case SimpleRead(v,p,a) => new {override val original = Some(f,op)} with SimpleRead(v,p,a)(op.mC,ctx)
    }
    def unerase[C<:DeliteCollection[String]:Manifest](op: SimpleRead[_]): SimpleRead[C] = op.asInstanceOf[SimpleRead[C]]
  }

 /**
   * Simplified groupByReduce
   * TODO: Not complete
   * @param oV        - symbol for loop iterator
   * @param loopSize  - size of loop (usually size of input collection)
   * @param keyFunc   - function from collection elements to keys (hashing function)
   * @param valFunc   - function from collection elements to hasmap values
   * @param alloc     - allocation rule for bins
  */
  /*case class SimpleGroupByReduce[K:Manifest,V:Manifest,H:DeliteCollection[V]:Manifest](oV: Sym[Int], loopSize: Exp[Int], keyFunc: Block[K], valFunc: Block[V], alloc: Exp[Int] => Exp[H])(implicit ctx: SourceContext) extends DeliteOpLoop[H] {
    type OpType <: SimpleGroupByReduce[K,V,H]

    final lazy val rV: (Sym[V],Sym[V]) = copyOrElse(_.rV)((fresh[V], fresh[V]))
    final lazy val allocVal: Sym[I] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[I])).asInstanceOf[Sym[I]]
    final lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val eV: Sym[V] = copyTransformedOrElse(_.eV)(fresh[V]).asInstanceOf[Sym[V]]
    def alloc(i: Exp[Int]): Exp[H]

  }*/

  /**
   * Simplified groupBy
   * TODO: Not debugged yet
   * @param oV        - symbol for loop iterator
   * @param loopSize  - size of loop (usually size of input collection)
   * @param keyFunc   - function from collection elements to keys (hashing function)
   * @param valFunc   - function from collection elements to hasmap values
   * @param allocC    - allocation rule for bins
   * @param allocH    - allocation rule for map
  */
  /*case class SimpleGroupBy[K:Manifest,V:Manifest,C<:DeliteCollection[V]:Manifest,H<:DeliteCollection[C]:Manifest](oV: Sym[Int], loopSize: Exp[Int], keyFunc: Block[K], valFunc: Block[V], allocC: Exp[Int] => Exp[C], allocH: Exp[Int] => Exp[H])(implicit ctx: SourceContext) extends DeliteOpLoop[H] {
    type OpType <: SimpleGroupBy[K,V,C,H]
    
    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    final lazy val allocVal: Sym[C] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[C])).asInstanceOf[Sym[C]]
    final lazy val aV2: Sym[C] = copyTransformedOrElse(_.aV2)(fresh[C]).asInstanceOf[Sym[C]]
    final lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val iV2: Sym[Int] = copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val iiV: Sym[Int] = copyTransformedOrElse(_.iiV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val iiV2: Sym[Int] = copyTransformedOrElse(_.iiV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val eV: Sym[V] = copyTransformedOrElse(_.eV)(fresh[V]).asInstanceOf[Sym[V]]

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
      keyFunc = this.keyFunc
      valFunc = this.valFunc
      cond = Nil
      iBufSize = reifyEffects(dc_size(ibufVal2)),
      iBuf = DeliteBufferElem(
        eV = this.eV,
        sV = this.sV,
        iV = this.iiV,
        iV2 = this.iiV2,
        allocVal = if (Config.soaEnabled) unusedSym else this.ibufVal.asInstanceOf[Sym[C]],
        aV2 = if (Config.soaEnabled) unusedSym else this.ibufVal2.asInstanceOf[Sym[C]],
        alloc = reifyEffects(this.alloc(sV)),
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

    val dmK = manifest[K]
    val dmV = manifest[V]
    val dmC = manifest[C]
    val dmH = manifest[H]
  }

  object GroupByFactory {
    def hashmap[K:Manifest,V:Manifest](oV: Sym[Int], size: Exp[Int], keyFunc: Block[K], valFunc: Block[V])(implicit ctx: SourceContext) = {
      val alloc = (z: Exp[Int]) => DeliteArray[V](z)
      new SimpleGroupBy[K,V,DeliteArray[V],DeliteMap[K,DeliteArray[V]]](oV,size,keyFunc,valFunc,alloc)
    }
    def mirror[K:Manifest,V:Manifest,C<:DeliteCollection[V]:Manifest,H<:DeliteCollection[C]:Manifest](op: SimpleGroupBy[K,V,C,H], f: Transformer)(implicit pos: SourceContext): SimpleGroupBy[K,V,C,H] = op match {
      case SimpleGroupBy(v,s,k,vv,a) => new {override val original = Some(f,op)} with SimpleGroupBy(v,s,k,vv,a)(op.mK,op.mV,op.mC,op.mH,pos)
    }
    def unerase[K:Manifest,V:Manifest,C<:DeliteCollection[V]:Manifest,H<:DeliteCollection[C]:Manifest](op: SimpleGroupBy[_,_,_,_]): SimpleGroupBy[K,V,C,H] = op.asInstanceOf[SimpleGroupBy[K,V,C,H]]
  }*/

  /**
   * Block Assemble (tiling parallel ops)
   * Currently roughly equivalent to a GroupByReduce-FlatMap
   *
   * TODO: Can we use a slice/view rather than an explicit copy?
   *
   * @param oVs       - symbols for nested loop iterators
   * @param orV       - symbols used to reify reduction function (unused if reduction function is None)
   * @param lSizes    - sizes of nested loops
   * @param oSizes    - dimensions of output 
   * @param lStrides  - list of input domain blocking factors
   * @param kFunc     - list of key functions
   * @param fFunc     - list of filter functions
   * @param func      - main body function (produces a single tile)
   * @param rFunc     - optional reduction function for partial updates
   */
  case class SimpleTileAssemble[A:Manifest,C<:DeliteCollection[A]:Manifest](oVs: List[Sym[Int]], orV: (Sym[C],Sym[C]), lSizes: List[Exp[Int]], oSizes: List[Exp[Int]], lStrides: List[Exp[Int]], kFunc: List[Block[RangeVector]], fFunc: List[Block[Boolean]], func: Block[C], rFunc: Option[Block[C]])(implicit ctx: SourceContext) extends DeliteOpLoopNest[C] {
    type OpType <: SimpleTileAssemble[A,C]

    val vs: List[Sym[Int]] = copyOrElse(_.vs)(oVs)
    lazy val rV: (Sym[C],Sym[C]) = copyOrElse(_.rV)(orV)

    val sizes: List[Exp[Int]] = copyOrElse(_.sizes)(lSizes)
    val strides: List[Exp[Int]] = copyOrElse(_.strides)(lStrides)

    //val t = ???           // Tile rank
    val n = oSizes.length   // Output rank
    val nestLayers = lSizes.length  // Input domain rank

    // --- bound vars
    final lazy val bV: List[Sym[Int]] = copyOrElse(_.bV)(List.fill(n){fresh[Int].asInstanceOf[Sym[Int]]})
    final lazy val tV: List[Sym[Int]] = copyOrElse(_.tV)(List.fill(n){fresh[Int].asInstanceOf[Sym[Int]]})
    final lazy val bD: List[Exp[Int]] = copyOrElse(_.bD)(oSizes)  // Dimensions of output buffer
    final lazy val tD: List[Sym[Int]] = copyOrElse(_.tD)(List.fill(n){fresh[Int].asInstanceOf[Sym[Int]]})
    final lazy val buffVal: Sym[C] = copyTransformedOrElse(_.buffVal)(reflectMutableSym(fresh[C])).asInstanceOf[Sym[C]]
    final lazy val tileVal: Sym[C] = copyTransformedOrElse(_.tileVal)(fresh[C]).asInstanceOf[Sym[C]]
    final lazy val partVal: Sym[C] = copyTransformedOrElse(_.partVal)(reflectMutableSym(fresh[C])).asInstanceOf[Sym[C]]
    final lazy val bE: Sym[A] = copyTransformedOrElse(_.bE)(fresh[A]).asInstanceOf[Sym[A]]
    final lazy val tE: Sym[A] = copyTransformedOrElse(_.tE)(fresh[A]).asInstanceOf[Sym[A]]

    lazy val body: Def[C] = copyBodyOrElse(DeliteTileElem[A,C,C](
      keys = this.kFunc,
      cond = this.fFunc,
      tile = this.func,
      rV = this.rV,
      rFunc = this.rFunc,
      buf = DeliteTileBuffer[A,C,C](
        bV = this.bV,
        tV = this.tV,
        tD = this.tD,
        buffVal = this.buffVal,
        tileVal = this.tileVal,
        partVal = this.partVal,
        bE = this.bE,
        tE = this.tE,

        bApply = reifyEffects(dc_block_apply(buffVal, bV)),
        tApply = reifyEffects(dc_block_apply(tileVal, tV)),
        bUpdate = reifyEffects(dc_block_update(buffVal, bV, tE)),
        tUpdate = reifyEffects(dc_block_update(partVal, tV, bE)),
        allocBuff = reifyEffects(dc_alloc_block[A,C](buffVal, bD)),
        allocTile = reifyEffects(dc_alloc_block[A,C](partVal, strides))
      ),
      numDynamicChunks = this.numDynamicChunks
    ))
    val mA = manifest[A]
    val mC = manifest[C]
  }

  object SimpleTileFactory {
    def mirror[A:Manifest,C<:DeliteCollection[A]:Manifest](op: SimpleTileAssemble[A,C], f: Transformer)(implicit ctx: SourceContext): SimpleTileAssemble[A,C] = op match {
      case SimpleTileAssemble(v,rv,ls,os,st,kF,fF,g,rF) => 
        new {override val original = Some(f,op)} with SimpleTileAssemble[A,C](v,rv,ls,os,st,kF,fF,g,rF)(op.mA,op.mC,ctx)
    }
    def unerase[A:Manifest,C<:DeliteCollection[A]:Manifest](op: SimpleTileAssemble[_,_]): SimpleTileAssemble[A,C] = op.asInstanceOf[SimpleTileAssemble[A,C]]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e: SimpleForeach => 
      reflectPure(ForeachFactory.mirror(e,f)(pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleForeach, u, es) => 
      reflectMirrored(Reflect(ForeachFactory.mirror(e,f)(pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleFold[_] => 
      val op = FoldFactory.unerase(e)(e.mA)
      reflectPure( FoldFactory.mirror(op,f)(e.mA,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleFold[_], u, es) => 
      val op = FoldFactory.unerase(e)(e.mA)
      reflectMirrored(Reflect(FoldFactory.mirror(op,f)(e.mA,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleReduce[_] => 
      val op = ReduceFactory.unerase(e)(e.mA)
      reflectPure( ReduceFactory.mirror(op,f)(e.mA,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleReduce[_], u, es) => 
      val op = ReduceFactory.unerase(e)(e.mA)
      reflectMirrored(Reflect(ReduceFactory.mirror(op,f)(e.mA,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleCollect[_,_] => 
      val op = CollectFactory.unerase(e)(e.mR,e.mC)
      reflectPure(CollectFactory.mirror(op,f)(e.mR,e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleCollect[_,_], u, es) => 
      val op = CollectFactory.unerase(e)(e.mR,e.mC)
      reflectMirrored(Reflect(CollectFactory.mirror(op,f)(e.mR,e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleFlatMap[_,_] => 
      val op = FlatMapFactory.unerase(e)(e.mR,e.mC)
      reflectPure(FlatMapFactory.mirror(op,f)(e.mR,e.mC,pos))(mtype(manifest[A]), pos) 
    case Reflect(e: SimpleFlatMap[_,_], u, es) => 
      val op = FlatMapFactory.unerase(e)(e.mR,e.mC)
      reflectMirrored(Reflect(FlatMapFactory.mirror(op,f)(e.mR,e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleRead[_] =>
      val op = SimpleReadFactory.unerase(e)(e.mC)
      reflectPure(SimpleReadFactory.mirror(op,f)(e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleRead[_], u, es) =>
      val op = SimpleReadFactory.unerase(e)(e.mC)
      reflectMirrored(Reflect(SimpleReadFactory.mirror(op,f)(e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: SimpleTileAssemble[_,_] => 
      val op = SimpleTileFactory.unerase(e)(e.mA,e.mC)
      reflectPure(SimpleTileFactory.mirror(op,f)(e.mA,e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleTileAssemble[_,_], u, es) =>
      val op = SimpleTileFactory.unerase(e)(e.mA,e.mC)
      reflectMirrored(Reflect(SimpleTileFactory.mirror(op,f)(e.mA,e.mC,pos),  mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    /*case e: SimpleGroupBy[_,_,_,_] => 
      val op = GroupByFactory.unerase(e)(e.mK,e.mV,e.mC,e.mH)
      reflectPure(GroupByFactory.mirror(op,f)(e.mK,e.mV,e.mC,e.mH,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleGroupBy[_,_,_,_], u, es) => 
      val op = GroupByFactory.unerase(e)(e.mK,e.mV,e.mC,e.mH)
      reflectMirrored(Reflect(GroupByFactory.mirror(op,f)(e.mK,e.mV,e.mC,e.mH,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    */

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  // Override to block view of blocks in Product iterator
  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: SimpleFlatMap[_,_] => blocks(op.body.asInstanceOf[DeliteCollectElem[_,_,_]].func) // blocks(op.body)
    case op: SimpleCollect[_,_] => blocks(op.body.asInstanceOf[DeliteCollectElem[_,_,_]].func) // blocks(op.body)
    case op: SimpleFold[_] => blocks(op.body.asInstanceOf[DeliteReduceElem[_]].rFunc) // blocks(op.body)
    case op: SimpleForeach => blocks(op.body.asInstanceOf[DeliteForeachElem[_]].func) // blocks(op.body)
    case op: SimpleRead[_] => Nil
    case op: SimpleReduce[_] => blocks(op.body)
    //case op: SimpleGroupBy[_,_,_,_] => Nil
    //case op: DeliteOpLoop[_] => blocks(op.body)
    //case op: DeliteCollectElem[_,_,_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.buf) ::: op.iFunc.toList 
    case _ => super.blocks(e)
  }
}

// profiling
trait SimpleProfileOps extends Base {
  def tic(deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_start(unit("app"),deps)
  def tic(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_start(component,deps)
  def toc(deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_stop(unit("app"),deps)
  def toc(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_stop(component,deps)
  
  def profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
}

trait SimpleProfileOpsExp extends SimpleProfileOps with EffectExp {
  case class ProfileStart(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]
  case class ProfileStop(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]

  def profile_start(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ProfileStart(component, deps.toList))
  def profile_stop(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ProfileStop(component, deps.toList))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ProfileStart(c,deps), u, es) => reflectMirrored(Reflect(ProfileStart(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ProfileStop(c,deps), u, es) => reflectMirrored(Reflect(ProfileStop(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  
}

trait ScalaGenSimpleProfileOps extends ScalaGenEffect {
  val IR: SimpleProfileOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ProfileStart(c,deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(" + quote(c) + ")")
    case ProfileStop(c,deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.stop(" + quote(c) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenSimpleProfileOps extends CGenEffect {
  val IR: SimpleProfileOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ProfileStart(c,deps) => stream.println("DeliteCppTimerStart(resourceInfo->threadId," + quote(c) + ",);")
    case ProfileStop(c,deps) => stream.println("DeliteCppTimerStop(resourceInfo->threadId," + quote(c) + ");")
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenSimpleProfileOps
