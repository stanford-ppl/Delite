package ppl.delite.framework.ops

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

import ppl.delite.framework.datastructures._

// TODO: Rename this to something better
trait DeliteSimpleOpsExp extends DeliteOpsExp with DeliteArrayBufferOpsExp {
  // --- String split

  /**
   * Parallel, effectful foreach
   * @param size - the size of the collection/dimensions to loop over
   * @param func - the foreach function
   */
  case class ForeachImpl(oV: Sym[Int], loopSize: Exp[Int], func: Block[Unit])(implicit ctx: SourceContext) extends DeliteOpLoop[Unit] {
    type OpType <: ForeachImpl

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = this.func,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "ForeachImpl(" + size + ", iter: " + v + ", " + body.asInstanceOf[DeliteForeachElem[Unit]].func + ")"
  }
  object ForeachFactory {
    def apply(oV: Sym[Int], size: Exp[Int], func: Block[Unit])(implicit pos: SourceContext) = new ForeachImpl(oV, size, func)
    
    def mirror(op: ForeachImpl, f: Transformer)(implicit pos: SourceContext) = op match {
      case ForeachImpl(v,s,b) => new { override val original = Some(f,op) } with ForeachImpl(v,s,b)(pos)
    }
  }

  /**
   * Parallel reduction. Reducing function must be associative.
   * TODO: This one will need to be changed with Vera's fold/reduce distinction
   * TODO: What are mutable and stripFirst for here?
   * @param oV       - symbol for loop iterator
   * @param orV      - pair of symbols representing two elements (Sym[A], Sym[A])
   * @param loopSize - the size of the input collection
   * @param lookup   - the apply method for the collection being reduced
   * @param func     - the reduction function; (Block[A]) Must be associative.
   * @param zero     - accumulator initialization
   */
  case class ReduceImpl[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), loopSize: Exp[Int], lookup: Block[A], func: Block[A], zero: Block[A])(implicit ctx: SourceContext) extends DeliteOpLoop[A] {
    type OpType <: ReduceImpl[A]
    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)(orV) // TODO: transform vars?? -- what does this mean?
 
    val stripFirst = !isPrimitiveType(manifest[A])

    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    val body: Def[A] = copyBodyOrElse(DeliteReduceElem[A](
      func = this.lookup,
      cond = Nil,
      zero = this.zero,
      accInit = this.zero, //if (isPrimitiveType(manifest[A])) zero else this.accInit
      rV = this.rV,
      rFunc = this.func,
      stripFirst = this.stripFirst,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "ReduceImpl(" + size + ", iter: " + v + ", rV: (" + rV._1 + ", " + rV._2 + "), " + body.asInstanceOf[DeliteReduceElem[A]].rFunc + ")"

    val mA = manifest[A]
  }
  object ReduceFactory {
    def apply[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), size: Exp[Int], lookup: Block[A], func: Block[A], alloc: Block[A])(implicit ctx: SourceContext) = new ReduceImpl[A](oV,orV,size,lookup,func,alloc)
    def apply[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), size: Exp[Int], lookup: Block[A], func: Block[A], zero: Exp[A])(implicit ctx: SourceContext) = new ReduceImpl[A](oV,orV,size,lookup,func,reifyEffects(zero))

    def mirror[A:Manifest](op: ReduceImpl[A], f: Transformer)(implicit pos: SourceContext): ReduceImpl[A] = op match {
      case ReduceImpl(v,r,s,l,b,z) => new { override val original = Some(f, op) } with ReduceImpl(v,r,s,l,b,z)(op.mA, pos)
    }
    def unerase[A:Manifest](op: ReduceImpl[_]): ReduceImpl[A] = op.asInstanceOf[ReduceImpl[A]]
  }

  object BufferFactory {
    def apply[R:Manifest,I<:DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](v: Sym[Int], ballocVal: Sym[I], allocI: Exp[Int] => Exp[I], finalizeI: Exp[I] => Exp[C]) = {
      // bound vars
      val beV: Sym[R] = fresh[R] //copyTransformedOrElse(_.eV)(fresh[R]).asInstanceOf[Sym[R]]
      val bsV: Sym[Int] = fresh[Int] //copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
      val biV: Sym[Int] = fresh[Int] //copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
      val biV2: Sym[Int] = fresh[Int] //copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
      val baV2: Sym[I] = fresh[I] //copyTransformedOrElse(_.aV2)(fresh[I]).asInstanceOf[Sym[I]]

      DeliteBufferElem(
        eV = beV,
        sV = bsV,
        iV = biV,
        iV2 = biV2,
        allocVal = ballocVal,
        aV2 = baV2,
        alloc = reifyEffects(allocI(bsV)),
        apply = unusedBlock,
        update = reifyEffects(dc_update(ballocVal,v,beV)),
        append = reifyEffects(dc_append(ballocVal,v,beV)),
        appendable = reifyEffects(dc_appendable(ballocVal,v,beV)),
        setSize = reifyEffects(dc_set_logical_size(ballocVal,bsV)),
        allocRaw = reifyEffects(dc_alloc[R,I](ballocVal,bsV)),
        copyRaw = reifyEffects(dc_copy(baV2,biV,ballocVal,biV2,bsV)),
        finalizer = reifyEffects(finalizeI(ballocVal))
      )
    }
  }

  /**
   * Parallel collect (includes map, zipwith, mapindices)
   * @param oV        - symbol for loop iterator
   * @param loopSize  - size of loop (usually size of input collection)
   * @param func      - map/collect function  (anything that productes Exp[R])
   * @param alc       - allocation rule for intermediate type (Exp[Int] => Exp[I])
   * @param finalizer - finalizer method to create collection from intermediate
   * @param cond      - optional filter condition
   */
  case class CollectImpl[R:Manifest,I <: DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](oV: Sym[Int], loopSize: Exp[Int], func: Block[R], alloc: Exp[Int] => Exp[I], finalizer: Exp[I] => Exp[C], cond: Option[Block[Boolean]])(implicit ctx: SourceContext) extends DeliteOpLoop[C] {
    type OpType <: CollectImpl[R,I,C]
 
    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)
    def allocI(i: Exp[Int]) = if (cond.isEmpty) alloc(i) else alloc(size)
 
    val body: Def[C] = copyBodyOrElse {
      val allocVal: Sym[I] = reflectMutableSym(fresh[I]) //copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[I])).asInstanceOf[Sym[I]]

      DeliteCollectElem[R,I,C](
        func = this.func,
        cond = this.cond.toList,
        par = dc_parallelization(allocVal, cond.isEmpty),
        buf = BufferFactory[R,I,C](v, allocVal, allocI, finalizer),
        numDynamicChunks = this.numDynamicChunks
    )}

    override def toString = {
      val condLs = body.asInstanceOf[DeliteCollectElem[R,I,C]].cond
      val condStr = if (condLs.isEmpty) "" else  ", cond: " + condLs.mkString(",")
      "CollectImpl(" + size + ", iter: " + v + ", " + body.asInstanceOf[DeliteCollectElem[R,I,C]].func + condStr + ")"
    }
    val mR = manifest[R]
    val mI = manifest[I]
    val mC = manifest[C]
  }
  object CollectFactory {
    /**
     * @param size     - size of loop (input collection)
     * @param func     - map/collect function  (anything that productes Exp[R])
     * @param cond     - list of (filter) conditions, if any
     */
    def array[R:Manifest](oV: Sym[Int], size: Exp[Int], func: Block[R], cond: Option[Block[Boolean]] = None)(implicit ctx: SourceContext) = {
      val alloc = (z: Exp[Int]) => DeliteArray[R](z)
      selfBacked[R,DeliteArray[R]](oV,size,func,alloc,cond)
    }

    /**
     * @param size  - size of loop (input collection)
     * @param func  - map/collect function  (anything that productes Exp[R])
     * @param alloc - allocation method for collection
     * @param cond  - list of (filter) conditions, if any
     */
    private def selfBacked[R:Manifest,C<:DeliteCollection[R]:Manifest](oV: Sym[Int], size: Exp[Int], func: Block[R], alloc: (Exp[Int]) => Exp[C], cond: Option[Block[Boolean]] = None)(implicit ctx: SourceContext) = {
      new CollectImpl[R,C,C](oV, size, func, alloc, {(x: Exp[C]) => x}, cond)
    }

    def mirror[R:Manifest,I<:DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](op: CollectImpl[R,I,C], f: Transformer)(implicit pos: SourceContext): CollectImpl[R,I,C] = op match {
      case CollectImpl(v,s,b,a,t,c) => new { override val original = Some(f,op) } with CollectImpl(v,s,b,a,t,c)(op.mR,op.mI,op.mC,pos)
    }

    def unerase[R:Manifest,I<:DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](op: CollectImpl[_,_,_]): CollectImpl[R,I,C] = op.asInstanceOf[CollectImpl[R,I,C]]
  }

  /**
   * Parallel flat map (more general form of collect)
   * @param oV        - symbol for loop iterator
   * @param loopSize  - size of loop (usually size of input collection)
   * @param func      - flatmap function - produces Exp[DeliteCollection[R]]
   * @param alloc     - allocation rule for intermediate type (Exp[Int] => Exp[I])
   * @param finalizer - finalizer method to create collection from intermediate
   */
  case class FlatMapImpl[R:Manifest,I<:DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](oV: Sym[Int], loopSize: Exp[Int], func: Block[DeliteCollection[R]], alloc: Exp[Int] => Exp[I], finalizer: Exp[I] => Exp[C])(implicit ctx: SourceContext) extends DeliteOpLoop[C] {
    type OpType <: FlatMapImpl[R,I,C]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)
    def allocI(i: Exp[Int]) = alloc(i)

    val body: Def[C] = copyBodyOrElse {
      // bound vars
      val biF: Sym[Int] = fresh[Int] // copyTransformedOrElse(_.iF)(fresh[Int]).asInstanceOf[Sym[Int]]
      val beF: Sym[DeliteCollection[R]] = fresh[DeliteCollection[R]](func.tp) //copyTransformedOrElse(_.eF)().asInstanceOf[Sym[DeliteCollection[R]]]
      val allocVal: Sym[I] = reflectMutableSym(fresh[I]) //copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[I])).asInstanceOf[Sym[I]]

      DeliteCollectElem[R,I,C](
        iFunc = Some(this.func),
        iF = Some(biF),
        sF = Some(reifyEffects(dc_size(beF))), //note: applying dc_size directly to iFunc can lead to iFunc being duplicated during mirroring
        eF = Some(beF),
        func = reifyEffects(dc_apply(beF,biF)),
        par = dc_parallelization(allocVal, true),
        buf = BufferFactory[R,I,C](v, allocVal, allocI, finalizer),
        numDynamicChunks = this.numDynamicChunks
    )}

    val mR = manifest[R]
    val mI = manifest[I]
    val mC = manifest[C]    
  }
  object FlatMapFactory {
    /**
     * @param in   - input data for loop
     * @param size - size of loop (input collection)
     * @param func - flatmap function
     */
    def array[R:Manifest](oV: Sym[Int], size: Exp[Int], func: Block[DeliteArray[R]])(implicit ctx: SourceContext) = {
      val alloc = (z: Exp[Int]) => DeliteArray[R](z)
      selfBacked[R,DeliteArray[R]](oV,size,func,alloc)
    }
    private def selfBacked[R:Manifest,C<:DeliteCollection[R]:Manifest](oV: Sym[Int], size: Exp[Int], func: Block[DeliteCollection[R]], alloc: (Exp[Int]) => Exp[C])(implicit ctx: SourceContext) = {
      new FlatMapImpl[R,C,C](oV,size,func,alloc,{(x: Exp[C]) => x})
    }

    def mirror[R:Manifest,I<:DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](op: FlatMapImpl[R,I,C], f: Transformer)(implicit pos: SourceContext): FlatMapImpl[R,I,C] = op match {
      case FlatMapImpl(v,s,b,a,t) => new {override val original = Some(f,op)} with FlatMapImpl(v,s,b,a,t)(op.mR,op.mI,op.mC,pos)
    }

    def unerase[R:Manifest,I<:DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](op: FlatMapImpl[_,_,_]): FlatMapImpl[R,I,C] = op.asInstanceOf[FlatMapImpl[R,I,C]]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e: ForeachImpl => 
      reflectPure(ForeachFactory.mirror(e,f)(pos))(mtype(manifest[A]), pos)
    case Reflect(e: ForeachImpl, u, es) => 
      reflectMirrored(Reflect(ForeachFactory.mirror(e,f)(pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: ReduceImpl[_] => 
      val op = ReduceFactory.unerase(e)(e.mA)
      reflectPure( ReduceFactory.mirror(op,f)(e.mA,pos))(mtype(manifest[A]), pos)
    case Reflect(e: ReduceImpl[_], u, es) => 
      val op = ReduceFactory.unerase(e)(e.mA)
      reflectMirrored(Reflect(ReduceFactory.mirror(op,f)(e.mA,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: CollectImpl[_,_,_] => 
      val op = CollectFactory.unerase(e)(e.mR,e.mI,e.mC)
      reflectPure(CollectFactory.mirror(op,f)(e.mR,e.mI,e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: CollectImpl[_,_,_], u, es) => 
      val op = CollectFactory.unerase(e)(e.mR,e.mI,e.mC)
      reflectMirrored(Reflect(CollectFactory.mirror(op,f)(e.mR,e.mI,e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: FlatMapImpl[_,_,_] => 
      val op = FlatMapFactory.unerase(e)(e.mR,e.mI,e.mC)
      reflectPure(FlatMapFactory.mirror(op,f)(e.mR,e.mI,e.mC,pos))(mtype(manifest[A]), pos) 
    case Reflect(e: FlatMapImpl[_,_,_], u, es) => 
      val op = FlatMapFactory.unerase(e)(e.mR,e.mI,e.mC)
      reflectMirrored(Reflect(FlatMapFactory.mirror(op,f)(e.mR,e.mI,e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  // This isn't in Loops.scala in LMS, should it be?
  // Causes IR dump to include ALL blocks in Elems, including buffer append details
  // Probably not necessary..
  /*override def blocks(e: Any): List[Block[Any]] = e match {
    case e: AbstractLoop[_] => blocks(e.body)
    case _ => super.blocks(e)
  }*/
}
