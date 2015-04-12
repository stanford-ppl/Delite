package ppl.delite.framework.ops

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

import ppl.delite.framework.datastructures._

// TODO: Rename this to something better
trait DeliteThinOpsExp extends DeliteOpsExp with DeliteArrayBufferOpsExp {
  // --- String split

  /**
   * Parallel, effectful foreach
   * @param size - the size of the collection/dimensions to loop over
   * @param func - the foreach function
   */
  case class ForeachImpl(size: Exp[Int])(implicit ctx: SourceContext) extends DeliteOpLoop[Unit] {
    type OpType <: ForeachImpl
    
    val func: Option[Block[Unit]] = None

    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = this.func.get,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "ForeachImpl(" + size + ", iter: " + v + ", " + body.asInstanceOf[DeliteForeachElem[Unit]].func + ")"
  }
  object ForeachFactory {
    def apply(_size: Exp[Int], _func: Block[Unit])(implicit pos: SourceContext): ForeachImpl
      = new { override val func = Some(_func) } with ForeachImpl(_size)
    
    def mirror(op: ForeachImpl, f: Transformer)(implicit pos: SourceContext): ForeachImpl
      = new { override val original = Some(f,op) } with ForeachImpl(f(op.size))(pos)
  }

  /**
   * Parallel reduction. Reducing function must be associative.
   * TODO: This one will need to be changed with Vera's fold/reduce distinction
   * TODO: What are mutable and stripFirst for here?
   * @param size    - the size of the input collection
   * @param lookup  - the apply method for the collection being reduced
   * @param func    - the reduction function; (Block[A]) Must be associative.
   * @param zero    - accumulator initialization
   */
  case class ReduceImpl[A:Manifest](size: Exp[Int])(implicit ctx: SourceContext) extends DeliteOpLoop[A] {
    type OpType <: ReduceImpl[A]
    final lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)((if (mutable) reflectMutableSym(fresh[A]) else fresh[A], fresh[A])) // TODO: transform vars?? -- what does this mean?
    
    val mutable: Boolean = false
    val stripFirst = !isPrimitiveType(manifest[A]) && !this.mutable

    val lookup: Option[Block[A]] = None
    val func: Option[Block[A]] = None
    val zero: Option[Block[A]] = None

    lazy val body: Def[A] = copyBodyOrElse(DeliteReduceElem[A](
      func = this.lookup.get,
      cond = Nil,
      zero = this.zero.get,
      accInit = this.zero.get, //if (isPrimitiveType(manifest[A])) zero else this.accInit
      rV = this.rV,
      rFunc = this.func.get,
      stripFirst = this.stripFirst,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "ReduceImpl(" + size + ", iter: " + v + ", rV: (" + rV._1 + ", " + rV._2 + "), " + body.asInstanceOf[DeliteReduceElem[A]].rFunc + ")"

    val mA = manifest[A]
  }
  object ReduceFactory {
    def apply[A:Manifest](_size: Exp[Int], _lookup: Block[A], _func: Block[A], _alloc: Block[A])(implicit ctx: SourceContext) = new {
      override val lookup = Some(_lookup)
      override val func = Some(_func)
      override val zero = Some(_alloc)
    } with ReduceImpl[A](_size)

    def apply[A:Manifest](_size: Exp[Int], _lookup: Block[A], _func: Block[A], _zero: Exp[A])(implicit ctx: SourceContext) = new {
      override val lookup = Some(_lookup)
      override val func = Some(_func)
      override val zero = Some(reifyEffects(_zero))
    } with ReduceImpl[A](_size)

    def mirror[A:Manifest](op: ReduceImpl[A], f: Transformer)(implicit pos: SourceContext): ReduceImpl[A] = {
      new { override val original = Some(f, op) } with ReduceImpl(f(op.size))(op.mA, pos)
    }
    def unerase[A:Manifest](op: ReduceImpl[_]): ReduceImpl[A] = op.asInstanceOf[ReduceImpl[A]]
  }

  /**
   * Parallel collect (includes map, zipwith, mapindices)
   * Intermediate DeliteCollection must be a true collection (i.e. have all dc_* functions defined)
   * @param size      - size of loop (input collection)
   * @param func      - map/collect function  (anything that productes Exp[R])
   * @param alc       - allocation rule for intermediate type (Exp[Int] => Exp[I])
   * @param finalizer - finalizer method to create collection from intermediate
   * @param cond      - list of (filter) conditions, if any
   */
  case class CollectImpl[R:Manifest,I <: DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](size: Exp[Int])(implicit ctx: SourceContext) extends DeliteOpLoop[C] {
    type OpType <: CollectImpl[R,I,C]

    val func: Option[Block[R]] = None
    val alloc: Option[Exp[Int] => Exp[I]] = None
    val finalizer: Option[Exp[I] => Exp[C]] = None
    val cond: Option[Block[Boolean]] = None 
    def allocI(i: Exp[Int]) = if (cond.isEmpty) (alloc.get)(i) else (alloc.get)(size)
 
    lazy val body: Def[C] = copyBodyOrElse {
      // bound vars
      val _eV: Sym[R] = fresh[R] //copyTransformedOrElse(_.eV)(fresh[R]).asInstanceOf[Sym[R]]
      val _sV: Sym[Int] = fresh[Int] //copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
      val _allocVal: Sym[I] = reflectMutableSym(fresh[I]) //copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[I])).asInstanceOf[Sym[I]]
      val _iV: Sym[Int] = fresh[Int] //copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
      val _iV2: Sym[Int] = fresh[Int] //copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
      val _aV2: Sym[I] = fresh[I] //copyTransformedOrElse(_.aV2)(fresh[I]).asInstanceOf[Sym[I]]

      DeliteCollectElem[R,I,C](
        func = this.func.get,
        cond = this.cond.toList,
        par = dc_parallelization(_allocVal, cond.isEmpty),
        buf = DeliteBufferElem(
                eV = _eV,
                sV = _sV,
                iV = _iV,
                iV2 = _iV2,
                allocVal = _allocVal,
                aV2 = _aV2,
                alloc = reifyEffects(this.allocI(_sV)),
                apply = unusedBlock,
                update = reifyEffects(dc_update(_allocVal,v,_eV)),
                append = reifyEffects(dc_append(_allocVal,v,_eV)),
                appendable = reifyEffects(dc_appendable(_allocVal,v,_eV)),
                setSize = reifyEffects(dc_set_logical_size(_allocVal,_sV)),
                allocRaw = reifyEffects(dc_alloc[R,I](_allocVal,_sV)),
                copyRaw = reifyEffects(dc_copy(_aV2,_iV,_allocVal,_iV2,_sV)),
                finalizer = reifyEffects((this.finalizer.get)(_allocVal))
              ),
        numDynamicChunks = this.numDynamicChunks
    )}

    override def toString = "CollectImpl(" + size + ", iter: " + v + ", " + body.asInstanceOf[DeliteCollectElem[R,I,C]].func + ")"
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
    def array[R:Manifest](size: Exp[Int], func: Block[R], cond: Option[Block[Boolean]] = None)(implicit ctx: SourceContext) = {
      val alloc = (z: Exp[Int]) => DeliteArray[R](z)
      selfBacked[R,DeliteArray[R]](size,func,alloc,cond)
    }

    /**
     * @param size  - size of loop (input collection)
     * @param func  - map/collect function  (anything that productes Exp[R])
     * @param alloc - allocation method for collection
     * @param cond  - list of (filter) conditions, if any
     */
    private def selfBacked[R:Manifest,C<:DeliteCollection[R]:Manifest](_size: Exp[Int], _func: Block[R], _alloc: (Exp[Int]) => Exp[C], _cond: Option[Block[Boolean]] = None)(implicit ctx: SourceContext) = new {
      override val func = Some(_func)
      override val alloc = Some(_alloc)
      override val finalizer = Some({(x: Exp[C]) => x})
      override val cond = _cond
    } with CollectImpl[R,C,C](_size)

    def mirror[R:Manifest,I <: DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](op: CollectImpl[R,I,C], f: Transformer)(implicit pos: SourceContext): CollectImpl[R,I,C] = {
      new { override val original = Some(f,op) } with CollectImpl(f(op.size))(op.mR,op.mI,op.mC,pos)
    }

    def unerase[R:Manifest,I <: DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](op: CollectImpl[_,_,_]): CollectImpl[R,I,C] = {
      op.asInstanceOf[CollectImpl[R,I,C]]
    }
  }

  /**
   * Parallel collect (includes map, zipwith, mapindices)
   * @param size      - size of loop (input collection)
   * @param func      - flatmap function - produces Exp[DeliteCollection[R]]
   * @param alloc     - allocation rule for intermediate type (Exp[Int] => Exp[I])
   * @param finalizer - finalizer method to create collection from intermediate
   * @param cond      - list of (filter) conditions, if any
   */
  case class FlatMapImpl[R:Manifest,I<:DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](size: Exp[Int])(implicit ctx: SourceContext) extends DeliteOpLoop[C] {
    type OpType <: FlatMapImpl[R,I,C]

    val func: Option[Block[DeliteCollection[R]]] = None
    val alloc: Option[Exp[Int] => Exp[I]] = None
    val finalizer: Option[Exp[I] => Exp[C]] = None
    
    def allocI(i: Exp[Int]) = (alloc.get)(i)
 
    // bound vars
    final lazy val eV: Sym[R] = copyTransformedOrElse(_.eV)(fresh[R]).asInstanceOf[Sym[R]]
    final lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val allocVal: Sym[I] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[I])).asInstanceOf[Sym[I]]
    final lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val iV2: Sym[Int] = copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val aV2: Sym[I] = copyTransformedOrElse(_.aV2)(fresh[I]).asInstanceOf[Sym[I]]

    lazy val body: Def[C] = copyBodyOrElse {
      val _iF: Sym[Int] = fresh[Int] // copyTransformedOrElse(_.iF)(fresh[Int]).asInstanceOf[Sym[Int]]
      val _eF: Sym[DeliteCollection[R]] = fresh[DeliteCollection[R]](func.get.tp) //copyTransformedOrElse(_.eF)().asInstanceOf[Sym[DeliteCollection[R]]]
      
      DeliteCollectElem[R,I,C](
      iFunc = Some(func.get),
      iF = Some(_iF),
      sF = Some(reifyEffects(dc_size(_eF))), //note: applying dc_size directly to iFunc can lead to iFunc being duplicated (during mirroring?)
      eF = Some(_eF),
      func = reifyEffects(dc_apply(_eF,_iF)),
      par = dc_parallelization(allocVal, true),
      buf = DeliteBufferElem(
              eV = this.eV,
              sV = this.sV,
              iV = this.iV,
              iV2 = this.iV2,
              allocVal = this.allocVal,
              aV2 = this.aV2,
              alloc = reifyEffects(this.allocI(sV)),
              apply = unusedBlock,
              update = reifyEffects(dc_update(allocVal,v,eV)),
              append = reifyEffects(dc_append(allocVal,v,eV)),
              appendable = reifyEffects(dc_appendable(allocVal,v,eV)),
              setSize = reifyEffects(dc_set_logical_size(allocVal,sV)),
              allocRaw = reifyEffects(dc_alloc[R,I](allocVal,sV)),
              copyRaw = reifyEffects(dc_copy(aV2,iV,allocVal,iV2,sV)),
              finalizer = reifyEffects((this.finalizer.get)(allocVal))
            ),
      numDynamicChunks = this.numDynamicChunks
    )}

    val mR = manifest[R]
    val mI = manifest[I]
    val mC = manifest[C]    
  }
  object FlatMapFactory {
    /**
     * @param size - size of loop (input collection)
     * @param func - flatmap function
     */
    def array[R:Manifest](size: Exp[Int], func: Block[DeliteCollection[R]])(implicit ctx: SourceContext) = {
      val alloc = (z: Exp[Int]) => DeliteArray[R](z)
      selfBacked[R,DeliteArray[R]](size,func,alloc)
    }
    private def selfBacked[R:Manifest,C<:DeliteCollection[R]:Manifest](_size: Exp[Int], _func: Block[DeliteCollection[R]], _alloc: (Exp[Int]) => Exp[C])(implicit ctx: SourceContext) = new {
      override val func = Some(_func)
      override val alloc = Some(_alloc)
      override val finalizer = Some({(x: Exp[C]) => x})
    } with FlatMapImpl[R,C,C](_size)

    def mirror[R:Manifest,I<:DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](op: FlatMapImpl[R,I,C], f: Transformer)(implicit pos: SourceContext): FlatMapImpl[R,I,C] = {
      new {override val original = Some(f,op)} with FlatMapImpl(f(op.size))(op.mR,op.mI,op.mC,pos)
    }

    def unerase[R:Manifest,I<:DeliteCollection[R]:Manifest,C<:DeliteCollection[R]:Manifest](op: FlatMapImpl[_,_,_]): FlatMapImpl[R,I,C] = {
      op.asInstanceOf[FlatMapImpl[R,I,C]]
    }
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e: ForeachImpl => reflectPure(ForeachFactory.mirror(e,f))(mtype(manifest[A]), pos)
    case Reflect(e: ForeachImpl, u, es) => reflectMirrored(Reflect( ForeachFactory.mirror(e,f), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: ReduceImpl[_] => 
      val op = ReduceFactory.unerase(e)(e.mA)
      reflectPure( ReduceFactory.mirror(op,f)(e.mA,pos))(mtype(manifest[A]), pos)
    case Reflect(e: ReduceImpl[_], u, es) => 
      val op = ReduceFactory.unerase(e)(e.mA)
      reflectMirrored(Reflect( ReduceFactory.mirror(op,f)(e.mA,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

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
  override def blocks(e: Any): List[Block[Any]] = e match {
    case e: AbstractLoop[_] => blocks(e.body)
    case _ => super.blocks(e)
  }
}
