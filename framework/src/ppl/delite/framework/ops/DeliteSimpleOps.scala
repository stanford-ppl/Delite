package ppl.delite.framework.ops

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

import ppl.delite.framework.datastructures._

trait DeliteSimpleOpsExp extends DeliteOpsExp {
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
  case class SimpleReduce[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), loopSize: Exp[Int], lookup: Block[A], func: Block[A], zero: Block[A])(implicit ctx: SourceContext) extends DeliteOpLoop[A] {
    type OpType <: SimpleReduce[A]
    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)(orV) // TODO: transform vars?? -- what does this mean?
 
    val stripFirst = !isPrimitiveType(manifest[A])

    val size: Exp[Int] = copyTransformedOrElse(_.size)(loopSize)

    lazy val body: Def[A] = copyBodyOrElse(DeliteReduceElem[A](
      func = this.lookup,
      cond = Nil,
      zero = this.zero,
      accInit = this.zero, //if (isPrimitiveType(manifest[A])) zero else this.accInit
      rV = this.rV,
      rFunc = this.func,
      stripFirst = this.stripFirst,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "SimpleReduce(" + size + ", iter: " + v + ", rV: (" + rV._1 + ", " + rV._2 + "), " + body.asInstanceOf[DeliteReduceElem[A]].rFunc + ")"
    val mA = manifest[A]
  }
  object ReduceFactory {
    def apply[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), size: Exp[Int], lookup: Block[A], func: Block[A], alloc: Block[A])(implicit ctx: SourceContext) = new SimpleReduce[A](oV,orV,size,lookup,func,alloc)
    def apply[A:Manifest](oV: Sym[Int], orV: (Sym[A],Sym[A]), size: Exp[Int], lookup: Block[A], func: Block[A], zero: Exp[A])(implicit ctx: SourceContext) = new SimpleReduce[A](oV,orV,size,lookup,func,reifyEffects(zero))

    def mirror[A:Manifest](op: SimpleReduce[A], f: Transformer)(implicit pos: SourceContext): SimpleReduce[A] = op match {
      case SimpleReduce(v,r,s,l,b,z) => new { override val original = Some(f, op) } with SimpleReduce(v,r,s,l,b,z)(op.mA, pos)
    }
    def unerase[A:Manifest](op: SimpleReduce[_]): SimpleReduce[A] = op.asInstanceOf[SimpleReduce[A]]
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
   * Parallel collect (includes map, zipwith, mapindices)
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
    /**
     * @param size - size of loop (input collection)
     * @param func - map/collect function  (anything that productes Exp[R])
     * @param cond - list of (filter) conditions, if any
     */
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
    /**
     * @param in   - input data for loop
     * @param size - size of loop (input collection)
     * @param func - flatmap function
     */
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
   * Extremely simple parallel file read - no string splitting, just returns collection of Strings
   * @param oV    - symbol for loop iterator
   * @param paths - list of file paths
   * @param alloc - allocation rule for output type (Exp[Int] => Exp[C])
  */
  /*case class SimpleRead[C:DeliteCollection[String]:Manifest](oV: Sym[Int], paths: Seq[Exp[String]], alloc: Exp[Int] => Exp[C])(implicit ctx: SourceContext) extends SimpleMapLike[String,C] {
    type OpType <: SimpleRead[C]

    override lazy val v: Sym[Int] = copyTransformedOrElse(_.v)(oV).asInstanceOf[Sym[Int]]
    
    val inputStream = dfis_new(paths, unit(null))
    val size = copyTransformedOrElse(_.size)(dfs_size(inputStream))

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
  }*/

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e: SimpleForeach => 
      reflectPure(ForeachFactory.mirror(e,f)(pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleForeach, u, es) => 
      reflectMirrored(Reflect(ForeachFactory.mirror(e,f)(pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

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

    /*case e: SimpleRead[_] =>
      val op = SimpleReadFactory.unerase(e)(e.mC)
      reflectPure(SimpleReadFactory.mirror(op,f)(e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: SimpleRead[_], u, es) =>
      val op = SimpleReadFactory.unerase(e)(e.mC)
      reflectMirrored(Reflect(SimpleReadFactory.mirror(op,f)(e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)*/

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  // Override to block view of blocks in Product iterator
  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: SimpleFlatMap[_,_] => blocks(op.body.asInstanceOf[DeliteCollectElem[_,_,_]].func) // blocks(op.body)
    case op: SimpleCollect[_,_] => blocks(op.body.asInstanceOf[DeliteCollectElem[_,_,_]].func) // blocks(op.body)
    case op: SimpleReduce[_] => blocks(op.body.asInstanceOf[DeliteReduceElem[_]].rFunc) // blocks(op.body)
    case op: SimpleForeach => blocks(op.body.asInstanceOf[DeliteForeachElem[_]].func) // blocks(op.body)
    //case op: SimpleRead => Nil
    //case op: DeliteOpLoop[_] => blocks(op.body)
    //case op: DeliteCollectElem[_,_,_] => blocks(op.func) ::: blocks(op.cond) ::: blocks(op.buf) ::: op.iFunc.toList 
    case _ => super.blocks(e)
  }
}
