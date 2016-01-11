package ppl.delite.framework.ops

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._

trait DeliteNestedOpsExp extends DeliteFigmentOpsExp with DeliteOpsExpIR { this: DeliteOpsExp =>

  // TODO: Should body be a Def? How much info do we need for lowering here?
  abstract class NestedLoop[A:Manifest] extends Fig[A] {
    val vs: List[Sym[Int]]
    val sizes: List[Exp[Int]]
    val strides: List[Exp[Int]]
    val body: Def[A]
    def nestLayers = vs.length
  }

  abstract class DeliteOpNestedLoop[A:Manifest](implicit ctx: SourceContext) extends NestedLoop[A] with DeliteOp[A] {
    type OpType <: DeliteOpNestedLoop[A]
    def copyBodyOrElse(e: => Def[A]): Def[A] = original.map(p=>mirrorLoopBody(p._2.asInstanceOf[OpType].body,p._1)).getOrElse(e)
    def copyTransformedListOrElse[B](f: OpType => List[Exp[B]])(e: => List[Exp[B]]): List[Exp[B]] = original.map(p => f(p._2.asInstanceOf[OpType]).map(p._1(_))).getOrElse(e)
    val numDynamicChunks: Int = 0
  }

  override def reflectPure[A:Manifest](d: Def[A])(implicit ctx: SourceContext): Exp[A] = d match {
    case x: DeliteOpNestedLoop[_] =>
      val mutableInputs = readMutableData(d)
      val re = Read(mutableInputs)
      val be = summarizeBody(x.body)
      reflectEffect(d, re andAlso be)
    case _ => super.reflectPure(d)
  }

  // Hacks for guaranteeing lazy evaluation of bodies
 override def reflectEffect[A:Manifest](d: Def[A], u: Summary)(implicit ctx: SourceContext): Exp[A] = d match {
    case x: DeliteOpNestedLoop[_] =>
      val z = x.body
      super.reflectEffect(d,u)
    case _ => super.reflectEffect(d,u)
  }

  override def reflectMirrored[A:Manifest](zd: Reflect[A])(implicit pos: SourceContext): Exp[A] = zd match {
    case Reflect(x: DeliteOpNestedLoop[_], u, es) =>
      val z = x.body
      super.reflectMirrored(zd)
    case _ => super.reflectMirrored(zd)
  }

  /**
   * Nested parallel, effectful foreach
   * @param vs       - symbols for loop iterators
   * @param lSizes   - loop iteration dimensions
   * @param lStrides - optional loop strides (default 1s)
   * @param func     - the foreach function
   */
  case class NestedForeach(ovs: List[Sym[Int]], lSizes: List[Exp[Int]], lStrides: Option[List[Exp[Int]]], func: Block[Unit])(implicit ctx: SourceContext) extends DeliteOpNestedLoop[Unit] {
    type OpType <: NestedForeach

    lazy val vs: List[Sym[Int]] = copyOrElse(_.vs)(ovs)
    val sizes: List[Exp[Int]] = copyTransformedListOrElse(_.sizes)(lSizes)
    val strides: List[Exp[Int]] = copyTransformedListOrElse(_.strides)( lStrides.getOrElse( List.fill(nestLayers)(unit(1)) ) )

    lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
      func = this.func,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "NestedForeach(" + lSizes.mkString(",") + ")(" + lStrides.mkString(",") + ")(" + vs.mkString(",") + " => " + body + ")"
  }
  object NestedForeach {
    def mirror(op: NestedForeach, f: Transformer)(implicit ctx: SourceContext) = op match {
      case NestedForeach(v,s,b,bfs) => new { override val original = Some(f,op) } with NestedForeach(v,s,b,bfs)(ctx)
    }
  }



  /**
   * Nested parallel reduction. Reducing function must be associative.
   * TODO: This will need to be changed with Vera's fold/reduce distinction
   * @param ovs      - symbols for loop iterators
   * @param orV      - pair of symbols representing two elements (Sym[A], Sym[A])
   * @param lSizes   - loop iteration dimensions
   * @param lStrides - optional loop strides (default 1s)
   * @param mFunc    - map function to obtain elements to reduce
   * @param rFunc    - the reduction function; (Block[A]) Must be associative.
   * @param init     - accumulator initialization - used only if mutable is true
   * @param zero     - zero value (not actually used to compute output result)
   * @param fFunc    - optional filter function
   * @param mutable  - mutable reduce (reduction is done by directly updating the accumulator)
   */
  case class NestedReduce[A:Manifest](ovs: List[Sym[Int]], orV: (Sym[A],Sym[A]), lSizes: List[Exp[Int]], lStrides: Option[List[Exp[Int]]], mFunc: Block[A], rFunc: Block[A], init: Block[A], zero: Block[A], fFunc: List[Block[Boolean]], mutable: Boolean)(implicit ctx: SourceContext) extends DeliteOpNestedLoop[A] {
    type OpType <: NestedReduce[A]

    lazy val vs: List[Sym[Int]] = copyOrElse(_.vs)(ovs)
    lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)(orV)
    val sizes: List[Exp[Int]] = copyTransformedListOrElse(_.sizes)(lSizes)
    val strides: List[Exp[Int]] = copyTransformedListOrElse(_.strides)(lStrides.getOrElse( List.fill(nestLayers)(unit(1)) ))

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

    override def toString = "NestedReduce(" + sizes.mkString(",") + ")(" + strides.mkString(",") + ")(" + vs.mkString(",") + ", (" + rV._1 + ", " + rV._2 + ") => " + body + ")"
    val mA = manifest[A]
  }
  object NestedReduce {
    def mirror[A:Manifest](op: NestedReduce[A], f: Transformer)(implicit ctx: SourceContext): NestedReduce[A] = op match {
      case NestedReduce(v,r,s,t,l,b,i,z,c,m) => new { override val original = Some(f,op) } with NestedReduce(v,r,s,t,l,b,i,z,c,m)(op.mA, ctx)
    }
    def unerase[A:Manifest](op: NestedReduce[_]): NestedReduce[A] = op.asInstanceOf[NestedReduce[A]]
  }



  /**
   * Nested parallel map (includes map, zipWith, mapIndices)
   * @param ovs       - symbols for loop iterators
   * @param lSizes    - size of loop (usually size of input collection)
   * @param mFunc     - map/collect function  (anything that productes Exp[R])
   */
  case class NestedMap[A:Manifest,C<:DeliteCollection[A]:Manifest](ovs: List[Sym[Int]], lSizes: List[Exp[Int]], mFunc: Block[A])(implicit ctx: SourceContext) extends DeliteOpNestedLoop[C] {
    type OpType <: NestedMap[A,C]

    lazy val vs: List[Sym[Int]] = copyOrElse(_.vs)(ovs)
    val sizes: List[Exp[Int]] = copyTransformedListOrElse(_.sizes)(lSizes)
    val strides: List[Exp[Int]] = List.fill(nestLayers)(unit(1))

    lazy val eV: Sym[A] = copyTransformedOrElse(_.eV)(fresh[A]).asInstanceOf[Sym[A]]
    lazy val sV: Sym[Int] = copyTransformedOrElse(_.sV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val iV: Sym[Int] = copyTransformedOrElse(_.iV)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val iV2: Sym[Int] = copyTransformedOrElse(_.iV2)(fresh[Int]).asInstanceOf[Sym[Int]]
    lazy val aV2: Sym[C] = copyTransformedOrElse(_.aV2)(fresh[C]).asInstanceOf[Sym[C]]
    lazy val allocVal: Sym[C] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[C])).asInstanceOf[Sym[C]]

    // TODO: Fix alloc and update - may want to revise all of these to have less info?
    lazy val buf = DeliteBufferElem[A,C,C](
      eV = this.eV,     // Current map result
      sV = this.sV,     // intermediate size? (should be unused)
      iV = this.iV,     // copy destination start? (should be unused)
      iV2 = this.iV2,   // copy source start?
      allocVal = this.allocVal,
      aV2 = this.aV2,
      alloc = unusedBlock, //reifyEffects(dc_alloc_block[A,C](this.allocVal, this.lSizes, Nil)),
      apply = unusedBlock,
      update = unusedBlock, //reifyEffects(dc_block_update(this.allocVal,vs,this.eV, Nil)),
      append = unusedBlock, //reifyEffects(dc_append(this.allocVal,v,this.eV)),
      appendable = unusedBlock, //reifyEffects(dc_appendable(this.allocVal,v,this.eV)),
      setSize = unusedBlock, //reifyEffects(dc_set_logical_size(this.allocVal,this.sV)),
      allocRaw = unusedBlock, //reifyEffects(dc_alloc[R,C](this.allocVal,this.sV)),
      copyRaw = unusedBlock, //reifyEffects(dc_copy(this.aV2,this.iV,this.allocVal,this.iV2,this.sV)),
      finalizer = unusedBlock
    )
    lazy val body: Def[C] = copyBodyOrElse(DeliteCollectElem[A,C,C](
      func = this.mFunc,
      cond = Nil,
      par = ParFlat,
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    override def toString = "NestedMap(" + sizes.mkString(",") + ")(" + strides.mkString(",") + ")(" + vs.mkString(",") + " => " + body + ")"
    val mA = manifest[A]
    val mC = manifest[C]
  }

  object NestedMap {
    def mirror[A:Manifest,C<:DeliteCollection[A]:Manifest](op: NestedMap[A,C], f: Transformer)(implicit ctx: SourceContext): NestedMap[A,C] = op match {
      case NestedMap(v,s,b) => new { override val original = Some(f,op) } with NestedMap(v,s,b)(op.mA,op.mC,ctx)
    }
    def unerase[A:Manifest,C<:DeliteCollection[A]:Manifest](op: NestedMap[_,_]): NestedMap[A,C] = op.asInstanceOf[NestedMap[A,C]]
  }


  //////////
  // blocks

  override def blocks(e: Any): List[Block[Any]] = e match {
    case e: NestedLoop[_] => blocks(e.body)
    case _ => super.blocks(e)
  }

  ////////////////
  // dependencies

  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: NestedLoop[_] => syms(e.sizes) ::: syms(e.strides) ::: syms(e.body)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case e: NestedLoop[_] => readSyms(e.sizes) ::: readSyms(e.strides) ::: readSyms(e.body)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: NestedLoop[_] => e.vs ::: boundSyms(e.body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: NestedLoop[_] => freqNormal(e.sizes) ::: freqNormal(e.strides) ::: freqHot(e.body)
    case _ => super.symsFreq(e)
  }

  /////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: NestedLoop[_] => aliasSyms(e.body)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case e: NestedLoop[_] => containSyms(e.body)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case e: NestedLoop[_] => extractSyms(e.body)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case e: NestedLoop[_] => copySyms(e.body)
    case _ => super.copySyms(e)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e: NestedForeach =>
      reflectPure(NestedForeach.mirror(e,f)(pos))(mtype(manifest[A]), pos)
    case Reflect(e: NestedForeach, u, es) =>
      reflectMirrored(Reflect(NestedForeach.mirror(e,f)(pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: NestedReduce[_] =>
      val op = NestedReduce.unerase(e)(e.mA)
      reflectPure( NestedReduce.mirror(op,f)(e.mA,pos))(mtype(manifest[A]), pos)
    case Reflect(e: NestedReduce[_], u, es) =>
      val op = NestedReduce.unerase(e)(e.mA)
      reflectMirrored(Reflect(NestedReduce.mirror(op,f)(e.mA,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e: NestedMap[_,_] =>
      val op = NestedMap.unerase(e)(e.mA,e.mC)
      reflectPure(NestedMap.mirror(op,f)(e.mA,e.mC,pos))(mtype(manifest[A]), pos)
    case Reflect(e: NestedMap[_,_], u, es) =>
      val op = NestedMap.unerase(e)(e.mA,e.mC)
      reflectMirrored(Reflect(NestedMap.mirror(op,f)(e.mA,e.mC,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }
}