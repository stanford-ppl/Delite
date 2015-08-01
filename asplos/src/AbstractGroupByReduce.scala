package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.visit._
import ppl.delite.framework.ops._

// HACK: GroupByReduce isn't easily inferred as a CAM from our original representation,
// but having this abstract version potentially prevents fusion with other nodes.
// For now, we have a rule to generate hardware from the abstract version, and a lowering rule for software targets
trait AbstractGroupByReduceOps { this: PPLOps => 
  def groupByReduce[K:Manifest,V:Manifest](d0: Rep[Int])(key: Rep[Int] => Rep[K])(value: Rep[Int] => Rep[V])(reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteMap[K,V]]
}

trait AbstractGroupByReduceExp extends AbstractGroupByReduceOps with DeliteVisit with DeliteNestedOpsExp { self: PPLOpsExp =>

  case class AbstractGroupByReduce[K:Manifest,V:Manifest](size: Exp[Int], key: Rep[Int] => Rep[K], value: Rep[Int] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext) extends DeliteOp[DeliteMap[K,V]] {
    type OpType <: AbstractGroupByReduce[K,V]

    val v: Sym[Int] = copyOrElse(_.v)(fresh[Int])
    val rV: (Sym[V],Sym[V]) = copyOrElse(_.rV)((fresh[V],fresh[V]))

    val vs: List[Sym[Int]] = List(v)
    val sizes: List[Exp[Int]] = copyTransformedSymListOrElse(_.sizes)(List(size))
    val strides: List[Exp[Int]] = copyTransformedSymListOrElse(_.strides)(List(unit(1)))

    val kFunc: Block[K] = copyTransformedBlockOrElse(_.kFunc)(reifyEffects(key(v)))
    val vFunc: Block[V] = copyTransformedBlockOrElse(_.vFunc)(reifyEffects(value(v)))
    val rFunc: Block[V] = copyTransformedBlockOrElse(_.rFunc)(reifyEffects(reduce(rV._1, rV._2)))

    val mK = manifest[K]
    val mV = manifest[V]
  }

  def groupByReduce[K:Manifest,V:Manifest](d0: Rep[Int])(key: Rep[Int] => Rep[K])(value: Rep[Int] => Rep[V])(reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteMap[K,V]]
    = reflectPure(AbstractGroupByReduce(d0, key, value, reduce))

  def lower_groupbyreduce[K:Manifest,V:Manifest](op: AbstractGroupByReduce[K,V], f: Transformer)(implicit ctx: SourceContext): Exp[DeliteMap[K,V]] = {
    // HACK - disable fusion if this method is called
    Config.opfusionEnabled = false

    def alloc[T:Manifest](size: Rep[Int]): Rep[Array1D[T]] = Array1D[T](size)

    val keys = fresh[Array1D[K]]
    val values = fresh[Array1D[V]]
    val index = fresh[DeliteIndex[K]]
    val v = op.v
    val size = f(op.sizes(0))

    val krV = (fresh[K],fresh[K])
    val kReduce = reifyEffects(krV._1)
    val kZero = reifyEffects(unit(null).asInstanceOf[Rep[K]])
    val _keyFunc = f(op.kFunc)
    def keyAlloc(len: Rep[Int]) = alloc[K](len)
    val bodyK = NestedGroupByReduce[K,K,Array1D[K]](v, krV, size, _keyFunc, _keyFunc, kReduce, kZero, keyAlloc, Nil).body

    val rV = op.rV
    val vReduce = f(op.rFunc)
    val vZero = reifyEffects(unit(null).asInstanceOf[Rep[V]])
    val valFunc = f(op.vFunc)
    def valAlloc(len: Rep[Int]) = alloc[V](len)
    val bodyV = NestedGroupByReduce[K,V,Array1D[V]](v, rV, size, _keyFunc, valFunc, vReduce, vZero, valAlloc, Nil).body

    val bodyI = DeliteHashIndexElem[K,DeliteIndex[K]](
      keyFunc = _keyFunc,
      cond = Nil,
      numDynamicChunks = 0
    )

    // Hash elems are easily the most complicated for codegen - using simple fat loop here for now
    // to avoid having to rewrite the codegen for nested loops
    val fatLoop = SimpleFatLoop(size, v,List(bodyK,bodyV,bodyI)) //SimpleFatLoopNest(List(size),List(unit(1)),List(v),List(bodyK,bodyV,bodyI))

    createFatDefinition(List(keys,values,index), List(bodyK,bodyV,bodyI), fatLoop)

    reflectPure(DeliteMapNewImm(keys, values, index, values.length))
  }

  val lowerGroupByReduce = new GroupByReduceLoweringTransformer{val IR: self.type = self}

  trait GroupByReduceLoweringTransformer extends TunnelingTransformer {
    val IR: self.type
    override val name = "GroupByReduce Lowering"
    override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
      case op: AbstractGroupByReduce[k,v] => 
        Some(lower_groupbyreduce[k,v](op, this.asInstanceOf[Transformer])(op.mK,op.mV,ctx))
      case _ => super.transformSym(s,d)
    }
    override def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit = {}
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@AbstractGroupByReduce(s,k,v,r) => reflectPure(new {override val original = Some(f,e) } with AbstractGroupByReduce(s,k,v,r)(e.mK,e.mV,ctx))(mtype(manifest[A]),ctx)
    case Reflect(e@AbstractGroupByReduce(s,k,v,r), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with AbstractGroupByReduce(s,k,v,r)(e.mK,e.mV,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: AbstractGroupByReduce[_,_] => blocks(op.kFunc) ::: blocks(op.vFunc) ::: blocks(op.rFunc)
    case _ => super.blocks(e)
  }

 // dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case op: AbstractGroupByReduce[_,_] => syms(op.sizes) ::: syms(op.strides) ::: syms(op.kFunc) ::: syms(op.vFunc) ::: syms(op.rFunc)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case op: AbstractGroupByReduce[_,_] => readSyms(op.sizes) ::: readSyms(op.strides) ::: readSyms(op.kFunc) ::: readSyms(op.vFunc) ::: readSyms(op.rFunc)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case op: AbstractGroupByReduce[_,_] => op.vs ::: List(op.rV._1, op.rV._2) ::: effectSyms(op.kFunc) ::: effectSyms(op.vFunc) ::: effectSyms(op.rFunc) 
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case op: AbstractGroupByReduce[_,_] => freqNormal(op.sizes) ::: freqNormal(op.strides) ::: freqNormal(op.kFunc) ::: freqNormal(op.vFunc) ::: freqNormal(op.rFunc)
    case _ => super.symsFreq(e)
  }

  // aliases and sharing
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case op: AbstractGroupByReduce[_,_] => Nil
    case _ => super.aliasSyms(e)
  }
  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case op: AbstractGroupByReduce[_,_] => Nil
    case _ => super.containSyms(e)
  }
  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case op: AbstractGroupByReduce[_,_] => Nil
    case _ => super.extractSyms(e)
  }
  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case op: AbstractGroupByReduce[_,_] => Nil
    case _ => super.copySyms(e)
  }
}