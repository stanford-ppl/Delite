package ppl.dsl.optigraph.ops

import ppl.delite.framework.ops._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastructures.DeliteArray
import ppl.dsl.optigraph._
import scala.virtualization.lms.common._
import reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import java.io.PrintWriter

trait GSeqOps extends Variables {
  this: OptiGraph =>

  /** NodeSeq constructors */
  object NodeSeq {
    def apply() = gseq_new[Node]()
  }
  object NQ {
    def apply() = NodeSeq.apply()
  }

  /** EdgeSeq constructors */
  object EdgeSeq {
    def apply() = gseq_new[Edge]()
  }
  object EQ {
    def apply() = EdgeSeq.apply()
  }

  implicit def repGSeqToGSeqOps[T:Manifest](o: Rep[GSeq[T]]) = new GSeqOpsCls(o)
  implicit def varToGSeqOps[T:Manifest](o: Var[GSeq[T]]) = new GSeqOpsCls(readVar(o))

  /** Operations on Order collections */
  class GSeqOpsCls[T:Manifest](o: Rep[GSeq[T]]) {
    /** Returns all the items in the collection */
    def Items: Rep[GIterable[T]] = gseq_items(o)
    /** Returns true if the collection contains the element */
    def Has(e: Rep[T]): Rep[Boolean] = gseq_contains(o, e)
    /** Returns the number of elements in the collection */
    def Size: Rep[Int] = gseq_size(o)
    /** Returns the first element of the order */
    def Front: Rep[T] = gseq_front(o)
    /** Returns the last element in the order */
    def Back: Rep[T] = gseq_back(o)
    /** Adds a new element to the front of the order */
    def PushFront(e: Rep[T]): Rep[Unit] = gseq_pushfront(o, e)
    /** Adds a new element to the back of the order */
    def PushBack(e: Rep[T]): Rep[Unit] = gseq_pushback(o, e)
    def Push(e: Rep[T]): Rep[Unit] = gseq_pushback(o, e)
    /** Prepends all the elements of o2 (in order) to the order */
    def PushFrontSeq(o2: Rep[GSeq[T]]): Rep[Unit] = gseq_pushfrontord(o, o2)
    /** Appends all the elements of o2 (in order) to the order */
    def PushBackSeq(o2: Rep[GSeq[T]]): Rep[Unit] = gseq_pushbackord(o, o2)
    /** Removes and returns the first element in the order */
    def PopFront(): Rep[T] = gseq_popfront(o)
    def Pop(): Rep[T] = gseq_popfront(o)
    /** Removes and returns the last element in the order */
    def PopBack(): Rep[T] = gseq_popback(o)
    /** Lookup the element at position idx in the order
     *  RuntimeException if idx is out of bounds */
    def apply(idx: Rep[Int]): Rep[T] = gseq_apply(o, idx)
  }

  def gseq_new[T:Manifest](): Rep[GSeq[T]]
  def gseq_raw_data[T:Manifest](o: Rep[GSeq[T]]): Rep[DeliteArray[T]]
  def gseq_set_raw_data[T:Manifest](o: Rep[GSeq[T]], d: Rep[DeliteArray[T]]): Rep[Unit]
  def gseq_size[T:Manifest](o: Rep[GSeq[T]]): Rep[Int]
  def gseq_apply[T:Manifest](o: Rep[GSeq[T]], idx: Rep[Int]): Rep[T]
  def gseq_items[T:Manifest](o: Rep[GSeq[T]]): Rep[GIterable[T]]
  def gseq_contains[T:Manifest](o: Rep[GSeq[T]], e: Rep[T]): Rep[Boolean]
  def gseq_front[T:Manifest](o: Rep[GSeq[T]]): Rep[T]
  def gseq_back[T:Manifest](o: Rep[GSeq[T]]): Rep[T]
  def gseq_pushback[T:Manifest](o: Rep[GSeq[T]], e: Rep[T]): Rep[Unit]
  def gseq_pushbackord[T:Manifest](o: Rep[GSeq[T]], o2: Rep[GSeq[T]]): Rep[Unit]
  def gseq_pushfront[T:Manifest](o: Rep[GSeq[T]], e: Rep[T]): Rep[Unit]
  def gseq_pushfrontord[T:Manifest](o: Rep[GSeq[T]], o2: Rep[GSeq[T]]): Rep[Unit]
  def gseq_popfront[T:Manifest](o: Rep[GSeq[T]]): Rep[T]
  def gseq_popback[T:Manifest](o: Rep[GSeq[T]]): Rep[T]
}

trait GSeqOpsExp extends GSeqOps with VariablesExp with BaseFatExp {
  this: OptiGraphExp =>

  case class GSeqObjectNew[T:Manifest]() extends DefWithManifest[T, GSeq[T]]
  case class GSeqRawData[T:Manifest](o: Exp[GSeq[T]]) extends DefWithManifest[T, DeliteArray[T]]
  case class GSeqSetRawData[T:Manifest](o: Exp[GSeq[T]], d: Exp[DeliteArray[T]]) extends DefWithManifest[T, Unit]

  case class GSeqSize[T:Manifest](o: Exp[GSeq[T]])
    extends DeliteOpSingleWithManifest[T, Int](reifyEffectsHere(gseq_size_impl(o)))

  case class GSeqApply[T:Manifest](o: Exp[GSeq[T]], idx: Exp[Int])
    extends DeliteOpSingleWithManifest[T, T](reifyEffectsHere(gseq_apply_impl(o, idx)))

  case class GSeqItems[T:Manifest](o: Exp[GSeq[T]])
    extends DeliteOpSingleWithManifest[T, GIterable[T]](reifyEffectsHere(gseq_items_impl(o)))

  case class GSeqContains[T:Manifest](o: Exp[GSeq[T]], e: Exp[T])
    extends DeliteOpSingleWithManifest[T, Boolean](reifyEffectsHere(gseq_contains_impl(o, e)))

  case class GSeqFront[T:Manifest](o: Exp[GSeq[T]])
    extends DeliteOpSingleWithManifest[T, T](reifyEffectsHere(gseq_front_impl(o)))

  case class GSeqBack[T:Manifest](o: Exp[GSeq[T]])
    extends DeliteOpSingleWithManifest[T, T](reifyEffectsHere(gseq_back_impl(o)))

  case class GSeqPushBack[T:Manifest](o: Exp[GSeq[T]], e: Exp[T])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(gseq_pushback_impl(o, e)))

  case class GSeqPushBackOrd[T:Manifest](o: Exp[GSeq[T]], o2: Exp[GSeq[T]])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(gseq_pushbackorder_impl(o, o2)))

  case class GSeqPushFront[T:Manifest](o: Exp[GSeq[T]], e: Exp[T])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(gseq_pushfront_impl(o, e)))

  case class GSeqPushFrontOrd[T:Manifest](o: Exp[GSeq[T]], o2: Exp[GSeq[T]])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(gseq_pushfrontorder_impl(o, o2)))

  case class GSeqPopFront[T:Manifest](o: Exp[GSeq[T]])
    extends DeliteOpSingleWithManifest[T, T](reifyEffectsHere(gseq_popfront_impl(o)))

  case class GSeqPopBack[T:Manifest](o: Exp[GSeq[T]])
    extends DeliteOpSingleWithManifest[T, T](reifyEffectsHere(gseq_popback_impl(o)))

  def gseq_new[T:Manifest]() = reflectMutable(GSeqObjectNew())
  def gseq_raw_data[T:Manifest](o: Exp[GSeq[T]]) = reflectPure(GSeqRawData(o))
  def gseq_set_raw_data[T:Manifest](o: Exp[GSeq[T]], d: Exp[DeliteArray[T]]) = reflectWrite(o)(GSeqSetRawData(o, d))

  def gseq_size[T:Manifest](o: Exp[GSeq[T]]) = reflectPure(GSeqSize(o))
  def gseq_apply[T:Manifest](o: Exp[GSeq[T]], idx: Exp[Int]) = reflectPure(GSeqApply(o, idx))
  def gseq_items[T:Manifest](o: Exp[GSeq[T]]) = reflectPure(GSeqItems(o))
  def gseq_contains[T:Manifest](o: Exp[GSeq[T]], e: Rep[T]) = reflectPure(GSeqContains(o, e))
  def gseq_front[T:Manifest](o: Exp[GSeq[T]]) = reflectPure(GSeqFront(o))
  def gseq_back[T:Manifest](o: Exp[GSeq[T]]) = reflectPure(GSeqBack(o))
  def gseq_pushback[T:Manifest](o: Exp[GSeq[T]], e: Exp[T]) = reflectWrite(o)(GSeqPushBack(o, e))
  def gseq_pushbackord[T:Manifest](o: Exp[GSeq[T]], o2: Exp[GSeq[T]]) = reflectWrite(o)(GSeqPushBackOrd(o, o2))
  def gseq_pushfront[T:Manifest](o: Exp[GSeq[T]], e: Exp[T]) = reflectWrite(o)(GSeqPushFront(o, e))
  def gseq_pushfrontord[T:Manifest](o: Exp[GSeq[T]], o2: Exp[GSeq[T]]) = reflectWrite(o)(GSeqPushFrontOrd(o, o2))
  def gseq_popfront[T:Manifest](o: Exp[GSeq[T]]) = reflectWrite(o)(GSeqPopFront(o))
  def gseq_popback[T:Manifest](o: Exp[GSeq[T]]) = reflectWrite(o)(GSeqPopBack(o))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case GSeqItems(o) => gseq_items(f(o))
    case GSeqContains(o,x) => gseq_contains(f(o),f(x))
    case GSeqSize(o) => gseq_size(f(o))
    case GSeqFront(o) => gseq_front(f(o))
    case GSeqBack(o) => gseq_back(f(o))
    case GSeqApply(o,n) => gseq_apply(f(o),f(n))
    case Reflect(e@GSeqObjectNew(), u, es) => reflectMirrored(Reflect(GSeqObjectNew()(e.mR), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqItems(o), u, es) => reflectMirrored(Reflect(GSeqItems(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqContains(o,x), u, es) => reflectMirrored(Reflect(GSeqContains(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqSize(o), u, es) => reflectMirrored(Reflect(GSeqSize(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqFront(o), u, es) => reflectMirrored(Reflect(GSeqFront(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqBack(o), u, es) => reflectMirrored(Reflect(GSeqBack(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqApply(o,n), u, es) => reflectMirrored(Reflect(GSeqApply(f(o),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqPushFront(o,x), u, es) => reflectMirrored(Reflect(GSeqPushFront(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqPushBack(o,x), u, es) => reflectMirrored(Reflect(GSeqPushBack(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqPushFrontOrd(o,x), u, es) => reflectMirrored(Reflect(GSeqPushFrontOrd(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqPushBackOrd(o,x), u, es) => reflectMirrored(Reflect(GSeqPushBackOrd(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqPopFront(o), u, es) => reflectMirrored(Reflect(GSeqPopFront(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GSeqPopBack(o), u, es) => reflectMirrored(Reflect(GSeqPopBack(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait BaseGenGSeqOps extends GenericFatCodegen {
  val IR: GSeqOpsExp
  import IR._
}

trait ScalaGenGSeqOps extends BaseGenGSeqOps with ScalaGenFat {
  val IR: GSeqOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case o@GSeqObjectNew() => emitValDef(sym, "new " + remap(o.mR) + "")
      case GSeqRawData(x) => emitValDef(sym, quote(x) + "._data")
      case GSeqSetRawData(x, d) => emitValDef(sym, quote(x) + "._data = " + quote(d))
      case _ => super.emitNode(sym, rhs)
    }
  }
}