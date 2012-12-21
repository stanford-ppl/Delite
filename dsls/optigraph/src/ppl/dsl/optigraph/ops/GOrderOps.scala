package ppl.dsl.optigraph.ops

import ppl.delite.framework.ops._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.dsl.optigraph._
import scala.virtualization.lms.common._
import scala.collection.mutable.Set
import reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import java.io.PrintWriter

trait GOrderOps extends Variables {
  this: OptiGraph =>

  /** NodeOrder constructors */
  object NodeOrder {
    def apply() = gorder_new[Node]()
  }
  object NO {
    def apply() = NodeOrder.apply()
  }

  /** EdgeOrder constructors */
  object EdgeOrder {
    def apply() = gorder_new[Edge]()
  }
  object EO {
    def apply() = EdgeOrder.apply()
  }

  implicit def repGOrderToGOrderOps[T:Manifest](o: Rep[GOrder[T]]) = new GOrderOpsCls(o)
  implicit def varToGOrderOps[T:Manifest](o: Var[GOrder[T]]) = new GOrderOpsCls(readVar(o))

  /** Operations on Order collections */
  class GOrderOpsCls[T:Manifest](o: Rep[GOrder[T]]) {
    /** Returns all the items in the collection */
    def Items: Rep[GIterable[T]] = gorder_items(o)
    /** Returns true if the collection contains the element */
    def Has(e: Rep[T]): Rep[Boolean] = gorder_contains(o, e)
    /** Returns the number of elements in the collection */
    def Size: Rep[Int] = gorder_size(o)
    /** Returns the first element of the order */
    def Front: Rep[T] = gorder_front(o)
    /** Returns the last element in the order */
    def Back: Rep[T] = gorder_back(o)
    /** Adds a new element to the front of the order */
    def PushFront(e: Rep[T]): Rep[Unit] = gorder_pushfront(o, e)
    /** Adds a new element to the back of the order */
    def PushBack(e: Rep[T]): Rep[Unit] = gorder_pushback(o, e)
    def Push(e: Rep[T]): Rep[Unit] = gorder_pushback(o, e)
    /** Prepends all the elements of o2 (in order) to the order */
    def PushFrontOrd(o2: Rep[GOrder[T]]): Rep[Unit] = gorder_pushfrontord(o, o2)
    /** Appends all the elements of o2 (in order) to the order */
    def PushBackOrd(o2: Rep[GOrder[T]]): Rep[Unit] = gorder_pushbackord(o, o2)
    /** Removes and returns the first element in the order */
    def PopFront(): Rep[T] = gorder_popfront(o)
    def Pop(): Rep[T] = gorder_popfront(o)
    /** Removes and returns the last element in the order */
    def PopBack(): Rep[T] = gorder_popback(o)
  }

  def gorder_new[T:Manifest](): Rep[GOrder[T]]
  def gorder_raw_data[T:Manifest](o: Rep[GOrder[T]]): Rep[Set[T]]
  def gorder_set_raw_data[T:Manifest](o: Rep[GOrder[T]], d: Rep[Set[T]]): Rep[Unit]
  def gorder_size[T:Manifest](o: Rep[GOrder[T]]): Rep[Int]
  def gorder_items[T:Manifest](o: Rep[GOrder[T]]): Rep[GIterable[T]]
  def gorder_contains[T:Manifest](o: Rep[GOrder[T]], e: Rep[T]): Rep[Boolean]
  def gorder_front[T:Manifest](o: Rep[GOrder[T]]): Rep[T]
  def gorder_back[T:Manifest](o: Rep[GOrder[T]]): Rep[T]
  def gorder_pushback[T:Manifest](o: Rep[GOrder[T]], e: Rep[T]): Rep[Unit]
  def gorder_pushbackord[T:Manifest](o: Rep[GOrder[T]], o2: Rep[GOrder[T]]): Rep[Unit]
  def gorder_pushfront[T:Manifest](o: Rep[GOrder[T]], e: Rep[T]): Rep[Unit]
  def gorder_pushfrontord[T:Manifest](o: Rep[GOrder[T]], o2: Rep[GOrder[T]]): Rep[Unit]
  def gorder_popfront[T:Manifest](o: Rep[GOrder[T]]): Rep[T]
  def gorder_popback[T:Manifest](o: Rep[GOrder[T]]): Rep[T]
}

trait GOrderOpsExp extends GOrderOps with VariablesExp with BaseFatExp {
  this: OptiGraphExp =>

  case class GOrderObjectNew[T:Manifest]() extends DefWithManifest[T, GOrder[T]]
  case class GOrderRawData[T:Manifest](o: Exp[GOrder[T]]) extends DefWithManifest[T, Set[T]]
  case class GOrderSetRawData[T:Manifest](o: Exp[GOrder[T]], d: Exp[Set[T]]) extends DefWithManifest[T, Unit]

  case class GOrderSize[T:Manifest](o: Exp[GOrder[T]])
    extends DeliteOpSingleWithManifest[T, Int](reifyEffectsHere(gorder_size_impl(o)))

  case class GOrderItems[T:Manifest](o: Exp[GOrder[T]])
    extends DeliteOpSingleWithManifest[T, GIterable[T]](reifyEffectsHere(gorder_items_impl(o)))

  case class GOrderContains[T:Manifest](o: Exp[GOrder[T]], e: Exp[T])
    extends DeliteOpSingleWithManifest[T, Boolean](reifyEffectsHere(gorder_contains_impl(o, e)))

  case class GOrderFront[T:Manifest](o: Exp[GOrder[T]])
    extends DeliteOpSingleWithManifest[T, T](reifyEffectsHere(gorder_front_impl(o)))

  case class GOrderBack[T:Manifest](o: Exp[GOrder[T]])
    extends DeliteOpSingleWithManifest[T, T](reifyEffectsHere(gorder_back_impl(o)))

  case class GOrderPushBack[T:Manifest](o: Exp[GOrder[T]], e: Exp[T])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(gorder_pushback_impl(o, e)))

  case class GOrderPushBackOrd[T:Manifest](o: Exp[GOrder[T]], o2: Exp[GOrder[T]])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(gorder_pushbackorder_impl(o, o2)))

  case class GOrderPushFront[T:Manifest](o: Exp[GOrder[T]], e: Exp[T])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(gorder_pushfront_impl(o, e)))

  case class GOrderPushFrontOrd[T:Manifest](o: Exp[GOrder[T]], o2: Exp[GOrder[T]])
    extends DeliteOpSingleWithManifest[T, Unit](reifyEffectsHere(gorder_pushfrontorder_impl(o, o2)))

  case class GOrderPopFront[T:Manifest](o: Exp[GOrder[T]])
    extends DeliteOpSingleWithManifest[T, T](reifyEffectsHere(gorder_popfront_impl(o)))

  case class GOrderPopBack[T:Manifest](o: Exp[GOrder[T]])
    extends DeliteOpSingleWithManifest[T, T](reifyEffectsHere(gorder_popback_impl(o)))

  def gorder_new[T:Manifest]() = reflectMutable(GOrderObjectNew())
  def gorder_raw_data[T:Manifest](o: Exp[GOrder[T]]) = reflectMutable(GOrderRawData(o))
  def gorder_set_raw_data[T:Manifest](o: Exp[GOrder[T]], d: Exp[Set[T]]) = reflectWrite(o)(GOrderSetRawData(o, d))

  def gorder_size[T:Manifest](o: Exp[GOrder[T]]) = reflectPure(GOrderSize(o))
  def gorder_items[T:Manifest](o: Exp[GOrder[T]]) = reflectPure(GOrderItems(o))
  def gorder_contains[T:Manifest](o: Exp[GOrder[T]], e: Rep[T]) = reflectPure(GOrderContains(o, e))
  def gorder_front[T:Manifest](o: Exp[GOrder[T]]) = reflectPure(GOrderFront(o))
  def gorder_back[T:Manifest](o: Exp[GOrder[T]]) = reflectPure(GOrderBack(o))
  def gorder_pushback[T:Manifest](o: Exp[GOrder[T]], e: Exp[T]) = reflectWrite(o)(GOrderPushBack(o, e))
  def gorder_pushbackord[T:Manifest](o: Exp[GOrder[T]], o2: Exp[GOrder[T]]) = reflectWrite(o)(GOrderPushBackOrd(o, o2))
  def gorder_pushfront[T:Manifest](o: Exp[GOrder[T]], e: Exp[T]) = reflectWrite(o)(GOrderPushFront(o, e))
  def gorder_pushfrontord[T:Manifest](o: Exp[GOrder[T]], o2: Exp[GOrder[T]]) = reflectWrite(o)(GOrderPushFrontOrd(o, o2))
  def gorder_popfront[T:Manifest](o: Exp[GOrder[T]]) = reflectWrite(o)(GOrderPopFront(o))
  def gorder_popback[T:Manifest](o: Exp[GOrder[T]]) = reflectWrite(o)(GOrderPopBack(o))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case GOrderItems(o) => gorder_items(f(o))
    case GOrderContains(o,x) => gorder_contains(f(o),f(x))
    case GOrderSize(o) => gorder_size(f(o))
    case GOrderFront(o) => gorder_front(f(o))
    case GOrderBack(o) => gorder_back(f(o))
    case Reflect(e@GOrderObjectNew(), u, es) => reflectMirrored(Reflect(GOrderObjectNew()(e.mR), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderItems(o), u, es) => reflectMirrored(Reflect(GOrderItems(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderContains(o,x), u, es) => reflectMirrored(Reflect(GOrderContains(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderSize(o), u, es) => reflectMirrored(Reflect(GOrderSize(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderFront(o), u, es) => reflectMirrored(Reflect(GOrderFront(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderBack(o), u, es) => reflectMirrored(Reflect(GOrderBack(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderPushFront(o,x), u, es) => reflectMirrored(Reflect(GOrderPushFront(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderPushBack(o,x), u, es) => reflectMirrored(Reflect(GOrderPushBack(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderPushFrontOrd(o,x), u, es) => reflectMirrored(Reflect(GOrderPushFrontOrd(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderPushBackOrd(o,x), u, es) => reflectMirrored(Reflect(GOrderPushBackOrd(f(o),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderPopFront(o), u, es) => reflectMirrored(Reflect(GOrderPopFront(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GOrderPopBack(o), u, es) => reflectMirrored(Reflect(GOrderPopBack(f(o)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait BaseGenGOrderOps extends GenericFatCodegen {
  val IR: GOrderOpsExp
  import IR._
}

trait ScalaGenGOrderOps extends BaseGenGOrderOps with ScalaGenFat {
  val IR: GOrderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case o@GOrderObjectNew() => emitValDef(sym, "new " + remap(o.mR) + "")
      case GOrderRawData(x) => emitValDef(sym, quote(x) + "._data")
      //TODO need to think about how this loses order information coming from a hashset
      case GOrderSetRawData(x, d) => emitValDef(sym, quote(x) + "._data = " + quote(d))
      case _ => super.emitNode(sym, rhs)
    }
  }
}