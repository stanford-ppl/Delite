package ppl.dsl.optigraph.ops

import ppl.delite.framework.ops._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.dsl.optigraph._
import scala.virtualization.lms.common._
import reflect.Manifest
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
    /** Returns the size of the collection */
    def Size: Rep[Int] = gorder_size(o)
    /** Returns the first element in the order */
    def Front: Rep[T] = gorder_front(o)
    /** Returns the last element in the order */
    def Back: Rep[T] = gorder_back(o)
    /** Adds a new element to the front of the order */
    def PushFront(e: Rep[T]): Rep[Unit] = gorder_pushfront(o, e)
    /** Adds a new element to the back of the order */
    def PushBack(e: Rep[T]): Rep[Unit] = gorder_pushback(o, e)
    /** Prepends all the elements of o2 (in order) to the order */
    def PushFrontOrder(o2: Rep[GOrder[T]]): Rep[Unit] = gorder_pushfrontord(o, o2)
    /** Appends all the elements of o2 (in order) to the order */
    def PushBackOrder(o2: Rep[GOrder[T]]): Rep[Unit] = gorder_pushbackord(o, o2)
    /** Removes and returns the first element in the order */
    def PopFront(): Rep[T] = gorder_popfront(o)
    /** Removes and returns the last element in the order */
    def PopBack(): Rep[T] = gorder_popback(o)
    /** Lookup the element at position idx in the order 
     *  RuntimeException if idx is out of bounds */
    def apply(idx: Rep[Int]): Rep[T] = gorder_apply(o, idx)
  }
  
  def gorder_new[T:Manifest](): Rep[GOrder[T]]
  def gorder_items[T:Manifest](o: Rep[GOrder[T]]): Rep[GIterable[T]]
  def gorder_contains[T:Manifest](o: Rep[GOrder[T]], e: Rep[T]): Rep[Boolean]
  def gorder_size[T:Manifest](o: Rep[GOrder[T]]): Rep[Int]
  def gorder_front[T:Manifest](o: Rep[GOrder[T]]): Rep[T]
  def gorder_back[T:Manifest](o: Rep[GOrder[T]]): Rep[T]
  def gorder_pushfront[T:Manifest](o: Rep[GOrder[T]], e: Rep[T]): Rep[Unit]
  def gorder_pushback[T:Manifest](o: Rep[GOrder[T]], e: Rep[T]): Rep[Unit]
  def gorder_pushfrontord[T:Manifest](o: Rep[GOrder[T]], o2: Rep[GOrder[T]]): Rep[Unit]
  def gorder_pushbackord[T:Manifest](o: Rep[GOrder[T]], o2: Rep[GOrder[T]]): Rep[Unit]
  def gorder_popfront[T:Manifest](o: Rep[GOrder[T]]): Rep[T]
  def gorder_popback[T:Manifest](o: Rep[GOrder[T]]): Rep[T]
  def gorder_apply[T:Manifest](o: Rep[GOrder[T]], idx: Rep[Int]): Rep[T]
}

trait GOrderOpsExp extends GOrderOps with VariablesExp with BaseFatExp {
  this: OptiGraphExp =>
    
  case class GOrderObjectNew[T]()(val mGO: Manifest[GOrder[T]]) extends Def[GOrder[T]]
  case class GOrderItems[T:Manifest](o: Exp[GOrder[T]]) extends Def[GIterable[T]]
  case class GOrderContains[T:Manifest](o: Exp[GOrder[T]], e: Exp[T]) extends Def[Boolean]
  case class GOrderSize[T:Manifest](o: Exp[GOrder[T]]) extends Def[Int]
  case class GOrderFront[T:Manifest](o: Exp[GOrder[T]]) extends Def[T]
  case class GOrderBack[T:Manifest](o: Exp[GOrder[T]]) extends Def[T]
  case class GOrderPushFront[T:Manifest](o: Exp[GOrder[T]], e: Exp[T]) extends Def[Unit]
  case class GOrderPushBack[T:Manifest](o: Exp[GOrder[T]], e: Exp[T]) extends Def[Unit]
  case class GOrderPushFrontOrd[T:Manifest](o: Exp[GOrder[T]], o2: Exp[GOrder[T]]) extends Def[Unit]
  case class GOrderPushBackOrd[T:Manifest](o: Exp[GOrder[T]], o2: Exp[GOrder[T]]) extends Def[Unit]
  case class GOrderPopFront[T:Manifest](o: Exp[GOrder[T]]) extends Def[T]
  case class GOrderPopBack[T:Manifest](o: Exp[GOrder[T]]) extends Def[T]
  case class GOrderApply[T:Manifest](o: Exp[GOrder[T]], idx: Exp[Int]) extends Def[T]
  
  def gorder_new[T:Manifest]() = reflectMutable(GOrderObjectNew()(manifest[GOrder[T]]))
  def gorder_items[T:Manifest](o: Exp[GOrder[T]]) = reflectPure(GOrderItems(o))
  def gorder_contains[T:Manifest](o: Exp[GOrder[T]], e: Rep[T]) = reflectPure(GOrderContains(o, e))
  def gorder_size[T:Manifest](o: Exp[GOrder[T]]) = reflectWrite(o)(GOrderSize(o))
  def gorder_front[T:Manifest](o: Exp[GOrder[T]]) = reflectPure(GOrderFront(o))
  def gorder_back[T:Manifest](o: Exp[GOrder[T]]) = reflectPure(GOrderBack(o))
  def gorder_pushfront[T:Manifest](o: Exp[GOrder[T]], e: Exp[T]) = reflectWrite(o)(GOrderPushFront(o, e))
  def gorder_pushback[T:Manifest](o: Exp[GOrder[T]], e: Exp[T]) = reflectWrite(o)(GOrderPushBack(o, e))
  def gorder_pushfrontord[T:Manifest](o: Exp[GOrder[T]], o2: Exp[GOrder[T]]) = reflectWrite(o)(GOrderPushFrontOrd(o, o2))
  def gorder_pushbackord[T:Manifest](o: Exp[GOrder[T]], o2: Exp[GOrder[T]]) = reflectWrite(o)(GOrderPushBackOrd(o, o2))
  def gorder_popfront[T:Manifest](o: Exp[GOrder[T]]) = reflectWrite(o)(GOrderPopFront(o))
  def gorder_popback[T:Manifest](o: Exp[GOrder[T]]) = reflectWrite(o)(GOrderPopBack(o))
  def gorder_apply[T:Manifest](o: Exp[GOrder[T]], idx: Exp[Int]) = reflectPure(GOrderApply(o, idx))
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
      case o@GOrderObjectNew() => emitValDef(sym, "new " + remap(o.mGO) +"")
      case GOrderItems(o) => emitValDef(sym, quote(o) + ".items")
      case GOrderContains(o, e) => emitValDef(sym, quote(o) + ".contains(" + quote(e) + ")")
      case GOrderSize(o) => emitValDef(sym, quote(o) + ".size")
      case GOrderFront(o) => emitValDef(sym, quote(o) + ".front")
      case GOrderBack(o) => emitValDef(sym, quote(o) + ".back")
      case GOrderPushFront(o, e) => emitValDef(sym, quote(o) + ".pushFront(" + quote(e) + ")")
      case GOrderPushBack(o, e) => emitValDef(sym, quote(o) + ".pushBack(" + quote(e) + ")")
      case GOrderPushFrontOrd(o, o2) => emitValDef(sym, quote(o) + ".pushFrontOrd(" + quote(o2) + ")")
      case GOrderPushBackOrd(o, o2) => emitValDef(sym, quote(o) + ".pushBackOrd(" + quote(o2) + ")")
      case GOrderPopFront(o) => emitValDef(sym, quote(o) + ".popFront()")
      case GOrderPopBack(o) => emitValDef(sym, quote(o) + ".popBack()")
      case GOrderApply(o, idx) => emitValDef(sym, quote(o) + ".apply(" + quote(idx) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}