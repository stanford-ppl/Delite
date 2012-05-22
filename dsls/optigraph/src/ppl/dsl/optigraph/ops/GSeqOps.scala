package ppl.dsl.optigraph.ops

import ppl.delite.framework.ops._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.dsl.optigraph._
import scala.virtualization.lms.common._
import reflect.Manifest
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
  
  /** Operations on Seq collections */
  class GSeqOpsCls[T:Manifest](s: Rep[GSeq[T]]) {
    /** Returns all the items in the collection */
    def Items = gseq_items(s)
    /** Returns true if the collection contains the element */
    def Has(e: Rep[T]) = gseq_contains(s, e)
    /** Returns the size of the collection */
    def Size = gseq_size(s)
    /** Returns the first element in the sequence */
    def Front = gseq_front(s)
    /** Returns the last element in the sequence */
    def Back = gseq_back(s)
    /** Adds a new element to the front of the sequence */
    def PushFront(e: Rep[T]) = gseq_pushfront(s, e)
    /** Adds a new element to the back of the sequence */
    def PushBack(e: Rep[T]) = gseq_pushback(s, e)
    /** Prepends all the elements of s2 (in order) to the sequence */
    def PushFrontSeq(s2: Rep[GSeq[T]]) = gseq_pushfrontseq(s, s2)
    /** Appends all the elements of s2 (in order) to the sequence */
    def PushBackSeq(s2: Rep[GSeq[T]]) = gseq_pushbackseq(s, s2)
    /** Removes and returns the first element in the sequence */
    def PopFront() = gseq_popfront(s)
    /** Removes and returns the last element in the sequence */
    def PopBack() = gseq_popback(s)
    /** Lookup the element at position idx in the sequence 
     *  RuntimeException if idx is out of bounds */
    def apply(idx: Rep[Int]): Rep[T] = gseq_apply(s, idx)
  }
  
  def gseq_new[T:Manifest](): Rep[GSeq[T]]
  def gseq_items[T:Manifest](o: Rep[GSeq[T]]): Rep[GIterable[T]]
  def gseq_contains[T:Manifest](o: Rep[GSeq[T]], e: Rep[T]): Rep[Boolean]
  def gseq_size[T:Manifest](o: Rep[GSeq[T]]): Rep[Int]
  def gseq_front[T:Manifest](o: Rep[GSeq[T]]): Rep[T]
  def gseq_back[T:Manifest](o: Rep[GSeq[T]]): Rep[T]
  def gseq_pushfront[T:Manifest](o: Rep[GSeq[T]], e: Rep[T]): Rep[Unit]
  def gseq_pushback[T:Manifest](o: Rep[GSeq[T]], e: Rep[T]): Rep[Unit]
  def gseq_pushfrontseq[T:Manifest](o: Rep[GSeq[T]], o2: Rep[GSeq[T]]): Rep[Unit]
  def gseq_pushbackseq[T:Manifest](o: Rep[GSeq[T]], o2: Rep[GSeq[T]]): Rep[Unit]
  def gseq_popfront[T:Manifest](o: Rep[GSeq[T]]): Rep[T]
  def gseq_popback[T:Manifest](o: Rep[GSeq[T]]): Rep[T]
  def gseq_apply[T:Manifest](o: Rep[GSeq[T]], idx: Rep[Int]): Rep[T]
}

trait GSeqOpsExp extends GSeqOps with VariablesExp with BaseFatExp {
  this: OptiGraphExp =>
    
  case class GSeqObjectNew[T]()(val mGS: Manifest[GSeq[T]]) extends Def[GSeq[T]]
  case class GSeqItems[T:Manifest](o: Exp[GSeq[T]]) extends Def[GIterable[T]]
  case class GSeqContains[T:Manifest](o: Exp[GSeq[T]], e: Exp[T]) extends Def[Boolean]
  case class GSeqSize[T:Manifest](o: Exp[GSeq[T]]) extends Def[Int]
  case class GSeqFront[T:Manifest](o: Exp[GSeq[T]]) extends Def[T]
  case class GSeqBack[T:Manifest](o: Exp[GSeq[T]]) extends Def[T]
  case class GSeqPushFront[T:Manifest](o: Exp[GSeq[T]], e: Exp[T]) extends Def[Unit]
  case class GSeqPushBack[T:Manifest](o: Exp[GSeq[T]], e: Exp[T]) extends Def[Unit]
  case class GSeqPushFrontSeq[T:Manifest](o: Exp[GSeq[T]], o2: Exp[GSeq[T]]) extends Def[Unit]
  case class GSeqPushBackSeq[T:Manifest](o: Exp[GSeq[T]], o2: Exp[GSeq[T]]) extends Def[Unit]
  case class GSeqPopFront[T:Manifest](o: Exp[GSeq[T]]) extends Def[T]
  case class GSeqPopBack[T:Manifest](o: Exp[GSeq[T]]) extends Def[T]
  case class GSeqApply[T:Manifest](o: Exp[GSeq[T]], idx: Exp[Int]) extends Def[T]
  
  
  def gseq_new[T:Manifest]() = reflectMutable(GSeqObjectNew()(manifest[GSeq[T]]))
  def gseq_items[T:Manifest](o: Exp[GSeq[T]]) = reflectPure(GSeqItems(o))
  def gseq_contains[T:Manifest](o: Exp[GSeq[T]], e: Rep[T]) = reflectPure(GSeqContains(o, e))
  def gseq_size[T:Manifest](o: Exp[GSeq[T]]) = reflectPure(GSeqSize(o))
  def gseq_front[T:Manifest](o: Exp[GSeq[T]]) = reflectPure(GSeqFront(o))
  def gseq_back[T:Manifest](o: Exp[GSeq[T]]) = reflectPure(GSeqBack(o))
  def gseq_pushfront[T:Manifest](o: Exp[GSeq[T]], e: Exp[T]) = reflectWrite(o)(GSeqPushFront(o, e))
  def gseq_pushback[T:Manifest](o: Exp[GSeq[T]], e: Exp[T]) = reflectWrite(o)(GSeqPushBack(o, e))
  def gseq_pushfrontseq[T:Manifest](o: Exp[GSeq[T]], o2: Exp[GSeq[T]]) = reflectWrite(o)(GSeqPushFrontSeq(o, o2))
  def gseq_pushbackseq[T:Manifest](o: Exp[GSeq[T]], o2: Exp[GSeq[T]]) = reflectWrite(o)(GSeqPushBackSeq(o, o2))
  def gseq_popfront[T:Manifest](o: Exp[GSeq[T]]) = reflectWrite(o)(GSeqPopFront(o))
  def gseq_popback[T:Manifest](o: Exp[GSeq[T]]) = reflectWrite(o)(GSeqPopBack(o))
  def gseq_apply[T:Manifest](o: Exp[GSeq[T]], idx: Exp[Int]) = reflectPure(GSeqApply(o, idx))
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
      case s@GSeqObjectNew() => emitValDef(sym, "new " + remap(s.mGS) +"")
      case GSeqItems(o) => emitValDef(sym, quote(o) + ".items")
      case GSeqContains(o, e) => emitValDef(sym, quote(o) + ".contains(" + quote(e) + ")")
      case GSeqSize(o) => emitValDef(sym, quote(o) + ".size")
      case GSeqFront(o) => emitValDef(sym, quote(o) + ".front")
      case GSeqBack(o) => emitValDef(sym, quote(o) + ".back")
      case GSeqPushFront(o, e) => emitValDef(sym, quote(o) + ".pushFront(" + quote(e) + ")")
      case GSeqPushBack(o, e) => emitValDef(sym, quote(o) + ".pushBack(" + quote(e) + ")")
      case GSeqPushFrontSeq(o, o2) => emitValDef(sym, quote(o) + ".pushFrontSeq(" + quote(o2) + ")")
      case GSeqPushBackSeq(o, o2) => emitValDef(sym, quote(o) + ".pushBackSeq(" + quote(o2) + ")")
      case GSeqPopFront(o) => emitValDef(sym, quote(o) + ".popFront()")
      case GSeqPopBack(o) => emitValDef(sym, quote(o) + ".popBack()")
      case GSeqApply(o, idx) => emitValDef(sym, quote(o) + ".apply(" + quote(idx) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}