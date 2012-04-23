package ppl.dsl.optigraph.ops

import ppl.delite.framework.ops._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.dsl.optigraph._
import scala.virtualization.lms.common._
import reflect.Manifest
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import java.io.PrintWriter

trait GSetOps extends Variables {
  this: OptiGraph =>
  
  /** NodeSet constructors */
  type NodeSet = GSet[Node]
  object NodeSet {
    def apply() = gset_new[Node]()
  }
  object NS {
    def apply() = NodeSet.apply()
  }
  
  /** EdgeSet constructors */
  object EdgeSet {
    def apply() = gset_new[Edge]()
  }
  object ES {
    def apply() = EdgeSet.apply()
  }
  
  implicit def varToGSetOps[A:Manifest](s: Var[GSet[A]]) = new GSetOpsCls(readVar(s))
  implicit def repGSetToGSetOps[A:Manifest](s: Rep[GSet[A]]) = new GSetOpsCls(s)
  
  /** Operations on Set collections */
  class GSetOpsCls[A:Manifest](s: Rep[GSet[A]]) {
    /** Returns all the items in the collection */
    def Items = gset_items(s)
    /** Returns true if the collection contains the element */
    def Has(e: Rep[A]) = gset_contains(s, e)
    /** Returns the size of the collection */
    def Size = gset_size(s)
    /** Adds a new element to the set */
    def Add(e: Rep[A]) = gset_add(s, e)
    /** Adds all the elements from s2 to the set */
    def AddSet(s2: Rep[GSet[A]]) = gset_addset(s, s2)
    /** Removes the element from the set */
    def Remove(e: Rep[A]) = gset_remove(s, e)
    /** Removes all the elements in s2 from the set */
    def RemoveSet(s2: Rep[GSet[A]]) = gset_removeset(s, s2)
    /** Removes all the elements from the set */
    def Clear = gset_clear(s)
    /** Returns a copy of this set */
    def cloneL() = gset_clone(s)
  }
  
  def gset_new[A:Manifest](): Rep[GSet[A]]
  def gset_items[A:Manifest](s: Rep[GSet[A]]): Rep[GIterable[A]]
  def gset_contains[A:Manifest](s: Rep[GSet[A]], e: Rep[A]): Rep[Boolean]
  def gset_size[A:Manifest](s: Rep[GSet[A]]): Rep[Int]
  def gset_add[A:Manifest](s: Rep[GSet[A]], e: Rep[A]): Rep[Unit]
  def gset_addset[A:Manifest](s: Rep[GSet[A]], s2: Rep[GSet[A]]): Rep[Unit]
  def gset_remove[A:Manifest](s: Rep[GSet[A]], e: Rep[A]): Rep[Unit]
  def gset_removeset[A:Manifest](s: Rep[GSet[A]], s2: Rep[GSet[A]]): Rep[Unit]
  def gset_clear[A:Manifest](s: Rep[GSet[A]]): Rep[Unit]
  def gset_clone[A:Manifest](s: Rep[GSet[A]]): Rep[GSet[A]]
}

trait GSetOpsExp extends GSetOps with VariablesExp with BaseFatExp {
  this: OptiGraphExp =>
    
  case class GSetObjectNew[A]() (val mGS: Manifest[GSet[A]]) extends Def[GSet[A]]  
  case class GSetItems[A:Manifest](s: Exp[GSet[A]]) extends Def[GIterable[A]]
  case class GSetContains[A:Manifest](s: Exp[GSet[A]], e: Exp[A]) extends Def[Boolean]
  case class GSetSize[A:Manifest](s: Exp[GSet[A]]) extends Def[Int]
  case class GSetAdd[A:Manifest](s: Exp[GSet[A]], e: Exp[A]) extends Def[Unit]
  case class GSetAddSet[A:Manifest](s: Exp[GSet[A]], s2: Exp[GSet[A]]) extends Def[Unit]
  case class GSetRemove[A:Manifest](s: Exp[GSet[A]], e: Exp[A]) extends Def[Unit]
  case class GSetRemoveSet[A:Manifest](s: Exp[GSet[A]], s2: Exp[GSet[A]]) extends Def[Unit]
  case class GSetClear[A:Manifest](s: Exp[GSet[A]]) extends Def[Unit]
  case class GSetClone[A:Manifest](s: Exp[GSet[A]]) extends Def[GSet[A]]
  
  def gset_new[A:Manifest]() = reflectMutable(GSetObjectNew()(manifest[GSet[A]]))
  def gset_items[A:Manifest](s: Exp[GSet[A]]) = reflectPure(GSetItems(s))
  def gset_contains[A:Manifest](s: Exp[GSet[A]], e: Exp[A]) = reflectPure(GSetContains(s, e))
  def gset_size[A:Manifest](s: Exp[GSet[A]]) = reflectPure(GSetSize(s))
  def gset_add[A:Manifest](s: Exp[GSet[A]], e: Exp[A]) = reflectPure(GSetAdd(s, e))
  def gset_addset[A:Manifest](s: Exp[GSet[A]], s2: Exp[GSet[A]]) = reflectPure(GSetAddSet(s, s2))
  def gset_remove[A:Manifest](s: Exp[GSet[A]], e: Exp[A]) = reflectPure(GSetRemove(s, e))
  def gset_removeset[A:Manifest](s: Exp[GSet[A]], s2: Exp[GSet[A]]) = reflectPure(GSetRemoveSet(s, s2))
  def gset_clear[A:Manifest](s: Exp[GSet[A]]) = reflectPure(GSetClear(s))
  def gset_clone[A:Manifest](s: Exp[GSet[A]]) = reflectPure(GSetClone(s))
}

trait BaseGenGSetOps extends GenericFatCodegen {
  val IR: GSetOpsExp
  import IR._

}

trait ScalaGenGSetOps extends BaseGenGSetOps with ScalaGenFat {
  val IR: GSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case s@GSetObjectNew() => emitValDef(sym, "new " + remap(s.mGS) +"")
      case GSetItems(s) => emitValDef(sym, quote(s) + ".items")
      case GSetContains(s, e) => emitValDef(sym, quote(s) + ".contains(" + quote(e) + ")")
      case GSetSize(s) => emitValDef(sym, quote(s) + ".size")
      case GSetAdd(s, e) => emitValDef(sym, quote(s) + ".add(" + quote(e) + ")")
      case GSetAddSet(s, s2) => emitValDef(sym, quote(s) + ".addSet(" + quote(s2) + ")")
      case GSetRemove(s, e) => emitValDef(sym, quote(s) + ".remove(" + quote(e) + ")")
      case GSetRemoveSet(s, s2) => emitValDef(sym, quote(s) + ".removeSet(" + quote(s2) + ")")
      case GSetClear(s) => emitValDef(sym, quote(s) + ".clear")
      case GSetClone(s) => emitValDef(sym, quote(s) + ".cloneL")
      case _ => super.emitNode(sym, rhs)
    }
  }
}