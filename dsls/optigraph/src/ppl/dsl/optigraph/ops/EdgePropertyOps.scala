package ppl.dsl.optigraph.ops

import ppl.delite.framework.ops._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.dsl.optigraph._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import java.io.PrintWriter

trait EdgePropertyOps extends Variables {
  this: OptiGraph =>
  
  /** EdgeProperty constructors */  
  object EdgeProperty {
    /** Create a new edge property (each edge's value is set to default of the type) */
    def apply[A:Manifest](g: Rep[Graph]) = edgeprop_new(g)
    /** Create a new edge property (each edge's value is set to init) */
    def apply[A:Manifest](g: Rep[Graph], init:Rep[A]) = edgeprop_new(g, init)
  }
  /** Alias for EdgeProperty */
  object EP {
    def apply[A:Manifest](g: Rep[Graph]) = EdgeProperty.apply(g)
    def apply[A:Manifest](g: Rep[Graph], init:Rep[A]) = EdgeProperty.apply(g, init)
  }
  
  implicit def repEdgePropertyToEdgePropertyOps[A:Manifest](ep: Rep[EdgeProperty[A]]) = new EdgePropertyOpsCls(ep)
  
  /** Operations on EdgeProperties */
  class EdgePropertyOpsCls[A:Manifest](ep: Rep[EdgeProperty[A]]) {
    /** Return the property value of edge e */
    def apply(e: Rep[Edge]) = edgeprop_apply(ep, e)
    /** Update the property value of edge e to x */
    def update(e: Rep[Edge], x: Rep[A]) = edgeprop_update(ep, e, x)
    /** Set the value of all the edges to x (parallel operation) */
    def setAll(x: Rep[A]) = edgeprop_setAll(ep, x)
    /** Defer assigning value x to edge e (any other previously deferred value will be overwritten) */
    def <= (e: Rep[Edge], x: Rep[A]) = edgeprop_defer(ep, e, x)
    /** Assign the value deferred for edge e (the latest deferred value) */
    def assign(e: Rep[Edge]) = edgeprop_assign(ep, e)
    /** Assign the values deferred for all the edges (parallel operation) */
    def assignAll() = edgeprop_assignAll(ep)
  }
  
  def edgeprop_new[A:Manifest](g: Rep[Graph]): Rep[EdgeProperty[A]]
  def edgeprop_new[A:Manifest](g: Rep[Graph], init:Rep[A]): Rep[EdgeProperty[A]]
  def edgeprop_apply[A:Manifest](ep: Rep[EdgeProperty[A]], e: Rep[Edge]): Rep[A]
  def edgeprop_update[A:Manifest](ep: Rep[EdgeProperty[A]], e: Rep[Edge], x: Rep[A]): Rep[Unit]
  def edgeprop_setAll[A:Manifest](ep: Rep[EdgeProperty[A]], x: Rep[A]): Rep[Unit]
  def edgeprop_defer[A:Manifest](ep: Rep[EdgeProperty[A]], e: Rep[Edge], x: Rep[A]): Rep[Unit]
  def edgeprop_assign[A:Manifest](ep: Rep[EdgeProperty[A]], e: Rep[Edge]): Rep[Unit]
  def edgeprop_assignAll[A:Manifest](ep: Rep[EdgeProperty[A]]): Rep[Unit]
}

trait EdgePropertyOpsExp extends EdgePropertyOps with VariablesExp with BaseFatExp {
  this: OptiGraphExp =>
    
  case class EdgePropObjectNew[A](g: Exp[Graph], size: Exp[Int]) (val mP: Manifest[EdgeProperty[A]]) extends Def[EdgeProperty[A]]
  case class EdgePropSetAll[A:Manifest](in: Exp[EdgeProperty[A]], x: Exp[A]) extends DeliteOpIndexedLoop {
    val size = dc_size(in)
    def func = i => dc_update(in, i, x)
  }
  case class EdgePropDefer[A:Manifest](ep: Exp[EdgeProperty[A]], idx: Exp[Int], x: Exp[A]) extends Def[Unit]
  case class EdgePropGetDef[A:Manifest](ep: Exp[EdgeProperty[A]], idx: Exp[Int]) extends Def[A]
  case class EdgePropHasDef[A:Manifest](ep: Exp[EdgeProperty[A]], idx: Exp[Int]) extends Def[Boolean]
  case class EdgePropClearDef[A:Manifest](ep: Exp[EdgeProperty[A]], idx: Exp[Int]) extends Def[Unit]
  case class EdgePropAssignAll[A:Manifest](in: Exp[EdgeProperty[A]]) extends DeliteOpIndexedLoop {
    val size = dc_size(in)
    def func = i => {
      if(edgeprop_has_def(in, i)) {
    	dc_update(in, i, edgeprop_get_def(in, i))
    	edgeprop_clear_def(in, i)
      } else {
        unit()
      }
    }
  }
  
  // default constructor
  def edgeprop_new[A:Manifest](g: Exp[Graph]) = {
    val size = g.NumEdges
    reflectMutable(EdgePropObjectNew(g, size)(manifest[EdgeProperty[A]]))
  }
  
  // constructor with initial value
  def edgeprop_new[A:Manifest](g: Exp[Graph], init: Exp[A]) = {
    val newEP = EdgeProperty(g)
    newEP.setAll(init)
    newEP
  } 
  def edgeprop_setAll[A:Manifest](ep: Exp[EdgeProperty[A]], x: Exp[A]) = reflectWrite(ep)(EdgePropSetAll(ep, x))
  
  def edgeprop_apply[A:Manifest](ep: Exp[EdgeProperty[A]], e: Exp[Edge]) = { 
    // TODO: check edge in graph (ep.g contains e)
    dc_apply(ep, e.Id) 
  }  
  
  def edgeprop_update[A:Manifest](ep: Exp[EdgeProperty[A]], e: Exp[Edge], x: Exp[A]) = {
    // TODO: check edge in graph (ep.g contains e)
    dc_update(ep, e.Id, x)
  }
  
  def edgeprop_defer[A:Manifest](ep: Exp[EdgeProperty[A]], e: Exp[Edge], x: Exp[A]) = reflectWrite(ep)(EdgePropDefer(ep, e.Id, x))
  def edgeprop_get_def[A:Manifest](ep: Exp[EdgeProperty[A]], idx: Exp[Int]) = reflectPure(EdgePropGetDef(ep, idx))
  def edgeprop_has_def[A:Manifest](ep: Exp[EdgeProperty[A]], idx: Exp[Int]) = reflectPure(EdgePropHasDef(ep, idx))
  def edgeprop_clear_def[A:Manifest](ep: Exp[EdgeProperty[A]], idx: Exp[Int]) = reflectWrite(ep)(EdgePropClearDef(ep, idx))
  def edgeprop_assignAll[A:Manifest](ep: Exp[EdgeProperty[A]]) = reflectWrite(ep)(EdgePropAssignAll(ep))
  def edgeprop_assign[A:Manifest](ep: Exp[EdgeProperty[A]], e: Exp[Edge]) = {
    val i = e.Id
    if(edgeprop_has_def(ep, i)) {
      dc_update(ep, i, edgeprop_get_def(ep, i))
      edgeprop_clear_def(ep, i)
    } 
  }
}

trait BaseGenEdgePropertyOps extends GenericFatCodegen {
  val IR: EdgePropertyOpsExp
  import IR._

}

trait ScalaGenEdgePropertyOps extends BaseGenEdgePropertyOps with ScalaGenFat {
  val IR: EdgePropertyOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ep@EdgePropObjectNew(g, size) => emitValDef(sym, "new " + remap(ep.mP) +"(" + quote(g) + "," + quote(size) + ")")
      case EdgePropDefer(ep, idx, x) => emitValDef(sym, quote(ep) + ".defer(" + quote(idx) + ", " + quote(x) + ")")
      case EdgePropGetDef(ep, idx) => emitValDef(sym, quote(ep) + ".getDeferredValue(" + quote(idx) + ")")
      case EdgePropHasDef(ep, idx) => emitValDef(sym, quote(ep) + ".hasDeferredValue(" + quote(idx) + ")")
      case EdgePropClearDef(ep, idx) => emitValDef(sym, quote(ep) + ".clearDeferredValue(" + quote(idx) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}