package ppl.dsl.optigraph.ops

import ppl.delite.framework.ops._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.dsl.optigraph._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import java.io.PrintWriter

trait NodePropertyOps extends Variables {
  this: OptiGraph =>

  /** NodeProperty constructors */ 
  object NodeProperty {
    /** Create a new node property (each node's value is set to default of the type) */
    def apply[A:Manifest](g: Rep[Graph]) = nodeprop_new(g)
    /** Create a new node property (each node's value is set to init) */
    def apply[A:Manifest](g: Rep[Graph], init:Rep[A]) = nodeprop_new(g, init)
  }
  /** Alias for NodeProperty */
  object NP {
    def apply[A:Manifest](g: Rep[Graph]) = NodeProperty.apply(g)
    def apply[A:Manifest](g: Rep[Graph], init:Rep[A]) = NodeProperty.apply(g, init)
  }
  
  implicit def repNodePropertyToNodePropertyOps[A:Manifest](np: Rep[NodeProperty[A]]) = new NodePropertyOpsCls(np)
  
  /** Operations on NodeProperties */
  class NodePropertyOpsCls[A:Manifest](np: Rep[NodeProperty[A]]) {
    /** Return the property value of node n */
    def apply(n: Rep[Node]) = nodeprop_apply(np, n)
    /** Update the property value of node n to x */
    def update(n: Rep[Node], x: Rep[A]) = nodeprop_update(np, n, x)
    /** Set the value of all the nodes to x (parallel operation) */
    def setAll(x: Rep[A]) = nodeprop_setAll(np, x)
    /** Defer assigning value x to node n (any other previously deferred value will be overwritten) */
    def <= (n: Rep[Node], x: Rep[A]) = nodeprop_defer(np, n, x)
    /** Assign the value deferred for node n (the latest deferred value) */
    def assign(n: Rep[Node]) = nodeprop_assign(np, n)
    /** Assign the values deferred for all the nodes (parallel operation) */
    def assignAll() = nodeprop_assignAll(np)
  }
  
  def nodeprop_new[A:Manifest](g: Rep[Graph]): Rep[NodeProperty[A]]
  def nodeprop_new[A:Manifest](g: Rep[Graph], init:Rep[A]): Rep[NodeProperty[A]]
  def nodeprop_apply[A:Manifest](np: Rep[NodeProperty[A]], n: Rep[Node]): Rep[A]
  def nodeprop_update[A:Manifest](np: Rep[NodeProperty[A]], n: Rep[Node], x: Rep[A]): Rep[Unit]
  def nodeprop_setAll[A:Manifest](np: Rep[NodeProperty[A]], x: Rep[A]): Rep[Unit]
  def nodeprop_defer[A:Manifest](np: Rep[NodeProperty[A]], n: Rep[Node], x: Rep[A]): Rep[Unit]
  def nodeprop_assign[A:Manifest](np: Rep[NodeProperty[A]], n: Rep[Node]): Rep[Unit]
  def nodeprop_assignAll[A:Manifest](np: Rep[NodeProperty[A]]): Rep[Unit]
}

trait NodePropertyOpsExp extends NodePropertyOps with VariablesExp with BaseFatExp {
  this: OptiGraphExp =>
    
  case class NodePropObjectNew[A](g: Exp[Graph], size: Exp[Int]) (val mNP: Manifest[NodeProperty[A]]) extends Def[NodeProperty[A]]
  case class NodePropSetAll[A:Manifest](in: Exp[NodeProperty[A]], x: Exp[A]) extends DeliteOpIndexedLoop {
    val size = dc_size(in)
    def func = i => dc_update(in, i, x)
  }
  //case class NodePropApply[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node]) extends Def[A]
  //case class NodePropUpdate[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node], x: Exp[A]) extends Def[Unit]
  
  case class NodePropDefer[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int], x: Exp[A]) extends Def[Unit]
  case class NodePropGetDef[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) extends Def[A]
  case class NodePropHasDef[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) extends Def[Boolean]
  case class NodePropClearDef[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) extends Def[Unit]
  case class NodePropAssignAll[A:Manifest](in: Exp[NodeProperty[A]]) extends DeliteOpIndexedLoop {
    val size = dc_size(in)
    def func = i => {
      if(nodeprop_has_def(in, i)) {
    	dc_update(in, i, nodeprop_get_def(in, i))
    	nodeprop_clear_def(in, i)
      } else {
        unit()
      }
    }
  }
  
  // default constructor
  def nodeprop_new[A:Manifest](g: Exp[Graph]) = {
    val size = g.NumNodes
    reflectMutable(NodePropObjectNew(g, size)(manifest[NodeProperty[A]]))
  }
  
  // constructor with initial value
  def nodeprop_new[A:Manifest](g: Exp[Graph], init: Exp[A]) = {
    val newNP = NodeProperty(g)
    newNP.setAll(init)
    newNP
  } 
  
  def nodeprop_setAll[A:Manifest](np: Exp[NodeProperty[A]], init: Exp[A]) = reflectWrite(np)(NodePropSetAll(np, init))
  
  def nodeprop_apply[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node]) = {
    // TODO: check node in graph (ep.g contains n)
    dc_apply(np, n.Id) 
    //reflectPure(NodePropApply(np, n))
  }
  def nodeprop_update[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node], x: Exp[A]) = {
    // TODO: check node in graph (ep.g contains n)
    dc_update(np, n.Id, x)
    //reflectWrite(np)(NodePropUpdate(np, n, x))
  }
  
  def nodeprop_defer[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node], x: Exp[A]) = reflectWrite(np)(NodePropDefer(np, n.Id, x))
  def nodeprop_get_def[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) = reflectPure(NodePropGetDef(np, idx))
  def nodeprop_has_def[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) = reflectPure(NodePropHasDef(np, idx))
  def nodeprop_clear_def[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) = reflectPure(NodePropClearDef(np, idx))
  def nodeprop_assignAll[A:Manifest](np: Exp[NodeProperty[A]]) = reflectWrite(np)(NodePropAssignAll(np))
  def nodeprop_assign[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node]) = {
    val i = n.Id
    if(nodeprop_has_def(np, i)) {
      dc_update(np, i, nodeprop_get_def(np, i))
      nodeprop_clear_def(np, i)
    }  
  }
}

trait BaseGenNodePropertyOps extends GenericFatCodegen {
  val IR: NodePropertyOpsExp
  import IR._

}

trait ScalaGenNodePropertyOps extends BaseGenNodePropertyOps with ScalaGenFat {
  val IR: NodePropertyOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case np@NodePropObjectNew(g, size) => emitValDef(sym, "new " + remap(np.mNP) +"(" + quote(g) + "," + quote(size) + ")")
      //case NodePropApply(np,n) => emitValDef(sym, quote(np) + "(" + quote(n) + ")")
      //case NodePropUpdate(np, n, x) => emitValDef(sym, quote(np) + "(" + quote(n) + ") = " + quote(x))
      case NodePropDefer(np, idx, x) => emitValDef(sym, quote(np) + ".defer(" + quote(idx) + ", " + quote(x) + ")")
      case NodePropGetDef(np, idx) => emitValDef(sym, quote(np) + ".getDeferredValue(" + quote(idx) + ")")
      case NodePropHasDef(np, idx) => emitValDef(sym, quote(np) + ".hasDeferredValue(" + quote(idx) + ")")
      case NodePropClearDef(np, idx) => emitValDef(sym, quote(np) + ".clearDeferredValue(" + quote(idx) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}