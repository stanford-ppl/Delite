package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._

trait EdgeOps extends Variables {
  this: OptiGraph =>
  
  implicit def repEdgeToEdgeOps(e: Rep[Edge]) = new EdgeOpsCls(e)

  /** Operations on edges */
  class EdgeOpsCls(e: Rep[Edge]) {
    /** Source node */
    def From = edge_from(e)
    /** Destination node */
    def To = edge_to(e)
    /** Edge id (unique per graph) */
    def Id = edge_id(e)
  }

  def edge_from(e: Rep[Edge]): Rep[Node]
  def edge_to(e: Rep[Edge]): Rep[Node]
  def edge_id(e: Rep[Edge]): Rep[Int]
}

trait EdgeOpsExp extends EdgeOps with EffectExp {
  this: OptiGraphExp =>
 
  case class EdgeFrom(e: Exp[Edge]) extends Def[Node]
  case class EdgeTo(e: Exp[Edge]) extends Def[Node]
  case class EdgeId(e: Exp[Edge]) extends Def[Int]
  
  def edge_from(e: Exp[Edge]) = reflectPure(EdgeFrom(e))
  def edge_to(e: Exp[Edge]) = reflectPure(EdgeTo(e))
  def edge_id(e: Exp[Edge]) = reflectPure(EdgeId(e))

}

trait BaseGenEdgeOps extends GenericNestedCodegen {
  val IR: EdgeOpsExp
  import IR._

}

trait ScalaGenEdgeOps extends BaseGenEdgeOps with ScalaGenBase {
  val IR: EdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case EdgeFrom(e) => emitValDef(sym, quote(e) + ".from")
      case EdgeTo(e) => emitValDef(sym, quote(e) + ".to")
      case EdgeId(e) => emitValDef(sym, quote(e) + ".id")
      case _ => super.emitNode(sym, rhs)
    }
  }
}