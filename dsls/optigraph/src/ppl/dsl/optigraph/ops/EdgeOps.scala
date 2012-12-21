package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._

trait EdgeOps extends Variables {
  this: OptiGraph =>

  implicit def repEdgeToEdgeOps(e: Rep[Edge]) = new EdgeOpsCls(e)

  object Edge {
    def apply(g: Rep[Graph], from: Rep[Node], to: Rep[Node]) = edge_new(g, from, to)
  }

  /** Operations on edges */
  class EdgeOpsCls(e: Rep[Edge]) {
    /** Source node */
    def From = edge_from(e)
    /** Destination node */
    def To = edge_to(e)
    /** Edge id (unique per graph) */
    def Id = edge_id(e)
  }

  def edge_new(g: Rep[Graph], from: Rep[Node], to: Rep[Node]): Rep[Edge]
  def edge_from(e: Rep[Edge]): Rep[Node]
  def edge_to(e: Rep[Edge]): Rep[Node]
  def edge_id(e: Rep[Edge]): Rep[Int]
  def edge_set_id(e: Rep[Edge], x: Rep[Int]): Rep[Unit]
}

trait EdgeOpsExp extends EdgeOps with EffectExp {
  this: OptiGraphExp =>

  case class EdgeNew(g: Exp[Graph], from: Exp[Node], to: Exp[Node]) extends Def[Edge]
  case class EdgeFrom(e: Exp[Edge]) extends Def[Node]
  case class EdgeTo(e: Exp[Edge]) extends Def[Node]
  case class EdgeId(e: Exp[Edge]) extends Def[Int]
  case class EdgeSetId(e: Exp[Edge], x: Exp[Int]) extends Def[Unit]

  def edge_new(g: Exp[Graph], from: Exp[Node], to: Exp[Node]) = reflectMutable(EdgeNew(g, from, to))
  def edge_from(e: Exp[Edge]) = reflectPure(EdgeFrom(e))
  def edge_to(e: Exp[Edge]) = reflectPure(EdgeTo(e))
  def edge_id(e: Exp[Edge]) = reflectPure(EdgeId(e))
  def edge_set_id(e: Exp[Edge], x: Exp[Int]) = reflectWrite(e)(EdgeSetId(e, x))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case EdgeFrom(x) => edge_from(f(x))
    case EdgeTo(x) => edge_to(f(x))
    case EdgeId(x) => edge_id(f(x))
    //TODO reflect EdgeSetId, EdgeNew
    case Reflect(e@EdgeFrom(x), u, es) => reflectMirrored(Reflect(EdgeFrom(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@EdgeTo(x), u, es) => reflectMirrored(Reflect(EdgeTo(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@EdgeId(x), u, es) => reflectMirrored(Reflect(EdgeId(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

}

trait BaseGenEdgeOps extends GenericNestedCodegen {
  val IR: EdgeOpsExp
  import IR._

}

trait ScalaGenEdgeOps extends BaseGenEdgeOps with ScalaGenBase {
  val IR: EdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case EdgeNew(g, from, to) => emitValDef(sym, "new Edge(" + quote(g) + ", " + quote(from) + ", " + quote(to) + ")")
      case EdgeFrom(e) => emitValDef(sym, quote(e) + ".from")
      case EdgeTo(e) => emitValDef(sym, quote(e) + ".to")
      case EdgeId(e) => emitValDef(sym, quote(e) + ".id")
      case EdgeSetId(e, x) => emitValDef(sym, quote(e) + ".id = " + quote(x))
      case _ => super.emitNode(sym, rhs)
    }
  }
}