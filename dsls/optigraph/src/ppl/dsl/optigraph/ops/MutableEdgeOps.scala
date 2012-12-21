package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._

trait MutableEdgeOps extends Variables {
  this: OptiGraph =>

  implicit def repMutableEdgeToMutableEdgeOps(e: Rep[MutableEdge]) = new MutableEdgeOpsCls(e)

  object MutableEdge {
    def apply(mg: Rep[MutableGraph], from: Rep[MutableNode], to: Rep[MutableNode]) = mutable_edge_new(mg, from, to)
  }

  /** Operations on mutable edges */
  class MutableEdgeOpsCls(e: Rep[MutableEdge]) {
    /** Source node */
    def From = mutable_edge_from(e)
    /** Destination node */
    def To = mutable_edge_to(e)
  }

  def mutable_edge_new(mg: Rep[MutableGraph], from: Rep[MutableNode], to: Rep[MutableNode]): Rep[MutableEdge]
  def mutable_edge_from(e: Rep[MutableEdge]): Rep[MutableNode]
  def mutable_edge_to(e: Rep[MutableEdge]): Rep[MutableNode]
}

trait MutableEdgeOpsExp extends MutableEdgeOps with EffectExp {
  this: OptiGraphExp =>

  case class MutableEdgeNew(mg: Exp[MutableGraph], from: Exp[MutableNode], to: Exp[MutableNode]) extends Def[MutableEdge]
  case class MutableEdgeFrom(e: Exp[MutableEdge]) extends Def[MutableNode]
  case class MutableEdgeTo(e: Exp[MutableEdge]) extends Def[MutableNode]

  def mutable_edge_new(mg: Exp[MutableGraph], from: Exp[MutableNode], to: Exp[MutableNode]) = reflectPure(MutableEdgeNew(mg, from, to))
  def mutable_edge_from(e: Exp[MutableEdge]) = reflectPure(MutableEdgeFrom(e))
  def mutable_edge_to(e: Exp[MutableEdge]) = reflectPure(MutableEdgeTo(e))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case MutableEdgeFrom(x) => mutable_edge_from(f(x))
    case MutableEdgeTo(x) => mutable_edge_to(f(x))
    // TODO Reflect MutableEdgeNew
    case Reflect(e@MutableEdgeFrom(x), u, es) => reflectMirrored(Reflect(MutableEdgeFrom(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MutableEdgeTo(x), u, es) => reflectMirrored(Reflect(MutableEdgeTo(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

}

trait BaseGenMutableEdgeOps extends GenericNestedCodegen {
  val IR: MutableEdgeOpsExp
  import IR._

}

trait ScalaGenMutableEdgeOps extends BaseGenMutableEdgeOps with ScalaGenBase {
  val IR: MutableEdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case MutableEdgeNew(mg, from, to) => emitValDef(sym, "new MutableEdge(" + quote(mg) + "," + quote(from) + "," + quote(to) + ")")
      case MutableEdgeFrom(e) => emitValDef(sym, quote(e) + ".from")
      case MutableEdgeTo(e) => emitValDef(sym, quote(e) + ".to")
      case _ => super.emitNode(sym, rhs)
    }
  }
}