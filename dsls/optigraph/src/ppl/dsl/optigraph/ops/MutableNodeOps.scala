package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._

trait MutableNodeOps extends Variables with ArrayOps {
  this: OptiGraph =>

  implicit def repMutableNodeToMutableNodeOps(n: Rep[MutableNode]) = new MutableNodeOpsCls(n)

  object MutableNode {
    def apply(g: Rep[MutableGraph]) = mutable_node_new(g)
  }

  /** Operations on Nodes */
  class MutableNodeOpsCls(n: Rep[MutableNode]) {}

  def mutable_node_new(mg: Rep[MutableGraph]): Rep[MutableNode]
  def mutable_node_graph(n: Rep[MutableNode]): Rep[MutableGraph]

  //TODO: implement equality ops
}

trait MutableNodeOpsExp extends MutableNodeOps with EffectExp {
  this: OptiGraphExp =>

  case class MutableNodeNew(g: Exp[MutableGraph]) extends Def[MutableNode]
  case class MutableNodeGraph(n: Exp[MutableNode]) extends Def[MutableGraph]

  def mutable_node_new(g: Exp[MutableGraph]) = reflectPure(MutableNodeNew(g))
  def mutable_node_graph(n: Exp[MutableNode]) = reflectPure(MutableNodeGraph(n))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case MutableNodeGraph(n) => mutable_node_graph(f(n))
    // TODO reflect NodeNew
    case Reflect(e@MutableNodeGraph(n), u, es) => reflectMirrored(Reflect(MutableNodeGraph(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait BaseGenMutableNodeOps extends GenericNestedCodegen {
  val IR: MutableNodeOpsExp
  import IR._
}

trait ScalaGenMutableNodeOps extends BaseGenMutableNodeOps with ScalaGenBase {
  val IR: MutableNodeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case MutableNodeNew(g) => emitValDef(sym, "new MutableNode(" + quote(g) + ")")
      case MutableNodeGraph(n) => emitValDef(sym, quote(n) + ".mg")
      case _ => super.emitNode(sym, rhs)
    }
  }
}