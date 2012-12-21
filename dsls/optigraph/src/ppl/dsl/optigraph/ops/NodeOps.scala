package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}
import scala.util.DynamicVariable

import ppl.delite.framework.{DeliteApplication}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures.DeliteArray
import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._

trait NodeOps extends Variables with ArrayOps {
  this: OptiGraph =>

  implicit def repNodeToNodeOps(n: Rep[Node]) = new NodeOpsCls(n)

  // context for operations during BFS traversals
  val bfsVisitedDynVar = new DynamicVariable[Rep[DeliteArray[Int]]](null)

  object Node {
    def apply(g: Rep[Graph]) = node_new(g)
  }

  /** Operations on Nodes */
  class NodeOpsCls(n: Rep[Node]) {
    /** Returns the nodes that node n has edges to */
    def Nbrs: Rep[GIterable[Node]] = node_out_neighbors(n)
    /** Returns the nodes that node n has edges to */
    def OutNbrs: Rep[GIterable[Node]] = node_out_neighbors(n)
    /** Returns the nodes that have edges to node n */
    def InNbrs: Rep[GIterable[Node]] = node_in_neighbors(n)
    /** During BFS: returns the in-neighbors that are closer to the BFS root node
     * than the current node n, in hop distance */
    def UpNbrs: Rep[GIterable[Node]] = node_up_neighbors(n)
    /** During BFS: returns the out-neighbors that are farther to the BFS root node
      * than the current node n, in hop distance */
    def DownNbrs: Rep[GIterable[Node]] = node_down_neighbors(n)
    /** Returns the outgoing edges of this node */
    def Edges: Rep[GIterable[Edge]] = node_out_edges(n)
    /** Returns the outgoing edges of this node */
    def OutEdges: Rep[GIterable[Edge]] = node_out_edges(n)
    /** Returns the incoming edges of node n */
    def InEdges: Rep[GIterable[Edge]] = node_in_edges(n)
    /** During BFS: returns the edges from upNeighbors */
    def UpEdges: Rep[GIterable[Edge]] = node_up_edges(n)
    /** During BFS: returns the edges to downNeighbors */
    def DownEdges: Rep[GIterable[Edge]] = node_down_edges(n)
    /** Returns the number of nodes that n has edges to */
    def NumNbrs: Rep[Int] = node_num_out_neighbors(n)
    /** Returns the number of nodes that n has edges to */
    def NumOutNbrs: Rep[Int] = node_num_out_neighbors(n)
    /** Returns the number of nodes that have edges to n */
    def NumInNbrs: Rep[Int] = node_num_in_neighbors(n)
    /** Returns the number of outgoing edges */
    def Degree: Rep[Int] = node_out_degree(n)
    /** Returns the number of outgoing edges */
    def OutDegree: Rep[Int] = node_out_degree(n)
    /** Returns the number of incoming edges */
    def InDegree: Rep[Int] = node_in_degree(n)
    /** Returns the id of the node (unique per graph) */
    def Id: Rep[Int] = node_id(n)
  }

  def node_new(g: Rep[Graph]): Rep[Node]
  def node_id(n: Rep[Node]): Rep[Int]
  def node_set_id(n: Rep[Node], x: Rep[Int]): Rep[Unit]
  def node_graph(n: Rep[Node]): Rep[Graph]

  def node_out_neighbors(n: Rep[Node]): Rep[GIterable[Node]]
  def node_in_neighbors(n: Rep[Node]): Rep[GIterable[Node]]
  def node_up_neighbors(n: Rep[Node]): Rep[GIterable[Node]]
  def node_down_neighbors(n: Rep[Node]): Rep[GIterable[Node]]
  def node_out_edges(n: Rep[Node]): Rep[GIterable[Edge]]
  def node_in_edges(n: Rep[Node]): Rep[GIterable[Edge]]
  def node_up_edges(n: Rep[Node]): Rep[GIterable[Edge]]
  def node_down_edges(n: Rep[Node]): Rep[GIterable[Edge]]
  def node_num_out_neighbors(n: Rep[Node]): Rep[Int]
  def node_num_in_neighbors(n: Rep[Node]): Rep[Int]
  def node_out_degree(n: Rep[Node]): Rep[Int]
  def node_in_degree(n: Rep[Node]): Rep[Int]

  //TODO: implement equality ops
}

trait NodeOpsExp extends NodeOps with EffectExp {
  this: OptiGraphExp =>

  case class NodeNew(g: Exp[Graph]) extends Def[Node]
  case class NodeId(n: Exp[Node]) extends Def[Int]
  case class NodeSetId(n: Exp[Node], x: Exp[Int]) extends Def[Unit]
  case class NodeGraph(n: Exp[Node]) extends Def[Graph]

  case class NodeOutNeighbors(n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(node_outneighbors_impl(n)))

  case class NodeInNeighbors(n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(node_inneighbors_impl(n)))

  case class NodeUpNeighbors(n: Exp[Node], visited: Exp[DeliteArray[Int]])
    extends DeliteOpSingleTask(reifyEffectsHere(node_upneighbors_impl(n, visited)))

  case class NodeDownNeighbors(n: Exp[Node], visited: Exp[DeliteArray[Int]])
    extends DeliteOpSingleTask(reifyEffectsHere(node_downneighbors_impl(n, visited)))

  case class NodeOutEdges(n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(node_outedges_impl(n)))

  case class NodeInEdges(n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(node_inedges_impl(n)))

  case class NodeUpEdges(n: Exp[Node], visited: Exp[DeliteArray[Int]])
    extends DeliteOpSingleTask(reifyEffectsHere(node_upedges_impl(n, visited)))

  case class NodeDownEdges(n: Exp[Node], visited: Exp[DeliteArray[Int]])
    extends DeliteOpSingleTask(reifyEffectsHere(node_downedges_impl(n, visited)))

  case class NodeNumOutNeighbors(n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(node_numoutneighbors_impl(n)))

  case class NodeNumInNeighbors(n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(node_numinneighbors_impl(n)))

  case class NodeOutDegree(n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(node_outdegree_impl(n)))

  case class NodeInDegree(n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(node_indegree_impl(n)))

  def node_new(g: Exp[Graph]) = reflectMutable(NodeNew(g))
  def node_id(n: Exp[Node]) = reflectPure(NodeId(n))
  def node_set_id(n: Exp[Node], x: Exp[Int]) = reflectWrite(n)(NodeSetId(n, x))
  def node_graph(n: Exp[Node]) = reflectPure(NodeGraph(n))

  def node_out_neighbors(n: Exp[Node]) = reflectPure(NodeOutNeighbors(n))
  def node_in_neighbors(n: Exp[Node]) = reflectPure(NodeInNeighbors(n))
  def node_up_neighbors(n: Exp[Node]) = reflectPure(NodeUpNeighbors(n, bfsVisitedDynVar.value))
  def node_down_neighbors(n: Exp[Node]) = reflectPure(NodeDownNeighbors(n, bfsVisitedDynVar.value))
  def node_out_edges(n: Exp[Node]) = reflectPure(NodeOutEdges(n))
  def node_in_edges(n: Exp[Node]) = reflectPure(NodeInEdges(n))
  def node_up_edges(n: Exp[Node]) = reflectPure(NodeUpEdges(n, bfsVisitedDynVar.value))
  def node_down_edges(n: Exp[Node]) = reflectPure(NodeDownEdges(n, bfsVisitedDynVar.value))
  def node_num_out_neighbors(n: Exp[Node]) = reflectPure(NodeNumOutNeighbors(n))
  def node_num_in_neighbors(n: Exp[Node]) = reflectPure(NodeNumInNeighbors(n))
  def node_out_degree(n: Exp[Node]) = reflectPure(NodeOutDegree(n))
  def node_in_degree(n: Exp[Node]) = reflectPure(NodeInDegree(n))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case NodeOutNeighbors(n) => node_out_neighbors(f(n))
    case NodeInNeighbors(n) => node_in_neighbors(f(n))
    case NodeUpNeighbors(n,v) => node_up_neighbors(f(n))
    case NodeDownNeighbors(n,v) => node_down_neighbors(f(n))
    case NodeOutEdges(n) => node_out_edges(f(n))
    case NodeInEdges(n) => node_in_edges(f(n))
    case NodeUpEdges(n,v) => node_up_edges(f(n))
    case NodeDownEdges(n,v) => node_down_edges(f(n))
    case NodeNumOutNeighbors(n) => node_num_out_neighbors(f(n))
    case NodeNumInNeighbors(n) => node_num_in_neighbors(f(n))
    case NodeOutDegree(n) => node_out_degree(f(n))
    case NodeInDegree(n) => node_in_degree(f(n))
    case NodeId(n) => node_id(f(n))
    case NodeGraph(n) => node_graph(f(n))

    // TODO reflect NodeNew, NodeSetId

    case Reflect(e@NodeOutNeighbors(n), u, es) => reflectMirrored(Reflect(NodeOutNeighbors(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeInNeighbors(n), u, es) => reflectMirrored(Reflect(NodeInNeighbors(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeUpNeighbors(n,v), u, es) => reflectMirrored(Reflect(NodeUpNeighbors(f(n),f(v)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeDownNeighbors(n,v), u, es) => reflectMirrored(Reflect(NodeDownNeighbors(f(n),f(v)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeOutEdges(n), u, es) => reflectMirrored(Reflect(NodeOutEdges(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeInEdges(n), u, es) => reflectMirrored(Reflect(NodeInEdges(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeUpEdges(n,v), u, es) => reflectMirrored(Reflect(NodeUpEdges(f(n),f(v)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeDownEdges(n,v), u, es) => reflectMirrored(Reflect(NodeDownEdges(f(n),f(v)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeNumOutNeighbors(n), u, es) => reflectMirrored(Reflect(NodeNumOutNeighbors(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeNumInNeighbors(n), u, es) => reflectMirrored(Reflect(NodeNumInNeighbors(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeOutDegree(n), u, es) => reflectMirrored(Reflect(NodeOutDegree(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeInDegree(n), u, es) => reflectMirrored(Reflect(NodeInDegree(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeId(n), u, es) => reflectMirrored(Reflect(NodeId(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodeGraph(n), u, es) => reflectMirrored(Reflect(NodeGraph(f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

}


trait BaseGenNodeOps extends GenericNestedCodegen {
  val IR: NodeOpsExp
  import IR._
}

trait ScalaGenNodeOps extends BaseGenNodeOps with ScalaGenBase {
  val IR: NodeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case NodeNew(g) => emitValDef(sym, "new Node(" + quote(g) + ")")
      case NodeId(n) => emitValDef(sym, quote(n) + ".id")
      case NodeSetId(n, x) => emitValDef(sym, quote(n) + ".id = " + quote(x))
      case NodeGraph(n) => emitValDef(sym, quote(n) + ".g")
      case _ => super.emitNode(sym, rhs)
    }
  }
}