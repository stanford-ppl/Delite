package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}
import scala.util.DynamicVariable

import ppl.delite.framework.{DeliteApplication}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._

trait NodeOps extends Variables with ArrayOps {
  this: OptiGraph =>

  implicit def repNodeToNodeOps(n: Rep[Node]) = new NodeOpsCls(n)

  // context for operations during BFS traversals
  val bfsVisitedDynVar = new DynamicVariable[Rep[Array[Int]]](null)
  
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
  def node_id(n: Rep[Node]): Rep[Int]
  //TODO: implement equality ops
}

trait NodeOpsExp extends NodeOps with EffectExp {
  this: OptiGraphExp =>
  
  case class NodeOutNeighbors(n: Exp[Node]) extends Def[GIterable[Node]]
  case class NodeInNeighbors(n: Exp[Node]) extends Def[GIterable[Node]]
  case class NodeUpNeighbors(n: Exp[Node], visited: Exp[Array[Int]]) extends Def[GIterable[Node]]
  case class NodeDownNeighbors(n: Exp[Node], visited: Exp[Array[Int]]) extends Def[GIterable[Node]]
  case class NodeOutEdges(n: Exp[Node]) extends Def[GIterable[Edge]]
  case class NodeInEdges(n: Exp[Node]) extends Def[GIterable[Edge]]
  case class NodeUpEdges(n: Exp[Node], visited: Exp[Array[Int]]) extends Def[GIterable[Edge]]
  case class NodeDownEdges(n: Exp[Node], visited: Exp[Array[Int]]) extends Def[GIterable[Edge]]
  case class NodeNumOutNeighbors(n: Exp[Node]) extends Def[Int]
  case class NodeNumInNeighbors(n: Exp[Node]) extends Def[Int]
  case class NodeOutDegree(n: Exp[Node]) extends Def[Int]
  case class NodeInDegree(n: Exp[Node]) extends Def[Int]
  case class NodeId(n: Exp[Node]) extends Def[Int]
  case class NodeGraph(n: Exp[Node]) extends Def[Graph]
  
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
  def node_id(n: Exp[Node]) = reflectPure(NodeId(n))
  def node_graph(n: Exp[Node]) = reflectPure(NodeGraph(n))
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
      case NodeOutNeighbors(n) => emitValDef(sym, quote(n) + ".outNeighbors")
      case NodeInNeighbors(n) => emitValDef(sym, quote(n) + ".inNeighbors")
      case NodeUpNeighbors(n, visited) => emitValDef(sym, quote(n) + ".upNeighbors(" + quote(visited) + ")")
      case NodeDownNeighbors(n, visited) => emitValDef(sym, quote(n) + ".downNeighbors(" + quote(visited) + ")")
      case NodeOutEdges(n) => emitValDef(sym, quote(n) + ".outEdges")
      case NodeInEdges(n) => emitValDef(sym, quote(n) + ".inEdges")
      case NodeUpEdges(n, visited) => emitValDef(sym, quote(n) + ".upEdges(" + quote(visited) + ")")
      case NodeDownEdges(n, visited) => emitValDef(sym, quote(n) + ".downEdges(" + quote(visited) + ")")
      case NodeNumOutNeighbors(n) => emitValDef(sym, quote(n) + ".numOutNeighbors")
      case NodeNumInNeighbors(n) => emitValDef(sym, quote(n) + ".numInNeighbors")
      case NodeOutDegree(n) => emitValDef(sym, quote(n) + ".outDegree")
      case NodeInDegree(n) => emitValDef(sym, quote(n) + ".inDegree")
      case NodeId(n) => emitValDef(sym, quote(n) + ".id")
      case NodeGraph(n) => emitValDef(sym, quote(n) + ".g")
      case _ => super.emitNode(sym, rhs)
    }
  }
}