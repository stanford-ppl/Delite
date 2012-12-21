package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import scala.collection.mutable.{HashMap}
import scala.collection.immutable.{List}
import ppl.dsl.optigraph._

trait MutableGraphOps extends Variables {
  this: OptiGraph =>

  /** Directed graph constructors */
  object MutableGraph {
    def apply() = mutable_dgraph_new()
  }
  object DMutableGraph {
    def apply() = mutable_dgraph_new()
  }
  /** Undirected graph constructors */
  object UMutableGraph {
    def apply() = mutable_ugraph_new()
  }

  implicit def repMutableGraphToMutableGraphOps(g: Rep[MutableGraph]) = new MutableGraphOpsCls(g)

  /** Operations on Graphs */
  class MutableGraphOpsCls(g: Rep[MutableGraph]) {
    /** Returns all the nodes in the graph */
    def Nodes: Rep[GIterable[MutableNode]] = mutable_graph_nodes(g)
    /** Returns all the edges in the graph */
    def Edges: Rep[GIterable[MutableEdge]] = mutable_graph_edges(g)
    /** Returns the number of nodes in the graph */
    def NumNodes: Rep[Int] = mutable_graph_num_nodes(g)
    /** Returns the number of edges in the graph */
    def NumEdges: Rep[Int] = mutable_graph_num_edges(g)
    /** Adds a new node to the graph and returns this node */
    def AddNode: Rep[MutableNode] = mutable_graph_add_node(g)
    /** Adds a new edge to the graph and returns this edge */
    def AddEdge(from: Rep[MutableNode], to: Rep[MutableNode]): Rep[MutableEdge] = mutable_graph_add_edge(g,from,to)
    /** Returns an immutable snapshot of the graph */
    def Snapshot: Rep[Graph] = mutable_graph_snapshot(g)
  }

  def mutable_dgraph_new(): Rep[MutableGraph]
  def mutable_ugraph_new(): Rep[MutableGraph]
  def mutable_graph_adj_map(g: Rep[MutableGraph]): Rep[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]
  def mutable_graph_set_adj_map(g: Rep[MutableGraph], x: Rep[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]): Rep[Unit]
  def mutable_graph_adj_map_reversed(g: Rep[MutableGraph]): Rep[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]
  def mutable_graph_set_adj_map_reversed(g: Rep[MutableGraph], x: Rep[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]): Rep[Unit]
  def mutable_graph_edge_list(g: Rep[MutableGraph]): Rep[List[MutableEdge]]
  def mutable_graph_set_edge_list(g: Rep[MutableGraph], x: Rep[List[MutableEdge]]): Rep[Unit]
  def mutable_graph_is_directed(g: Rep[MutableGraph]): Rep[Boolean]

  def mutable_graph_nodes(g: Rep[MutableGraph]): Rep[GIterable[MutableNode]]
  def mutable_graph_edges(g: Rep[MutableGraph]): Rep[GIterable[MutableEdge]]
  def mutable_graph_num_nodes(g: Rep[MutableGraph]): Rep[Int]
  def mutable_graph_num_edges(g: Rep[MutableGraph]): Rep[Int]
  def mutable_graph_add_node(g: Rep[MutableGraph]): Rep[MutableNode]
  def mutable_graph_add_edge(g: Rep[MutableGraph], from: Rep[MutableNode], to: Rep[MutableNode]): Rep[MutableEdge]
  def mutable_graph_snapshot(g: Rep[MutableGraph]): Rep[Graph]
}

trait MutableGraphOpsExp extends MutableGraphOps with EffectExp with NodeOps {
  this: OptiGraphExp =>

  case class DMutableGraphObjectNew() extends Def[MutableGraph]
  case class UMutableGraphObjectNew() extends Def[MutableGraph]
  case class MutableGraphAdjMap(g: Exp[MutableGraph]) extends Def[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]
  case class MutableGraphSetAdjMap(g: Exp[MutableGraph], x: Exp[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]) extends Def[Unit]
  case class MutableGraphAdjMapReversed(g: Exp[MutableGraph]) extends Def[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]
  case class MutableGraphSetAdjMapReversed(g: Exp[MutableGraph], x: Exp[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]) extends Def[Unit]
  case class MutableGraphEdgeList(g: Exp[MutableGraph]) extends Def[List[MutableEdge]]
  case class MutableGraphSetEdgeList(g: Exp[MutableGraph], x: Exp[List[MutableEdge]]) extends Def[Unit]
  case class MutableGraphIsDirected(g: Exp[MutableGraph]) extends Def[Boolean]

  case class MutableGraphNodes(g: Exp[MutableGraph])
    extends DeliteOpSingleTask(reifyEffectsHere(mutable_graph_nodes_impl(g)))

  case class MutableGraphEdges(g: Exp[MutableGraph])
    extends DeliteOpSingleTask(reifyEffectsHere(mutable_graph_edges_impl(g)))

  case class MutableGraphNumNodes(g: Exp[MutableGraph])
    extends DeliteOpSingleTask(reifyEffectsHere(mutable_graph_numnodes_impl(g)))

  case class MutableGraphNumEdges(g: Exp[MutableGraph])
    extends DeliteOpSingleTask(reifyEffectsHere(mutable_graph_numedges_impl(g)))

  case class MutableGraphAddNode(g: Exp[MutableGraph])
    extends DeliteOpSingleTask(reifyEffectsHere(mutable_graph_addnode_impl(g)))

  case class MutableGraphAddEdge(g: Exp[MutableGraph], from: Rep[MutableNode], to: Rep[MutableNode])
    extends DeliteOpSingleTask(reifyEffectsHere(mutable_graph_addedge_impl(g, from, to)))

  case class MutableGraphSnapshot(g: Exp[MutableGraph])
    extends DeliteOpSingleTask(reifyEffectsHere(mutable_graph_snapshot_impl(g)))

  def mutable_dgraph_new() = reflectMutable(DMutableGraphObjectNew())
  def mutable_ugraph_new() = reflectMutable(UMutableGraphObjectNew())
  def mutable_graph_adj_map(g: Exp[MutableGraph]) = reflectPure(MutableGraphAdjMap(g))
  def mutable_graph_set_adj_map(g: Exp[MutableGraph], x: Exp[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]) = reflectWrite(g)(MutableGraphSetAdjMap(g, x))
  def mutable_graph_adj_map_reversed(g: Exp[MutableGraph]) = reflectPure(MutableGraphAdjMapReversed(g))
  def mutable_graph_set_adj_map_reversed(g: Exp[MutableGraph], x: Exp[HashMap[MutableNode, List[(MutableEdge,MutableNode)]]]) = reflectWrite(g)(MutableGraphSetAdjMapReversed(g, x))
  def mutable_graph_edge_list(g: Exp[MutableGraph]) = reflectPure(MutableGraphEdgeList(g))
  def mutable_graph_set_edge_list(g: Exp[MutableGraph], x: Exp[List[MutableEdge]]) = reflectWrite(g)(MutableGraphSetEdgeList(g, x))
  def mutable_graph_is_directed(g: Exp[MutableGraph]) = reflectPure(MutableGraphIsDirected(g))

  def mutable_graph_nodes(g: Exp[MutableGraph]) = reflectPure(MutableGraphNodes(g))
  def mutable_graph_edges(g: Exp[MutableGraph]) = reflectPure(MutableGraphEdges(g))
  def mutable_graph_num_nodes(g: Exp[MutableGraph]) = reflectPure(MutableGraphNumNodes(g))
  def mutable_graph_num_edges(g: Exp[MutableGraph]) = reflectPure(MutableGraphNumEdges(g))
  def mutable_graph_add_node(g: Exp[MutableGraph]) = reflectWrite(g)(MutableGraphAddNode(g))
  def mutable_graph_add_edge(g: Exp[MutableGraph], from: Rep[MutableNode], to: Rep[MutableNode]) = reflectWrite(g)(MutableGraphAddEdge(g,from,to))
  def mutable_graph_snapshot(g: Exp[MutableGraph]) = reflectPure(MutableGraphSnapshot(g))

  // TODO Is this needed?

  // alias/sharing
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case MutableGraphSnapshot(g) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
   //case GraphSnapshot(g) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
   //case GraphSnapshot(g) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    //case GraphSnapshot(g) => Nil
    case _ => super.copySyms(e)
  }

  //////////////
  // mirroring

// TODO Add mirroring for:
// * MutableGraphAdjMap
// * MutableGraphSetAdjMap
// * MutableGraphAdjMapReversed
// * MutableGraphSetAdjMapReversed
// * MutableGraphEdgeList
// * MutableGraphSetEdgeList
// * MutableGraphIsDirected

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case MutableGraphNodes(g) => mutable_graph_nodes(f(g))
    case MutableGraphEdges(g) => mutable_graph_edges(f(g))
    case MutableGraphNumNodes(g) => mutable_graph_num_nodes(f(g))
    case MutableGraphNumEdges(g) => mutable_graph_num_edges(f(g))
    case MutableGraphSnapshot(g) => mutable_graph_snapshot(f(g))
    case Reflect(e@MutableGraphNodes(g), u, es) => reflectMirrored(Reflect(MutableGraphNodes(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MutableGraphEdges(g), u, es) => reflectMirrored(Reflect(MutableGraphEdges(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MutableGraphNumNodes(g), u, es) => reflectMirrored(Reflect(MutableGraphNumNodes(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MutableGraphNumEdges(g), u, es) => reflectMirrored(Reflect(MutableGraphNumEdges(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MutableGraphSnapshot(g), u, es) => reflectMirrored(Reflect(MutableGraphSnapshot(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DMutableGraphObjectNew(), u, es) => reflectMirrored(Reflect(DMutableGraphObjectNew(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@UMutableGraphObjectNew(), u, es) => reflectMirrored(Reflect(UMutableGraphObjectNew(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MutableGraphAddNode(g), u, es) => reflectMirrored(Reflect(MutableGraphAddNode(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MutableGraphAddEdge(g,fr,to), u, es) => reflectMirrored(Reflect(MutableGraphAddEdge(f(g),f(fr),f(to)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

}


/* Code generation */
trait BaseGenMutableGraphOps extends GenericNestedCodegen {
  val IR: MutableGraphOpsExp
  import IR._

}

trait ScalaGenMutableGraphOps extends BaseGenMutableGraphOps with ScalaGenBase {
  val IR: MutableGraphOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case g@DMutableGraphObjectNew() => emitValDef(sym, "new MutableGraph(true)")
      case g@UMutableGraphObjectNew() => emitValDef(sym, "new MutableGraph(false)")
      case MutableGraphAdjMap(g) => emitValDef(sym, quote(g) + ".adjMap")
      case MutableGraphSetAdjMap(g, x) => emitValDef(sym, quote(g) + ".adjMap = " + quote(x))
      case MutableGraphAdjMapReversed(g) => emitValDef(sym, quote(g) + ".adjMapReversed")
      case MutableGraphSetAdjMapReversed(g, x) => emitValDef(sym, quote(g) + ".adjMapReversed = " + quote(x))
      case MutableGraphEdgeList(g) => emitValDef(sym, quote(g) + ".edgeList")
      case MutableGraphSetEdgeList(g, x) => emitValDef(sym, quote(g) + ".edgeList = " + quote(x))
      case MutableGraphIsDirected(g) => emitValDef(sym, quote(g) + ".isDirected")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

