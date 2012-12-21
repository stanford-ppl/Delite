package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.delite.framework.datastructures.DeliteArray
import ppl.dsl.optigraph._

trait GraphOps extends Variables {
  this: OptiGraph =>

  /** Directed graph constructors */
  object Graph {
    def apply(isDirected: Rep[Boolean]) = graph_new(isDirected)
  }
  object DGraph {
    def apply() = dgraph_new()
  }
  /** Undirected graph constructors */
  object UGraph {
    def apply() = ugraph_new()
  }
/*
  object RandUniformGraph {
    def apply(numNodes: Rep[Int], numEdges: Rep[Int], seed: Rep[Long]) = graph_randu(numNodes: Rep[Int], numEdges: Rep[Int], seed: Rep[Long])
  }
*/
  implicit def repGraphToGraphOps(g: Rep[Graph]) = new GraphOpsCls(g)

  /** Operations on Graphs */
  class GraphOpsCls(g: Rep[Graph]) {
    /** Returns all the nodes in the graph */
    def Nodes: Rep[GIterable[Node]] = graph_nodes(g)
    /** Returns all the edges in the graph */
    def Edges: Rep[GIterable[Edge]] = graph_edges(g)
    /** Returns the number of nodes in the graph */
    def NumNodes: Rep[Int] = graph_num_nodes(g)
    /** Returns the number of edges in the graph */
    def NumEdges: Rep[Int] = graph_num_edges(g)
    /** Returns the node with the given id */
    def Node(nodeId: Rep[Int]): Rep[Node] = graph_node(g, nodeId)
    /** Returns the edge with the given id */
    def Edge(edgeId: Rep[Int]): Rep[Edge] = graph_edge(g, edgeId)
    /** Flips the direction of edges in the graph (TODO) */
    def ^ : Rep[Graph] = graph_flip(g)
    /** BFS traversals (with optional filter and inReverse clauses) */
    def InBFS(from: Rep[Node], block: Rep[Node] => Rep[Unit]): Rep[Unit] = graph_inbfs(from, None, block, None)
    def InBFS(from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit]): Rep[Unit] = graph_inbfs(from, Some(filter), block, None)
    def InBFS(from: Rep[Node], block: Rep[Node] => Rep[Unit], inReverse: Rep[Node] => Rep[Unit])(implicit o: Overloaded2): Rep[Unit] = graph_inbfs(from, None, block, Some(inReverse))
    def InBFS(from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit], inReverse: Rep[Node] => Rep[Unit]): Rep[Unit] = graph_inbfs(from, Some(filter), block, Some(inReverse))
    /** DFS traversals (with optional filter and inPost clauses) */
    def InDFS(from: Rep[Node], block: Rep[Node] => Rep[Unit]): Rep[Unit] = graph_indfs(from, None, block, None)
    def InDFS(from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit]): Rep[Unit] = graph_indfs(from, Some(filter), block, None)
    def InDFS(from: Rep[Node], block: Rep[Node] => Rep[Unit], inPost: Rep[Node] => Rep[Unit])(implicit o: Overloaded2): Rep[Unit] = graph_indfs(from, None, block, Some(inPost))
    def InDFS(from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit], inPost: Rep[Node] => Rep[Unit]): Rep[Unit] = graph_indfs(from, Some(filter), block, Some(inPost))
  }

  def graph_new(isDirected: Rep[Boolean]): Rep[Graph]
  def dgraph_new(): Rep[Graph]
  def ugraph_new(): Rep[Graph]
  def graph_is_directed(g: Rep[Graph]): Rep[Boolean]

  def graph_raw_nodes(g: Rep[Graph]): Rep[DeliteArray[Node]]
  def graph_raw_edges(g: Rep[Graph]): Rep[DeliteArray[Edge]]
  def graph_set_raw_nodes(g: Rep[Graph], x: Rep[DeliteArray[Node]]): Rep[Unit]
  def graph_set_raw_edges(g: Rep[Graph], x: Rep[DeliteArray[Edge]]): Rep[Unit]

  def graph_raw_node_in_edges(g: Rep[Graph]): Rep[DeliteArray[GIterable[Edge]]]
  def graph_raw_node_out_edges(g: Rep[Graph]): Rep[DeliteArray[GIterable[Edge]]]
  def graph_raw_node_in_neighbors(g: Rep[Graph]): Rep[DeliteArray[GIterable[Node]]]
  def graph_raw_node_out_neighbors(g: Rep[Graph]): Rep[DeliteArray[GIterable[Node]]]
  def graph_set_raw_node_in_edges(g: Rep[Graph], x: Rep[DeliteArray[GIterable[Edge]]]): Rep[Unit]
  def graph_set_raw_node_out_edges(g: Rep[Graph], x: Rep[DeliteArray[GIterable[Edge]]]): Rep[Unit]
  def graph_set_raw_node_in_neighbors(g: Rep[Graph], x: Rep[DeliteArray[GIterable[Node]]]): Rep[Unit]
  def graph_set_raw_node_out_neighbors(g: Rep[Graph], x: Rep[DeliteArray[GIterable[Node]]]): Rep[Unit]

  def graph_nodes(g: Rep[Graph]): Rep[GIterable[Node]]
  def graph_edges(g: Rep[Graph]): Rep[GIterable[Edge]]
  def graph_num_nodes(g: Rep[Graph]): Rep[Int]
  def graph_num_edges(g: Rep[Graph]): Rep[Int]
  def graph_node(g: Rep[Graph], nId: Rep[Int]): Rep[Node]
  def graph_edge(g: Rep[Graph],eeId: Rep[Int]): Rep[Edge]
  def graph_flip(g: Rep[Graph]): Rep[Graph]
  def graph_inbfs(from: Rep[Node], filter: Option[Rep[Node] => Rep[Boolean]], block: Rep[Node] => Rep[Unit], inReverse: Option[Rep[Node] => Rep[Unit]]): Rep[Unit]
  //def graph_inbfs_reverse(from: Rep[Node], filter: Option[Rep[Node] => Rep[Boolean]], block: Rep[Node] => Rep[Unit], inReverse: Rep[Node] => Rep[Unit]): Rep[Unit]
  def graph_indfs(from: Rep[Node], filter: Option[Rep[Node] => Rep[Boolean]], block: Rep[Node] => Rep[Unit], inPost: Option[Rep[Node] => Rep[Unit]]): Rep[Unit]
  //def graph_randu(numNodes: Rep[Int], numEdges: Rep[Int], seed: Rep[Long]): Rep[Graph]

  def graph_outneighbors(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Node]]
  def graph_inneighbors(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Node]]
  def graph_outedges(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Edge]]
  def graph_inedges(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Edge]]
  def graph_outdegree(g: Rep[Graph], n: Rep[Node]): Rep[Int]
  def graph_indegree(g: Rep[Graph], n: Rep[Node]): Rep[Int]
  def graph_upneighbors(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]]
  def graph_downneighbors(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Node]]
  def graph_upedges(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]]
  def graph_downedges(g: Rep[Graph], n: Rep[Node], visited: Rep[DeliteArray[Int]]): Rep[GIterable[Edge]]
}

trait GraphOpsExp extends GraphOps with EffectExp with NodeOps {
  this: OptiGraphExp =>

  case class GraphObjectNew(isDirected: Exp[Boolean]) extends Def[Graph]
  case class DGraphObjectNew() extends Def[Graph]
  case class UGraphObjectNew() extends Def[Graph]
  case class GraphIsDirected(g: Exp[Graph]) extends Def[Boolean]

  case class GraphRawNodes(g: Exp[Graph]) extends Def[DeliteArray[Node]]
  case class GraphRawEdges(g: Exp[Graph]) extends Def[DeliteArray[Edge]]
  case class GraphSetRawNodes(g: Exp[Graph], x: Exp[DeliteArray[Node]]) extends Def[Unit]
  case class GraphSetRawEdges(g: Exp[Graph], x: Exp[DeliteArray[Edge]]) extends Def[Unit]

  case class GraphRawNodeInEdges(g: Exp[Graph]) extends Def[DeliteArray[GIterable[Edge]]]
  case class GraphRawNodeOutEdges(g: Exp[Graph]) extends Def[DeliteArray[GIterable[Edge]]]
  case class GraphRawNodeInNeighbors(g: Exp[Graph]) extends Def[DeliteArray[GIterable[Node]]]
  case class GraphRawNodeOutNeighbors(g: Exp[Graph]) extends Def[DeliteArray[GIterable[Node]]]
  case class GraphSetRawNodeInEdges(g: Exp[Graph], x: Exp[DeliteArray[GIterable[Edge]]]) extends Def[Unit]
  case class GraphSetRawNodeOutEdges(g: Exp[Graph], x: Exp[DeliteArray[GIterable[Edge]]]) extends Def[Unit]
  case class GraphSetRawNodeInNeighbors(g: Exp[Graph], x: Exp[DeliteArray[GIterable[Node]]]) extends Def[Unit]
  case class GraphSetRawNodeOutNeighbors(g: Exp[Graph], x: Exp[DeliteArray[GIterable[Node]]]) extends Def[Unit]

  case class GraphNodes(g: Exp[Graph])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_nodes_impl(g)))

  case class GraphEdges(g: Exp[Graph])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_edges_impl(g)))

  case class GraphNumNodes(g: Exp[Graph])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_numnodes_impl(g)))

  case class GraphNumEdges(g: Exp[Graph])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_numedges_impl(g)))

  case class GraphNode(g: Exp[Graph], nId: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_getnode_impl(g, nId)))

  case class GraphEdge(g: Exp[Graph], eId: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_getedge_impl(g, eId)))

  case class GraphFlip(g: Exp[Graph])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_flip_impl(g)))

  case class GraphOutNeighbors(g: Exp[Graph], n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_outneighbors_impl(g, n)))

  case class GraphInNeighbors(g: Exp[Graph], n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_inneighbors_impl(g, n)))

  case class GraphOutEdges(g: Exp[Graph], n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_outedges_impl(g, n)))

  case class GraphInEdges(g: Exp[Graph], n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_inedges_impl(g, n)))

  case class GraphOutDegree(g: Exp[Graph], n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_outdegree_impl(g, n)))

  case class GraphInDegree(g: Exp[Graph], n: Exp[Node])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_indegree_impl(g, n)))

  case class GraphUpNeighbors(g: Exp[Graph], n: Exp[Node], visited: Exp[DeliteArray[Int]])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_upneighbors_impl(g, n, visited)))

  case class GraphDownNeighbors(g: Exp[Graph], n: Exp[Node], visited: Exp[DeliteArray[Int]])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_downneighbors_impl(g, n, visited)))

  case class GraphUpEdges(g: Exp[Graph], n: Exp[Node], visited: Exp[DeliteArray[Int]])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_upedges_impl(g, n, visited)))

  case class GraphDownEdges(g: Exp[Graph], n: Exp[Node], visited: Exp[DeliteArray[Int]])
    extends DeliteOpSingleTask(reifyEffectsHere(graph_downedges_impl(g, n, visited)))

  //case class GraphRandUniform(isDirected: Exp[Boolean], numNodes: Exp[Int], numEdges: Exp[Int], seed: Exp[Long]) extends Def[Graph]

  //case class GraphInDFS(from: Exp[Node], filter: Option[Exp[Node] => Exp[Boolean]],
    //  b: Exp[Node] => Exp[Unit], inPost: Option[Exp[Node] => Exp[Unit]])
   //extends DeliteOpSingleTask(reifyEffectsHere(graph_indfs_impl(from, filter, b, inPost)))

  def graph_new(isDirected: Exp[Boolean]) = reflectMutable(GraphObjectNew(isDirected))
  def dgraph_new() = reflectMutable(DGraphObjectNew())
  def ugraph_new() = reflectMutable(UGraphObjectNew())
  def graph_is_directed(g: Exp[Graph]) = reflectPure(GraphIsDirected(g))

  def graph_raw_nodes(g: Exp[Graph]) = reflectPure(GraphRawNodes(g))
  def graph_raw_edges(g: Exp[Graph]) = reflectPure(GraphRawEdges(g))
  def graph_set_raw_nodes(g: Exp[Graph], x: Exp[DeliteArray[Node]]) = reflectWrite(g)(GraphSetRawNodes(g, x))
  def graph_set_raw_edges(g: Exp[Graph], x: Exp[DeliteArray[Edge]]) = reflectWrite(g)(GraphSetRawEdges(g, x))

  def graph_raw_node_in_edges(g: Exp[Graph]) = reflectPure(GraphRawNodeInEdges(g))
  def graph_raw_node_out_edges(g: Exp[Graph]) = reflectPure(GraphRawNodeOutEdges(g))
  def graph_raw_node_in_neighbors(g: Exp[Graph]) = reflectPure(GraphRawNodeInNeighbors(g))
  def graph_raw_node_out_neighbors(g: Exp[Graph]) = reflectPure(GraphRawNodeOutNeighbors(g))
  def graph_set_raw_node_in_edges(g: Exp[Graph], x: Exp[DeliteArray[GIterable[Edge]]]) = reflectWrite(g)(GraphSetRawNodeInEdges(g, x))
  def graph_set_raw_node_out_edges(g: Exp[Graph], x: Exp[DeliteArray[GIterable[Edge]]]) = reflectWrite(g)(GraphSetRawNodeOutEdges(g, x))
  def graph_set_raw_node_in_neighbors(g: Exp[Graph], x: Exp[DeliteArray[GIterable[Node]]]) = reflectWrite(g)(GraphSetRawNodeInNeighbors(g, x))
  def graph_set_raw_node_out_neighbors(g: Exp[Graph], x: Exp[DeliteArray[GIterable[Node]]]) = reflectWrite(g)(GraphSetRawNodeOutNeighbors(g, x))

  def graph_nodes(g: Exp[Graph]) = reflectPure(GraphNodes(g))
  def graph_edges(g: Exp[Graph]) = reflectPure(GraphEdges(g))
  def graph_num_nodes(g: Exp[Graph]) = reflectPure(GraphNumNodes(g))
  def graph_num_edges(g: Exp[Graph]) = reflectPure(GraphNumEdges(g))
  def graph_node(g: Exp[Graph], nId: Exp[Int]) = reflectPure(GraphNode(g, nId))
  def graph_edge(g: Exp[Graph], eId: Exp[Int]) = reflectPure(GraphEdge(g, eId))
  def graph_flip(g: Exp[Graph]) = reflectPure(GraphFlip(g))

  // BFS traversal
  def graph_inbfs(from: Exp[Node], pred: Option[Exp[Node] => Exp[Boolean]], block: Exp[Node] => Exp[Unit], inReverse: Option[Exp[Node] => Exp[Unit]]) = {
    var bfsLevel = var_new(NodeSet())
    bfsLevel.Add(from)
    var depth = var_new(unit(0))
    val visited = DeliteArray[Int](node_graph(from).NumNodes)
    val bfsLevels = HashMap[Int,GIterable[Node]]()

    while(bfsLevel.Size > unit(0)) {
      val items = bfsLevel.Items
      // mark nodes as visited in the current bfs level
      // set to depth+1 to differentiate from unvisited
      items.foreach(n=>{visited(n.Id) = (depth + 1)})
      bfsVisitedDynVar.withValue(visited) {
        // apply the block to each node
        pred match {
           case None => items.foreach(block)
           case Some(p) => items.foreach(n => if(p(n)) { block(n); unit() } else { unit() })
        }
        // compute the next BFS level
        bfsLevel = iter_next_bfs_level(items)
      }
      // record the levels for reverse traversal
      inReverse match {
        case Some(x) => bfsLevels(depth) = items
        case None =>
      }
      depth += 1
    }

    // in reverse-order traversal
    inReverse match {
        case Some(inRev) => {
          depth -= 1
          while(depth >= unit(0)) {
            val items = bfsLevels(depth)
            bfsVisitedDynVar.withValue(visited) {
            	// apply the block to each node
            	pred match {
            		case None => items.foreach(inRev)
            		case Some(p) => items.foreach(n => if(p(n)) { inRev(n); unit() } else { unit() })
            	}
            }
            depth -= 1
          }}
        case None => // do nothing
    }
  }

  // DFS traversal
  def graph_indfs(from: Exp[Node], filter: Option[Exp[Node] => Exp[Boolean]], block: Exp[Node] => Exp[Unit], inPost: Option[Exp[Node] => Exp[Unit]]) = {
    // for pre-order traversal
    val queue = NodeOrder()
    // remember the nodes that have been already visited
    // TODO: node bitmap could be used instead of a set (need to analyze tradeoffs for small/large graphs)
    val visited = NodeSet()
    // for post-order traversal: remember the order in which the nodes where traversed
    val visitedQueue = NodeOrder()

    // in pre-order
    queue.PushFront(from)
    while(queue.Size > unit(0)) {
      val n = queue.PopFront()
      if(!visited.Has(n)) {
        filter match {
          case None => block(n)
          case Some(p) => if(p(n)) { block(n) }
        }
        n.OutNbrs.forseq(i=>{ queue.PushFront(i) })
        visited.Add(n)

        inPost match {
          case Some(x) => visitedQueue.PushFront(n)
          case None =>
        }
        unit()
      }
    }

    // in-post order
    inPost match {
      case Some(inPostBlock) => {
        while(visitedQueue.Size > unit(0)) {
        	val n = visitedQueue.PopFront()
        	inPostBlock(n)
        }
      }
      case None => // do nothing
    }
  }

  //def graph_randu(numNodes: Exp[Int], numEdges: Exp[Int], seed: Exp[Long]) = reflectPure(GraphRandUniform(unit(true), numNodes, numEdges, seed))

  def graph_outneighbors(g: Exp[Graph], n: Exp[Node]) = reflectPure(GraphOutNeighbors(g, n))
  def graph_inneighbors(g: Exp[Graph], n: Exp[Node]) = reflectPure(GraphInNeighbors(g, n))
  def graph_outedges(g: Exp[Graph], n: Exp[Node]) = reflectPure(GraphOutEdges(g, n))
  def graph_inedges(g: Exp[Graph], n: Exp[Node]) = reflectPure(GraphInEdges(g, n))
  def graph_outdegree(g: Exp[Graph], n: Exp[Node]) = reflectPure(GraphOutDegree(g, n))
  def graph_indegree(g: Exp[Graph], n: Exp[Node]) = reflectPure(GraphInDegree(g, n))
  //TODO should these 4 be reflectMutable since the GIterable object is created?
  def graph_upneighbors(g: Exp[Graph], n: Exp[Node], visited: Exp[DeliteArray[Int]]) = reflectPure(GraphUpNeighbors(g, n, visited))
  def graph_downneighbors(g: Exp[Graph], n: Exp[Node], visited: Exp[DeliteArray[Int]]) = reflectPure(GraphDownNeighbors(g, n, visited))
  def graph_upedges(g: Exp[Graph], n: Exp[Node], visited: Exp[DeliteArray[Int]]) = reflectPure(GraphUpEdges(g, n, visited))
  def graph_downedges(g: Exp[Graph], n: Exp[Node], visited: Exp[DeliteArray[Int]]) = reflectPure(GraphDownEdges(g, n, visited))

  //////////////
  // mirroring

  // TODO reflect:
  //GraphIsDirected
  //GraphRawNodes
  //GraphRawEdges
  //GraphSetRawNodes
  //GraphSetRawEdges
  //GraphRawNodeInEdges
  //GraphRawNodeOutEdges
  //GraphRawNodeInNeighbors
  //GraphRawNodeOutNeighbors
  //GraphSetRawNodeInEdges
  //GraphSetRawNodeOutEdges
  //GraphSetRawNodeInNeighbors
  //GraphSetRawNodeOutNeighbors

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case GraphNode(g,x) => graph_node(f(g),f(x))
    case GraphEdge(g,x) => graph_edge(f(g),f(x))
    case GraphNodes(g) => graph_nodes(f(g))
    case GraphEdges(g) => graph_edges(f(g))
    case GraphNumNodes(g) => graph_num_nodes(f(g))
    case GraphNumEdges(g) => graph_num_edges(f(g))
    case GraphFlip(g) => graph_flip(f(g))
    case Reflect(e@GraphNode(g,x), u, es) => reflectMirrored(Reflect(GraphNode(f(g),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphEdge(g,x), u, es) => reflectMirrored(Reflect(GraphEdge(f(g),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphNodes(g), u, es) => reflectMirrored(Reflect(GraphNodes(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphEdges(g), u, es) => reflectMirrored(Reflect(GraphEdges(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphNumNodes(g), u, es) => reflectMirrored(Reflect(GraphNumNodes(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphNumEdges(g), u, es) => reflectMirrored(Reflect(GraphNumEdges(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphFlip(g), u, es) => reflectMirrored(Reflect(GraphFlip(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DGraphObjectNew(), u, es) => reflectMirrored(Reflect(DGraphObjectNew(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@UGraphObjectNew(), u, es) => reflectMirrored(Reflect(UGraphObjectNew(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphObjectNew(x), u, es) => reflectMirrored(Reflect(GraphObjectNew(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    //case Reflect(e@GraphRandUniform(dir,nn,ne,se), u, es) => reflectMirrored(Reflect(GraphRandUniform(f(dir),f(nn),f(ne),f(se)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

}

/* Code generation */

trait BaseGenGraphOps extends GenericNestedCodegen {
  val IR: GraphOpsExp
  import IR._

}

trait ScalaGenGraphOps extends BaseGenGraphOps with ScalaGenBase {
  val IR: GraphOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case g@GraphObjectNew(isDirected) => emitValDef(sym, "new Graph(" + quote(isDirected) + ")")
      case g@DGraphObjectNew() => emitValDef(sym, "new Graph(true)")
      case g@UGraphObjectNew() => emitValDef(sym, "new Graph(false)")
      case GraphIsDirected(g) => emitValDef(sym, quote(g) + ".isDirected")

      case GraphRawNodes(g) => emitValDef(sym, quote(g) + "._nodes")
      case GraphRawEdges(g) => emitValDef(sym, quote(g) + "._edges")
      case GraphSetRawNodes(g, x) => emitValDef(sym, quote(g) + "._nodes = " + quote(x))
      case GraphSetRawEdges(g, x) => emitValDef(sym, quote(g) + "._edges = " + quote(x))

      case GraphRawNodeInEdges(g) => emitValDef(sym, quote(g) + ".nodeInEdges")
      case GraphRawNodeOutEdges(g) => emitValDef(sym, quote(g) + ".nodeOutEdges")
      case GraphRawNodeInNeighbors(g) => emitValDef(sym, quote(g) + ".nodeInNeighbors")
      case GraphRawNodeOutNeighbors(g) => emitValDef(sym, quote(g) + ".nodeOutNeighbors")
      case GraphSetRawNodeInEdges(g, x) => emitValDef(sym, quote(g) + ".nodeInEdges = " + quote(x))
      case GraphSetRawNodeOutEdges(g, x) => emitValDef(sym, quote(g) + ".nodeOutEdges = " + quote(x))
      case GraphSetRawNodeInNeighbors(g, x) => emitValDef(sym, quote(g) + ".nodeInNeighbors = " + quote(x))
      case GraphSetRawNodeOutNeighbors(g, x) => emitValDef(sym, quote(g) + ".nodeOutNeighbors = " + quote(x))
      //case GraphRandUniform(isD, n,m,seed) => emitValDef(sym, "Graph.uniformRandomGraph("+ quote(isD) + "," + quote(n) + "," + quote(m) + "," + quote(seed) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

