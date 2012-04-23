package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._

trait GraphOps extends Variables {
  this: OptiGraph =>
  
  /** Directed graph constructors */
  object Graph {
    def apply() = dgraph_new()
  }
  object DGraph {
    def apply() = dgraph_new()
  }
  /** Undirected graph constructors */
  object UGraph {
    def apply() = ugraph_new()
  }
  
  object RandUniformGraph {
    def apply(numNodes: Rep[Int], numEdges: Rep[Int], seed: Rep[Long]) = graph_randu(numNodes: Rep[Int], numEdges: Rep[Int], seed: Rep[Long])
  }
    
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
    // mutable graph ops (for testing) 
    /** Adds a new node to the graph and returns this node */
    def AddNode: Rep[Node] = graph_add_node(g)
    /** Adds a new edge to the graph and returns this edge */
    def AddEdge(from: Rep[Node], to: Rep[Node]): Rep[Edge] = graph_add_edge(g,from,to)
    /** Freeze the graph, making it immutable */
    def Freeze: Rep[Unit] = graph_freeze(g)
    /** Returns an immutable snapshot of the graph */
    def Snapshot: Rep[Graph] = graph_snapshot(g)
  }
  
  def dgraph_new(): Rep[Graph]
  def ugraph_new(): Rep[Graph]
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
  def graph_add_node(g: Rep[Graph]): Rep[Node]
  def graph_add_edge(g: Rep[Graph], from: Rep[Node], to: Rep[Node]): Rep[Edge]
  def graph_freeze(g: Rep[Graph]): Rep[Unit]
  def graph_snapshot(g: Rep[Graph]): Rep[Graph]
  def graph_randu(numNodes: Rep[Int], numEdges: Rep[Int], seed: Rep[Long]): Rep[Graph]
  def graph_load(fileName: Rep[String]): Rep[Graph]
}

trait GraphOpsExp extends GraphOps with EffectExp with NodeOps {
  this: OptiGraphExp =>

  case class DGraphObjectNew() extends Def[Graph]
  case class UGraphObjectNew() extends Def[Graph]
  case class GraphNodes(g: Exp[Graph]) extends Def[GIterable[Node]]
  case class GraphEdges(g: Exp[Graph]) extends Def[GIterable[Edge]]
  case class GraphNumNodes(g: Exp[Graph]) extends Def[Int]
  case class GraphNumEdges(g: Exp[Graph]) extends Def[Int]
  case class GraphNode(g: Exp[Graph], nId: Exp[Int]) extends Def[Node]
  case class GraphEdge(g: Exp[Graph], eId: Exp[Int]) extends Def[Edge]
  case class GraphFlip(g: Exp[Graph]) extends Def[Graph]
  
  // mutable ops (for testing)
  case class GraphAddNode(g: Exp[Graph]) extends Def[Node]
  case class GraphAddEdge(g: Exp[Graph], from: Rep[Node], to: Rep[Node]) extends Def[Edge]
  case class GraphFreeze(g: Exp[Graph]) extends Def[Unit]
  case class GraphSnapshot(g: Exp[Graph]) extends Def[Graph]
  case class GraphRandUniform(isDirected: Exp[Boolean], numNodes: Exp[Int], numEdges: Exp[Int], seed: Exp[Long]) extends Def[Graph]
  case class GraphLoad(fileName: Exp[String]) extends Def[Graph]
  
  //case class GraphInDFS(from: Exp[Node], filter: Option[Exp[Node] => Exp[Boolean]], 
    //  b: Exp[Node] => Exp[Unit], inPost: Option[Exp[Node] => Exp[Unit]]) 
   //extends DeliteOpSingleTask(reifyEffectsHere(graph_indfs_impl(from, filter, b, inPost)))
 
  def dgraph_new() = reflectMutable(DGraphObjectNew())
  def ugraph_new() = reflectMutable(UGraphObjectNew())
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
    val visited = NewArray[Int](node_graph(from).NumNodes)
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
  
  // mutable graph operations
  def graph_add_node(g: Exp[Graph]) = reflectWrite(g)(GraphAddNode(g))
  def graph_add_edge(g: Exp[Graph], from: Rep[Node], to: Rep[Node]) = reflectWrite(g)(GraphAddEdge(g,from,to))
  def graph_freeze(g: Exp[Graph]) = reflectWrite(g)(GraphFreeze(g))
  def graph_snapshot(g: Exp[Graph]) = reflectPure(GraphSnapshot(g))
  def graph_randu(numNodes: Exp[Int], numEdges: Exp[Int], seed: Exp[Long]) = reflectPure(GraphRandUniform(unit(true), numNodes, numEdges, seed))
  def graph_load(fileName: Exp[String]) = reflectPure(GraphLoad(fileName))
  
  // alias/sharing
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case GraphSnapshot(g) => Nil
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
}


/* Code generation */

trait BaseGenGraphOps extends GenericNestedCodegen {
  val IR: GraphOpsExp
  import IR._

}

trait ScalaGenGraphOps extends BaseGenGraphOps with ScalaGenBase {
  val IR: GraphOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case g@DGraphObjectNew() => emitValDef(sym, "new Graph(true)")
      case g@UGraphObjectNew() => emitValDef(sym, "new Graph(false)")
      case GraphNodes(g) => emitValDef(sym, quote(g) + ".nodes")
      case GraphEdges(g) => emitValDef(sym, quote(g) + ".edges")
      case GraphNumNodes(g) => emitValDef(sym, quote(g) + ".numNodes")
      case GraphNumEdges(g) => emitValDef(sym, quote(g) + ".numEdges")
      case GraphNode(g, nId) => emitValDef(sym, quote(g) + ".getNode(" + quote(nId) + ")")
      case GraphEdge(g, eId) => emitValDef(sym, quote(g) + ".getEdge(" + quote(eId) + ")")
      case GraphAddNode(g) => emitValDef(sym, quote(g) + ".addNode")
      case GraphAddEdge(g,from,to) => emitValDef(sym, quote(g) + ".addEdge(" + quote(from) + "," + quote(to) + ")")
      case GraphFreeze(g) => emitValDef(sym, quote(g) + ".freeze")
      case GraphSnapshot(g) => emitValDef(sym, quote(g) + ".snapshot")
      case GraphFlip(g) => emitValDef(sym, quote(g) + ".reverse")
      case GraphRandUniform(isD, n,m,seed) => emitValDef(sym, "Graph.uniformRandomGraph("+ quote(isD) + "," + quote(n) + "," + quote(m) + "," + quote(seed) + ")")
      case GraphLoad(f) => emitValDef(sym, "Graph.loadGraph("+ quote(f) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

