package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}

import reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._
import ppl.delite.framework.datastructures.DeliteArray

trait GraphOps extends Variables {
  this: OptiGraph =>
  
  /** Directed graph constructors */
  object Graph {
    def apply() = dgraph_new()
    def fromArray(array: Rep[DeliteArray[(Int,Int)]]):Rep[Graph] = graph_load_array(array)
    def apply[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String] = unit("|")) = optigraph_table_input_reader(path, shape, separator)
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
    def InDFS(from: Rep[Node], block: Rep[Node] => Rep[Unit]): Rep[Unit] = graph_indfs(g, from, None, block, None)
    def InDFS(from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit]): Rep[Unit] = graph_indfs(g, from, Some(filter), block, None)
    def InDFS(from: Rep[Node], block: Rep[Node] => Rep[Unit], inPost: Rep[Node] => Rep[Unit])(implicit o: Overloaded2): Rep[Unit] = graph_indfs(g,  from, None, block, Some(inPost))
    def InDFS(from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit], inPost: Rep[Node] => Rep[Unit]): Rep[Unit] = graph_indfs(g, from, Some(filter), block, Some(inPost))
    // mutable graph ops (for testing) 
    /** Adds a new node to the graph and returns this node */
    def AddNode: Rep[Node] = graph_add_node(g)
    /** Adds a new edge to the graph and returns this edge */
    def AddEdge(from: Rep[Node], to: Rep[Node]): Rep[Edge] = graph_add_edge(g,from,to)
    /** Freeze the graph, making it immutable */
    def Freeze: Rep[Unit] = graph_freeze(g)
    /** Returns an immutable snapshot of the graph */
    def Snapshot: Rep[Graph] = graph_snapshot(g)

    /* Return incoming neighbors of given node */
    def InNbrs(n: Rep[Node]) = graph_node_in_neighbors(g, n)
    def OutNbrs(n: Rep[Node]) = graph_node_out_neighbors(g, n)
    def Nbrs(n: Rep[Node]) = graph_node_out_neighbors(g, n)
    def HasInNbr(n: Rep[Node], from: Rep[Node]) = graph_node_has_in_neighbor(g, n, from)
    def HasOutNbr(n: Rep[Node], to: Rep[Node]) = graph_node_has_out_neighbor(g, n, to)
  }
  
  def graph_new(): Rep[Graph]
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
  def graph_indfs(g: Rep[Graph], from: Rep[Node], filter: Option[Rep[Node] => Rep[Boolean]], block: Rep[Node] => Rep[Unit], inPost: Option[Rep[Node] => Rep[Unit]]): Rep[Unit]
  def graph_add_node(g: Rep[Graph]): Rep[Node]
  def graph_add_edge(g: Rep[Graph], from: Rep[Node], to: Rep[Node]): Rep[Edge]
  def graph_freeze(g: Rep[Graph]): Rep[Unit]
  def graph_snapshot(g: Rep[Graph]): Rep[Graph]
  def graph_randu(numNodes: Rep[Int], numEdges: Rep[Int], seed: Rep[Long]): Rep[Graph]
  def graph_load(fileName: Rep[String]): Rep[Graph]
  def graph_load_array(array: Rep[DeliteArray[(Int,Int)]]): Rep[Graph]
  def graph_node_in_neighbors(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Node]]
  def graph_node_out_neighbors(g: Rep[Graph], n: Rep[Node]): Rep[GIterable[Node]]
  def graph_node_has_in_neighbor(g: Rep[Graph], n: Rep[Node], from: Rep[Node]): Rep[Boolean]
  def graph_node_has_out_neighbor(g: Rep[Graph], n: Rep[Node], to: Rep[Node]): Rep[Boolean]

  def graph_raw_nodes(g: Rep[Graph])(implicit ctx: SourceContext): Rep[DeliteArray[Node]]
  def graph_raw_edges(g: Rep[Graph])(implicit ctx: SourceContext): Rep[DeliteArray[Edge]]
  def graph_raw_nodeoutedges(g: Rep[Graph], n: Rep[Node])(implicit ctx: SourceContext): Rep[GIterable[Edge]]
  def graph_raw_nodeinedges(g: Rep[Graph], n: Rep[Node])(implicit ctx: SourceContext): Rep[GIterable[Edge]]
  def graph_raw_nodeoutneighbors(g: Rep[Graph], n: Rep[Node])(implicit ctx: SourceContext): Rep[GIterable[Node]]
  def graph_raw_nodeinneighbors(g: Rep[Graph], n: Rep[Node])(implicit ctx: SourceContext): Rep[GIterable[Node]]
  def graph_set_raw_nodes(g: Rep[Graph], newVal: Rep[DeliteArray[Node]])(implicit ctx: SourceContext): Rep[Unit] 
  def graph_set_raw_edges(g: Rep[Graph], newVal: Rep[DeliteArray[Edge]])(implicit ctx: SourceContext): Rep[Unit]
  def graph_set_raw_nodeoutedges(g: Rep[Graph], n: Rep[Node], newVal: Rep[GIterable[Edge]])(implicit ctx: SourceContext): Rep[Unit]
  def graph_set_raw_nodeinedges(g: Rep[Graph], n: Rep[Node], newVal: Rep[GIterable[Edge]])(implicit ctx: SourceContext): Rep[Unit]
  def graph_set_raw_nodeoutneighbors(g: Rep[Graph], n: Rep[Node], newVal: Rep[GIterable[Node]])(implicit ctx: SourceContext): Rep[Unit]
  def graph_set_raw_nodeinneighbors(g: Rep[Graph], n: Rep[Node], newVal: Rep[GIterable[Node]])(implicit ctx: SourceContext): Rep[Unit]
  def graph_set_raw_inneighbors(g: Rep[Graph], newVal: Rep[DeliteArray[GIterable[Node]]])(implicit ctx: SourceContext): Rep[Unit]
  def graph_set_raw_outneighbors(g: Rep[Graph], newVal: Rep[DeliteArray[GIterable[Node]]])(implicit ctx: SourceContext): Rep[Unit]
  def optigraph_table_input_reader[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]): Rep[DeliteArray[T]]

}

trait GraphOpsExp extends GraphOps with EffectExp with NodeOps {
  this: GraphImplOps with OptiGraphExp =>

  case class DGraphObjectNew() extends Def[Graph]
  case class UGraphObjectNew() extends Def[Graph]
  //case class GraphNodes(g: Exp[Graph]) extends Def[GIterable[Node]]
  case class GraphNodes(g: Exp[Graph]) extends DeliteOpSingleTask(reifyEffectsHere(graph_nodes_impl(g)))
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
  //case class GraphLoad(fileName: Exp[String]) extends Def[Graph]
  case class GraphLoadFile(fileName: Exp[String]) extends DeliteOpSingleTask(reifyEffectsHere(graph_load_impl(fileName)))
  case class GraphLoadArray(array: Exp[DeliteArray[(Int,Int)]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_load_array_impl(array)))
  case class OptiGraphTableInputReader[T:Manifest](readBlock: Block[DeliteArray[T]]) extends DeliteOpSingleTask(readBlock) {
    val mT = manifest[T]
  }
 
  case class GraphObjectNew() extends DeliteStruct[Graph] {
    val elems = copyTransformedElems(collection.Seq("isImmutable" -> unit(true),
                                                    "_nodes" -> var_new(DeliteArray[Node](0)).e,
                                                    //"_edges" -> var_new(DeliteArray[Edge](0)).e,
                                                    //"_nodeOutEdges" -> var_new(DeliteArray[GIterable[Edge]](0)).e, 
                                                    //"_nodeInEdges" -> var_new(DeliteArray[GIterable[Edge]](0)).e,
                                                    "_nodeOutNeighbors" -> var_new(DeliteArray[GIterable[Node]](0)).e,
                                                    "_nodeInNeighbors" -> var_new(DeliteArray[GIterable[Node]](0)).e))
  }

  def graph_raw_nodes(g: Exp[Graph])(implicit ctx: SourceContext) = field[DeliteArray[Node]](g, "_nodes")
  def graph_raw_edges(g: Exp[Graph])(implicit ctx: SourceContext) = field[DeliteArray[Edge]](g, "_edges")
  def graph_raw_nodeoutedges(g: Exp[Graph], n: Exp[Node])(implicit ctx: SourceContext) = field[DeliteArray[GIterable[Edge]]](g, "_nodeOutEdges").apply(n.Id)
  def graph_raw_nodeinedges(g: Exp[Graph], n: Exp[Node])(implicit ctx: SourceContext) = field[DeliteArray[GIterable[Edge]]](g, "nodeInEdges").apply(n.Id)
  def graph_raw_nodeoutneighbors(g: Exp[Graph], n: Exp[Node])(implicit ctx: SourceContext) = field[DeliteArray[GIterable[Node]]](g, "_nodeOutNeighbors").apply(n.Id)
  def graph_raw_nodeinneighbors(g: Exp[Graph], n: Exp[Node])(implicit ctx: SourceContext) = field[DeliteArray[GIterable[Node]]](g, "_nodeInNeighbors").apply(n.Id)
  def graph_set_raw_nodes(g: Exp[Graph], newVal: Exp[DeliteArray[Node]])(implicit ctx: SourceContext) = field_update[DeliteArray[Node]](g, "_nodes", newVal)
  def graph_set_raw_edges(g: Exp[Graph], newVal: Exp[DeliteArray[Edge]])(implicit ctx: SourceContext) = field_update[DeliteArray[Edge]](g, "_edges", newVal)
  def graph_set_raw_nodeoutedges(g: Exp[Graph], n: Exp[Node], newVal: Exp[GIterable[Edge]])(implicit ctx: SourceContext) = field[DeliteArray[GIterable[Edge]]](g, "_nodeOutEdges").update(n.Id, newVal)
  def graph_set_raw_nodeinedges(g: Exp[Graph], n: Exp[Node], newVal: Exp[GIterable[Edge]])(implicit ctx: SourceContext) = field[DeliteArray[GIterable[Edge]]](g, "nodeInEdges").update(n.Id, newVal)
  def graph_set_raw_nodeoutneighbors(g: Exp[Graph], n: Exp[Node], newVal: Exp[GIterable[Node]])(implicit ctx: SourceContext) = field[DeliteArray[GIterable[Node]]](g, "_nodeOutNeighbors").update(n.Id, newVal)
  def graph_set_raw_nodeinneighbors(g: Exp[Graph], n: Exp[Node], newVal: Exp[GIterable[Node]])(implicit ctx: SourceContext) = field[DeliteArray[GIterable[Node]]](g, "_nodeInNeighbors").update(n.Id, newVal)
  def graph_set_raw_inneighbors(g: Exp[Graph], newVal: Exp[DeliteArray[GIterable[Node]]])(implicit ctx: SourceContext) = field_update[DeliteArray[GIterable[Node]]](g, "_nodeInNeighbors", newVal)
  def graph_set_raw_outneighbors(g: Exp[Graph], newVal: Exp[DeliteArray[GIterable[Node]]])(implicit ctx: SourceContext) = field_update[DeliteArray[GIterable[Node]]](g, "_nodeOutNeighbors", newVal)
 
  case class GraphNodeInNeighbors(g: Exp[Graph], n: Exp[Node]) extends Def[GIterable[Node]]
  case class GraphNodeOutNeighbors(g: Exp[Graph], n: Exp[Node]) extends Def[GIterable[Node]]

  //case class GraphInDFS(from: Exp[Node], filter: Option[Exp[Node] => Exp[Boolean]], 
    //  b: Exp[Node] => Exp[Unit], inPost: Option[Exp[Node] => Exp[Unit]]) 
   //extends DeliteOpSingleTask(reifyEffectsHere(graph_indfs_impl(from, filter, b, inPost)))
 
  def graph_new() = reflectMutable(GraphObjectNew())
  def dgraph_new() = reflectMutable(DGraphObjectNew())
  def ugraph_new() = reflectMutable(UGraphObjectNew())
  def graph_nodes(g: Exp[Graph]) = {
    val nodes = graph_raw_nodes(g)
    new_iterable(nodes, unit(0), nodes.length)
  }
  def graph_edges(g: Exp[Graph]) = reflectPure(GraphEdges(g))
  def graph_num_nodes(g: Exp[Graph]) = field[DeliteArray[Node]](g, "_nodes").length
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
  def graph_indfs(g: Exp[Graph], from: Exp[Node], filter: Option[Exp[Node] => Exp[Boolean]], block: Exp[Node] => Exp[Unit], inPost: Option[Exp[Node] => Exp[Unit]]) = {
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
        g.OutNbrs(n).forseq(i=>{ queue.PushFront(i) })
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
  
  def graph_node_in_neighbors(g: Exp[Graph], n: Exp[Node]) = field[DeliteArray[GIterable[Node]]](g, "_nodeInNeighbors").apply(n.Id)
  def graph_node_out_neighbors(g: Exp[Graph], n: Exp[Node]) = field[DeliteArray[GIterable[Node]]](g,"_nodeOutNeighbors").apply(n.Id)
  def graph_node_has_in_neighbor(g: Exp[Graph], n: Exp[Node], from: Exp[Node]) = graph_node_in_neighbors(g,n).contains(from)
  def graph_node_has_out_neighbor(g: Exp[Graph], n: Exp[Node], to: Exp[Node]) = graph_node_out_neighbors(g,n).contains(to)

  // mutable graph operations
  def graph_add_node(g: Exp[Graph]) = reflectWrite(g)(GraphAddNode(g))
  def graph_add_edge(g: Exp[Graph], from: Rep[Node], to: Rep[Node]) = reflectWrite(g)(GraphAddEdge(g,from,to))
  def graph_freeze(g: Exp[Graph]) = reflectWrite(g)(GraphFreeze(g))
  def graph_snapshot(g: Exp[Graph]) = reflectPure(GraphSnapshot(g))
  def graph_randu(numNodes: Exp[Int], numEdges: Exp[Int], seed: Exp[Long]) = reflectPure(GraphRandUniform(unit(true), numNodes, numEdges, seed))
  def graph_load(fileName: Exp[String]) = reflectPure(GraphLoadFile(fileName))
  def graph_load_array(array: Exp[DeliteArray[(Int,Int)]]) = reflectPure(GraphLoadArray(array))
  def optigraph_table_input_reader[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]) = {
    reflectEffect(OptiGraphTableInputReader(reifyEffectsHere(optigraph_table_input_reader_impl(path,shape,separator))))
  }
  
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
  
  //////////////
  // mirroring
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case GraphObjectNew() => reflectPure(new { override val original = Some(f,e) } with GraphObjectNew())(mtype(manifest[A]),implicitly[SourceContext])
    case GraphNodeInNeighbors(g,n) => graph_node_in_neighbors(f(g),f(n)) //relfectPure(new { override val original = Some(f,e) } with GraphNodeInNeighbors(f(g),f(n)))(mtype(manifest[A]),implicitly[SourceContext])
    case GraphNodeOutNeighbors(g,n) => graph_node_out_neighbors(f(g),f(n)) //relfectPure(new { override val original = Some(f,e) } with GraphNodeOutNeighbors(f(g),f(n)))(mtype(manifest[A]),implicitly[SourceContext])
    case GraphNode(g,x) => graph_node(f(g),f(x))
    case GraphEdge(g,x) => graph_edge(f(g),f(x))
    case GraphNodes(g) => reflectPure(new { override val original = Some(f,e) } with GraphNodes(f(g)))(mtype(manifest[A]),implicitly[SourceContext])
    case GraphEdges(g) => graph_edges(f(g))
    case GraphNumNodes(g) => graph_num_nodes(f(g))
    case GraphNumEdges(g) => graph_num_edges(f(g))
    case GraphFlip(g) => graph_flip(f(g))
    case GraphSnapshot(g) => graph_snapshot(f(g))
    //case GraphLoad(fn) => graph_load(f(fn))
    case GraphLoadFile(fn) => reflectPure(new { override val original = Some(f,e) } with GraphLoadFile(f(fn)))(mtype(manifest[A]),implicitly[SourceContext])
    case GraphLoadArray(a) => reflectPure(new { override val original = Some(f,e) } with GraphLoadArray(f(a)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@OptiGraphTableInputReader(block) => reflectPure(new { override val original = Some(f,e) } with OptiGraphTableInputReader(f(block))(e.mT))(mtype(manifest[A]),implicitly[SourceContext])      
    case GraphRandUniform(dir,nn,ne,se) => graph_randu(f(nn),f(ne),f(se))
    case Reflect(e@GraphObjectNew(), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GraphObjectNew(), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphNodeInNeighbors(g,n), u, es) => reflectMirrored(Reflect(GraphNodeInNeighbors(f(g),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx) //reflectMirrored(Reflect(new { override val original = Some(f,e) } with GraphNodeInNeighbors(f(g),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphNodeOutNeighbors(g,n), u, es) => reflectMirrored(Reflect(GraphNodeOutNeighbors(f(g),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx) //reflectMirrored(Reflect(new { override val original = Some(f,e) } with GraphNodeOutNeighbors(f(g),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphNode(g,x), u, es) => reflectMirrored(Reflect(GraphNode(f(g),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphEdge(g,x), u, es) => reflectMirrored(Reflect(GraphEdge(f(g),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphNodes(g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GraphNodes(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphEdges(g), u, es) => reflectMirrored(Reflect(GraphEdges(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphNumNodes(g), u, es) => reflectMirrored(Reflect(GraphNumNodes(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphNumEdges(g), u, es) => reflectMirrored(Reflect(GraphNumEdges(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphFlip(g), u, es) => reflectMirrored(Reflect(GraphFlip(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphSnapshot(g), u, es) => reflectMirrored(Reflect(GraphSnapshot(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    //case Reflect(e@GraphLoad(fn), u, es) => reflectMirrored(Reflect(GraphLoad(f(fn)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GraphLoadFile(fn), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GraphLoadFile(f(fn)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphLoadArray(a), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GraphLoadArray(f(a)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@OptiGraphTableInputReader(block), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with OptiGraphTableInputReader(f(block))(e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DGraphObjectNew(), u, es) => reflectMirrored(Reflect(DGraphObjectNew(), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@UGraphObjectNew(), u, es) => reflectMirrored(Reflect(UGraphObjectNew(), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphAddNode(g), u, es) => reflectMirrored(Reflect(GraphAddNode(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphAddEdge(g,fr,to), u, es) => reflectMirrored(Reflect(GraphAddEdge(f(g),f(fr),f(to)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphFreeze(g), u, es) => reflectMirrored(Reflect(GraphFreeze(f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GraphRandUniform(dir,nn,ne,se), u, es) => reflectMirrored(Reflect(GraphRandUniform(f(dir),f(nn),f(ne),f(se)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
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
      case g@DGraphObjectNew() => emitValDef(sym, "new Graph(true)")
      case g@UGraphObjectNew() => emitValDef(sym, "new Graph(false)")
      //case GraphNodes(g) => emitValDef(sym, quote(g) + ".nodes")
      case GraphEdges(g) => emitValDef(sym, quote(g) + ".edges")
      //case GraphNumNodes(g) => emitValDef(sym, quote(g) + ".numNodes")
      case GraphNumEdges(g) => emitValDef(sym, quote(g) + ".numEdges")
      case GraphNode(g, nId) => emitValDef(sym, quote(g) + ".getNode(" + quote(nId) + ")")
      case GraphEdge(g, eId) => emitValDef(sym, quote(g) + ".getEdge(" + quote(eId) + ")")
      case GraphAddNode(g) => emitValDef(sym, quote(g) + ".addNode")
      case GraphAddEdge(g,from,to) => emitValDef(sym, quote(g) + ".addEdge(" + quote(from) + "," + quote(to) + ")")
      case GraphFreeze(g) => emitValDef(sym, quote(g) + ".freeze")
      case GraphSnapshot(g) => emitValDef(sym, quote(g) + ".snapshot")
      case GraphFlip(g) => emitValDef(sym, quote(g) + ".reverse")
      case GraphRandUniform(isD, n,m,seed) => emitValDef(sym, "Graph.uniformRandomGraph("+ quote(isD) + "," + quote(n) + "," + quote(m) + "," + quote(seed) + ")")
      //case GraphLoad(f) => emitValDef(sym, "Graph.loadGraph("+ quote(f) + ")")
      //case GraphNodeInNeighbors(g,n) => emitValDef(sym, quote(g) + ".inNeighbors(" + quote(n) + ")")
      //case GraphNodeOutNeighbors(g,n) => emitValDef(sym, quote(g) + ".outNeighbors(" + quote(n) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

