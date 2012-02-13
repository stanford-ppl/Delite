package ppl.dsl.optiml.graph

import ppl.dsl.optiml.CudaGenDataStruct
import java.io.{PrintWriter}

import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import scala.collection.mutable.HashMap
import ppl.dsl.optiml._

trait GraphOps extends Variables {
  this: OptiML =>

  object Graph {
    def apply() = graph_obj_new()
  }

  implicit def repGraphToGraphOps(g: Rep[Graph]) = new GraphOpsCls(g)

  class GraphOpsCls(g: Rep[Graph]) {
    def vertices = graph_vertices(g)
    def edges = graph_edges(g)
    //def adjacent(a: V, b: V) = graph_adjacent(g,a,b)
    def neighborsOf(a: Rep[Vertex]) = graph_neighbors_of(g,a)
    def neighborsSelfOf(a: Rep[Vertex]) = graph_neighbors_self_of(g,a)
    def edgesOf(a: Rep[Vertex]) = graph_edges_of(g,a)
    def containsEdge(a: Rep[Edge]) = graph_contains_edge(g,a)
    def containsVertex(a: Rep[Vertex]) = graph_contains_vertex(g,a)
    def addVertex(a: Rep[Vertex]) = graph_add_vertex(g,a)
    def addEdge(e: Rep[Edge], a: Rep[Vertex], b: Rep[Vertex]) = graph_add_edge(g,e,a,b)
    //def removeEdge(a: V, b: V) = graph_remove_edge(g,a,b)
    def freeze() = graph_freeze(g)
    def frozen = graph_frozen(g)
    def clearTasks(i: Rep[Int]) = graph_cleartasks(g,i)
  }

  // object defs
  def graph_obj_new(): Rep[Graph]

  // class defs
  def graph_vertices(g: Rep[Graph]): Rep[DenseVector[Vertex]]
  def graph_edges(g: Rep[Graph]): Rep[DenseVector[Edge]]
  //def graph_adjacent(g: Rep[Graph], a: Rep[Vertex], b: Rep[Vertex]): Rep[Boolean]
  def graph_neighbors_of(g: Rep[Graph], a: Rep[Vertex]): Rep[DenseVector[Vertex]]
  def graph_neighbors_self_of(g: Rep[Graph], a: Rep[Vertex]): Rep[DenseVector[Vertex]]
  def graph_edges_of(g: Rep[Graph], a: Rep[Vertex]): Rep[DenseVector[Edge]]
  def graph_contains_edge(g: Rep[Graph], a: Rep[Edge]): Rep[Boolean]
  def graph_contains_vertex(g: Rep[Graph], a: Rep[Vertex]): Rep[Boolean]
  def graph_add_vertex(g: Rep[Graph], a: Rep[Vertex]): Rep[Unit]
  def graph_add_edge(g: Rep[Graph], e: Rep[Edge], a: Rep[Vertex], b: Rep[Vertex]): Rep[Unit]
  //def graph_remove_edge(g: Rep[Graph], a: Rep[Vertex], b: Rep[Vertex]): Rep[Unit]
  def graph_freeze(g: Rep[Graph]): Rep[Unit]
  def graph_frozen(g: Rep[Graph]): Rep[Boolean]  
  def graph_cleartasks(g: Rep[Graph], i: Rep[Int]): Rep[Unit]
}

trait GraphCompilerOps extends GraphOps {
  this: OptiML =>

  def graph_get_edgetovertices(g: Rep[Graph]): Rep[HashMap[Edge, (Vertex, Vertex)]] 
  def graph_get_adjacencies(g: Rep[Graph]): Rep[HashMap[Vertex, List[(Edge, Vertex)]]]
  def graph_get_vertexids(g: Rep[Graph]): Rep[HashMap[Vertex, Int]]
  def graph_get_vertices(g: Rep[Graph]): Rep[Array[Vertex]]
  def graph_get_edges(g: Rep[Graph]): Rep[Array[Edge]]
  def graph_get_vertexedges(g: Rep[Graph]): Rep[Array[DenseVector[Edge]]]
  def graph_get_neighbors(g: Rep[Graph]): Rep[Array[DenseVector[Vertex]]]
  def graph_get_neighborsself(g: Rep[Graph]): Rep[Array[DenseVector[Vertex]]]
  
  def graph_set_edgetovertices(g: Rep[Graph], x: Rep[HashMap[Edge, (Vertex, Vertex)]]): Rep[Unit]
  def graph_set_adjacencies(g: Rep[Graph], x: Rep[HashMap[Vertex, List[(Edge, Vertex)]]]): Rep[Unit]
  def graph_set_vertexids(g: Rep[Graph], x: Rep[HashMap[Vertex, Int]]): Rep[Unit]
  def graph_set_vertices(g: Rep[Graph], x: Rep[Array[Vertex]]): Rep[Unit]
  def graph_set_edges(g: Rep[Graph], x: Rep[Array[Edge]]): Rep[Unit]
  def graph_set_vertexedges(g: Rep[Graph], x: Rep[Array[DenseVector[Edge]]]): Rep[Unit]
  def graph_set_neighbors(g: Rep[Graph], x: Rep[Array[DenseVector[Vertex]]]): Rep[Unit]
  def graph_set_neighborsself(g: Rep[Graph], x: Rep[Array[DenseVector[Vertex]]]): Rep[Unit]
  def graph_set_frozen(g: Rep[Graph], x: Rep[Boolean]): Rep[Unit]
}

trait GraphOpsExp extends GraphCompilerOps with EffectExp {
  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GraphObjectNew() extends Def[Graph]
  case class GraphGetEdgeToVertices(g: Exp[Graph]) extends Def[HashMap[Edge, (Vertex, Vertex)]]
  case class GraphGetAdjacencies(g: Exp[Graph]) extends Def[HashMap[Vertex, List[(Edge,Vertex)]]] 
  case class GraphGetVertexIds(g: Exp[Graph]) extends Def[HashMap[Vertex, Int]]
  case class GraphGetVertices(g: Exp[Graph]) extends Def[Array[Vertex]]
  case class GraphGetEdges(g: Exp[Graph]) extends Def[Array[Edge]]
  case class GraphGetVertexEdges(g: Exp[Graph]) extends Def[Array[DenseVector[Edge]]]
  case class GraphGetNeighbors(g: Exp[Graph]) extends Def[Array[DenseVector[Vertex]]]
  case class GraphGetNeighborsSelf(g: Exp[Graph]) extends Def[Array[DenseVector[Vertex]]]     
  case class GraphFrozen(g: Exp[Graph]) extends Def[Boolean]    
  
  case class GraphSetEdgeToVertices(g: Exp[Graph], x: Exp[HashMap[Edge, (Vertex, Vertex)]]) extends Def[Unit]
  case class GraphSetAdjacencies(g: Exp[Graph], x: Exp[HashMap[Vertex, List[(Edge, Vertex)]]]) extends Def[Unit]
  case class GraphSetVertexIds(g: Exp[Graph], x: Exp[HashMap[Vertex, Int]]) extends Def[Unit]
  case class GraphSetVertices(g: Exp[Graph], x: Exp[Array[Vertex]]) extends Def[Unit]
  case class GraphSetEdges(g: Exp[Graph], x: Exp[Array[Edge]]) extends Def[Unit]
  case class GraphSetVertexEdges(g: Exp[Graph], x: Exp[Array[DenseVector[Edge]]]) extends Def[Unit]
  case class GraphSetNeighbors(g: Exp[Graph], x: Exp[Array[DenseVector[Vertex]]]) extends Def[Unit]
  case class GraphSetNeighborsSelf(g: Exp[Graph], x: Exp[Array[DenseVector[Vertex]]]) extends Def[Unit]
  case class GraphSetFrozen(g: Exp[Graph], x: Exp[Boolean]) extends Def[Unit]

  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)
  
  case class GraphVertices(g: Exp[Graph]) extends DeliteOpSingleTask(reifyEffectsHere(graph_vertices_impl(g)))
  case class GraphEdges(g: Exp[Graph]) extends DeliteOpSingleTask(reifyEffectsHere(graph_edges_impl(g)))
  //case class GraphAdjacent(g: Exp[Graph], a: Exp[Vertex], b: Exp[Vertex]) extends Def[Boolean]
  case class GraphNeighborsOf(g: Exp[Graph], a: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(graph_neighborsof_impl(g,a)))
  case class GraphNeighborsSelfOf(g: Exp[Graph], a: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(graph_neighborsselfof_impl(g,a)))
  case class GraphEdgesOf(g: Exp[Graph], a: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(graph_edgesof_impl(g,a)))
  case class GraphContainsEdge(g: Exp[Graph], a: Exp[Edge]) extends DeliteOpSingleTask(reifyEffectsHere(graph_containsedge_impl(g,a)))
  case class GraphContainsVertex(g: Exp[Graph], a: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(graph_containsvertex_impl(g,a)))
  case class GraphAddVertex(g: Exp[Graph], a: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(graph_addvertex_impl(g,a)))
  case class GraphAddEdge(g: Exp[Graph], e: Exp[Edge], a: Exp[Vertex], b: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(graph_addedge_impl(g,e,a,b)))
  //case class GraphRemoveEdge(g: Exp[Graph], a: Exp[Vertex], b: Exp[Vertex]) extends Def[Unit]
  case class GraphFreeze(g: Exp[Graph]) extends DeliteOpSingleTask(reifyEffectsHere(graph_freeze_impl(g)))
  case class GraphClearTasks(g: Exp[Graph], i: Exp[Int]) extends DeliteOpSingleTask(reifyEffectsHere(graph_cleartasks_impl(g,i)))
  
  /////////////////////
  // object interface

  def graph_obj_new() = reflectMutable(GraphObjectNew())

  /////////////////////
  // class interface

  def graph_vertices(g: Exp[Graph]) = reflectPure(GraphVertices(g))
  def graph_edges(g: Exp[Graph]) = reflectPure(GraphEdges(g))
  //def graph_adjacent(g: Exp[Graph], a: Exp[Vertex], b: Exp[Vertex]) = GraphAdjacent(/*reflectRead*/(g),/*reflectRead*/(a),/*reflectRead*/(b))
  def graph_neighbors_of(g: Exp[Graph], a: Exp[Vertex]) = reflectPure(GraphNeighborsOf(g,a))
  def graph_neighbors_self_of(g: Exp[Graph], a: Exp[Vertex]) = reflectPure(GraphNeighborsSelfOf(g,a))
  def graph_edges_of(g: Exp[Graph], a: Exp[Vertex]) = reflectPure(GraphEdgesOf(g,a))
  def graph_contains_edge(g: Exp[Graph], a: Exp[Edge]) = reflectPure(GraphContainsEdge(g,a))
  def graph_contains_vertex(g: Exp[Graph], a: Exp[Vertex]) = reflectPure(GraphContainsVertex(g,a))
  def graph_add_vertex(g: Exp[Graph], a: Exp[Vertex]) = reflectWrite(g)(GraphAddVertex(g,a))
  def graph_add_edge(g: Exp[Graph], e: Exp[Edge], a: Exp[Vertex], b: Exp[Vertex]) = reflectWrite(g)(GraphAddEdge(g,e,a,b))
  //def graph_remove_edge(g: Exp[Graph], a: Exp[Vertex], b: Exp[Vertex]) = reflectWrite(g)(reflectMutation(GraphRemoveEdge(g,a,b))
  def graph_freeze(g: Exp[Graph]) = reflectWrite(g)(GraphFreeze(g))
  def graph_frozen(g: Exp[Graph]) = reflectPure(GraphFrozen(g))
  def graph_cleartasks(g: Exp[Graph], i: Exp[Int]) = reflectWrite(g)(GraphClearTasks(g,i))
  
  
  ///////////////////
  // internal
  
  def graph_get_edgetovertices(g: Exp[Graph]) = reflectPure(GraphGetEdgeToVertices(g))
  def graph_get_adjacencies(g: Exp[Graph]) = reflectPure(GraphGetAdjacencies(g))
  def graph_get_vertexids(g: Exp[Graph]) = reflectPure(GraphGetVertexIds(g))
  def graph_get_vertices(g: Exp[Graph]) = reflectPure(GraphGetVertices(g))
  def graph_get_edges(g: Exp[Graph]) = reflectPure(GraphGetEdges(g))
  def graph_get_vertexedges(g: Exp[Graph]) = reflectPure(GraphGetVertexEdges(g))
  def graph_get_neighbors(g: Exp[Graph]) = reflectPure(GraphGetNeighbors(g))
  def graph_get_neighborsself(g: Exp[Graph]) = reflectPure(GraphGetNeighborsSelf(g))
  
  def graph_set_edgetovertices(g: Exp[Graph], x: Exp[HashMap[Edge, (Vertex, Vertex)]]) = reflectPure(GraphSetEdgeToVertices(g,x))
  def graph_set_adjacencies(g: Exp[Graph], x: Exp[HashMap[Vertex, List[(Edge, Vertex)]]]) = reflectPure(GraphSetAdjacencies(g,x))
  def graph_set_vertexids(g: Exp[Graph], x: Exp[HashMap[Vertex, Int]]) = reflectPure(GraphSetVertexIds(g,x))
  def graph_set_vertices(g: Exp[Graph], x: Exp[Array[Vertex]]) = reflectPure(GraphSetVertices(g,x))
  def graph_set_edges(g: Exp[Graph], x: Exp[Array[Edge]]) = reflectPure(GraphSetEdges(g,x))
  def graph_set_vertexedges(g: Exp[Graph], x: Exp[Array[DenseVector[Edge]]]) = reflectPure(GraphSetVertexEdges(g,x))
  def graph_set_neighbors(g: Exp[Graph], x: Exp[Array[DenseVector[Vertex]]]) = reflectPure(GraphSetNeighbors(g,x))
  def graph_set_neighborsself(g: Exp[Graph], x: Exp[Array[DenseVector[Vertex]]]) = reflectPure(GraphSetNeighborsSelf(g,x))
  def graph_set_frozen(g: Exp[Graph], x: Exp[Boolean]) = reflectPure(GraphSetFrozen(g,x))
}


trait BaseGenGraphOps extends GenericNestedCodegen {
  val IR: GraphOpsExp
  import IR._

}

trait ScalaGenGraphOps extends BaseGenGraphOps with ScalaGenBase {
  val IR: GraphOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case GraphObjectNew() => emitValDef(sym, "new generated.scala.Graph()")
      case GraphGetEdgeToVertices(g) => emitValDef(sym, quote(g) + "._edgeToVertices")
      case GraphGetAdjacencies(g) => emitValDef(sym, quote(g) + "._adjaciences")
      case GraphGetVertexIds(g) => emitValDef(sym, quote(g) + "._vertexIds")
      case GraphGetVertices(g) => emitValDef(sym, quote(g) + "._vertices")
      case GraphGetEdges(g) => emitValDef(sym, quote(g) + "._edges")
      case GraphGetVertexEdges(g) => emitValDef(sym, quote(g) + "._vertexEdges")
      case GraphGetNeighbors(g) => emitValDef(sym, quote(g) + "._neighbors")
      case GraphGetNeighborsSelf(g) => emitValDef(sym, quote(g) + "._neighborsSelf")
      case GraphFrozen(g) => emitValDef(sym, quote(g) + "._frozen")
      
      case GraphSetEdgeToVertices(g,x) => emitValDef(sym, quote(g) + "._edgeToVertices = " + quote(x))
      case GraphSetAdjacencies(g,x) => emitValDef(sym, quote(g) + "._adjaciences = " + quote(x))
      case GraphSetVertexIds(g,x) => emitValDef(sym, quote(g) + "._vertexIds = " + quote(x))
      case GraphSetVertices(g,x) => emitValDef(sym, quote(g) + "._vertices = " + quote(x))
      case GraphSetEdges(g,x) => emitValDef(sym, quote(g) + "._edges = " + quote(x))
      case GraphSetVertexEdges(g,x) => emitValDef(sym, quote(g) + "._vertexEdges = " + quote(x))
      case GraphSetNeighbors(g,x) => emitValDef(sym, quote(g) + "._neighbors = " + quote(x))
      case GraphSetNeighborsSelf(g,x) => emitValDef(sym, quote(g) + "._neighborsSelf = " + quote(x))
      case GraphSetFrozen(g,x) => emitValDef(sym, quote(g) + "._frozen = " + quote(x))      
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenGraphOps extends BaseGenGraphOps with CudaGenBase with CudaGenDataStruct {
  val IR: GraphOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenGraphOps extends BaseGenGraphOps with CGenBase {
  val IR: GraphOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}