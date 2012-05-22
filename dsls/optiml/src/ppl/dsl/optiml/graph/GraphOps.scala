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
    def apply[VD:Manifest,ED:Manifest]() = graph_obj_new[VD,ED]()
  }

  implicit def repGraphToGraphOps[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]) = new GraphOpsCls(g)

  class GraphOpsCls[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]) {
    def vertices = graph_vertices(g)
    def edges = graph_edges(g)
    //def adjacent(a: V, b: V) = graph_adjacent(g,a,b)
    def neighborsOf(a: Rep[Vertex[VD,ED]]) = graph_neighbors_of(g,a)
    def neighborsSelfOf(a: Rep[Vertex[VD,ED]]) = graph_neighbors_self_of(g,a)
    def edgesOf(a: Rep[Vertex[VD,ED]]) = graph_edges_of(g,a)
    def containsEdge(a: Rep[Edge[VD,ED]]) = graph_contains_edge(g,a)
    def containsVertex(a: Rep[Vertex[VD,ED]]) = graph_contains_vertex(g,a)
    def addVertex(a: Rep[Vertex[VD,ED]]) = graph_add_vertex(g,a)
    def addEdge(e: Rep[Edge[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]) = graph_add_edge(g,e,a,b)
    //def removeEdge(a: V, b: V) = graph_remove_edge(g,a,b)
    def freeze() = graph_freeze(g)
    def frozen = graph_frozen(g)
    def clearTasks(i: Rep[Int]) = graph_cleartasks(g,i)
  }

  // object defs
  def graph_obj_new[VD:Manifest,ED:Manifest](): Rep[Graph[VD,ED]]

  // class defs
  def graph_vertices[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def graph_edges[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[DenseVector[Edge[VD,ED]]]
  //def graph_adjacent[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Boolean]
  def graph_neighbors_of[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def graph_neighbors_self_of[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def graph_edges_of[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[DenseVector[Edge[VD,ED]]]
  def graph_contains_edge[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Edge[VD,ED]]): Rep[Boolean]
  def graph_contains_vertex[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[Boolean]
  def graph_add_vertex[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]]): Rep[Unit]
  def graph_add_edge[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], e: Rep[Edge[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Unit]
  //def graph_remove_edge[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Unit]
  def graph_freeze[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[Unit]
  def graph_frozen[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[Boolean]  
  def graph_cleartasks[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], i: Rep[Int]): Rep[Unit]
}

trait GraphCompilerOps extends GraphOps {
  this: OptiML =>

  def graph_get_edgetovertices[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[HashMap[Edge[VD,ED], (Vertex[VD,ED], Vertex[VD,ED])]] 
  def graph_get_adjacencies[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[HashMap[Vertex[VD,ED], List[(Edge[VD,ED], Vertex[VD,ED])]]]
  def graph_get_vertexids[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[HashMap[Vertex[VD,ED], Int]]
  def graph_get_vertices[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[Array[Vertex[VD,ED]]]
  def graph_get_edges[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[Array[Edge[VD,ED]]]
  def graph_get_vertexedges[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[Array[DenseVector[Edge[VD,ED]]]]
  def graph_get_neighbors[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[Array[DenseVector[Vertex[VD,ED]]]]
  def graph_get_neighborsself[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]]): Rep[Array[DenseVector[Vertex[VD,ED]]]]
  
  def graph_set_edgetovertices[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], x: Rep[HashMap[Edge[VD,ED], (Vertex[VD,ED], Vertex[VD,ED])]]): Rep[Unit]
  def graph_set_adjacencies[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], x: Rep[HashMap[Vertex[VD,ED], List[(Edge[VD,ED], Vertex[VD,ED])]]]): Rep[Unit]
  def graph_set_vertexids[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], x: Rep[HashMap[Vertex[VD,ED], Int]]): Rep[Unit]
  def graph_set_vertices[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], x: Rep[Array[Vertex[VD,ED]]]): Rep[Unit]
  def graph_set_edges[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], x: Rep[Array[Edge[VD,ED]]]): Rep[Unit]
  def graph_set_vertexedges[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], x: Rep[Array[DenseVector[Edge[VD,ED]]]]): Rep[Unit]
  def graph_set_neighbors[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], x: Rep[Array[DenseVector[Vertex[VD,ED]]]]): Rep[Unit]
  def graph_set_neighborsself[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], x: Rep[Array[DenseVector[Vertex[VD,ED]]]]): Rep[Unit]
  def graph_set_frozen[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], x: Rep[Boolean]): Rep[Unit]
}

trait GraphOpsExp extends GraphCompilerOps with EffectExp {
  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GraphObjectNew[VD:Manifest,ED:Manifest]() extends Def[Graph[VD,ED]] {
    val mVD = manifest[VD]
    val mED = manifest[ED]
  }
  case class GraphGetEdgeToVertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends Def[HashMap[Edge[VD,ED], (Vertex[VD,ED], Vertex[VD,ED])]]
  case class GraphGetAdjacencies[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends Def[HashMap[Vertex[VD,ED], List[(Edge[VD,ED],Vertex[VD,ED])]]] 
  case class GraphGetVertexIds[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends Def[HashMap[Vertex[VD,ED], Int]]
  case class GraphGetVertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends Def[Array[Vertex[VD,ED]]]
  case class GraphGetEdges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends Def[Array[Edge[VD,ED]]]
  case class GraphGetVertexEdges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends Def[Array[DenseVector[Edge[VD,ED]]]]
  case class GraphGetNeighbors[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends Def[Array[DenseVector[Vertex[VD,ED]]]]
  case class GraphGetNeighborsSelf[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends Def[Array[DenseVector[Vertex[VD,ED]]]]     
  case class GraphFrozen[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends Def[Boolean]    
  
  case class GraphSetEdgeToVertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[HashMap[Edge[VD,ED], (Vertex[VD,ED], Vertex[VD,ED])]]) extends Def[Unit]
  case class GraphSetAdjacencies[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[HashMap[Vertex[VD,ED], List[(Edge[VD,ED], Vertex[VD,ED])]]]) extends Def[Unit]
  case class GraphSetVertexIds[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[HashMap[Vertex[VD,ED], Int]]) extends Def[Unit]
  case class GraphSetVertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[Vertex[VD,ED]]]) extends Def[Unit]
  case class GraphSetEdges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[Edge[VD,ED]]]) extends Def[Unit]
  case class GraphSetVertexEdges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[DenseVector[Edge[VD,ED]]]]) extends Def[Unit]
  case class GraphSetNeighbors[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[DenseVector[Vertex[VD,ED]]]]) extends Def[Unit]
  case class GraphSetNeighborsSelf[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[DenseVector[Vertex[VD,ED]]]]) extends Def[Unit]
  case class GraphSetFrozen[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Boolean]) extends Def[Unit]

  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)
  
  case class GraphVertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_vertices_impl(g)))
  case class GraphEdges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_edges_impl(g)))
  //case class GraphAdjacent[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]], b: Exp[Vertex[VD,ED]]) extends Def[Boolean]
  case class GraphNeighborsOf[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_neighborsof_impl(g,a)))
  case class GraphNeighborsSelfOf[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_neighborsselfof_impl(g,a)))
  case class GraphEdgesOf[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_edgesof_impl(g,a)))
  case class GraphContainsEdge[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Edge[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_containsedge_impl(g,a)))
  case class GraphContainsVertex[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_containsvertex_impl(g,a)))
  case class GraphAddVertex[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_addvertex_impl(g,a)))
  case class GraphAddEdge[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], e: Exp[Edge[VD,ED]], a: Exp[Vertex[VD,ED]], b: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_addedge_impl(g,e,a,b)))
  //case class GraphRemoveEdge[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]], b: Exp[Vertex[VD,ED]]) extends Def[Unit]
  case class GraphFreeze[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(graph_freeze_impl(g)))
  case class GraphClearTasks[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], i: Exp[Int]) extends DeliteOpSingleTask(reifyEffectsHere(graph_cleartasks_impl(g,i)))
  
  /////////////////////
  // object interface

  def graph_obj_new[VD:Manifest,ED:Manifest]() = reflectMutable(GraphObjectNew())

  /////////////////////
  // class interface

  def graph_vertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphVertices(g))
  def graph_edges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphEdges(g))
  //def graph_adjacent[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]], b: Exp[Vertex[VD,ED]]) = GraphAdjacent(/*reflectRead*/(g),/*reflectRead*/(a),/*reflectRead*/(b))
  def graph_neighbors_of[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) = reflectPure(GraphNeighborsOf(g,a))
  def graph_neighbors_self_of[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) = reflectPure(GraphNeighborsSelfOf(g,a))
  def graph_edges_of[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) = reflectPure(GraphEdgesOf(g,a))
  def graph_contains_edge[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Edge[VD,ED]]) = reflectPure(GraphContainsEdge(g,a))
  def graph_contains_vertex[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) = reflectPure(GraphContainsVertex(g,a))
  def graph_add_vertex[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]]) = reflectWrite(g)(GraphAddVertex(g,a))
  def graph_add_edge[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], e: Exp[Edge[VD,ED]], a: Exp[Vertex[VD,ED]], b: Exp[Vertex[VD,ED]]) = reflectWrite(g)(GraphAddEdge(g,e,a,b))
  //def graph_remove_edge[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], a: Exp[Vertex[VD,ED]], b: Exp[Vertex[VD,ED]]) = reflectWrite(g)(reflectMutation(GraphRemoveEdge(g,a,b))
  def graph_freeze[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectWrite(g)(GraphFreeze(g))
  def graph_frozen[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphFrozen(g))
  def graph_cleartasks[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], i: Exp[Int]) = reflectWrite(g)(GraphClearTasks(g,i))
  
  
  ///////////////////
  // internal
  
  def graph_get_edgetovertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphGetEdgeToVertices(g))
  def graph_get_adjacencies[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphGetAdjacencies(g))
  def graph_get_vertexids[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphGetVertexIds(g))
  def graph_get_vertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphGetVertices(g))
  def graph_get_edges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphGetEdges(g))
  def graph_get_vertexedges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphGetVertexEdges(g))
  def graph_get_neighbors[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphGetNeighbors(g))
  def graph_get_neighborsself[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]]) = reflectPure(GraphGetNeighborsSelf(g))
  
  def graph_set_edgetovertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[HashMap[Edge[VD,ED], (Vertex[VD,ED], Vertex[VD,ED])]]) = reflectWrite(g)(GraphSetEdgeToVertices(g,x))
  def graph_set_adjacencies[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[HashMap[Vertex[VD,ED], List[(Edge[VD,ED], Vertex[VD,ED])]]]) = reflectWrite(g)(GraphSetAdjacencies(g,x))
  def graph_set_vertexids[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[HashMap[Vertex[VD,ED], Int]]) = reflectWrite(g)(GraphSetVertexIds(g,x))
  def graph_set_vertices[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[Vertex[VD,ED]]]) = reflectWrite(g)(GraphSetVertices(g,x))
  def graph_set_edges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[Edge[VD,ED]]]) = reflectWrite(g)(GraphSetEdges(g,x))
  def graph_set_vertexedges[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[DenseVector[Edge[VD,ED]]]]) = reflectWrite(g)(GraphSetVertexEdges(g,x))
  def graph_set_neighbors[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[DenseVector[Vertex[VD,ED]]]]) = reflectWrite(g)(GraphSetNeighbors(g,x))
  def graph_set_neighborsself[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Array[DenseVector[Vertex[VD,ED]]]]) = reflectWrite(g)(GraphSetNeighborsSelf(g,x))
  def graph_set_frozen[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], x: Exp[Boolean]) = reflectWrite(g)(GraphSetFrozen(g,x))
}


trait BaseGenGraphOps extends GenericNestedCodegen {
  val IR: GraphOpsExp
  import IR._

}

trait ScalaGenGraphOps extends BaseGenGraphOps with ScalaGenBase {
  val IR: GraphOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case g@GraphObjectNew() => emitValDef(sym, "new generated.scala.Graph[" + remap(g.mVD) + "," + remap(g.mED) + "]()")
      case GraphGetEdgeToVertices(g) => emitValDef(sym, quote(g) + "._edgeToVertices")
      case GraphGetAdjacencies(g) => emitValDef(sym, quote(g) + "._adjacencies")
      case GraphGetVertexIds(g) => emitValDef(sym, quote(g) + "._vertexIds")
      case GraphGetVertices(g) => emitValDef(sym, quote(g) + "._vertices")
      case GraphGetEdges(g) => emitValDef(sym, quote(g) + "._edges")
      case GraphGetVertexEdges(g) => emitValDef(sym, quote(g) + "._vertexEdges")
      case GraphGetNeighbors(g) => emitValDef(sym, quote(g) + "._neighbors")
      case GraphGetNeighborsSelf(g) => emitValDef(sym, quote(g) + "._neighborsSelf")
      case GraphFrozen(g) => emitValDef(sym, quote(g) + "._frozen")
      
      case GraphSetEdgeToVertices(g,x) => emitValDef(sym, quote(g) + "._edgeToVertices = " + quote(x))
      case GraphSetAdjacencies(g,x) => emitValDef(sym, quote(g) + "._adjacencies = " + quote(x))
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

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenGraphOps extends BaseGenGraphOps with CGenBase {
  val IR: GraphOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}