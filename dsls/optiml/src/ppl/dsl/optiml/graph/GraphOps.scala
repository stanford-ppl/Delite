package ppl.dsl.optiml.graph

import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import ppl.dsl.optiml.datastruct.scala._
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen, CGenBase, CudaGenBase, ScalaGenBase}
import ppl.dsl.optiml.{OptiMLExp, OptiML}

trait GraphOps extends DSLType with Variables {
  this: OptiML =>

  object Graph {
    def apply[V <: Vertex, E <: Edge]()(implicit mV: Manifest[V], mE: Manifest[E]) = graph_obj_new()
  }

  implicit def repGraphToGraphOps[V <: Vertex, E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]) = new graphOpsCls(g)

  class graphOpsCls[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]) {
    def vertices = graph_vertices(g)
    def edges = graph_edges(g)
    //def adjacent(a: V, b: V) = graph_adjacent(g,a,b)
    def neighborsOf(a: V) = graph_neighbors_of(g,a)
    def edgesOf(a: V) = graph_edges_of(g,a)
    def containsEdge(a: E) = graph_contains_edge(g,a)
    def containsVertex(a: V) = graph_contains_vertex(g,a)

    def addVertex(a: V) = graph_add_vertex(g,a)
    def addEdge(e: E, a: V, b: V) = graph_add_edge(g,e,a,b)
    //def removeEdge(a: V, b: V) = graph_remove_edge(g,a,b)
    def freeze() = graph_freeze(g)
    def frozen = graph_frozen(g)
  }


  // object defs
  def graph_obj_new[V <: Vertex,E <: Edge]()(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Graph[V,E]]

  // class defs
  def graph_vertices[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Vertices[V]]
  def graph_edges[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Edges[E]]
  //def graph_adjacent[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Boolean]
  def graph_neighbors_of[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Vertices[V]]
  def graph_edges_of[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Edges[E]]
  def graph_contains_edge[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Edge])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Boolean]
  def graph_contains_vertex[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Boolean]

  def graph_add_vertex[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Unit]
  def graph_add_edge[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], e: Rep[Edge], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Unit]
  //def graph_remove_edge[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Unit]
  def graph_freeze[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Unit]
  def graph_frozen[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Boolean]
}

trait GraphOpsExp extends GraphOps with EffectExp {

  this: GraphImplOps with OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GraphObjectNew[V <: Vertex,E <: Edge]()(implicit mV: Manifest[V], mE: Manifest[E])
    extends Def[Graph[V,E]] {
    val mG = manifest[BidirectionalGraphImpl[V,E]]
  }
  case class GraphVertices[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Vertices[V]]
  case class GraphEdges[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Edges[E]]
  //case class GraphAdjacent[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Boolean]
  case class GraphNeighborsOf[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Vertices[V]]
  case class GraphEdgesOf[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Edges[E]]
  case class GraphContainsEdge[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Edge])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Boolean]
  case class GraphContainsVertex[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Boolean]

  case class GraphAddVertex[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Unit]
  case class GraphAddEdge[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], e: Rep[Edge], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Unit]
  //case class GraphRemoveEdge[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Unit]
  case class GraphFreeze[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Unit]
  case class GraphFrozen[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]) extends Def[Boolean]

  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)



  ////////////////////////////////
  // implemented via delite ops




  /////////////////////
  // object interface

  def graph_obj_new[V <: Vertex,E <: Edge]()(implicit mV: Manifest[V], mE: Manifest[E]) = reflectEffect(GraphObjectNew())


  /////////////////////
  // class interface

  def graph_vertices[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E])
    = GraphVertices(reflectRead(g))
  def graph_edges[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E])
    = GraphEdges(reflectRead(g))
  //def graph_adjacent[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E])
  //  = GraphAdjacent(reflectRead(g),reflectRead(a),reflectRead(b))
  def graph_neighbors_of[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E])
    = GraphNeighborsOf(reflectRead(g),reflectRead(a))
  def graph_edges_of[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E])
    = GraphEdgesOf(reflectRead(g),reflectRead(a))
  def graph_contains_edge[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Edge])(implicit mV: Manifest[V], mE: Manifest[E])
    = GraphContainsEdge(reflectRead(g),reflectRead(a))
  def graph_contains_vertex[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E])
    = GraphContainsVertex(reflectRead(g),reflectRead(a))

  def graph_add_vertex[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E])
    = reflectMutation(GraphAddVertex(reflectReadWrite(g),a))
  def graph_add_edge[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], e: Rep[Edge], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E])
    = reflectMutation(GraphAddEdge(reflectReadWrite(g),e,a,b))
  //def graph_remove_edge[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E])
  //  = reflectMutation(GraphRemoveEdge(reflectReadWrite(g),a,b))
  def graph_freeze[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E])
    = reflectMutation(GraphFreeze(reflectReadWrite(g)))
  def graph_frozen[V <: Vertex,E <: Edge](g: Rep[Graph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E])
    = GraphSorted(reflectRead(g))
}


trait BaseGenGraphOps extends GenericNestedCodegen {
  val IR: GraphOpsExp
  import IR._

  //override def syms(e: Any): List[Sym[Any]] = e match {
    //case _ => super.syms(e)
  //}
}

trait ScalaGenGraphOps extends BaseGenGraphOps with ScalaGenBase {
  val IR: GraphOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case g@GraphObjectNew() => emitValDef(sym, "new " + remap(g.mG) + "()")
      case GraphVertices(g) => emitValDef(sym, quote(g) + ".vertices")
      case GraphEdges(g) => emitValDef(sym, quote(g) + ".edges")
      //case GraphAdjacent(g,a,b) => emitValDef(sym, quote(g) + ".adjacent(" + quote(a) + "," + quote(b) + ")")
      case GraphNeighborsOf(g,a) => emitValDef(sym, quote(g) + ".neighborsOf(" + quote(a) + ")")
      case GraphEdgesOf(g,a) => emitValDef(sym, quote(g) + ".edgesOf(" + quote(a) + ")")
      case GraphContainsEdge(g,a) => emitValDef(sym, quote(g) + ".containsEdge(" + quote(a) + ")")
      case GraphContainsVertex(g,a) => emitValDef(sym, quote(g) + ".containsVertex(" + quote(a) + ")")

      case GraphAddVertex(g,a) => emitValDef(sym, quote(g) + ".addVertex(" + quote(a) + ")")
      case GraphAddEdge(g,e,a,b) => emitValDef(sym, quote(g) + ".addEdge(" + quote(e) + "," + quote(a) + "," + quote(b) + ")")
      //case GraphRemoveEdge(g,a,b) => emitValDef(sym, quote(g) + ".removeEdge(" + quote(a) + "," + quote(b) + ")")
      case GraphFreeze(g) => emitValDef(sym, quote(g) + ".freeze()")
      case GraphFrozen(g) => emitValDef(sym, quote(g) + ".frozen")
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenGraphOps extends BaseGenGraphOps with CudaGenBase with CudaGenDataStruct {
  val IR: GraphOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenGraphOps extends BaseGenGraphOps with CGenBase {
  val IR: GraphOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}