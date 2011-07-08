package ppl.dsl.optiml.graph

import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import ppl.dsl.optiml.datastruct.scala._
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optiml.{OptiMLExp, OptiML}

trait VertexOps extends DSLType with Variables {
  this: OptiML =>

  // base class vertices cannot be instantiated at the moment

  implicit def repVertexToVertexOps[V<:Vertex:Manifest](v: Rep[V]) = new vertexOpsCls(v)

  class vertexOpsCls[V<:Vertex:Manifest](v: Rep[V]) {
    def edges = vertex_edges(v)
    def neighbors = vertex_neighbors(v)
    def neighborsSelf = vertex_neighbors_self(v)
    def addTask(t: Rep[V]) = vertex_add_task(v, t)
    def clearTasks() = vertex_clear_tasks(v)
    def tasks = vertex_tasks(v)
  }

  // class defs
  def vertex_edges[V<:Vertex](v: Rep[V]): Rep[Edges[Edge]]
  def vertex_neighbors[V<:Vertex:Manifest](v: Rep[V]): Rep[Vertices[V]]
  def vertex_neighbors_self[V<:Vertex:Manifest](v: Rep[V]): Rep[Vertices[V]]

  def vertex_tasks[V<:Vertex:Manifest](v: Rep[V]) : Rep[Vertices[V]]
  def vertex_clear_tasks[V<:Vertex](v: Rep[V]) : Rep[Unit]
  def vertex_add_task[V<:Vertex](v: Rep[V], t: Rep[V]) : Rep[Unit]
}

trait VertexOpsExp extends VertexOps with EffectExp {
  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class VertexEdges[V<:Vertex](v: Exp[V]) extends Def[Edges[Edge]]
  case class VertexNeighbors[V<:Vertex:Manifest](v: Exp[V]) extends Def[Vertices[V]]
  case class VertexNeighborsSelf[V<:Vertex:Manifest](v: Exp[V]) extends Def[Vertices[V]]

  case class VertexTasks[V<:Vertex:Manifest](v: Exp[V]) extends Def[Vertices[V]]
  case class VertexAddTask[V<:Vertex](v: Exp[V], t: Exp[V]) extends Def[Unit]
  case class VertexClearTasks[V<:Vertex](v: Exp[V]) extends Def[Unit]

  /////////////////////
  // class interface

  def vertex_edges[V<:Vertex](v: Exp[V]) = reflectPure(VertexEdges(v))
  def vertex_neighbors[V<:Vertex:Manifest](v: Exp[V]) = reflectPure(VertexNeighbors(v))
  def vertex_neighbors_self[V<:Vertex:Manifest](v: Exp[V]) = reflectPure(VertexNeighborsSelf(v))

  def vertex_tasks[V<:Vertex:Manifest](v: Exp[V]) = reflectPure(VertexTasks(v))
  def vertex_clear_tasks[V<:Vertex](v: Exp[V]) = reflectWrite(v)(VertexClearTasks(v))
  def vertex_add_task[V<:Vertex](v: Exp[V], t: Exp[V]) = reflectWrite(v)(VertexAddTask(v, t))
}


trait BaseGenVertexOps extends GenericNestedCodegen {
  val IR: VertexOpsExp
  import IR._
}

trait ScalaGenVertexOps extends BaseGenVertexOps with ScalaGenBase {
  val IR: VertexOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case VertexEdges(v) => emitValDef(sym, quote(v) + ".edges")
      case VertexNeighbors(v) => emitValDef(sym, quote(v) + ".neighbors")
      case VertexNeighborsSelf(v) => emitValDef(sym, quote(v) + ".neighborsSelf")
      case VertexTasks(v) => emitValDef(sym, quote(v) + ".tasks")
      case VertexAddTask(v,t) => emitValDef(sym, quote(v) + ".addTask(" + quote(t) + ")")
      case VertexClearTasks(v) => emitValDef(sym, quote(v) + ".clearTasks()")
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenVertexOps extends BaseGenVertexOps with CudaGenBase with CudaGenDataStruct {
  val IR: VertexOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVertexOps extends BaseGenVertexOps with CGenBase {
  val IR: VertexOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}