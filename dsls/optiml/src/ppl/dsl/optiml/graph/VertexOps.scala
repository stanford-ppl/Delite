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

trait VertexOps extends DSLType with Variables {
  this: OptiML =>

  // base class vertices cannot be instantiated at the moment

  implicit def repVertexToVertexOps(v: Rep[Vertex]) = new vertexOpsCls(v)

  class vertexOpsCls(v: Rep[Vertex]) {
    def edges = vertex_edges(v)
    def neighbors = vertex_neighbors(v)
    def addTask(t: Rep[Vertex]) = vertex_add_task(v, t)
    def clearTasks() = vertex_clear_tasks(v)
    def tasks = vertex_tasks(v)
  }

  // class defs
  def vertex_edges(v: Rep[Vertex]): Rep[Edges[Edge]]
  def vertex_neighbors(v: Rep[Vertex]): Rep[Vertices[Vertex]]

  def vertex_tasks(v: Rep[Vertex]) : Rep[Vertices[Vertex]]
  def vertex_clear_tasks(v: Rep[Vertex]) : Rep[Unit]
  def vertex_add_task(v: Rep[Vertex], t: Rep[Vertex]) : Rep[Unit]
}

trait VertexOpsExp extends VertexOps with EffectExp {

  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class VertexEdges(v: Exp[Vertex]) extends Def[Edges[Edge]]
  case class VertexNeighbors(v: Exp[Vertex]) extends Def[Vertices[Vertex]]

  case class VertexTasks(v: Exp[Vertex]) extends Def[Vertices[Vertex]]
  case class VertexAddTask(v: Exp[Vertex], t: Exp[Vertex]) extends Def[Unit]
  case class VertexClearTasks(v: Exp[Vertex]) extends Def[Unit]

  /////////////////////
  // class interface

  def vertex_edges(v: Exp[Vertex]) = VertexEdges(v)
  def vertex_neighbors(v: Exp[Vertex]) = VertexNeighbors(v)

  def vertex_tasks(v: Exp[Vertex]) = VertexTasks(v)
  def vertex_clear_tasks(v: Exp[Vertex]) = VertexClearTasks(v)
  def vertex_add_task(v: Exp[Vertex], t: Exp[Vertex]) = VertexAddTask(v, t)
}


trait BaseGenVertexOps extends GenericNestedCodegen {
  val IR: VertexOpsExp
  import IR._

  //override def syms(e: Any): List[Sym[Any]] = e match {
    //case _ => super.syms(e)
  //}
}

trait ScalaGenVertexOps extends BaseGenVertexOps with ScalaGenBase {
  val IR: VertexOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case VertexEdges(v) => emitValDef(sym, quote(v) + ".edges")
      case VertexNeighbors(v) => emitValDef(sym, quote(v) + ".neighbors")
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

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVertexOps extends BaseGenVertexOps with CGenBase {
  val IR: VertexOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}