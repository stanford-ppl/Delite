package ppl.dsl.optiml.graph

import java.io.{PrintWriter}
import scala.reflect.Manifest
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml._

trait VertexOps extends Variables {
  this: OptiML =>

  object Vertex {
    def apply(g: Rep[Graph], d: Rep[MessageData]) = vertex_new(g,d)
  }
  
  implicit def repToVertexOps(v: Rep[Vertex]) = new VertexOpsCls(v)

  class VertexOpsCls(v: Rep[Vertex]) {
    def edges = vertex_edges(v)
    def neighbors = vertex_neighbors(v)
    def neighborsSelf = vertex_neighbors_self(v)
    def addTask(t: Rep[Vertex]) = vertex_add_task(v, t)
    def clearTasks() = vertex_clear_tasks(v)
    def tasks = vertex_tasks(v)
    
    def data = vertex_get_data(v)
    def graph = vertex_get_graph(v)
    def target(e: Rep[Edge]) = vertex_target(v,e)    
  }

  // object defs
  def vertex_new(g: Rep[Graph], d: Rep[MessageData]): Rep[Vertex]
  
  // class defs
  def vertex_edges(v: Rep[Vertex]): Rep[DenseVector[Edge]]
  def vertex_neighbors(v: Rep[Vertex]): Rep[DenseVector[Vertex]]
  def vertex_neighbors_self(v: Rep[Vertex]): Rep[DenseVector[Vertex]]

  def vertex_tasks(v: Rep[Vertex]): Rep[DenseVector[Vertex]]
  def vertex_clear_tasks(v: Rep[Vertex]): Rep[Unit]
  def vertex_add_task(v: Rep[Vertex], t: Rep[Vertex]): Rep[Unit]
  
  def vertex_get_graph(v: Rep[Vertex]): Rep[Graph]
  def vertex_get_data(v: Rep[Vertex]): Rep[MessageData]
  def vertex_get_tasks(v: Rep[Vertex]): Rep[ArrayBuffer[Vertex]]
  def vertex_target(v: Rep[Vertex], e: Rep[Edge]): Rep[Vertex]  
}

trait VertexOpsExp extends VertexOps with EffectExp {
  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class VertexNew(g: Exp[Graph], d: Exp[MessageData]) extends Def[Vertex]
  case class VertexGetData(v: Exp[Vertex]) extends Def[MessageData]  
  case class VertexGetGraph(v: Exp[Vertex]) extends Def[Graph]  
  case class VertexGetTasks(v: Exp[Vertex]) extends Def[ArrayBuffer[Vertex]]  
  
  //////////////////////////////////////////////////
  // implemented via kernel embedding

  case class VertexEdges(v: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_edges_impl(v)))
  case class VertexNeighbors(v: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_neighbors_impl(v)))
  case class VertexNeighborsSelf(v: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_neighborsself_impl(v)))
  case class VertexTasks(v: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_tasks_impl(v)))
  case class VertexAddTask(v: Exp[Vertex], t: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_addtask_impl(v,t)))
  case class VertexClearTasks(v: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_cleartasks_impl(v)))
  case class VertexTarget(v: Exp[Vertex], e: Exp[Edge]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_target_impl(v,e)))
  
  /////////////////////
  // class interface
  
  def vertex_new(g: Exp[Graph], d: Exp[MessageData]) = reflectPure(VertexNew(g,d))
  def vertex_edges(v: Exp[Vertex]) = reflectPure(VertexEdges(v))
  def vertex_neighbors(v: Exp[Vertex]) = reflectPure(VertexNeighbors(v))
  def vertex_neighbors_self(v: Exp[Vertex]) = reflectPure(VertexNeighborsSelf(v))
  def vertex_tasks(v: Exp[Vertex]) = reflectPure(VertexTasks(v))
  def vertex_clear_tasks(v: Exp[Vertex]) = reflectWrite(v)(VertexClearTasks(v))
  def vertex_add_task(v: Exp[Vertex], t: Exp[Vertex]) = reflectWrite(v)(VertexAddTask(v, t))  
  def vertex_target(v: Exp[Vertex], e: Exp[Edge]) = reflectPure(VertexTarget(v,e))
  
  def vertex_get_graph(v: Exp[Vertex]) = reflectPure(VertexGetGraph(v))
  def vertex_get_data(v: Exp[Vertex]) = reflectPure(VertexGetData(v))
  def vertex_get_tasks(v: Exp[Vertex]) = reflectPure(VertexGetTasks(v))
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
      case VertexNew(g,d) => emitValDef(sym, "new generated.scala.Vertex(" + quote(g) + "," + quote(d) + ")")
      case VertexGetData(v) => emitValDef(sym, quote(v) + "._data")
      case VertexGetGraph(v) => emitValDef(sym, quote(v) + "._graph")      
      case VertexGetTasks(v) => emitValDef(sym, quote(v) + "._tasks")            
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenVertexOps extends BaseGenVertexOps with CudaGenBase with CudaGenDataStruct {
  val IR: VertexOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenVertexOps extends BaseGenVertexOps with CGenBase {
  val IR: VertexOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}