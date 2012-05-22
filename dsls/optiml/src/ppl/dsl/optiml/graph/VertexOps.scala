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
    def apply[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], d: Rep[VD]) = vertex_new(g,d)
  }
  
  implicit def repToVertexOps[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]) = new VertexOpsCls(v)

  class VertexOpsCls[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]) {
    def edges = vertex_edges(v)
    def neighbors = vertex_neighbors(v)
    def neighborsSelf = vertex_neighbors_self(v)
    def addTask(t: Rep[Vertex[VD,ED]]) = vertex_add_task(v, t)
    def clearTasks() = vertex_clear_tasks(v)
    def tasks = vertex_tasks(v)
    
    def data = vertex_get_data(v)
    def graph = vertex_get_graph(v)
    def target(e: Rep[Edge[VD,ED]]) = vertex_target(v,e)    
  }

  // object defs
  def vertex_new[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], d: Rep[VD]): Rep[Vertex[VD,ED]]
  
  // class defs
  def vertex_edges[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Edge[VD,ED]]]
  def vertex_neighbors[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def vertex_neighbors_self[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]

  def vertex_tasks[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[DenseVector[Vertex[VD,ED]]]
  def vertex_clear_tasks[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[Unit]
  def vertex_add_task[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]], t: Rep[Vertex[VD,ED]]): Rep[Unit]
  
  def vertex_get_graph[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[Graph[VD,ED]]
  def vertex_get_data[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[VD]
  def vertex_get_tasks[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]]): Rep[ArrayBuffer[Vertex[VD,ED]]]
  def vertex_target[VD:Manifest,ED:Manifest](v: Rep[Vertex[VD,ED]], e: Rep[Edge[VD,ED]]): Rep[Vertex[VD,ED]]  
}

trait VertexOpsExp extends VertexOps with EffectExp {
  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class VertexNew[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], d: Exp[VD]) extends Def[Vertex[VD,ED]]{
    val mVD = manifest[VD]
    val mED = manifest[ED]
  }
  case class VertexGetData[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) extends Def[VD]  
  case class VertexGetGraph[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) extends Def[Graph[VD,ED]]  
  case class VertexGetTasks[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) extends Def[ArrayBuffer[Vertex[VD,ED]]]  
  
  //////////////////////////////////////////////////
  // implemented via kernel embedding

  case class VertexEdges[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_edges_impl(v)))
  case class VertexNeighbors[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_neighbors_impl(v)))
  case class VertexNeighborsSelf[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_neighborsself_impl(v)))
  case class VertexTasks[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_tasks_impl(v)))
  case class VertexAddTask[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]], t: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_addtask_impl(v,t)))
  case class VertexClearTasks[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_cleartasks_impl(v)))
  case class VertexTarget[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]], e: Exp[Edge[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(vertex_target_impl(v,e)))
  
  /////////////////////
  // class interface
  
  def vertex_new[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], d: Exp[VD]) = reflectPure(VertexNew(g.unsafeImmutable,d))
  def vertex_edges[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) = reflectPure(VertexEdges(v))
  def vertex_neighbors[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) = reflectPure(VertexNeighbors(v))
  def vertex_neighbors_self[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) = reflectPure(VertexNeighborsSelf(v))
  def vertex_tasks[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) = reflectPure(VertexTasks(v))
  def vertex_clear_tasks[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) = reflectWrite(v)(VertexClearTasks(v))
  def vertex_add_task[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]], t: Exp[Vertex[VD,ED]]) = reflectWrite(v)(VertexAddTask(v, t))  
  def vertex_target[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]], e: Exp[Edge[VD,ED]]) = reflectPure(VertexTarget(v,e))
  
  def vertex_get_graph[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) = reflectPure(VertexGetGraph(v))
  def vertex_get_data[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) = reflectPure(VertexGetData(v))
  def vertex_get_tasks[VD:Manifest,ED:Manifest](v: Exp[Vertex[VD,ED]]) = reflectPure(VertexGetTasks(v))
}


trait BaseGenVertexOps extends GenericNestedCodegen {
  val IR: VertexOpsExp
  import IR._
}

trait ScalaGenVertexOps extends BaseGenVertexOps with ScalaGenBase {
  val IR: VertexOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case v@VertexNew(g,d) => emitValDef(sym, "new generated.scala.Vertex[" + remap(v.mVD) + "," + remap(v.mED) + "](" + quote(g) + "," + quote(d) + ")")
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

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenVertexOps extends BaseGenVertexOps with CGenBase {
  val IR: VertexOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}