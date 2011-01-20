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
  }

  // class defs
  def vertex_edges(v: Rep[Vertex]): Rep[Edges[Edge]]
  def vertex_neighbors(v: Rep[Vertex]): Rep[Vertices[Vertex]]
}

trait VertexOpsExp extends VertexOps with EffectExp {

  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class VertexEdges(v: Rep[Vertex]) extends Def[Edges[Edge]]
  case class VertexNeighbors(v: Rep[Vertex]) extends Def[Vertices[Vertex]]


  /////////////////////
  // class interface

  def vertex_edges(v: Exp[Vertex]) = VertexEdges(v)
  def vertex_neighbors(v: Exp[Vertex]) = VertexNeighbors(v)
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