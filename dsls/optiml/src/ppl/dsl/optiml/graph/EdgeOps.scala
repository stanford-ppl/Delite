package ppl.dsl.optiml.graph

import ppl.dsl.optiml.CudaGenDataStruct
import ppl.dsl.optiml._
import java.io.{PrintWriter}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}

trait EdgeOps extends Variables {
  this: OptiML =>

  object Edge {
    def apply(g: Rep[Graph], in: Rep[MessageData], out: Rep[MessageData], a: Rep[Vertex], b: Rep[Vertex]) = edge_obj_new(g,in,out,a,b)
  }

  implicit def repEdgeToEdgeOps(e: Rep[Edge]) = new EdgeOpsCls(e)

  class EdgeOpsCls(e: Rep[Edge]) {
    def graph: Rep[Graph] = edge_get_graph(e)
    def inData: Rep[MessageData] = edge_get_indata(e) 
    def outData: Rep[MessageData] = edge_get_outdata(e)
    def v1: Rep[Vertex] = edge_get_v1(e)
    def v2: Rep[Vertex] = edge_get_v2(e)
     
    def in(v: Rep[Vertex]) = edge_in(e,v)
    def out(v: Rep[Vertex]) = edge_out(e,v)
    def target(source: Rep[Vertex]) = edge_target(e,source)
  }

  // object defs
  def edge_obj_new(g: Rep[Graph], in: Rep[MessageData], out: Rep[MessageData], a: Rep[Vertex], b: Rep[Vertex]): Rep[Edge]

  // class defs
  def edge_get_graph(e: Rep[Edge]): Rep[Graph] 
  def edge_get_indata(e: Rep[Edge]): Rep[MessageData] 
  def edge_get_outdata(e: Rep[Edge]): Rep[MessageData] 
  def edge_get_v1(e: Rep[Edge]): Rep[Vertex] 
  def edge_get_v2(e: Rep[Edge]): Rep[Vertex] 
  
  def edge_in(e: Rep[Edge], v: Rep[Vertex]): Rep[MessageData]
  def edge_out(e: Rep[Edge], v: Rep[Vertex]): Rep[MessageData]
  def edge_target(e: Rep[Edge], v: Rep[Vertex]): Rep[Vertex]
}

trait EdgeOpsExp extends EdgeOps with EffectExp {

  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class EdgeObjectNew(g: Exp[Graph], in: Exp[MessageData], out: Exp[MessageData], a: Exp[Vertex], b: Exp[Vertex])
    extends Def[Edge] {
    //val mE = manifest[EdgeImpl]
  }
  
  case class EdgeGetGraph(e: Exp[Edge]) extends Def[Graph]
  case class EdgeGetInData(e: Exp[Edge]) extends Def[MessageData]
  case class EdgeGetOutData(e: Exp[Edge]) extends Def[MessageData]
  case class EdgeGetV1(e: Exp[Edge]) extends Def[Vertex]
  case class EdgeGetV2(e: Exp[Edge]) extends Def[Vertex]
  
  
  //////////////////////////////////////////////////
  // implemented via kernel embedding
  
  case class EdgeIn(e: Exp[Edge], v: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(edge_in_impl(e,v)))
  case class EdgeOut(e: Exp[Edge], v: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(edge_out_impl(e,v)))
  case class EdgeTarget(e: Exp[Edge], v: Exp[Vertex]) extends DeliteOpSingleTask(reifyEffectsHere(edge_target_impl(e,v)))


  /////////////////////
  // object interface

  def edge_obj_new(g: Exp[Graph], in: Exp[MessageData], out: Exp[MessageData], a: Exp[Vertex], b: Exp[Vertex]) = reflectEffect(EdgeObjectNew(g,in,out,a,b))

  /////////////////////
  // class interface
  
  def edge_get_graph(e: Exp[Edge]) = reflectPure(EdgeGetGraph(e))
  def edge_get_indata(e: Exp[Edge]) = reflectPure(EdgeGetInData(e))
  def edge_get_outdata(e: Exp[Edge]) = reflectPure(EdgeGetOutData(e))
  def edge_get_v1(e: Exp[Edge]) = reflectPure(EdgeGetV1(e))
  def edge_get_v2(e: Exp[Edge]) = reflectPure(EdgeGetV2(e))
  
  def edge_in(e: Exp[Edge], v: Exp[Vertex]) = reflectPure(EdgeIn(e,v))
  def edge_out(e: Exp[Edge], v: Exp[Vertex]) = reflectPure(EdgeOut(e,v))
  def edge_target(e: Exp[Edge], v: Exp[Vertex]) = reflectPure(EdgeTarget(e,v))
}


trait BaseGenEdgeOps extends GenericNestedCodegen {
  val IR: EdgeOpsExp
  import IR._

}

trait ScalaGenEdgeOps extends BaseGenEdgeOps with ScalaGenBase {
  val IR: EdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case e@EdgeObjectNew(g,in,out,a,b) => emitValDef(sym, "new generated.scala.Edge(" + quote(g) + "," + quote(in) + "," + quote(out) + "," + quote(a) + "," + quote(b) + ")")
      case EdgeGetGraph(e) => emitValDef(sym, quote(e) + "._graph")
      case EdgeGetInData(e) => emitValDef(sym, quote(e) + "._inData")
      case EdgeGetOutData(e) => emitValDef(sym, quote(e) + "._outData")
      case EdgeGetV1(e) => emitValDef(sym, quote(e) + "._v1")
      case EdgeGetV2(e) => emitValDef(sym, quote(e) + "._v2")
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenEdgeOps extends BaseGenEdgeOps with CudaGenBase with CudaGenDataStruct {
  val IR: EdgeOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenEdgeOps extends BaseGenEdgeOps with CGenBase {
  val IR: EdgeOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}