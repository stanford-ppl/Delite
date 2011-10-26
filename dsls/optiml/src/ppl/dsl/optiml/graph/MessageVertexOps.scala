package ppl.dsl.optiml.graph

import ppl.dsl.optiml.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optiml._

trait MessageVertexOps extends Variables {
  this: OptiML =>

  object MessageVertex {
    def apply(g: Rep[Graph[MessageVertex,MessageEdge]], d: Rep[MessageData]) = message_vertex_obj_new(g,d)
  }

  implicit def repMessageVertexToMessageVertexOps(v: Rep[MessageVertex]) = new MessageVertexOpsCls(v)

  class MessageVertexOpsCls(v: Rep[MessageVertex]) {
    def data = message_vertex_data(v)
    def target(e: Rep[MessageEdge]) = message_vertex_target(v,e)
  }

  // object defs
  def message_vertex_obj_new(g: Rep[Graph[MessageVertex,MessageEdge]], d: Rep[MessageData]): Rep[MessageVertex]

  // class defs
  def message_vertex_data(v: Rep[MessageVertex]): Rep[MessageData]
  def message_vertex_target(v: Rep[MessageVertex], e: Rep[MessageEdge]): Rep[MessageVertex]
}

trait MessageVertexOpsExp extends MessageVertexOps with EffectExp {
  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class MessageVertexObjectNew(g: Exp[Graph[MessageVertex,MessageEdge]], d: Exp[MessageData])
    extends Def[MessageVertex]
  case class MessageVertexData(v: Exp[MessageVertex]) extends Def[MessageData]
  case class MessageVertexTarget(v: Exp[MessageVertex], e: Exp[MessageEdge]) extends Def[MessageVertex]


  /////////////////////
  // object interface

  def message_vertex_obj_new(g: Exp[Graph[MessageVertex,MessageEdge]], d: Exp[MessageData]) = reflectMutable(MessageVertexObjectNew(g,d))

  /////////////////////
  // class interface

  def message_vertex_data(v: Exp[MessageVertex]) = reflectPure(MessageVertexData(v))
  def message_vertex_target(v: Exp[MessageVertex], e: Exp[MessageEdge]) = reflectPure(MessageVertexTarget(v,e))
}


trait BaseGenMessageVertexOps extends GenericNestedCodegen {
  val IR: MessageVertexOpsExp
  import IR._

}

trait ScalaGenMessageVertexOps extends BaseGenMessageVertexOps with ScalaGenBase {
  val IR: MessageVertexOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case v@MessageVertexObjectNew(g,d) => emitValDef(sym, "new generated.scala.MessageVertexImpl(" + quote(g) + "," + quote(d) + ")")
      case MessageVertexData(v) => emitValDef(sym, quote(v) + ".data")
      case MessageVertexTarget(v,e) => emitValDef(sym, quote(v) + ".target(" + quote(e) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenMessageVertexOps extends BaseGenMessageVertexOps with CudaGenBase with CudaGenDataStruct {
  val IR: MessageVertexOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMessageVertexOps extends BaseGenMessageVertexOps with CGenBase {
  val IR: MessageVertexOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}