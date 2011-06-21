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

trait MessageEdgeOps extends DSLType with Variables {
  this: OptiML =>

  object MessageEdge {
    def apply(g: Rep[Graph[MessageVertex,MessageEdge]], in: Rep[MessageData], out: Rep[MessageData], a: Rep[MessageVertex], b: Rep[MessageVertex]) = message_edge_obj_new(g,in,out,a,b)
  }

  implicit def repMessageEdgeToMessageEdgeOps(e: Rep[MessageEdge]) = new messageEdgeOpsCls(e)

  class messageEdgeOpsCls(e: Rep[MessageEdge]) {
    def in(v: Rep[MessageVertex]) = message_edge_in(e,v)
    def out(v: Rep[MessageVertex]) = message_edge_out(e,v)
    def target(source: Rep[MessageVertex]) = message_edge_target(e,source)
  }

  // object defs
  def message_edge_obj_new(g: Rep[Graph[MessageVertex,MessageEdge]], in: Rep[MessageData], out: Rep[MessageData], a: Rep[MessageVertex], b: Rep[MessageVertex]): Rep[MessageEdge]

  // class defs
  def message_edge_in(e: Rep[MessageEdge], v: Rep[MessageVertex]): Rep[MessageData]
  def message_edge_out(e: Rep[MessageEdge], v: Rep[MessageVertex]): Rep[MessageData]
  def message_edge_target(e: Rep[MessageEdge], v: Rep[MessageVertex]): Rep[MessageVertex]

}

trait MessageEdgeOpsExp extends MessageEdgeOps with EffectExp {

  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class MessageEdgeObjectNew(g: Exp[Graph[MessageVertex,MessageEdge]], in: Exp[MessageData], out: Exp[MessageData], a: Exp[MessageVertex], b: Exp[MessageVertex])
    extends Def[MessageEdge] {
    val mE = manifest[MessageEdgeImpl]
  }
  case class MessageEdgeIn(e: Exp[MessageEdge], v: Exp[MessageVertex]) extends Def[MessageData]
  case class MessageEdgeOut(e: Exp[MessageEdge], v: Exp[MessageVertex]) extends Def[MessageData]
  case class MessageEdgeTarget(e: Exp[MessageEdge], v: Exp[MessageVertex]) extends Def[MessageVertex]


  /////////////////////
  // object interface

  def message_edge_obj_new(g: Exp[Graph[MessageVertex,MessageEdge]], in: Exp[MessageData], out: Exp[MessageData], a: Exp[MessageVertex], b: Exp[MessageVertex]) = reflectEffect(MessageEdgeObjectNew(g,in,out,a,b))

  /////////////////////
  // class interface

  def message_edge_in(e: Exp[MessageEdge], v: Exp[MessageVertex]) = reflectPure(MessageEdgeIn(e,v))
  def message_edge_out(e: Exp[MessageEdge], v: Exp[MessageVertex]) = reflectPure(MessageEdgeOut(e,v))
  def message_edge_target(e: Exp[MessageEdge], v: Exp[MessageVertex]) = reflectPure(MessageEdgeTarget(e,v))
}


trait BaseGenMessageEdgeOps extends GenericNestedCodegen {
  val IR: MessageEdgeOpsExp
  import IR._

}

trait ScalaGenMessageEdgeOps extends BaseGenMessageEdgeOps with ScalaGenBase {
  val IR: MessageEdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case e@MessageEdgeObjectNew(g,in,out,a,b) => emitValDef(sym, "new " + remap(e.mE) + "(" + quote(g) + "," + quote(in) + "," + quote(out) + "," + quote(a) + "," + quote(b) + ")")
      case MessageEdgeIn(e,v) => emitValDef(sym, quote(e) + ".in(" + quote(v) + ")")
      case MessageEdgeOut(e,v) => emitValDef(sym, quote(e) + ".out(" + quote(v) + ")")
      case MessageEdgeTarget(e,v) => emitValDef(sym, quote(e) + ".target(" + quote(v) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenMessageEdgeOps extends BaseGenMessageEdgeOps with CudaGenBase with CudaGenDataStruct {
  val IR: MessageEdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMessageEdgeOps extends BaseGenMessageEdgeOps with CGenBase {
  val IR: MessageEdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}