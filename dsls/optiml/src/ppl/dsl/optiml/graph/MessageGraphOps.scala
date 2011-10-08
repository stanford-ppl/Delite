//package ppl.dsl.optiml.graph
//
//import ppl.dsl.optiml.CudaGenDataStruct
//import ppl.dsl.optiml.datastruct.scala._
//import java.io.{PrintWriter}
//
//import ppl.delite.framework.{DeliteApplication, DSLType}
//import ppl.delite.framework.ops.DeliteOpsExp
//import reflect.Manifest
//import scala.virtualization.lms.common._
//import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
//import ppl.dsl.optiml.{OptiMLExp, OptiML}
//
//trait MessageGraphOps extends DSLType with Variables {
//  this: OptiML =>
//
//  object MessageGraph {
//    def apply[V <: Vertex, E <: Edge]()(implicit mV: Manifest[V], mE: Manifest[E]) = message_graph_obj_new()
//  }
//
//  implicit def repMessageGraphToMessageGraphOps[V <: MessageVertex, E <: MessageEdge](g: Rep[MessageGraph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]) = new messageGraphOpsCls(g)
//
//  class messageGraphOpsCls[V <: MessageVertex,E <: MessageEdge](g: Rep[MessageGraph[V,E]])(implicit mV: Manifest[V], mE: Manifest[E]) {
//    def addVertex(d: MessageData)(implicit b: Buildable[V]) = message_graph_add_vertex(g,d)
//    def addEdge(in: MessageData, out: MessageData, a: V, b: V)(implicit b: Buildable[E]) = message_graph_add_edge(g,in,out,a,b)
//  }
//
//
//  // object defs
//  def message_graph_obj_new[V <: MessageVertex,E <: MessageEdge]()(implicit mV: Manifest[V], mE: Manifest[E]): Rep[MessageGraph[V,E]]
//
//  // class defs
//  def message_graph_add_vertex[V <: MessageVertex,E <: MessageEdge](g: Rep[MessageGraph[V,E]], d: Rep[MessageData], a: Rep[MessageVertex])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Unit]
//  def message_graph_add_edge[V <: MessageVertex,E <: MessageEdge](g: Rep[MessageGraph[V,E]], in: Rep[MessageData], out: Rep[MessageData], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E]): Rep[Unit]
//}
//
//trait MessageGraphOpsExp extends MessageGraphOps with EffectExp {
//
//  this: MessageGraphImplOps with OptiMLExp =>
//
//  ///////////////////////////////////////////////////
//  // implemented via method on real data structure
//
//  case class MessageGraphObjectNew[V <: Vertex,E <: Edge]()(implicit mV: Manifest[V], mE: Manifest[E])
//    extends Def[MessageGraph[V,E]] {
//    val mG = manifest[MessageGraphImpl[V,E]]
//  }
//
//  /////////////////////////////////////////////////
//  // implemented via kernel embedding (sequential)
//
//  case class MessageGraphAddVertex[V <: MessageVertex,E <: MessageEdge]
//    (g: Exp[MessageGraph[V,E]], data: Exp[MessageData])
//    (implicit mV: Manifest[V], mE: Manifest[E])
//    extends Def[Unit]
//
//  case class MessageGraphAddEdge[V <: MessageVertex,E <: MessageEdge]
//    (g: Exp[MessageGraph[V,E]], inData: Exp[MessageData], outData: Exp[MessageData], a: Exp[MessageVertex], b: Exp[MessageVertex])
//    (implicit mV: Manifest[V], mE: Manifest[E])
//     extends Def[Unit]
//
//
//  ////////////////////////////////
//  // implemented via delite ops
//
//
//
//
//  /////////////////////
//  // object interface
//
//  def message_graph_obj_new[V <: Vertex,E <: Edge]()(implicit mV: Manifest[V], mE: Manifest[E]) = reflectEffect(MessageGraphObjectNew())
//
//
//  /////////////////////
//  // class interface
//
//  def message_graph_add_vertex[V <: Vertex,E <: Edge](g: Rep[MessageGraph[V,E]], a: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E])
//    = reflectMutation(MessageGraphAddVertex(reflectReadWrite(g),a))
//  def message_graph_add_edge[V <: Vertex,E <: Edge](g: Rep[MessageGraph[V,E]], e: Rep[Edge], a: Rep[Vertex], b: Rep[Vertex])(implicit mV: Manifest[V], mE: Manifest[E])
//    = reflectMutation(MessageGraphAddEdge(reflectReadWrite(g),e,a,b))
//
//}
//
//
//trait BaseGenMessageGraphOps extends GenericNestedCodegen {
//  val IR: MessageGraphOpsExp
//  import IR._
//
//}
//
//trait ScalaGenMessageGraphOps extends BaseGenMessageGraphOps with ScalaGenBase {
//  val IR: MessageGraphOpsExp
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
//    rhs match {
//      case g@MessageGraphObjectNew() => emitValDef(sym, "new " + remap(g.mG) + "()")
//      case _ => super.emitNode(sym, rhs)
//    }
//  }
//}
//
//
//trait CudaGenMessageGraphOps extends BaseGenMessageGraphOps with CudaGenBase with CudaGenDataStruct {
//  val IR: MessageGraphOpsExp
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
//    case _ => super.emitNode(sym, rhs)
//  }
//}
//
//trait CGenMessageGraphOps extends BaseGenMessageGraphOps with CGenBase {
//  val IR: MessageGraphOpsExp
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
//    case _ => super.emitNode(sym, rhs)
//  }
//}