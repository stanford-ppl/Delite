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
    def apply[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], in: Rep[ED], out: Rep[ED], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]) = edge_obj_new(g,in,out,a,b)
  }

  implicit def repEdgeToEdgeOps[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]]) = new EdgeOpsCls(e)

  class EdgeOpsCls[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]]) {
    def graph: Rep[Graph[VD,ED]] = edge_get_graph(e)
    def inData: Rep[ED] = edge_get_indata(e) 
    def outData: Rep[ED] = edge_get_outdata(e)
    def v1: Rep[Vertex[VD,ED]] = edge_get_v1(e)
    def v2: Rep[Vertex[VD,ED]] = edge_get_v2(e)
     
    def in(v: Rep[Vertex[VD,ED]]) = edge_in(e,v)
    def out(v: Rep[Vertex[VD,ED]]) = edge_out(e,v)
    def target(source: Rep[Vertex[VD,ED]]) = edge_target(e,source)
  }

  // object defs
  def edge_obj_new[VD:Manifest,ED:Manifest](g: Rep[Graph[VD,ED]], in: Rep[ED], out: Rep[ED], a: Rep[Vertex[VD,ED]], b: Rep[Vertex[VD,ED]]): Rep[Edge[VD,ED]]

  // class defs
  def edge_get_graph[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]]): Rep[Graph[VD,ED]] 
  def edge_get_indata[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]]): Rep[ED] 
  def edge_get_outdata[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]]): Rep[ED] 
  def edge_get_v1[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]]): Rep[Vertex[VD,ED]] 
  def edge_get_v2[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]]): Rep[Vertex[VD,ED]] 
  
  def edge_in[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[ED]
  def edge_out[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[ED]
  def edge_target[VD:Manifest,ED:Manifest](e: Rep[Edge[VD,ED]], v: Rep[Vertex[VD,ED]]): Rep[Vertex[VD,ED]]
}

trait EdgeOpsExp extends EdgeOps with EffectExp {

  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  
  ///////////////////////////////////////////////////
  // implemented via delite ops

  // case class EdgesForeach[E <:Edge:Manifest](in: Exp[Edges[E]], v: Sym[E], func: Block[Unit])
  //   extends DeliteOpForeachBounded[Edge,E,Edges] {
  // 
  //   val i = fresh[Int]
  //   val sync = reifyEffects(List())
  // }

  case class EdgeObjectNew[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], in: Exp[ED], out: Exp[ED], a: Exp[Vertex[VD,ED]], b: Exp[Vertex[VD,ED]])
    extends Def[Edge[VD,ED]] {
    val mVD = manifest[VD]
    val mED = manifest[ED]
  }
  
  case class EdgeGetGraph[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) extends Def[Graph[VD,ED]]
  case class EdgeGetInData[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) extends Def[ED]
  case class EdgeGetOutData[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) extends Def[ED]
  case class EdgeGetV1[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) extends Def[Vertex[VD,ED]]
  case class EdgeGetV2[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) extends Def[Vertex[VD,ED]]
  
  
  //////////////////////////////////////////////////
  // implemented via kernel embedding
  
  case class EdgeIn[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]], v: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(edge_in_impl(e,v)))
  case class EdgeOut[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]], v: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(edge_out_impl(e,v)))
  case class EdgeTarget[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]], v: Exp[Vertex[VD,ED]]) extends DeliteOpSingleTask(reifyEffectsHere(edge_target_impl(e,v)))


  /////////////////////
  // object interface

  def edge_obj_new[VD:Manifest,ED:Manifest](g: Exp[Graph[VD,ED]], in: Exp[ED], out: Exp[ED], a: Exp[Vertex[VD,ED]], b: Exp[Vertex[VD,ED]]) = reflectPure(EdgeObjectNew(g.unsafeImmutable,in,out,a,b))

  /////////////////////
  // class interface
  
  def edge_get_graph[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) = reflectPure(EdgeGetGraph[VD,ED](e))
  def edge_get_indata[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) = reflectPure(EdgeGetInData(e))
  def edge_get_outdata[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) = reflectPure(EdgeGetOutData(e))
  def edge_get_v1[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) = reflectPure(EdgeGetV1(e))
  def edge_get_v2[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]]) = reflectPure(EdgeGetV2(e))
  
  def edge_in[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]], v: Exp[Vertex[VD,ED]]) = reflectPure(EdgeIn(e,v))
  def edge_out[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]], v: Exp[Vertex[VD,ED]]) = reflectPure(EdgeOut(e,v))
  def edge_target[VD:Manifest,ED:Manifest](e: Exp[Edge[VD,ED]], v: Exp[Vertex[VD,ED]]) = reflectPure(EdgeTarget(e,v))
}


trait BaseGenEdgeOps extends GenericNestedCodegen {
  val IR: EdgeOpsExp
  import IR._

}

trait ScalaGenEdgeOps extends BaseGenEdgeOps with ScalaGenBase {
  val IR: EdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case e@EdgeObjectNew(g,in,out,a,b) => emitValDef(sym, "new generated.scala.Edge[" + remap(e.mVD) + "," + remap(e.mED) + "](" + quote(g) + "," + quote(in) + "," + quote(out) + "," + quote(a) + "," + quote(b) + ")")
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

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenEdgeOps extends BaseGenEdgeOps with CGenBase {
  val IR: EdgeOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}