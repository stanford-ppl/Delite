package ppl.dsl.optiml.graph

import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.dsl.optiml._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import java.io.PrintWriter
import ppl.dsl.optiml.CudaGenDataStruct

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Jan 24, 2011
 * Time: 12:36:36 AM
 * To change this template use File | Settings | File Templates.
 */

trait VerticesOps extends Variables {
  this: OptiML =>

  object Vertices {
    def apply[V <: Vertex :Manifest](len: Rep[Int]) = vertices_obj_new(len)
  }

  implicit def repVerToVerOps[V <: Vertex:Manifest](x: Rep[Vertices[V]]) = new verOpsCls(x)

  class verOpsCls[V <: Vertex:Manifest](x: Rep[Vertices[V]]) {
    def apply(n: Rep[Int]) = vertices_apply(x, n)
    def foreach(block: Rep[V] => Rep[Unit]) = vertices_foreach(x, block)
    def mforeach(block: Rep[V] => Rep[Unit]) = vertices_foreach(x, block)
    def cloneL() = vertices_clone(x) 
    def mutable() = vertices_mutable_clone(x)
    def printBeliefs() = vertices_pbeliefs(x)
    def toList() = vertices_tolist(x)
  }

  def vertices_obj_new[V <: Vertex:Manifest](len: Rep[Int]): Rep[Vertices[V]]
  def vertices_apply[V <: Vertex:Manifest](x: Rep[Vertices[V]], n: Rep[Int]): Rep[V]
  def vertices_foreach[V <: Vertex:Manifest](x: Rep[Vertices[V]], block: Rep[V] => Rep[Unit]): Rep[Unit]
  def vertices_mforeach[V <: Vertex:Manifest](x: Rep[Vertices[V]], block: Rep[V] => Rep[Unit]): Rep[Unit]
  def vertices_clone[V <: Vertex:Manifest](x: Rep[Vertices[V]]): Rep[Vertices[V]]
  def vertices_mutable_clone[V <: Vertex:Manifest](x: Rep[Vertices[V]]): Rep[Vertices[V]]
  def vertices_pbeliefs[V <: Vertex:Manifest](x: Rep[Vertices[V]]): Rep[Unit]
  def vertices_tolist[V <: Vertex:Manifest](x: Rep[Vertices[V]]): Rep[List[V]]
}

trait VerticesOpsExp extends VerticesOps with VariablesExp {
  this: OptiMLExp =>

  case class VerticesObjNew[V <: Vertex:Manifest](len: Exp[Int])
    extends Def[Vertices[V]] {
    val mV = manifest[V]
  }
  
  case class VerticesClone[V <: Vertex:Manifest](x: Exp[Vertices[V]]) extends Def[Vertices[V]]
  case class VerticesPBeliefs[V <: Vertex:Manifest](x: Exp[Vertices[V]]) extends Def[Unit]
  case class VerticesToList[V <: Vertex:Manifest](x: Exp[Vertices[V]]) extends Def[List[V]]
  
  case class VerticesForeach[V <:Vertex:Manifest](in: Exp[Vertices[V]], v: Sym[V], func: Exp[Unit])
    extends DeliteOpForeachBounded[Vertex,V,Vertices] {

    val i = fresh[Int]
    val sync = reifyEffects(in(i).neighborsSelf.toList)
  }
  
  case class VerticesApply[V<:Vertex:Manifest](x: Exp[Vertices[V]], n: Exp[Int]) extends Def[V]

  def vertices_obj_new[V <: Vertex:Manifest](len: Exp[Int]) = reflectMutable(VerticesObjNew[V](len))
  
  def vertices_apply[V<:Vertex:Manifest](x: Rep[Vertices[V]], n: Rep[Int]) = reflectMutable(VerticesApply(x,n))

  def vertices_foreach[V <: Vertex:Manifest](x: Exp[Vertices[V]], block: Exp[V] => Exp[Unit]) = {
    val v = fresh[V]
    val func = reifyEffects(block(v))
    reflectEffect(VerticesForeach(x, v, func))
  }
  
  def vertices_mforeach[V <: Vertex:Manifest](x: Exp[Vertices[V]], block: Exp[V] => Exp[Unit]) = {
    val v = fresh[V]
    val func = reifyEffects(block(v))
    reflectWrite(x)(VerticesForeach(x, v, func))
  }
  
  def vertices_mutable_clone[V <: Vertex:Manifest](x: Exp[Vertices[V]]) = reflectMutable(VerticesClone(x))
  def vertices_clone[V <: Vertex:Manifest](x: Exp[Vertices[V]]) = reflectPure(VerticesClone(x))
  def vertices_pbeliefs[V <: Vertex:Manifest](x: Exp[Vertices[V]]) = reflectEffect(VerticesPBeliefs(x))
  def vertices_tolist[V <: Vertex:Manifest](x: Exp[Vertices[V]]) = reflectPure(VerticesToList(x))
}

trait BaseGenVerticesOps extends GenericNestedCodegen {
  val IR: VerticesOpsExp
  import IR._

}

trait ScalaGenVerticesOps extends BaseGenVerticesOps with ScalaGenBase {
  val IR: VerticesOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case v@VerticesObjNew(len) => emitValDef(sym, "new generated.scala.VerticesImpl[" + remap(v.mV) + "](" + quote(len) + ")")
      case VerticesApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case VerticesClone(x) => emitValDef(sym, quote(x) + ".cloneV")
      case VerticesPBeliefs(x) => emitValDef(sym, quote(x) + ".printBeliefs")
      case VerticesToList(x) => emitValDef(sym, quote(x) + ".toList")
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenVerticesOps extends BaseGenVerticesOps with CudaGenBase with CudaGenDataStruct {
  val IR: VerticesOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVerticesOps extends BaseGenVerticesOps with CGenBase {
  val IR: VerticesOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}