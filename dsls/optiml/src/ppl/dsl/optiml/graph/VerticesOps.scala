package ppl.dsl.optiml.graph

import ppl.delite.framework.DSLType
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.dsl.optiml.datastruct.scala._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import java.io.PrintWriter
import ppl.dsl.optiml.datastruct.CudaGenDataStruct

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Jan 24, 2011
 * Time: 12:36:36 AM
 * To change this template use File | Settings | File Templates.
 */

trait VerticesOps extends DSLType with Variables {
  this: OptiML =>

  object Vertices {
    def apply[V <: Vertex :Manifest](len: Rep[Int]) = vertices_obj_new(len)
  }

  implicit def repVerToVerOps[V <: Vertex:Manifest](x: Rep[Vertices[V]]) = new verOpsCls(x)

  class verOpsCls[V <: Vertex:Manifest](x: Rep[Vertices[V]]) {
    def foreach(block: Rep[V] => Rep[Unit]) = vertices_foreach(x, block)
  }

  def vertices_obj_new[V <: Vertex:Manifest](len: Rep[Int]): Rep[Vertices[V]]
  def vertices_foreach[V <: Vertex:Manifest](x: Rep[Vertices[V]], block: Rep[V] => Rep[Unit]): Rep[Unit]
}

trait VerticesOpsExp extends VerticesOps with VariablesExp {
  this: OptiMLExp =>

  case class VerticesObjNew[V <: Vertex:Manifest](len: Exp[Int])
    extends Def[Vertices[V]] {
    val mV = manifest[VerticesImpl[V]]
  }

  def vertices_obj_new[V <: Vertex:Manifest](len: Exp[Int]) = reflectMutable(VerticesObjNew[V](len))

  case class VerticesForeach[V <:Vertex:Manifest](in: Exp[Vertices[V]], v: Sym[V], func: Exp[Unit])
    extends DeliteOpForeachBounded[Vertex,V,Vertices] {

    val i = fresh[Int]
    val sync = reifyEffects(in(i).neighborsSelf.toList)
  }

  def vertices_foreach[V <: Vertex:Manifest](x: Exp[Vertices[V]], block: Exp[V] => Exp[Unit]) = {
    val v = fresh[V]
    val func = reifyEffects(block(v))
    reflectEffect(VerticesForeach(x, v, func))
  }
}

trait BaseGenVerticesOps extends GenericNestedCodegen {
  val IR: VerticesOpsExp
  import IR._

  //override def syms(e: Any): List[Sym[Any]] = e match {
    //case _ => super.syms(e)
  //}
}

trait ScalaGenVerticesOps extends BaseGenVerticesOps with ScalaGenBase {
  val IR: VerticesOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case v@VerticesObjNew(len) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(len) + ")")
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