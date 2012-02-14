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

  // object Vertices {
  //   def apply(len: Rep[Int]) = vertices_obj_new(len)
  // }

  implicit def repToVerticesOps[VD:Manifest,ED:Manifest](x: Rep[DenseVector[Vertex[VD,ED]]]) = new VerticesOpsCls(x)

  class VerticesOpsCls[VD:Manifest,ED:Manifest](x: Rep[DenseVector[Vertex[VD,ED]]]) {
    // def apply(n: Rep[Int]) = vertices_apply(x, n)
    def foreach(block: Rep[Vertex[VD,ED]] => Rep[Unit]) = vertices_foreach(x, block)
    // def mforeach(block: Rep[Vertex[VD,ED]] => Rep[Unit]) = vertices_foreach(x, block)
    // def Clone() = vertices_clone(x) 
    // def mutable() = vertices_mutable_clone(x)
    // def printBeliefs() = vertices_pbeliefs(x)
    def toList() = vertices_tolist(x)
  }

  // def vertices_obj_new(len: Rep[Int]): Rep[DenseVector[Vertex[VD,ED]]]]
  // def vertices_apply(x: Rep[DenseVector[Vertex[VD,ED]]]], n: Rep[Int]): Rep[Vertex[VD,ED]]
  def vertices_foreach[VD:Manifest,ED:Manifest](x: Rep[DenseVector[Vertex[VD,ED]]], block: Rep[Vertex[VD,ED]] => Rep[Unit]): Rep[Unit]
  // def vertices_mforeach(x: Rep[DenseVector[Vertex[VD,ED]]]], block: Rep[Vertex[VD,ED]] => Rep[Unit]): Rep[Unit]
  // def vertices_clone(x: Rep[DenseVector[Vertex[VD,ED]]]]): Rep[DenseVector[Vertex[VD,ED]]]]
  // def vertices_mutable_clone(x: Rep[DenseVector[Vertex[VD,ED]]]]): Rep[DenseVector[Vertex[VD,ED]]]]
  // def vertices_pbeliefs(x: Rep[DenseVector[Vertex[VD,ED]]]]): Rep[Unit]
  def vertices_tolist[VD:Manifest,ED:Manifest](x: Rep[DenseVector[Vertex[VD,ED]]]): Rep[List[Vertex[VD,ED]]]
}

trait VerticesOpsExp extends VerticesOps with VariablesExp {
  this: OptiMLExp =>

  // case class VerticesObjNew(len: Exp[Int])
  //   extends Def[DenseVector[Vertex]] {
  //   val mV = manifest[V]
  // }  
  // case class VerticesClone(x: Exp[DenseVector[Vertex[VD,ED]]]) extends Def[DenseVector[Vertex]]
  // case class VerticesPBeliefs(x: Exp[DenseVector[Vertex[VD,ED]]]) extends Def[Unit]
  // case class VerticesToList(x: Exp[DenseVector[Vertex[VD,ED]]]) extends Def[List[Vertex]]  
  
  case class VerticesToList[VD:Manifest,ED:Manifest](x: Exp[DenseVector[Vertex[VD,ED]]]) extends DeliteOpSingleTask(reifyEffectsHere(vertices_tolist_impl(x)))
  
  case class VerticesForeach[VD:Manifest,ED:Manifest](in: Exp[DenseVector[Vertex[VD,ED]]], v: Sym[Vertex[VD,ED]], func: Exp[Unit])
    extends DeliteOpForeachBounded[Vertex[VD,ED],Vertex[VD,ED],DenseVector] {

    val i = fresh[Int]
    val sync = reifyEffects(in(i).neighborsSelf.toList)
  }  
  //case class VerticesApply(x: Exp[DenseVector[Vertex[VD,ED]]], n: Exp[Int]) extends Def[Vertex]

  //def vertices_obj_new(len: Exp[Int]) = reflectMutable(VerticesObjNew[V](len))  
  //def vertices_apply(x: Exp[DenseVector[Vertex[VD,ED]]], n: Exp[Int]) = /*reflectMutable(*/VerticesApply(x,n)/*)*/
  def vertices_foreach[VD:Manifest,ED:Manifest](x: Exp[DenseVector[Vertex[VD,ED]]], block: Exp[Vertex[VD,ED]] => Exp[Unit]) = {
    val v = fresh[Vertex[VD,ED]]
    val func = reifyEffects(block(v))
    reflectEffect(VerticesForeach(x,v,func), summarizeEffects(func).star)
  }  
  // def vertices_mforeach(x: Exp[DenseVector[Vertex[VD,ED]]], block: Exp[Vertex] => Exp[Unit]) = {
  //   val v = fresh[V]
  //   val func = reifyEffects(block(v))
  //   reflectWrite(x)(VerticesForeach(x, v, func))
  // }  
  // def vertices_mutable_clone(x: Exp[DenseVector[Vertex[VD,ED]]]) = reflectMutable(VerticesClone(x))
  // def vertices_clone(x: Exp[DenseVector[Vertex[VD,ED]]]) = reflectPure(VerticesClone(x))
  // def vertices_pbeliefs(x: Exp[DenseVector[Vertex[VD,ED]]]) = reflectEffect(VerticesPBeliefs(x))
  def vertices_tolist[VD:Manifest,ED:Manifest](x: Exp[DenseVector[Vertex[VD,ED]]]) = reflectPure(VerticesToList(x))
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
      //case v@VerticesObjNew(len) => emitValDef(sym, "new generated.scala.Vertices[" + remap(v.mV) + "](" + quote(len) + ")")
      //case VerticesApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      //case VerticesClone(x) => emitValDef(sym, quote(x) + ".Clone")
      //case VerticesPBeliefs(x) => emitValDef(sym, quote(x) + ".printBeliefs")
      //case VerticesToList(x) => emitValDef(sym, quote(x) + ".toList")
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenVerticesOps extends BaseGenVerticesOps with CudaGenBase with CudaGenDataStruct {
  val IR: VerticesOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenVerticesOps extends BaseGenVerticesOps with CGenBase {
  val IR: VerticesOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}