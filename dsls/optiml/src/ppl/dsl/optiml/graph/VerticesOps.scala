package ppl.dsl.optiml.graph

import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.dsl.optiml._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import java.io.PrintWriter
import ppl.dsl.optiml.CudaGenDataStruct

trait VerticesOps extends Variables {
  this: OptiML =>

  implicit def repToVerticesOps[VD:Manifest,ED:Manifest](x: Rep[DenseVector[Vertex[VD,ED]]]) = new VerticesOpsCls(x)

  class VerticesOpsCls[VD:Manifest,ED:Manifest](x: Rep[DenseVector[Vertex[VD,ED]]]) {
    def foreach(block: Rep[Vertex[VD,ED]] => Rep[Unit]) = vertices_foreach(x, block)
    def toList() = vertices_tolist(x)
  }

  def vertices_foreach[VD:Manifest,ED:Manifest](x: Rep[DenseVector[Vertex[VD,ED]]], block: Rep[Vertex[VD,ED]] => Rep[Unit]): Rep[Unit]
  def vertices_tolist[VD:Manifest,ED:Manifest](x: Rep[DenseVector[Vertex[VD,ED]]]): Rep[List[Vertex[VD,ED]]]
}

trait VerticesOpsExp extends VerticesOps with VariablesExp {
  this: OptiMLExp =>

  case class VerticesToList[VD:Manifest,ED:Manifest](x: Exp[DenseVector[Vertex[VD,ED]]]) extends DeliteOpSingleTask(reifyEffectsHere(vertices_tolist_impl(x)))
  
  case class VerticesForeach[VD:Manifest,ED:Manifest](in: Exp[DenseVector[Vertex[VD,ED]]], v: Sym[Vertex[VD,ED]], func: Block[Unit])
    extends DeliteOpForeachBounded[Vertex[VD,ED],Vertex[VD,ED],DenseVector] {

    val i = fresh[Int]
    val sync = reifyEffects(in(i).neighborsSelf.toList)
  }  

  def vertices_foreach[VD:Manifest,ED:Manifest](x: Exp[DenseVector[Vertex[VD,ED]]], block: Exp[Vertex[VD,ED]] => Exp[Unit]) = {
    val v = fresh[Vertex[VD,ED]]
    val func = reifyEffects(block(v))
    reflectEffect(VerticesForeach(x,v,func), summarizeEffects(func).star)
  }  
  def vertices_tolist[VD:Manifest,ED:Manifest](x: Exp[DenseVector[Vertex[VD,ED]]]) = reflectPure(VerticesToList(x))
}

trait BaseGenVerticesOps extends GenericNestedCodegen {
  val IR: VerticesOpsExp
  import IR._

}

trait ScalaGenVerticesOps extends BaseGenVerticesOps with ScalaGenBase {
  val IR: VerticesOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
  //   rhs match {
  //     case _ => super.emitNode(sym, rhs)
  //   }
  // }
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