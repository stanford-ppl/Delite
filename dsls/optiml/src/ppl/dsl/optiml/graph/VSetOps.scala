package ppl.dsl.optiml.graph

import java.io.PrintWriter
import scala.reflect.Manifest
import scala.collection.mutable.Set
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml._

trait VSetOps extends Variables {
  def vset_vertices(s: Rep[Set[Vertex]]) : Rep[DenseVector[Vertex]]
}

trait VSetOpsExp extends VSetOps with EffectExp {
  case class VSetVertices(s: Exp[Set[Vertex]]) extends Def[DenseVector[Vertex]] {
    val mV = manifest[Vertex]
  }

  def vset_vertices(s: Exp[Set[Vertex]]) = reflectMutable(VSetVertices(s))
}

trait BaseGenVSetOps extends GenericNestedCodegen {
  val IR: VSetOpsExp
  import IR._

  /*override def syms(e: Any): List[Sym[Any]] = e match {
    case _ => super.syms(e)
  } */
}

trait ScalaGenVSetOps extends BaseGenVSetOps with ScalaGenEffect {
  val IR: VSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case o@VSetVertices(s) => emitValDef(sym, "new generated.scala.DenseVector[Vertex](" + quote(s) + ".toArray)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenVSetOps extends BaseGenVSetOps with CLikeCodegen {
  val IR: VSetOpsExp
  import IR._

/*  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  } */
}

trait CudaGenVSetOps extends CudaGenEffect with CLikeGenVSetOps
trait CGenVSetOps extends CGenEffect with CLikeGenVSetOps