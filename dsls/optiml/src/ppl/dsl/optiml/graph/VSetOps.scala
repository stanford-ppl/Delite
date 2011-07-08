package ppl.dsl.optiml.graph

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 05/03/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import ppl.dsl.optiml.datastruct.scala._
import collection.mutable.{Set => MSet}

import ppl.delite.framework.{DeliteApplication, DSLType}
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optiml.{OptiMLExp, OptiML}

import java.io.PrintWriter
import scala.virtualization.lms.internal._

trait VSetOps extends DSLType with Variables {
  def vset_vertices[V<:Vertex:Manifest](s: Rep[MSet[V]]) : Rep[Vertices[V]]
}

trait VSetOpsExp extends VSetOps with EffectExp {
  case class VSetVertices[V<:Vertex:Manifest](s: Exp[MSet[V]]) extends Def[Vertices[V]] {
    val mV = manifest[VerticesImpl[V]]
  }

  def vset_vertices[V<:Vertex:Manifest](s: Exp[MSet[V]]) = reflectMutable(VSetVertices(s))
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
    case o@VSetVertices(s) => emitValDef(sym, "new " + remap(o.mV) + "(" + quote(s) + ".toArray)")
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