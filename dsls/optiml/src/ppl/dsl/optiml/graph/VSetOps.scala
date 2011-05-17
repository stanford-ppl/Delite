package ppl.dsl.optiml.graph

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 05/03/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import java.io.PrintWriter
import scala.virtualization.lms.internal._
import scala.collection.mutable.Set

trait VSetOps extends Base {
  object VSet {
    def apply[V<:Vertex:Manifest](xs: Rep[V]*) = vset_new(xs, manifest[V])
  }

  implicit def repVSetToVSetOps[V<:Vertex:Manifest](v: Rep[VSet[V]]) = new vsetOpsCls(v)

  class vsetOpsCls[V<:Vertex:Manifest](s: Rep[VSet[V]]) {
    def contains(i: Rep[V]) = vset_contains(s, i)
    def add(i: Rep[V]) = vset_add(s, i)
    def remove(i: Rep[V]) = vset_remove(s, i)
    def clear() = vset_clear(s)
    def vertices() = vset_vertices(s)
  }

  def vset_new[V<:Vertex:Manifest](xs: Seq[Rep[V]], mA: Manifest[V]) : Rep[VSet[V]]
  def vset_contains[V<:Vertex:Manifest](s: Rep[VSet[V]], i: Rep[V]) : Rep[Boolean]
  def vset_add[V<:Vertex:Manifest](s: Rep[VSet[V]], i: Rep[V]) : Rep[Unit]
  def vset_remove[V<:Vertex:Manifest](s: Rep[VSet[V]], i: Rep[V]) : Rep[Unit]
  def vset_clear[V<:Vertex:Manifest](s: Rep[VSet[V]]) : Rep[Unit]
  def vset_vertices[V<:Vertex:Manifest](s: Rep[VSet[V]]) : Rep[Vertices[V]]
}

trait VSetOpsExp extends VSetOps with EffectExp {
  case class VSetNew[V<:Vertex:Manifest](xs: Seq[Rep[V]], mA: Manifest[V]) extends Def[VSet[V]]
  case class VSetContains[V<:Vertex:Manifest](s: Rep[VSet[V]], i: Rep[V]) extends Def[Boolean]
  case class VSetAdd[V<:Vertex:Manifest](s: Rep[VSet[V]], i: Rep[V]) extends Def[Unit]
  case class VSetRemove[V<:Vertex:Manifest](s: Rep[VSet[V]], i: Rep[V]) extends Def[Unit]
  case class VSetClear[V<:Vertex:Manifest](s: Rep[VSet[V]]) extends Def[Unit]
  case class VSetVertices[V<:Vertex:Manifest](s: Rep[VSet[V]]) extends Def[Vertices[V]] {
    val mV = manifest[Vertices[V]]
  }

  def vset_new[V<:Vertex:Manifest](xs: Seq[Rep[V]], mA: Manifest[V]) = reflectEffect(VSetNew(xs, mA))
  def vset_contains[V<:Vertex:Manifest](s: Rep[VSet[V]], i: Rep[V]) = reflectEffect(VSetContains(s, i))
  def vset_add[V<:Vertex:Manifest](s: Rep[VSet[V]], i: Rep[V]) = reflectEffect(VSetAdd(s, i))
  def vset_remove[V<:Vertex:Manifest](s: Rep[VSet[V]], i: Rep[V]) = reflectEffect(VSetRemove(s, i))
  def vset_clear[V<:Vertex:Manifest](s: Rep[VSet[V]]) = reflectEffect(VSetClear(s))
  def vset_vertices[V<:Vertex:Manifest](s: Rep[VSet[V]]) = reflectMutable(VSetVertices(s))
}

trait BaseGenVSetOps extends GenericNestedCodegen {
  val IR: VSetOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case VSetNew(xs, mA) => (xs flatMap { syms }).toList
    case _ => super.syms(e)
  }

}

trait ScalaGenVSetOps extends BaseGenVSetOps with ScalaGenEffect {
  val IR: VSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case VSetNew(xs, mA) => emitValDef(sym, "collection.mutable.HashVSet[" + remap(mA) + "](" + (xs map {quote}).mkString(",") + ")")
    case VSetContains(s,i) => emitValDef(sym, quote(s) + ".contains(" + quote(i) + ")")
    case VSetAdd(s,i) => emitValDef(sym, quote(s) + ".add(" + quote(i) + ")")
    case VSetRemove(s,i) => emitValDef(sym, quote(s) + ".remove(" + quote(i) + ")")
    case VSetClear(s) => emitValDef(sym, quote(s) + ".clear()")
    case s@VSetVertices(s) => emitValDef(sym, "new " + remap(s.mV) + "(" + quote(s) + ".toArray)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenVSetOps extends BaseGenVSetOps with CLikeCodegen {
  val IR: VSetOpsExp
  import IR._

//  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
//    case _ => super.emitNode(sym, rhs)
//  }
}

trait CudaGenVSetOps extends CudaGenEffect with CLikeGenVSetOps
trait CGenVSetOps extends CGenEffect with CLikeGenVSetOps