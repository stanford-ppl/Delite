package ppl.dsl.deliszt

import datastruct.scala._
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import reflect.Manifest
import scala.virtualization.lms.internal.GenericFatCodegen
import scala.virtualization.lms.common._

/* Machinery provided by DeLiszt itself (language features and control structures).
 *
 * author: Michael Wu (mikemwu@stanford.edu)
 * created: Mar 14, 2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait LanguageOps extends Base { this: DeLiszt =>
  def Print(as : Rep[Any]*) : Unit
	def FieldWithConst[MO <: MeshObj, VT](url : Rep[VT])(implicit mo : Manifest[MO], vt : Manifest[VT]) : Rep[Field[MO,VT]]
	def FieldWithURL[MO <: MeshObj, VT](url : String)(implicit mo : Manifest[MO], vt : Manifest[VT]) : Rep[Field[MO,VT]]
	def BoundarySet[MO <: MeshObj](name : String)(implicit mo : Manifest[MO]) : Rep[Set[MO]]

	def mesh : Rep[Mesh]

	def vertices(e : Rep[Mesh])(implicit x : Overloaded1) : Rep[Set[Vertex]]
	def vertices(e : Rep[Vertex])(implicit x : Overloaded2) : Rep[Set[Vertex]]
	def vertices(e : Rep[Edge])(implicit x : Overloaded3) : Rep[Set[Vertex]]
	def vertices(e : Rep[Face])(implicit x : Overloaded4) : Rep[Set[Vertex]]
	def vertices(e : Rep[Cell])(implicit x : Overloaded5) : Rep[Set[Vertex]]

	def verticesCCW(e : Rep[Face]) : Rep[Set[Vertex]]
	def verticesCW(e : Rep[Face]) : Rep[Set[Vertex]]

	def cells(e : Rep[Mesh])(implicit x : Overloaded1) : Rep[Set[Cell]]
	def cells(e : Rep[Vertex])(implicit x : Overloaded2) : Rep[Set[Cell]]
	def cells(e : Rep[Edge])(implicit x : Overloaded3) : Rep[Set[Cell]]
	def cells(e : Rep[Face])(implicit x : Overloaded4) : Rep[Set[Cell]]
	def cells(e : Rep[Cell])(implicit x : Overloaded5) : Rep[Set[Cell]]

	def cellsCCW(e : Rep[Edge]) : Rep[Set[Cell]]
	def cellsCW(e : Rep[Edge]) : Rep[Set[Cell]]

	def edges(e : Rep[Mesh])(implicit x : Overloaded1) : Rep[Set[Edge]]
	def edges(e : Rep[Vertex])(implicit x : Overloaded2) : Rep[Set[Edge]]
	def edges(e : Rep[Face])(implicit x : Overloaded3) : Rep[Set[Edge]]
	def edges(e : Rep[Cell])(implicit x : Overloaded4) : Rep[Set[Edge]]

	def edgesCCW(e : Rep[Face]) : Rep[Set[Edge]]
	def edgesCW(e : Rep[Face]) : Rep[Set[Edge]]

	def faces(e : Rep[Mesh])(implicit x : Overloaded1) : Rep[Set[Face]]
	def faces(e : Rep[Vertex])(implicit x : Overloaded2) : Rep[Set[Face]]
	def faces(e : Rep[Edge])(implicit x : Overloaded3) : Rep[Set[Face]]
	def faces(e : Rep[Cell])(implicit x : Overloaded4) : Rep[Set[Face]]

	def facesCCW(e : Rep[Edge]) : Rep[Set[Face]]
	def facesCW(e : Rep[Edge]) : Rep[Set[Face]]

	def head(e : Rep[Edge]) : Rep[Vertex]
	def tail(e : Rep[Edge]) : Rep[Vertex]

	def inside(e : Rep[Face]) : Rep[Cell]
	def outside(e : Rep[Face]) : Rep[Cell]

	def flip(e : Rep[Edge])(implicit x : Overloaded1) : Rep[Edge]
	def flip(e : Rep[Face])(implicit x : Overloaded2) : Rep[Face]

	def towards(e : Rep[Edge],v : Rep[Vertex])(implicit x : Overloaded1) : Rep[Edge]
	def towards(e : Rep[Face],c : Rep[Cell])(implicit x : Overloaded2) : Rep[Face]
	def size[MO <: MeshObj](s : Rep[Set[MO]])(implicit m : Manifest[MO]) : Rep[Int]

	def foreach[MO <: MeshObj](x : Rep[Set[MO]], fn : Rep[MO] => Any) : Unit
	def ID[MO <: MeshObj](x : Rep[MO]) : Rep[Int]
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: OptiMLExp with LanguageImplOps =>

  case class InternalRandDouble() extends Def[Double]
  case class InternalRandFloat() extends Def[Float]
  case class InternalRandInt() extends Def[Int]
  case class InternalRandLong() extends Def[Long]
  case class InternalRandBoolean() extends Def[Boolean]

  case class RandDouble() extends Def[Double]
  case class RandFloat() extends Def[Float]
  case class RandInt() extends Def[Int]
  case class RandLong() extends Def[Long]
  case class RandBoolean() extends Def[Boolean]

  case class RandGaussian() extends Def[Double]

  case class RandReseed() extends Def[Unit]


  /**
   * Random
   */
  def optiml_internal_rand_double() = reflectEffect(InternalRandDouble())
  def optiml_internal_rand_float() = reflectEffect(InternalRandFloat())
  def optiml_internal_rand_int() = reflectEffect(InternalRandInt())
  def optiml_internal_rand_long() = reflectEffect(InternalRandLong())
  def optiml_internal_rand_boolean() = reflectEffect(InternalRandBoolean())

  def optiml_rand_double() = reflectEffect(RandDouble())
  def optiml_rand_float() = reflectEffect(RandFloat())
  def optiml_rand_int() = reflectEffect(RandInt())
  def optiml_rand_long() = reflectEffect(RandLong())
  def optiml_rand_boolean() = reflectEffect(RandBoolean())
  def optiml_rand_gaussian() = reflectEffect(RandGaussian())

  def optiml_reseed() = reflectEffect(RandReseed())


  /**
   * Sum
   */

  case class Sum[A:Manifest:Arith](start: Exp[Int], end: Exp[Int], mV: Sym[Int], map: Exp[A])
    extends DeliteOpMapReduce[Int,A,Vector] {

    val in = Vector.range(start, end)
    val rV = (fresh[A],fresh[A])
    val reduce = reifyEffects(rV._1 += rV._2)
    //val mapreduce = reifyEffects(ops.+=(acc, reifyEffects(block(mV))))
  }

  def optiml_sum[A:Manifest:Arith](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A]) = {

    val mV = fresh[Int]
    val map = reifyEffects(block(mV))
    // reflectEffect should not be necessary -- see IndexVectorConstruct for explanation
    reflectEffect(Sum(start, end, mV, map))
  }


  case class VectorDistance[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]], metric: Rep[Int])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_vectordistance_impl(v1,v2,metric)))

  case class MatrixDistance[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], metric: Rep[Int])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_matrixdistance_impl(m1,m2,metric)))

  def optiml_vector_dist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]], metric: Rep[Int]) = VectorDistance(v1,v2,metric)
}

trait BaseGenLanguageOps extends GenericFatCodegen {
  val IR: LanguageOpsExp
  import IR._
}

trait ScalaGenLanguageOps extends ScalaGenEffect with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case InternalRandDouble() => emitValDef(sym, "generated.scala.Global.intRandRef.nextDouble()")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

