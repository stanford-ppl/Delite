package ppl.dsl.deliszt

import java.io.PrintWriter
import reflect.Manifest

import scala.virtualization.lms.internal.GenericFatCodegen
import scala.virtualization.lms.common._

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import ppl.dsl.deliszt.datastruct.scala._

/* Machinery provided by DeLiszt itself (language features and control structures).
 *
 * author: Michael Wu (mikemwu@stanford.edu)
 * created: Mar 14, 2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait LanguageOps extends Base { this: DeLiszt =>
  def _init() : Unit

  def Print(as : Rep[Any]*) : Unit
	def BoundarySet[MO<:MeshObj:Manifest](name : Rep[String]) : Rep[MeshSet[MO]]

	def mesh : Rep[Mesh]

	def vertices(e : Rep[Mesh])(implicit x : Overloaded1) : Rep[MeshSet[Vertex]]
	def vertices(e : Rep[Vertex])(implicit x : Overloaded2) : Rep[MeshSet[Vertex]]
	def vertices(e : Rep[Edge])(implicit x : Overloaded3) : Rep[MeshSet[Vertex]]
	def vertices(e : Rep[Face])(implicit x : Overloaded4) : Rep[MeshSet[Vertex]]
	def vertices(e : Rep[Cell])(implicit x : Overloaded5) : Rep[MeshSet[Vertex]]

	def verticesCCW(e : Rep[Face]) : Rep[MeshSet[Vertex]]
	def verticesCW(e : Rep[Face]) : Rep[MeshSet[Vertex]]

	def cells(e : Rep[Mesh])(implicit x : Overloaded1) : Rep[MeshSet[Cell]]
	def cells(e : Rep[Vertex])(implicit x : Overloaded2) : Rep[MeshSet[Cell]]
	def cells(e : Rep[Edge])(implicit x : Overloaded3) : Rep[MeshSet[Cell]]
	def cells(e : Rep[Face])(implicit x : Overloaded4) : Rep[MeshSet[Cell]]
	def cells(e : Rep[Cell])(implicit x : Overloaded5) : Rep[MeshSet[Cell]]

	def cellsCCW(e : Rep[Edge]) : Rep[MeshSet[Cell]]
	def cellsCW(e : Rep[Edge]) : Rep[MeshSet[Cell]]

	def edges(e : Rep[Mesh])(implicit x : Overloaded1) : Rep[MeshSet[Edge]]
	def edges(e : Rep[Vertex])(implicit x : Overloaded2) : Rep[MeshSet[Edge]]
	def edges(e : Rep[Face])(implicit x : Overloaded3) : Rep[MeshSet[Edge]]
	def edges(e : Rep[Cell])(implicit x : Overloaded4) : Rep[MeshSet[Edge]]

	def edgesCCW(e : Rep[Face]) : Rep[MeshSet[Edge]]
	def edgesCW(e : Rep[Face]) : Rep[MeshSet[Edge]]

	def faces(e : Rep[Mesh])(implicit x : Overloaded1) : Rep[MeshSet[Face]]
	def faces(e : Rep[Vertex])(implicit x : Overloaded2) : Rep[MeshSet[Face]]
	def faces(e : Rep[Edge])(implicit x : Overloaded3) : Rep[MeshSet[Face]]
	def faces(e : Rep[Cell])(implicit x : Overloaded4) : Rep[MeshSet[Face]]

	def facesCCW(e : Rep[Edge]) : Rep[MeshSet[Face]]
	def facesCW(e : Rep[Edge]) : Rep[MeshSet[Face]]

	def head(e : Rep[Edge]) : Rep[Vertex]
	def tail(e : Rep[Edge]) : Rep[Vertex]

	def inside(e : Rep[Face]) : Rep[Cell]
	def outside(e : Rep[Face]) : Rep[Cell]

	def flip(e : Rep[Edge])(implicit x : Overloaded1) : Rep[Edge]
	def flip(e : Rep[Face])(implicit x : Overloaded2) : Rep[Face]

	def towards(e : Rep[Edge],v : Rep[Vertex])(implicit x : Overloaded1) : Rep[Edge]
	def towards(e : Rep[Face],c : Rep[Cell])(implicit x : Overloaded2) : Rep[Face]
	def size[MO<:MeshObj:Manifest](s : Rep[MeshSet[MO]]) : Rep[Int]

	def ID[MO<:MeshObj:Manifest](x : Rep[MO]) : Rep[Int]

	def foreach[MO<:MeshObj:Manifest](x : Rep[MeshSet[MO]])(fn : Rep[MO] => Rep[Unit]) : Rep[Unit]

  def MATH_PI() : Rep[Double]
  def MIN_FLOAT() : Rep[Float]
  def MAX_FLOAT() : Rep[Float]
  def sqrt(a: Rep[Double]) : Rep[Double]
  def fabs(value : Rep[Float]) : Rep[Float]
  def MPI_Wtime() : Rep[Double]
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: LanguageImplOps with DeLisztExp =>

  /******* Ops *********/
  case class DeLisztInit() extends Def[Unit]
  case class DeLisztPrint(as: Seq[Exp[Any]]) extends DeliteOpSingleTask(reifyEffectsHere(print_impl(as)))

  case class DeLisztBoundarySet[MO<:MeshObj : Manifest](name : Exp[String]) extends Def[MeshSet[MO]]

  case class DeLisztMesh() extends Def[Mesh]

  case class DeLisztVertices[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[MeshSet[Vertex]]

  case class DeLisztFaceVerticesCCW(e : Exp[Face]) extends Def[MeshSet[Vertex]]
  case class DeLisztFaceVerticesCW(e : Exp[Face]) extends Def[MeshSet[Vertex]]

  case class DeLisztCells[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[MeshSet[Cell]]

  case class DeLisztEdgeCellsCCW(e : Exp[Edge]) extends Def[MeshSet[Cell]]
  case class DeLisztEdgeCellsCW(e : Exp[Edge]) extends Def[MeshSet[Cell]]

  case class DeLisztEdges[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[MeshSet[Edge]]

  case class DeLisztFaces[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[MeshSet[Face]]

  case class DeLisztEdgeFacesCCW(e : Exp[Edge]) extends Def[MeshSet[Face]]
  case class DeLisztEdgeFacesCW(e : Exp[Edge]) extends Def[MeshSet[Face]]
  
  case class DeLisztFaceEdgesCCW(e : Exp[Face]) extends Def[MeshSet[Edge]]
  case class DeLisztFaceEdgesCW(e : Exp[Face]) extends Def[MeshSet[Edge]]

  case class DeLisztEdgeHead(e : Exp[Edge]) extends Def[Vertex]
  case class DeLisztEdgeTail(e : Exp[Edge]) extends Def[Vertex]

  case class DeLisztFaceInside(e : Exp[Face]) extends Def[Cell]
  case class DeLisztFaceOutside(e : Exp[Face]) extends Def[Cell]

  case class DeLisztFlip[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[MO]

  case class DeLisztTowardsEdgeVertex(e : Exp[Edge], v: Exp[Vertex]) extends Def[Edge]
  case class DeLisztTowardsFaceCell(e : Exp[Face], c: Exp[Cell]) extends Def[Face]

  case class DeLisztSize[MO<:MeshObj:Manifest](s: Exp[MeshSet[MO]]) extends Def[Int]

  case class DeLisztForeach[MO<:MeshObj:Manifest](in: Exp[MeshSet[MO]], func: Exp[MO] => Exp[Unit])
    extends DeliteOpForeach[MO] {

    val sync = (n:Exp[Int]) => List()
    val size = in.size
  }
  
  case class DeLisztID[MO<:MeshObj:Manifest](x: Exp[MO]) extends Def[Int]

  case class MinFloat() extends Def[Float]
  case class MaxFloat() extends Def[Float]
  case class MathFAbs(a: Exp[Float]) extends Def[Float]
  case class MPIWTime() extends Def[Double]

  /******* Language functions *********/
  def _init() = reflectEffect(DeLisztInit())
  def Print(as: Exp[Any]*) = reflectEffect(DeLisztPrint(as))

  def BoundarySet[MO<:MeshObj:Manifest](name : Exp[String])
    = reflectPure(DeLisztBoundarySet(name))

  def mesh = reflectPure(DeLisztMesh())

  def vertices(e : Exp[Mesh])(implicit x : Overloaded1) = reflectPure(DeLisztVertices(e))
  def vertices(e : Exp[Vertex])(implicit x : Overloaded2) = reflectPure(DeLisztVertices(e))
  def vertices(e : Exp[Edge])(implicit x : Overloaded3) = reflectPure(DeLisztVertices(e))
  def vertices(e : Exp[Face])(implicit x : Overloaded4) = reflectPure(DeLisztVertices(e))
  def vertices(e : Exp[Cell])(implicit x : Overloaded5) = reflectPure(DeLisztVertices(e))

  def verticesCCW(e : Exp[Face]) = reflectPure(DeLisztFaceVerticesCCW(e))
  def verticesCW(e : Exp[Face]) = reflectPure(DeLisztFaceVerticesCW(e))

  def cells(e : Exp[Mesh])(implicit x : Overloaded1) = reflectPure(DeLisztCells(e))
  def cells(e : Exp[Vertex])(implicit x : Overloaded2) = reflectPure(DeLisztCells(e))
  def cells(e : Exp[Edge])(implicit x : Overloaded3) = reflectPure(DeLisztCells(e))
  def cells(e : Exp[Face])(implicit x : Overloaded4) = reflectPure(DeLisztCells(e))
  def cells(e : Exp[Cell])(implicit x : Overloaded5) = reflectPure(DeLisztCells(e))

  def cellsCCW(e : Exp[Edge]) = reflectPure(DeLisztEdgeCellsCCW(e))
  def cellsCW(e : Exp[Edge]) = reflectPure(DeLisztEdgeCellsCW(e))

  def edges(e : Exp[Mesh])(implicit x : Overloaded1) = reflectPure(DeLisztEdges(e))
  def edges(e : Exp[Vertex])(implicit x : Overloaded2) = reflectPure(DeLisztEdges(e))
  def edges(e : Exp[Face])(implicit x : Overloaded3) = reflectPure(DeLisztEdges(e))
  def edges(e : Exp[Cell])(implicit x : Overloaded4) = reflectPure(DeLisztEdges(e))

  def edgesCCW(e : Exp[Face]) = reflectPure(DeLisztFaceEdgesCCW(e))
  def edgesCW(e : Exp[Face]) = reflectPure(DeLisztFaceEdgesCW(e))

  def faces(e : Exp[Mesh])(implicit x : Overloaded1) = reflectPure(DeLisztFaces(e))
  def faces(e : Exp[Vertex])(implicit x : Overloaded2) = reflectPure(DeLisztFaces(e))
  def faces(e : Exp[Edge])(implicit x : Overloaded3) = reflectPure(DeLisztFaces(e))
  def faces(e : Exp[Cell])(implicit x : Overloaded4) = reflectPure(DeLisztFaces(e))

  def facesCCW(e : Exp[Edge]) = reflectPure(DeLisztEdgeFacesCCW(e))
  def facesCW(e : Exp[Edge]) = reflectPure(DeLisztEdgeFacesCW(e))

  def head(e : Exp[Edge]) = reflectPure(DeLisztEdgeHead(e))
  def tail(e : Exp[Edge]) = reflectPure(DeLisztEdgeTail(e))

  def inside(e : Exp[Face]) = reflectPure(DeLisztFaceInside(e))
  def outside(e : Exp[Face]) = reflectPure(DeLisztFaceOutside(e))

  def flip(e : Exp[Edge])(implicit x : Overloaded1) = reflectPure(DeLisztFlip(e))
  def flip(e : Exp[Face])(implicit x : Overloaded2) = reflectPure(DeLisztFlip(e))

  def towards(e : Exp[Edge], v: Exp[Vertex])(implicit x : Overloaded1) = reflectPure(DeLisztTowardsEdgeVertex(e,v))
  def towards(e : Exp[Face], c: Exp[Cell])(implicit x : Overloaded2) = reflectPure(DeLisztTowardsFaceCell(e,c))
  def size[MO<:MeshObj:Manifest](s : Exp[MeshSet[MO]]) = reflectPure(DeLisztSize(s))

  def foreach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]])(block: Exp[MO] => Exp[Unit]) = {
    reflectEffect(DeLisztForeach(x, block))
  }

	def ID[MO<:MeshObj:Manifest](x : Exp[MO]) = reflectPure(DeLisztID(x))

  def MATH_PI() = MathPi()
  def MIN_FLOAT() = MinFloat()
  def MAX_FLOAT() = MaxFloat()
  def sqrt(a: Exp[Double]) = MathSqrt(a)
  def fabs(a: Exp[Float]) = MathFAbs(a)
  def MPI_Wtime() = MPIWTime()
}

trait ScalaGenLanguageOps extends ScalaGenBase {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case DeLisztInit() => emitValDef(sym, "generated.scala.Liszt.init()")
      case DeLisztMesh() => emitValDef(sym, "generated.scala.Mesh.mesh")
      case DeLisztBoundarySet(name) => emitValDef(sym, "")

      case DeLisztCells(e) => emitValDef(sym, "generated.scala.Mesh.cells(" + quote(e) + ")")
      case DeLisztEdgeCellsCCW(e) => emitValDef(sym, "generated.scala.Mesh.cellsCCW(" + quote(e) + ")")
      case DeLisztEdgeCellsCW(e) => emitValDef(sym, "generated.scala.Mesh.cellsCW(" + quote(e) + ")")
      case DeLisztFaceInside(e) => emitValDef(sym, "generated.scala.Mesh.inside(" + quote(e) + ")")
      case DeLisztFaceOutside(e) => emitValDef(sym, "generated.scala.Mesh.outside(" + quote(e) + ")")

      case DeLisztEdges(e) => emitValDef(sym, "generated.scala.Mesh.edges(" + quote(e) + ")")
      case DeLisztEdgeHead(e) => emitValDef(sym, "generated.scala.Mesh.head(" + quote(e) + ")")
      case DeLisztEdgeTail(e) => emitValDef(sym, "generated.scala.Mesh.tail(" + quote(e) + ")")

      case DeLisztFaces(e) => emitValDef(sym, "generated.scala.Mesh.faces(" + quote(e) + ")")
      case DeLisztFaceEdgesCCW(e) => emitValDef(sym, "generated.scala.Mesh.facesCCW(" + quote(e) + ")")
      case DeLisztFaceEdgesCW(e) => emitValDef(sym, "generated.scala.Mesh.facesCW(" + quote(e) + ")")

      case DeLisztVertices(e) => emitValDef(sym, "generated.scala.Mesh.vertices(" + quote(e) + ")")
      case DeLisztFaceVerticesCCW(e) => emitValDef(sym, "generated.scala.Mesh.verticesCCW(" + quote(e) + ")")
      case DeLisztFaceVerticesCW(e) => emitValDef(sym, "generated.scala.Mesh.verticesCW(" + quote(e) + ")")

      case DeLisztFlip(e) => emitValDef(sym, "generated.scala.Mesh.flip(" + quote(e) + ")")

      case MinFloat() => emitValDef(sym, "scala.Float.MinValue")
      case MaxFloat() => emitValDef(sym, "scala.Float.MaxValue")
      case MathFAbs(a) => emitValDef(sym, "Math.abs(" + quote(a) + ")")
      case MPIWTime() => emitValDef(sym, "SOMETHING")
      case _ => super.emitNode(sym, rhs)
    }
  }

  /*
  case class DeLisztTowardsEdgeVertex(e : Exp[Edge], v: Exp[Vertex]) extends Def[Edge]
  case class DeLisztTowardsFaceCell(e : Exp[Face], c: Exp[Cell]) extends Def[Face]

  case class DeLisztSize[MO<:MeshObj : Manifest](s: Exp[MeshSet[MO]]) extends Def[Int]

  case class DeLisztForeach[MO<:MeshObj : Manifest](x: Exp[MeshSet[MO]], v: Sym[MO], func: Exp[Unit])
    extends DeliteOpForeach[MO,Set]

  case class DeLisztID[MO<:MeshObj : Manifest](x: Exp[MO]) extends Def[Int]
   */
}

trait CudaGenLanguageOps extends CudaGenBase with CudaGenDataStruct {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenLanguageOps extends CGenBase {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

