package ppl.dsl.deliszt

import datastruct.scala._
import datastruct.scala.MetaInteger._
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
  def _init() : Unit

  def Print(as : Rep[Any]*) : Unit
	def FieldWithConst[MO<:MeshObj, VT](url : Rep[VT])(implicit mo : Manifest[MO], vt : Manifest[VT]) : Rep[Field[MO,VT]]
	def FieldWithURL[MO<:MeshObj, VT](url : String)(implicit mo : Manifest[MO], vt : Manifest[VT]) : Rep[Field[MO,VT]]
	def BoundarySet[MO<:MeshObj](name : String)(implicit mo : Manifest[MO]) : Rep[Set[MO]]

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
	def size[MO<:MeshObj](s : Rep[Set[MO]])(implicit m : Manifest[MO]) : Rep[Int]

	def ID[MO<:MeshObj](x : Rep[MO]) : Rep[Int]

	def foreach[MO<:MeshObj](x : Rep[Set[MO]], fn : Rep[Unit]) : Rep[Unit]

  def MATH_PI() = Rep[Float]
  def MIN_FLOAT() = Rep[Float]
  def MAX_FLOAT() = Rep[Float]
  def sqrt(a: Rep[Float]) : Rep[Float]
  def fabs(value : Rep[Float]) : Rep[Float]
  def MPI_Wtime() : Rep[Float]
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: DeLisztExp with LanguageImplOps =>

  /******* Ops *********/
  case class DeLisztInit() extends Def[Unit]
  case class DeLisztPrint(as: Exp[Any]*) extends Def[Unit]

  case class DeLisztFieldWithConst[MO<:MeshObj : Manifest, VT : Manifest](url : Exp[VT]) extends Def[Field[MO,VT]]
  case class DeLisztFieldWithURL[MO<:MeshObj : Manifest, VT : Manifest](url : Exp[String]) extends Def[Field[MO,VT]]
  case class DeLisztBoundarySet[MO<:MeshObj : Manifest](name : Exp[String]) extends Def[Set[MO]]

  case class DeLisztMesh() extends Def[Mesh]

  case class DeLisztVertices[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[Set[Vertex]]

  case class DeLisztVerticesFaceCCW(e : Exp[Face]) extends Def[Set[Vertex]]
  case class DeLisztVerticesFaceCW(e : Exp[Face]) extends Def[Set[Vertex]]

  case class DeLisztCells[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[Set[Cell]]

  case class DeLisztCellsEdgeCCW(e : Exp[Edge]) extends Def[Set[Cell]]
  case class DeLisztCellsEdgeCW(e : Exp[Edge]) extends Def[Set[Cell]]

  case class DeLisztEdges[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[Set[Edge]]

  case class DeLisztFaces[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[Set[Face]]

  case class DeLisztFacesEdgeCCW(e : Exp[Edge]) extends Def[Set[Face]]
  case class DeLisztFacesEdgeCW(e : Exp[Edge]) extends Def[Set[Face]]

  case class DeLisztEdgeHead(e : Exp[Edge]) extends Def[Vertex]
  case class DeLisztEdgeTail(e : Exp[Edge]) extends Def[Vertex]

  case class DeLisztFaceInside(e : Exp[Face]) extends Def[Cell]
  case class DeLisztFaceOutside(e : Exp[Face]) extends Def[Cell]

  case class DeLisztFlip[MO<:MeshObj:Manifest](e: Exp[MO]) extends Def[MO]

  case class DeLisztTowardsEdgeVertex(e : Exp[Edge], v: Exp[Vertex]) extends Def[Edge]
  case class DeLisztTowardsFaceCell(e : Exp[Face], c: Exp[Cell]) extends Def[Face]

  case class DeLisztSize[MO<:MeshObj : Manifest](s: Exp[Set[MO]]) extends Def[Int]

  case class DeLisztForeach[MO<:MeshObj : Manifest](x: Exp[Set[MO]], v: Sym[MO], func: Exp[Unit])
    extends DeliteOpForeach[MO,Set]
  
  case class DeLisztID[MO<:MeshObj : Manifest](x: Exp[MO]) extends Def[Int]

  case class MathPi() extends Def[Float]
  case class MinFloat() extends Def[Float]
  case class MaxFloat() extends Def[Float]
  case class MathSqrt(a: Exp[Float]) extends Def[Float]
  case class MathFAbs(a: Exp[Float]) extends Def[Float]
  case class MPIWTime() extends Def[Float]

  /******* Language fucntions *********/
  def _init() = reflectEffect(DeLizstInit())
  def Print(as: Exp[Any]*) = reflectEffect(DeLisztPrint(as))

  def FieldWithConst[MO<:MeshObj, VT](url : Exp[VT])(implicit mo : Manifest[MO], vt : Manifest[VT])
    = reflectMutable(DeLisztFieldWithConst(url))
	def FieldWithURL[MO<:MeshObj, VT](url : String)(implicit mo : Manifest[MO], vt : Manifest[VT])
    = reflectMutable(DeLisztFieldWithURL(url))
	def BoundarySet[MO<:MeshObj](name : String)(implicit mo : Manifest[MO])
    = reflectPure(DeLisztBoundarySet(name))

	def mesh = reflectPure(DeLisztMesh())

	def vertices(e : Exp[Mesh])(implicit x : Overloaded1) = reflectPure(DeLisztVertices(e))
	def vertices(e : Exp[Vertex])(implicit x : Overloaded2) = reflectPure(DeLisztVertices(e))
	def vertices(e : Exp[Edge])(implicit x : Overloaded3) = reflectPure(DeLisztVertices(e))
	def vertices(e : Exp[Face])(implicit x : Overloaded4) = reflectPure(DeLisztVertices(e))
	def vertices(e : Exp[Cell])(implicit x : Overloaded5) = reflectPure(DeLisztVertices(e))

	def verticesCCW(e : Exp[Face]) = reflectPure(DeLisztVerticesFaceCCW(e))
	def verticesCW(e : Exp[Face]) = reflectPure(DeLisztVerticesFaceCW(e))

	def cells(e : Exp[Mesh])(implicit x : Overloaded1) = reflectPure(DeLisztCells(e))
	def cells(e : Exp[Vertex])(implicit x : Overloaded2) = reflectPure(DeLisztCells(e))
	def cells(e : Exp[Edge])(implicit x : Overloaded3) = reflectPure(DeLisztCells(e))
	def cells(e : Exp[Face])(implicit x : Overloaded4) = reflectPure(DeLisztCells(e))
	def cells(e : Exp[Cell])(implicit x : Overloaded5) = reflectPure(DeLisztCells(e))

	def cellsCCW(e : Exp[Edge]) = reflectPure(DeLisztCellsEdgeCCW(e))
	def cellsCW(e : Exp[Edge]) = reflectPure(DeLisztCellsEdgeCW(e))

	def edges(e : Exp[Mesh])(implicit x : Overloaded1) = reflectPure(DeLisztEdges(e))
	def edges(e : Exp[Vertex])(implicit x : Overloaded2) = reflectPure(DeLisztEdges(e))
	def edges(e : Exp[Face])(implicit x : Overloaded3) = reflectPure(DeLisztEdges(e))
	def edges(e : Exp[Cell])(implicit x : Overloaded4) = reflectPure(DeLisztEdges(e))

	def edgesCCW(e : Exp[Face]) = reflectPure(DeLisztEdgesFaceCCW(e))
	def edgesCW(e : Exp[Face]) = reflectPure(DeLisztEdgesFaceCW(e))

	def faces(e : Exp[Mesh])(implicit x : Overloaded1) = reflectPure(DeLisztFaces(e))
	def faces(e : Exp[Vertex])(implicit x : Overloaded2) = reflectPure(DeLisztFaces(e))
	def faces(e : Exp[Edge])(implicit x : Overloaded3) = reflectPure(DeLisztFaces(e))
	def faces(e : Exp[Cell])(implicit x : Overloaded4) = reflectPure(DeLisztFaces(e))

	def facesCCW(e : Exp[Edge]) = reflectPure(DeLisztFacesEdgeCCW(e))
	def facesCW(e : Exp[Edge]) = reflectPure(DeLisztFacesEdgeCW(e))

	def head(e : Exp[Edge]) = reflectPure(DeLisztEdgeHead(e))
	def tail(e : Exp[Edge]) = reflectPure(DeLisztEdgeTail(e))

	def inside(e : Exp[Face]) = reflectPure(DeLisztFaceInside(e))
	def outside(e : Exp[Face]) = reflectPure(DeLisztFaceOutside(e))

	def flip(e : Exp[Edge])(implicit x : Overloaded1) = reflectPure(DeLisztFlip(e))
	def flip(e : Exp[Face])(implicit x : Overloaded2) = reflectPure(DeLisztFlip(e))

	def towards(e : Exp[Edge], v: Exp[Vertex])(implicit x : Overloaded1) = reflectPure(DeLisztTowardsEdgeVertex(e,v))
	def towards(e : Exp[Face], c: Exp[Cell])(implicit x : Overloaded2) = reflectPure(DeLisztTowardsFaceCell(e,c))
	def size[MO<:MeshObj](s : Exp[Set[MO]])(implicit m : Manifest[MO]) = reflectPure(DeLisztSize(s))

	def foreach[MO<:MeshObj](x : Exp[Set[MO]], block : Exp[Unit]) = {
    val v = fresh[MO]
    val func = reifyEffects(block(v))
    reflectEffect(DeLisztForeach(x, v, func))
  }

	def ID[MO<:MeshObj](x : Exp[MO]) = reflectPure(DeLisztID(x))

  def MATH_PI() = MathPi()
  def MIN_FLOAT() = MinFloat()
  def MAX_FLOAT() = MaxFloat()
  def sqrt(a: Exp[Float]) = MathSqrt(a)
  def fabs(value: Exp[Float]) = MathFAbs(a)
  def MPI_Wtime() = MPIWTime()
}

trait BaseGenLanguageOps extends GenericFatCodegen {
  val IR: LanguageOpsExp
}

trait ScalaGenLanguageOps extends ScalaGenEffect with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case DeLisztInit() => emitValDef(sym, "generated.scala.Liszt.init()")
      case DeLisztPrint(as) => emitValDef(sym, "generated.scala.Misc.Print(" + quote(as) + ")")
      case DeLisztFieldWithConst(url) => emitValDef(sym, "FieldWithConst(url)")
      case DeLisztFieldWithURL(url) => emitValDef(sym, "FieldWithURL(url)")
      case DeLisztMesh() => emitValDef(sym, "generated.scala.Mesh.mesh")
      case DeLisztBoundarySet(name) => emitValDef(sym, "")

      case DeLisztCells(e) => emitValDef(sym, "generated.scala.Mesh.cells(" + quote(e) + ")")
      case DeLisztCellsEdgeCCW(e) => emitValDef(sym, "generated.scala.Mesh.cellsCCW(" + quote(e) + ")")
      case DeLisztCellsEdgeCW(e) => emitValDef(sym, "generated.scala.Mesh.cellsCW(" + quote(e) + ")")
      case DeLisztFaceInside(e) => emitValDef(sym, "generated.scala.Mesh.inside(" + quote(e) + ")")
      case DeLisztFaceOutside(e) => emitValDef(sym, "generated.scala.Mesh.outside(" + quote(e) + ")")

      case DeLisztEdges(e) => emitValDef(sym, "generated.scala.Mesh.edges(" + quote(e) + ")")
      case DeLisztEdgeHead(e) => emitValDef(sym, "generated.scala.Mesh.head(" + quote(e) + ")")
      case DeLisztEdgeTail(e) => emitValDef(sym, "generated.scala.Mesh.tail(" + quote(e) + ")")

      case DeLisztFaces(e) => emitValDef(sym, "generated.scala.Mesh.faces(" + quote(e) + ")")
      case DeLisztFacesEdgeCCW(e) => emitValDef(sym, "generated.scala.Mesh.facesCCW(" + quote(e) + ")")
      case DeLisztFacesEdgeCW(e) => emitValDef(sym, "generated.scala.Mesh.facesCW(" + quote(e) + ")")

      case DeLisztVertices(e) => emitValDef(sym, "generated.scala.Mesh.vertices(" + quote(e) + ")")
      case DeLisztVerticesFaceCCW(e) => emitValDef(sym, "generated.scala.Mesh.verticesCCW(" + quote(e) + ")")
      case DeLisztVerticesFaceCW(e) => emitValDef(sym, "generated.scala.Mesh.verticesCW(" + quote(e) + ")")

      case DeLisztFlip(e) => emitValDef(sym, "generated.scala.Mesh.flip(" + quote(e) + ")")

      case MathPi() => emitValDef(sym, "Math.Pi")
      case MinFloat() => emitValDef(sym, "scala.Float.MinValue")
      case MaxFloat() => emitValDef(sym, "scala.Float.MaxValue")
      case MathSqrt(a) => emitValDef(sym, "Math.sqrt(" + quote(a) + ")")
      case MathFAbs(a) => emitValDef(sym, "Math.abs(" + quote(a) + ")")
      case MPIWTime() => emitValDef(sym, "SOMETHING")
      case _ => super.emitNode(sym, rhs)
    }
  }

  /*
  case class DeLisztTowardsEdgeVertex(e : Exp[Edge], v: Exp[Vertex]) extends Def[Edge]
  case class DeLisztTowardsFaceCell(e : Exp[Face], c: Exp[Cell]) extends Def[Face]

  case class DeLisztSize[MO<:MeshObj : Manifest](s: Exp[Set[MO]]) extends Def[Int]

  case class DeLisztForeach[MO<:MeshObj : Manifest](x: Exp[Set[MO]], v: Sym[MO], func: Exp[Unit])
    extends DeliteOpForeach[MO,Set]

  case class DeLisztID[MO<:MeshObj : Manifest](x: Exp[MO]) extends Def[Int]
   */
}

