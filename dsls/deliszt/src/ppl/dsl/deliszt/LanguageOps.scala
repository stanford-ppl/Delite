package ppl.dsl.deliszt

import java.io.PrintWriter
import reflect.Manifest

import scala.virtualization.lms.internal.{GenericFatCodegen, GenerationFailedException}
import scala.virtualization.lms.common._

import ppl.delite.framework.ops.DeliteOpsExp

/* Machinery provided by DeLiszt itself (language features and control structures).
 *
 * author: Michael Wu (mikemwu@stanford.edu)
 * created: Mar 14, 2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait LanguageOps extends Base { this: DeLiszt with MathOps =>
  def _init(args: Rep[Array[String]]) : Unit

  def Print(as: Rep[Any]*) : Unit
  def BoundarySet[MO<:Cell:Manifest](name: Rep[String])(implicit ev : MO =:= Cell) : Rep[MeshSet[Cell]]
  def BoundarySet[MO<:Edge:Manifest](name: Rep[String])(implicit ev : MO =:= Edge, o: Overloaded1) : Rep[MeshSet[Edge]]
  def BoundarySet[MO<:Face:Manifest](name: Rep[String])(implicit ev : MO =:= Face, o: Overloaded2) : Rep[MeshSet[Face]]
  def BoundarySet[MO<:Vertex:Manifest](name: Rep[String])(implicit ev : MO =:= Vertex, o: Overloaded3) : Rep[MeshSet[Vertex]]

  def mesh: Rep[Mesh]

  def vertices(e: Rep[Mesh])(implicit x: Overloaded1) : Rep[MeshSet[Vertex]]
  def vertices(e: Rep[Vertex])(implicit x: Overloaded2) : Rep[MeshSet[Vertex]]
  def vertices(e: Rep[Edge])(implicit x: Overloaded3) : Rep[MeshSet[Vertex]]
  def vertices(e: Rep[Face])(implicit x: Overloaded4) : Rep[MeshSet[Vertex]]
  def vertices(e: Rep[Cell])(implicit x: Overloaded5) : Rep[MeshSet[Vertex]]

  def verticesCCW(e: Rep[Face]) : Rep[MeshSet[Vertex]]
  def verticesCW(e: Rep[Face]) : Rep[MeshSet[Vertex]]
  
  def vertex(e: Rep[Cell], i: Rep[Int]) : Rep[Vertex]

  def cells(e: Rep[Mesh])(implicit x: Overloaded1) : Rep[MeshSet[Cell]]
  def cells(e: Rep[Vertex])(implicit x: Overloaded2) : Rep[MeshSet[Cell]]
  def cells(e: Rep[Edge])(implicit x: Overloaded3) : Rep[MeshSet[Cell]]
  def cells(e: Rep[Face])(implicit x: Overloaded4) : Rep[MeshSet[Cell]]
  def cells(e: Rep[Cell])(implicit x: Overloaded5) : Rep[MeshSet[Cell]]

  def cellsCCW(e: Rep[Edge]) : Rep[MeshSet[Cell]]
  def cellsCW(e: Rep[Edge]) : Rep[MeshSet[Cell]]

  def edges(e: Rep[Mesh])(implicit x: Overloaded1) : Rep[MeshSet[Edge]]
  def edges(e: Rep[Vertex])(implicit x: Overloaded2) : Rep[MeshSet[Edge]]
  def edges(e: Rep[Face])(implicit x: Overloaded3) : Rep[MeshSet[Edge]]
  def edges(e: Rep[Cell])(implicit x: Overloaded4) : Rep[MeshSet[Edge]]

  def edgesCCW(e: Rep[Face]) : Rep[MeshSet[Edge]]
  def edgesCW(e: Rep[Face]) : Rep[MeshSet[Edge]]

  def faces(e: Rep[Mesh])(implicit x: Overloaded1) : Rep[MeshSet[Face]]
  def faces(e: Rep[Vertex])(implicit x: Overloaded2) : Rep[MeshSet[Face]]
  def faces(e: Rep[Edge])(implicit x: Overloaded3) : Rep[MeshSet[Face]]
  def faces(e: Rep[Cell])(implicit x: Overloaded4) : Rep[MeshSet[Face]]

  def facesCCW(e: Rep[Edge]) : Rep[MeshSet[Face]]
  def facesCW(e: Rep[Edge]) : Rep[MeshSet[Face]]
  
  def face(e: Rep[Edge], i: Rep[Int]) : Rep[Face]

  def head(e: Rep[Edge]) : Rep[Vertex]
  def tail(e: Rep[Edge]) : Rep[Vertex]

  def inside(e: Rep[Face]) : Rep[Cell]
  def outside(e: Rep[Face]) : Rep[Cell]

  def flip(e: Rep[Edge])(implicit x: Overloaded1) : Rep[Edge]
  def flip(e: Rep[Face])(implicit x: Overloaded2) : Rep[Face]

  def towards(e: Rep[Edge],v: Rep[Vertex])(implicit x: Overloaded1) : Rep[Edge]
  def towards(e: Rep[Face],c: Rep[Cell])(implicit x: Overloaded2) : Rep[Face]
  def size[MO<:MeshObj:Manifest](s: Rep[MeshSet[MO]]) : Rep[Int]

  def ID[MO<:MeshObj:Manifest](x: Rep[MO]) : Rep[Int]

  def MATH_PI(): Rep[Double]
  def MIN_FLOAT(): Rep[Float]
  def MAX_FLOAT(): Rep[Float]
  def min[A:Manifest:Numeric](x: Rep[A], y: Rep[A]) = math_min(x, y)
  def max[A:Manifest:Numeric](x: Rep[A], y: Rep[A]) = math_max(x, y)
  def sqrt(a: Rep[Double]) = math_sqrt(a)
  def sqrtf(a: Rep[Float]) = math_sqrt(a).asInstanceOfL[Float]
  def expf(a: Rep[Float]) = math_exp(a).asInstanceOfL[Float]
  def sinf(a: Rep[Float]) = math_sin(a).asInstanceOfL[Float]
  def cosf(a: Rep[Float]) = math_cos(a).asInstanceOfL[Float]
  def acosf(a: Rep[Float]) = math_acos(a).asInstanceOfL[Float]
  def atan2f(a: Rep[Float], b: Rep[Float]) = math_atan2(a,b).asInstanceOfL[Float]
  def powf(a: Rep[Float], b: Rep[Float]) = math_pow(a,b).asInstanceOfL[Float]
  def fabs(a: Rep[Float]) = math_abs(a).asInstanceOfL[Float]
  
  def wall_time() : Rep[Double]
  def processor_time() : Rep[Double]
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: LanguageImplOps with DeLisztExp =>

  /******* Ops *********/
  case class DeLisztInit(args: Exp[Array[String]]) extends Def[Unit]
  case class DeLisztPrint(as: Seq[Exp[Any]]) extends DeliteOpSingleTask(reifyEffectsHere(print_impl(as)))

  case class DeLisztBoundarySetCells(name: Exp[String]) extends Def[MeshSet[Cell]]
  case class DeLisztBoundarySetEdges(name: Exp[String]) extends Def[MeshSet[Edge]]
  case class DeLisztBoundarySetFaces(name: Exp[String]) extends Def[MeshSet[Face]]
  case class DeLisztBoundarySetVertices(name: Exp[String]) extends Def[MeshSet[Vertex]]

  case class DeLisztMesh() extends Def[Mesh]
  
  object DeLisztVertices {
    def unapply[MO<:MeshObj:Manifest](m: DeLisztVertices[MO]) = Some(m.e)
  }
  
  abstract class DeLisztVertices[MO<:MeshObj:Manifest](val e: Exp[MO]) extends Def[MeshSet[Vertex]]
  case class DeLisztVerticesCell(override val e: Exp[Cell], mesh: Exp[Mesh]) extends DeLisztVertices[Cell](e)
  case class DeLisztVerticesEdge(override val e: Exp[Edge], mesh: Exp[Mesh]) extends DeLisztVertices[Edge](e)
  case class DeLisztVerticesFace(override val e: Exp[Face], mesh: Exp[Mesh]) extends DeLisztVertices[Face](e)
  case class DeLisztVerticesVertex(override val e: Exp[Vertex], mesh: Exp[Mesh]) extends DeLisztVertices[Vertex](e)
  case class DeLisztVerticesMesh(override val e: Exp[Mesh]) extends DeLisztVertices[Mesh](e)
  
  case class DeLisztVertex(e: Exp[Cell], i: Exp[Int], mesh: Exp[Mesh]) extends Def[Vertex]
  
  case class DeLisztFaceVerticesCCW(e: Exp[Face], mesh: Exp[Mesh]) extends Def[MeshSet[Vertex]]
  case class DeLisztFaceVerticesCW(e: Exp[Face], mesh: Exp[Mesh]) extends Def[MeshSet[Vertex]]

  object DeLisztCells {
    def unapply[MO<:MeshObj:Manifest](m: DeLisztCells[MO]) = Some(m.e)
  }
  
  abstract class DeLisztCells[MO<:MeshObj:Manifest](val e: Exp[MO]) extends Def[MeshSet[Cell]]
  case class DeLisztCellsCell(override val e: Exp[Cell], mesh: Exp[Mesh]) extends DeLisztCells[Cell](e)
  case class DeLisztCellsEdge(override val e: Exp[Edge], mesh: Exp[Mesh]) extends DeLisztCells[Edge](e)
  case class DeLisztCellsFace(override val e: Exp[Face], mesh: Exp[Mesh]) extends DeLisztCells[Face](e)
  case class DeLisztCellsVertex(override val e: Exp[Vertex], mesh: Exp[Mesh]) extends DeLisztCells[Vertex](e)
  case class DeLisztCellsMesh(override val e: Exp[Mesh]) extends DeLisztCells[Mesh](e)
  
  case class DeLisztEdgeCellsCCW(e: Exp[Edge], mesh: Exp[Mesh]) extends Def[MeshSet[Cell]]
  case class DeLisztEdgeCellsCW(e: Exp[Edge], mesh: Exp[Mesh]) extends Def[MeshSet[Cell]]

  object DeLisztEdges {
    def unapply[MO<:MeshObj:Manifest](m: DeLisztEdges[MO]) = Some(m.e)
  }
  
  abstract class DeLisztEdges[MO<:MeshObj:Manifest](val e: Exp[MO]) extends Def[MeshSet[Edge]]
  case class DeLisztEdgesCell(override val e: Exp[Cell], mesh: Exp[Mesh]) extends DeLisztEdges[Cell](e)
  case class DeLisztEdgesFace(override val e: Exp[Face], mesh: Exp[Mesh]) extends DeLisztEdges[Face](e)
  case class DeLisztEdgesVertex(override val e: Exp[Vertex], mesh: Exp[Mesh]) extends  DeLisztEdges[Vertex](e)
  case class DeLisztEdgesMesh(override val e: Exp[Mesh]) extends  DeLisztEdges[Mesh](e)

  object DeLisztFaces {
    def unapply[MO<:MeshObj:Manifest](m: DeLisztFaces[MO]) = Some(m.e)
  }
  
  abstract class DeLisztFaces[MO<:MeshObj:Manifest](val e: Exp[MO]) extends Def[MeshSet[Face]]
  case class DeLisztFacesCell(override val e: Exp[Cell], mesh: Exp[Mesh]) extends DeLisztFaces[Cell](e)
  case class DeLisztFacesEdge(override val e: Exp[Edge], mesh: Exp[Mesh]) extends DeLisztFaces[Edge](e)
  case class DeLisztFacesVertex(override val e: Exp[Vertex], mesh: Exp[Mesh]) extends DeLisztFaces[Vertex](e)
  case class DeLisztFacesMesh(override val e: Exp[Mesh]) extends DeLisztFaces[Mesh](e)

  case class DeLisztEdgeFacesCCW(e: Exp[Edge], mesh: Exp[Mesh]) extends Def[MeshSet[Face]]
  case class DeLisztEdgeFacesCW(e: Exp[Edge], mesh: Exp[Mesh]) extends Def[MeshSet[Face]]
  
  case class DeLisztFaceEdgesCCW(e: Exp[Face], mesh: Exp[Mesh]) extends Def[MeshSet[Edge]]
  case class DeLisztFaceEdgesCW(e: Exp[Face], mesh: Exp[Mesh]) extends Def[MeshSet[Edge]]
  
  case class DeLisztEdgeHead(e: Exp[Edge], mesh: Exp[Mesh]) extends Def[Vertex]
  case class DeLisztEdgeTail(e: Exp[Edge], mesh: Exp[Mesh]) extends Def[Vertex]

  case class DeLisztFaceInside(e: Exp[Face], mesh: Exp[Mesh]) extends Def[Cell]
  case class DeLisztFaceOutside(e: Exp[Face], mesh: Exp[Mesh]) extends Def[Cell]
  
  case class DeLisztFace(e: Exp[Edge], i: Exp[Int], mesh: Exp[Mesh]) extends Def[Face]
  
  case class DeLisztFlipEdge(e: Exp[Edge]) extends Def[Edge]
  case class DeLisztFlipFace(e: Exp[Face]) extends Def[Face]

  case class DeLisztTowardsEdgeVertex(e: Exp[Edge], v: Exp[Vertex], mesh: Exp[Mesh]) extends Def[Edge]
  case class DeLisztTowardsFaceCell(e: Exp[Face], c: Exp[Cell], mesh: Exp[Mesh]) extends Def[Face]

  case class DeLisztSize[MO<:MeshObj:Manifest](s: Exp[MeshSet[MO]]) extends Def[Int]
  
  case class DeLisztID[MO<:MeshObj:Manifest](x: Exp[MO]) extends Def[Int]

  case class MinFloat() extends Def[Float]
  case class MaxFloat() extends Def[Float]
  case class MathFAbs(a: Exp[Float]) extends Def[Float]
  case class WallTime() extends Def[Double]
  case class ProcessorTime() extends Def[Double]

  /******* Language functions *********/
  def _init(args: Exp[Array[String]]) = reflectEffect(DeLisztInit(args))
  def Print(as: Exp[Any]*) = reflectEffect(DeLisztPrint(as))

  def BoundarySet[MO<:Cell:Manifest](name: Exp[String])(implicit ev : MO =:= Cell) = reflectPure(DeLisztBoundarySetCells(name))
  def BoundarySet[MO<:Edge:Manifest](name: Exp[String])(implicit ev : MO =:= Edge, o: Overloaded1) = reflectPure(DeLisztBoundarySetEdges(name))
  def BoundarySet[MO<:Face:Manifest](name: Exp[String])(implicit ev : MO =:= Face, o: Overloaded2) = reflectPure(DeLisztBoundarySetFaces(name))
  def BoundarySet[MO<:Vertex:Manifest](name: Exp[String])(implicit ev : MO =:= Vertex, o: Overloaded3) = reflectPure(DeLisztBoundarySetVertices(name))
  
  def mesh = reflectPure(DeLisztMesh())

  def vertices(e: Exp[Cell])(implicit x: Overloaded5) = reflectPure(DeLisztVerticesCell(e, mesh))
  def vertices(e: Exp[Edge])(implicit x: Overloaded3) = reflectPure(DeLisztVerticesEdge(e, mesh))
  def vertices(e: Exp[Face])(implicit x: Overloaded4) = reflectPure(DeLisztVerticesFace(e, mesh))
  def vertices(e: Exp[Vertex])(implicit x: Overloaded2) = reflectPure(DeLisztVerticesVertex(e, mesh))
  def vertices(e: Exp[Mesh])(implicit x: Overloaded1) = reflectPure(DeLisztVerticesMesh(e))

  def verticesCCW(e: Exp[Face]) = reflectPure(DeLisztFaceVerticesCCW(e, mesh))
  def verticesCW(e: Exp[Face]) = reflectPure(DeLisztFaceVerticesCW(e, mesh))
  
  def vertex(e: Exp[Cell], i: Exp[Int]) = reflectPure(DeLisztVertex(e,i, mesh))

  def cells(e: Exp[Cell])(implicit x: Overloaded5) = reflectPure(DeLisztCellsCell(e, mesh))
  def cells(e: Exp[Edge])(implicit x: Overloaded3) = reflectPure(DeLisztCellsEdge(e, mesh))
  def cells(e: Exp[Face])(implicit x: Overloaded4) = reflectPure(DeLisztCellsFace(e, mesh))
  def cells(e: Exp[Vertex])(implicit x: Overloaded2) = reflectPure(DeLisztCellsVertex(e, mesh))
  def cells(e: Exp[Mesh])(implicit x: Overloaded1) = reflectPure(DeLisztCellsMesh(e))
  
  def cellsCCW(e: Exp[Edge]) = reflectPure(DeLisztEdgeCellsCCW(e, mesh))
  def cellsCW(e: Exp[Edge]) = reflectPure(DeLisztEdgeCellsCW(e, mesh))

  def edges(e: Exp[Cell])(implicit x: Overloaded4) = reflectPure(DeLisztEdgesCell(e, mesh))
  def edges(e: Exp[Face])(implicit x: Overloaded3) = reflectPure(DeLisztEdgesFace(e, mesh))
  def edges(e: Exp[Vertex])(implicit x: Overloaded2) = reflectPure(DeLisztEdgesVertex(e, mesh))
  def edges(e: Exp[Mesh])(implicit x: Overloaded1) = reflectPure(DeLisztEdgesMesh(e))

  def edgesCCW(e: Exp[Face]) = reflectPure(DeLisztFaceEdgesCCW(e, mesh))
  def edgesCW(e: Exp[Face]) = reflectPure(DeLisztFaceEdgesCW(e, mesh))

  def faces(e: Exp[Edge])(implicit x: Overloaded3) = reflectPure(DeLisztFacesEdge(e, mesh))
  def faces(e: Exp[Cell])(implicit x: Overloaded4) = reflectPure(DeLisztFacesCell(e, mesh))
  def faces(e: Exp[Vertex])(implicit x: Overloaded2) = reflectPure(DeLisztFacesVertex(e, mesh))
  def faces(e: Exp[Mesh])(implicit x: Overloaded1) = reflectPure(DeLisztFacesMesh(e))

  def facesCCW(e: Exp[Edge]) = reflectPure(DeLisztEdgeFacesCCW(e, mesh))
  def facesCW(e: Exp[Edge]) = reflectPure(DeLisztEdgeFacesCW(e, mesh))
  
  def face(e: Exp[Edge], i: Exp[Int]) = reflectPure(DeLisztFace(e, i, mesh))

  def head(e: Exp[Edge]) = reflectPure(DeLisztEdgeHead(e, mesh))
  def tail(e: Exp[Edge]) = reflectPure(DeLisztEdgeTail(e, mesh))

  def inside(e: Exp[Face]) = reflectPure(DeLisztFaceInside(e, mesh))
  def outside(e: Exp[Face]) = reflectPure(DeLisztFaceOutside(e, mesh))

  def flip(e: Exp[Edge])(implicit x: Overloaded1) = reflectPure(DeLisztFlipEdge(e))
  def flip(e: Exp[Face])(implicit x: Overloaded2) = reflectPure(DeLisztFlipFace(e))

  def towards(e: Exp[Edge], v: Exp[Vertex])(implicit x: Overloaded1) = reflectPure(DeLisztTowardsEdgeVertex(e,v, mesh))
  def towards(e: Exp[Face], c: Exp[Cell])(implicit x: Overloaded2) = reflectPure(DeLisztTowardsFaceCell(e,c, mesh))
  
  def size[MO<:MeshObj:Manifest](s: Exp[MeshSet[MO]]) = reflectPure(DeLisztSize(s))

  def ID[MO<:MeshObj:Manifest](x: Exp[MO]) = reflectPure(DeLisztID(x))

  def MATH_PI() = reflectPure(MathPi())
  def MIN_FLOAT() = reflectPure(MinFloat())
  def MAX_FLOAT() = reflectPure(MaxFloat())
  
  def wall_time() = reflectEffect(WallTime())
  def processor_time() = reflectEffect(ProcessorTime())
}

trait ScalaGenLanguageOps extends ScalaGenBase {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case DeLisztInit(args) => emitValDef(sym, "generated.scala.Liszt.init(" + quote(args) + ")")
      case DeLisztMesh() => emitValDef(sym, "generated.scala.Mesh.mesh")
      
      case DeLisztBoundarySetCells(name) => emitValDef(sym, "generated.scala.Mesh.boundarySetCells(" + quote(name) + ")")
      case DeLisztBoundarySetEdges(name) => emitValDef(sym, "generated.scala.Mesh.boundarySetCells(" + quote(name) + ")")
      case DeLisztBoundarySetFaces(name) => emitValDef(sym, "generated.scala.Mesh.boundarySetCells(" + quote(name) + ")")
      case DeLisztBoundarySetVertices(name) => emitValDef(sym, "generated.scala.Mesh.boundarySetCells(" + quote(name) + ")")

      case DeLisztCellsCell(e, m) => emitValDef(sym, "generated.scala.Mesh.cellsCell(" + quote(e) + ")")
      case DeLisztCellsEdge(e, m) => emitValDef(sym, "generated.scala.Mesh.cellsEdge(" + quote(e) + ")")
      case DeLisztCellsFace(e, m) => emitValDef(sym, "generated.scala.Mesh.cellsFace(" + quote(e) + ")")
      case DeLisztCellsVertex(e, m) => emitValDef(sym, "generated.scala.Mesh.cellsVertex(" + quote(e) + ")")
      case DeLisztCellsMesh(e) => emitValDef(sym, "generated.scala.Mesh.cellsMesh(" + quote(e) + ")")
      
      case DeLisztEdgeCellsCCW(e,m) => emitValDef(sym, "generated.scala.Mesh.cellsCCW(" + quote(e) + ")")
      case DeLisztEdgeCellsCW(e,m) => emitValDef(sym, "generated.scala.Mesh.cellsCW(" + quote(e) + ")")
      case DeLisztFaceInside(e,m) => emitValDef(sym, "generated.scala.Mesh.inside(" + quote(e) + ")")
      case DeLisztFaceOutside(e,m) => emitValDef(sym, "generated.scala.Mesh.outside(" + quote(e) + ")")

      case DeLisztEdgesCell(e, m) => emitValDef(sym, "generated.scala.Mesh.edgesCell(" + quote(e) + ")")
      case DeLisztEdgesFace(e, m) => emitValDef(sym, "generated.scala.Mesh.edgesFace(" + quote(e) + ")")
      case DeLisztEdgesVertex(e, m) => emitValDef(sym, "generated.scala.Mesh.edgesVertex(" + quote(e) + ")")
      case DeLisztEdgesMesh(e) => emitValDef(sym, "generated.scala.Mesh.edgesMesh(" + quote(e) + ")")
      
      case DeLisztEdgeHead(e,m) => emitValDef(sym, "generated.scala.Mesh.head(" + quote(e) + ")")
      case DeLisztEdgeTail(e,m) => emitValDef(sym, "generated.scala.Mesh.tail(" + quote(e) + ")")
      
      case DeLisztEdgeFacesCCW(e,m) => emitValDef(sym, "generated.scala.Mesh.facesCCW(" + quote(e) + ")")
      case DeLisztEdgeFacesCW(e,m) => emitValDef(sym, "generated.scala.Mesh.facesCW(" + quote(e) + ")")

      case DeLisztFacesCell(e, m) => emitValDef(sym, "generated.scala.Mesh.facesCell(" + quote(e) + ")")
      case DeLisztFacesEdge(e, m) => emitValDef(sym, "generated.scala.Mesh.facesEdge(" + quote(e) + ")")
      case DeLisztFacesVertex(e, m) => emitValDef(sym, "generated.scala.Mesh.facesVertex(" + quote(e) + ")")
      case DeLisztFacesMesh(e) => emitValDef(sym, "generated.scala.Mesh.facesMesh(" + quote(e) + ")")
      
      case DeLisztFaceEdgesCCW(e,m) => emitValDef(sym, "generated.scala.Mesh.edgesCCW(" + quote(e) + ")")
      case DeLisztFaceEdgesCW(e,m) => emitValDef(sym, "generated.scala.Mesh.edgesCW(" + quote(e) + ")")
      case DeLisztFace(e,i,m) => emitValDef(sym, "generated.scala.Mesh.face(" + quote(e) + "," + quote(i) + ")")

      case DeLisztVerticesCell(e, m) => emitValDef(sym, "generated.scala.Mesh.verticesCell(" + quote(e) + ")")
      case DeLisztVerticesEdge(e, m) => emitValDef(sym, "generated.scala.Mesh.verticesEdge(" + quote(e) + ")")
      case DeLisztVerticesFace(e, m) => emitValDef(sym, "generated.scala.Mesh.verticesFace(" + quote(e) + ")")
      case DeLisztVerticesVertex(e, m) => emitValDef(sym, "generated.scala.Mesh.verticesVertex(" + quote(e) + ")")
      case DeLisztVerticesMesh(e) => emitValDef(sym, "generated.scala.Mesh.verticesMesh(" + quote(e) + ")")
      
      case DeLisztFaceVerticesCCW(e,m) => emitValDef(sym, "generated.scala.Mesh.verticesCCW(" + quote(e) + ")")
      case DeLisztFaceVerticesCW(e,m) => emitValDef(sym, "generated.scala.Mesh.verticesCW(" + quote(e) + ")")
      case DeLisztVertex(e,i,m) => emitValDef(sym, "generated.scala.Mesh.vertex(" + quote(e) + "," + quote(i) + ")")

      case DeLisztFlipEdge(e) => emitValDef(sym, "generated.scala.Mesh.flip(" + quote(e) + ")")
      case DeLisztFlipFace(e) => emitValDef(sym, "generated.scala.Mesh.flip(" + quote(e) + ")")
      
      case DeLisztTowardsEdgeVertex(e,v,m) => emitValDef(sym, "generated.scala.Mesh.towardsEdgeVertex(" + quote(e) + "," + quote(v) + ")")
      case DeLisztTowardsFaceCell(e,c,m) => emitValDef(sym, "generated.scala.Mesh.towardsFaceCell(" + quote(e) + "," + quote(c) + ")")
      
      case DeLisztID(x) => emitValDef(sym, "generated.scala.Mesh.internalId(" + quote(x) + ")")
      
      case DeLisztSize(s) => emitValDef(sym, quote(s) + ".size")

      case MinFloat() => emitValDef(sym, "scala.Float.MinValue")
      case MaxFloat() => emitValDef(sym, "scala.Float.MaxValue")
      case MathFAbs(a) => emitValDef(sym, "Math.abs(" + quote(a) + ")")
      case ProcessorTime() => emitValDef(sym, "generated.scala.Mesh.processor_time()")
      case WallTime() => emitValDef(sym, "generated.scala.Mesh.wall_time()")
      case _ => super.emitNode(sym, rhs)
    }
  }
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

