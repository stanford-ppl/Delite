package ppl.dsl.deliszt.analysis

import java.io.{PrintWriter}

import scala.collection.immutable.Set
import scala.collection.mutable.{Set => MSet, Map}

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

import ppl.delite.framework.DeliteApplication

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt._

object MeshObjVal {
}

trait MeshObjVal {
  def +(b: MeshObjVal) : MeshObjVal = new MeshObjValImpl(objs ++ b.objs)
  val objs : Set[MeshObj]
}

class MeshObjValImpl(val objs : Set[MeshObj] = Set[MeshObj]()) extends MeshObjVal {
}

case class NoObjs() extends MeshObjVal {
  val objs = Set[MeshObj]()
  
  override def +(b: MeshObjVal) = b
}

case class MeshObjSetVal[MO<:MeshObj](val ms : MeshSet[MO]) {
  val objs = ms.toSet
} 

class ReadWriteSet {
  val read = MSet[MeshObj]()
  val write = MSet[MeshObj]()
}

trait DeLisztCodeGenAnalysis extends DeLisztCodeGenScala {
val IR: DeliteApplication with DeLisztExp
  import IR._

  type StencilMap = Map[MeshObj,ReadWriteSet]
  val forMap = Map[String,StencilMap]()
  val syms = Map[String,MeshObjVal]()
  
  def storeSym(sym: String, mo: MeshObjVal) {
    syms(sym) = mo
  }
  
  def getSym(sym: String) = {
    syms.getOrElse(sym, NoObjs())
  }
  
  def getValue(node: Def[Any]) {
    node match {
      case DeLisztBoundarySet(name) => NoObjs()

      case DeLisztMesh() => Nil

      case DeLisztVertices(e) => Nil

      case DeLisztVertex(e, i) => Nil

      case DeLisztFaceVerticesCCW(e) => Nil
      case DeLisztFaceVerticesCW(e) => Nil

      case DeLisztCells(e) => Nil

      case DeLisztEdgeCellsCCW(e) => Nil
      case DeLisztEdgeCellsCW(e) => Nil

      case DeLisztEdges(e) => Nil

      case DeLisztFaces(e) => Nil

      case DeLisztEdgeFacesCCW(e) => Nil
      case DeLisztEdgeFacesCW(e) => Nil
  
      case DeLisztFaceEdgesCCW(e) => Nil
      case DeLisztFaceEdgesCW(e) => Nil

      case DeLisztEdgeHead(e) => Nil
      case DeLisztEdgeTail(e) => Nil

      case DeLisztFaceInside(e) => Nil
      case DeLisztFaceOutside(e) => Nil
  
      case DeLisztFace(e, i) => Nil

      case DeLisztFlip(e) => Nil

      case DeLisztTowardsEdgeVertex(e, v) => Nil
      case DeLisztTowardsFaceCell(e, c) => Nil
      
      case While(c,b) => Nil
      case IfThenElse(c,a,b) => Nil
      case MeshSetForeach(m, f) => Nil
      case FieldApply(f,i) => Nil
        
      case FieldPlusUpdate(f,i,v) => Nil
      case FieldTimesUpdate(f,i,v) => Nil
      case FieldMinusUpdate(f,i,v) => Nil
      case FieldDivideUpdate(f,i,v) => Nil
      
      // Try to get rid of arithmetic?
      case ArithPlus(l,r) => Nil
      case ArithMinus(l,r) => Nil
      case ArithTimes(l,r) => Nil
      case ArithNegate(l) => Nil
      case ArithFractionalDivide(l,r) => Nil
      case ArithAbs(l) => Nil
      case ArithExp(l) => Nil
      case OrderingMin(l,r) => Nil
      case OrderingMax(l,r) => Nil
      case _ => NoObjs()
    }
  }
  
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      // MESH ACCESSES
      case DeLisztBoundarySet(name) => Nil

      case DeLisztMesh() => Nil

      case DeLisztVertices(e) => Nil

      case DeLisztVertex(e, i) => Nil

      case DeLisztFaceVerticesCCW(e) => Nil
      case DeLisztFaceVerticesCW(e) => Nil

      case DeLisztCells(e) => Nil

      case DeLisztEdgeCellsCCW(e) => Nil
      case DeLisztEdgeCellsCW(e) => Nil

      case DeLisztEdges(e) => Nil

      case DeLisztFaces(e) => Nil

      case DeLisztEdgeFacesCCW(e) => Nil
      case DeLisztEdgeFacesCW(e) => Nil
  
      case DeLisztFaceEdgesCCW(e) => Nil
      case DeLisztFaceEdgesCW(e) => Nil

      case DeLisztEdgeHead(e) => Nil
      case DeLisztEdgeTail(e) => Nil

      case DeLisztFaceInside(e) => Nil
      case DeLisztFaceOutside(e) => Nil
  
      case DeLisztFace(e, i) => Nil

      case DeLisztFlip(e) => Nil

      case DeLisztTowardsEdgeVertex(e, v) => Nil
      case DeLisztTowardsFaceCell(e, c) => Nil
      
      // While loops. Execute once. Store results if results are a mesh element
      case While(c,b) => Nil
      // Execute both branches. Store results if results are a mesh element
      case IfThenElse(c,a,b) => Nil
      // Foreach, only apply to top level foreach though...
      case MeshSetForeach(m, f) => {
          // Mark top level foreach
        }
      // Just mark accesses
      case FieldApply(f,i) => Nil
        
      case FieldPlusUpdate(f,i,v) => Nil
      case FieldTimesUpdate(f,i,v) => Nil
      case FieldMinusUpdate(f,i,v) => Nil
      case FieldDivideUpdate(f,i,v) => Nil
      // Try to get rid of arithmetic?
      case ArithPlus(l,r) => super.emitNode(sym, rhs)
      case ArithMinus(l,r) => super.emitNode(sym, rhs)
      case ArithTimes(l,r) => super.emitNode(sym, rhs)
      case ArithNegate(l) => super.emitNode(sym, rhs)
      case ArithFractionalDivide(l,r) => super.emitNode(sym, rhs)
      case ArithAbs(l) => super.emitNode(sym, rhs)
      case ArithExp(l) => super.emitNode(sym, rhs)
      case OrderingMin(l,r) => super.emitNode(sym, rhs)
      case OrderingMax(l,r) => super.emitNode(sym, rhs)
      case _ => Nil
    }
  
    super.emitNode(sym, rhs)
  }
}
