package ppl.dsl.deliszt.mesh

import java.io.PrintWriter

import ppl.delite.framework.DSLType
import scala.virtualization.lms.common._
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}
import ppl.dsl.deliszt.datastruct._
import ppl.dsl.deliszt.datastruct.scala._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 05/03/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait MeshPrivateOps extends DSLType with Variables {
  this: DeLiszt =>

  def meshSet[MO<:MeshObj:Manifest] : Rep[MeshSet[MO]]
  def label[MO<:MeshObj:Manifest,VT:Manifest] : Rep[LabelField[MO,VT]]
}

trait MeshPrivateOpsExp extends MeshPrivateOps with VariablesExp with BaseFatExp {
  this: DeLisztExp =>

  case class MeshLoaderInit() extends Def[Unit]
  case class MeshSetNew[MO<:MeshObj:Manifest]() extends Def[MeshSet[MO]] {
    val moM = manifest[MO]
  }
  
  case class LabelFieldPrivateNew[MO<:MeshObj:Manifest,VT:Manifest]() extends Def[LabelField[MO,VT]] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }

  def meshSet[MO<:MeshObj:Manifest] = MeshSetNew[MO]()
  def label[MO<:MeshObj:Manifest,VT:Manifest] = reflectMutable(LabelFieldPrivateNew[MO,VT]())
}

trait ScalaGenVecPrivateOps extends ScalaGenBase {
  val IR: MeshPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case ms@MeshSetNew() => emitValDef(sym, "generated.scala.datastruct.Mesh.mesh.meshSet[" + remap(ms.moM) + "]")
      case lf@LabelFieldPrivateNew() => emitValDef(sym, "generated.scala.datastruct.Mesh.mesh.label[" + remap(lf.moM) + "," + remap(lf.vtM) + "]")
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenVecPrivateOps extends CudaGenBase with CudaGenDataStruct {
  val IR: MeshPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVecPrivateOps extends CGenBase {
  val IR: MeshPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
