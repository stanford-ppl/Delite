package ppl.dsl.deliszt.meshset

import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import java.io.PrintWriter
import ppl.dsl.deliszt.datastruct.scala._

import ppl.delite.framework.DSLType
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait MeshSetOps extends DSLType with Variables {
  this: DeLiszt =>

  implicit def repMeshSetToMeshSetOps[MO <: MeshObj:Manifest](x: Rep[MeshSet[MO]]) = new meshSetOpsCls[MO](x)
  implicit def varToMeshSetOps[MO <: MeshObj:Manifest](x: Var[MeshSet[MO]]) = new meshSetOpsCls[MO](readVar(x))

  /**
   * This class defines the public interface for the Field[T] class.
   */
  class meshSetOpsCls[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]]) {
    def foreach(block: Rep[MO] => Rep[Unit]) = meshset_foreach(x, block)
  }

  def meshset_foreach[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]], block: Rep[MO] => Rep[Unit]) : Rep[Unit]
}

trait MeshSetOpsExp extends MeshSetOps with VariablesExp with BaseFatExp {
  this: DeLisztExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class MeshSetForeach[MO<:MeshObj:Manifest](in: Exp[MeshSet[MO]], func: Exp[MO] => Exp[Unit])
    extends DeliteOpForeach[MO] {

    def sync = n => List()
    val size = in.size
  }

  ////////////////////////////////
  // implemented via delite ops

  //////////////
  // mirroring

/*  override def mirror[VT:Manifest](e: Def[VT], f: Transformer): Exp[VT] = (e match {
    case FieldApply(x, n) => vec_apply(f(x), f(n))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[VT]] */

  /////////////////////
  // object interface

  /////////////////////
  // class interface
  def meshset_foreach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], block: Exp[MO] => Exp[Unit]) = MeshSetForeach(x, block)
}

trait MeshSetOpsExpOpt extends MeshSetOpsExp {
  this: DeLisztExp =>
}

trait ScalaGenFieldOps extends ScalaGenBase {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenFieldOps extends CudaGenBase with CudaGenDataStruct {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenFieldOps extends CGenBase {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
