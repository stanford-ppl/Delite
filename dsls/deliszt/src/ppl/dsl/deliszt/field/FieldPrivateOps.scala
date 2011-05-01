package ppl.dsl.deliszt.field

import ppl.dsl.delizst.datastruct.CudaGenDataStruct
import java.io.PrintWriter
import ppl.dsl.deliszt.datastruct.scala._

import ppl.delite.framework.DSLType
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait FieldPrivateOps extends DSLType with Variables {
  this: DeLiszt =>

  object Field {
    def apply[MO<:MeshObj:Manifest,VT:Manifest](mesh: Rep[Mesh]) = field_obj_new(mesh)
  }

  implicit def repFieldToFieldOps[MO <: MeshObj, VT : Manifest](x: Rep[Field[MO, VT]]) = new fieldOpsCls(x)
  implicit def varToFieldOps[MO <: MeshObj, VT : Manifest](x: Var[Field[MO, VT]]) = new fieldOpsCls(readVar(x))

  /**
   * This class defines the private interface for the Field[T] class.
   */
  class fieldOpsCls[MO <: MeshObj, VT](x: Rep[Field[MO, VT]]) {
    def apply(idx : Rep[Int]) = field_apply(x, idx)
    def update(idx : Rep[Int], v : Rep[VT]) = field_update(x, idx,v)
    def size = field_size(x)
  }

  def field_obj_new[MO<:MeshObj:Manifest,VT:Manifest](mesh: Rep[Mesh]): Rep[Field[MO,VT]]

  def field_apply[MO <: MeshObj, VT](x: Rep[Field[MO, VT]], idx: Rep[Int]) : Rep[VT]
  def field_update[MO <: MeshObj, VT](x: Rep[Field[MO, VT]], mo: Rep[MO], v : Rep[VT]) : Rep[Unit]
  def field_size[MO <: MeshObj, VT](x: Rep[Field[MO, VT]]) : Rep[Int]
}

trait FieldPrivateOpsExp extends FieldPrivateOps with VariablesExp with BaseFatExp with CleanRoom {
  this: DeLisztExp =>

  def reflectPure[A:Manifest](x: Def[A]): Exp[A] = toAtom(x)

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class FieldObjectNew[MO<:MeshObj:Manifest,VT:Manifest](len: Exp[Int], isRow: Exp[Boolean]) extends Def[Field[MO,VT]] {
     val fM = manifest[FieldImpl[A]]
  }

  case class FieldApply[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], idx: Exp[Int]) extends Def[VT]
  case class FieldUpdate[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], idx: Exp[Int], v: Exp[VT]) extends Def[Unit]

  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait BaseGenFieldPrivateOps extends GenericFatCodegen {
  val IR: FieldPrivateOpsExp
  import IR._

  override def unapplySimpleIndex(e: Def[Any]) = e match { // TODO: move elsewhere
    case _ => super.unapplySimpleIndex(e)
  }
}

trait ScalaGenFieldPrivateOps extends BaseGenFieldPrivateOps with ScalaGenFat {
  val IR: FieldPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case f@FieldObjectNew(mesh) => emitValDef(sym, remap(f.fM) + "(" + quote(mesh) + ")")
      case FieldApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case FieldUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenFieldPrivateOps extends BaseGenFieldPrivateOps with CudaGenFat with CudaGenDataStruct {
  val IR: FieldPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenFieldPrivateOps extends BaseGenFieldPrivateOps with CGenFat {
  val IR: FieldPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
