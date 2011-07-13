package ppl.dsl.deliszt.field

import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
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
    def apply[MO<:MeshObj:Manifest,VT:Manifest]() = field_obj_new[MO,VT]()
  }

  implicit def repFieldToFieldPrivateOps[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]]) = new fieldPrivateOpsCls(x)
  implicit def varToFieldPrivateOps[MO<:MeshObj:Manifest,VT:Manifest](x: Var[Field[MO,VT]]) = new fieldPrivateOpsCls(readVar(x))

  /**
   * This class defines the private interface for the Field[T] class.
   */
  class fieldPrivateOpsCls[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]]) {
    def apply(n : Rep[Int]) = field_apply(x, n)
    def update(n : Rep[Int], v : Rep[VT]) = field_update(x, n,v)
    def size = field_size(x)
  }

  def label[MO<:MeshObj:Manifest,VT:Manifest](url: Rep[String]): Rep[Field[MO,VT]]
  def field_obj_new[MO<:MeshObj:Manifest,VT:Manifest](): Rep[Field[MO,VT]]

  def field_apply[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]], n: Rep[Int]): Rep[VT]
  def field_update[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]], n: Rep[Int], v: Rep[VT]): Rep[Unit]
  def field_size[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]]): Rep[Int]
}

trait FieldPrivateOpsExp extends FieldPrivateOps with VariablesExp with BaseFatExp {
  this: DeLisztExp =>

  def reflectPure[A:Manifest](x: Def[A]): Exp[A] = toAtom(x)

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class FieldObjectNew[MO<:MeshObj:Manifest,VT:Manifest]() extends Def[Field[MO,VT]] {
    val fM = manifest[FieldImpl[MO,VT]]
  }

  case class LabelField[MO<:MeshObj:Manifest,VT:Manifest](url: Exp[String]) extends Def[Field[MO,VT]] {
    val mM = manifest[MO]
  }

  case class FieldIntApply[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int]) extends Def[VT]
  case class FieldIntUpdate[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int], v: Exp[VT]) extends Def[Unit]
  case class FieldSize[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]]) extends Def[Int]

  def label[MO<:MeshObj:Manifest,VT:Manifest](url: Exp[String]) = LabelField[MO,VT](url)

  def field_obj_new[MO<:MeshObj:Manifest,VT:Manifest]() = reflectMutable(FieldObjectNew[MO,VT]())

  def field_apply[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int]) = FieldIntApply[MO,VT](x,n)
  def field_update[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int], v: Exp[VT]) = reflectWrite(x)(FieldIntUpdate[MO,VT](x,n,v))
  def field_size[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]]) = FieldSize(x)
}

trait ScalaGenFieldPrivateOps extends ScalaGenBase {
  val IR: FieldPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case f@FieldObjectNew() => emitValDef(sym, remap(f.fM) + "()")
      case f@LabelField(url) => emitValDef(sym, "generated.scala.Mesh.mesh.label[" + remap(f.mM) + "](" + quote(url) + ")")
      case FieldIntApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case FieldIntUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(v))
      case FieldSize(x) => emitValDef(sym, quote(x) + ".size")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenFieldPrivateOps extends CudaGenBase {
  val IR: FieldPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenFieldPrivateOps extends CGenBase {
  val IR: FieldPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
