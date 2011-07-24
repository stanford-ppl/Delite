package ppl.dsl.deliszt.field

import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import java.io.PrintWriter
import ppl.dsl.deliszt.datastruct.scala._

import ppl.delite.framework.DSLType
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait FieldOps extends DSLType with Variables with OverloadHack {
  this: DeLiszt =>

  implicit def repFieldToFieldOps[MO <: MeshObj:Manifest, VT : Manifest](x: Rep[Field[MO, VT]]) = new fieldOpsCls[MO,VT](x)
  implicit def varToFieldOps[MO <: MeshObj:Manifest, VT : Manifest](x: Var[Field[MO, VT]]) = new fieldOpsCls[MO,VT](readVar(x))

  object Field {
    def apply[MO<:MeshObj:Manifest,VT:Manifest]() = field_obj_new[MO,VT]()
  }

  /**
   * This class defines the public interface for the Field[T] class.
   */
  class fieldOpsCls[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]]) {
    def apply(mo : Rep[MO])(implicit o: Overloaded1) = field_apply(x, mo)
    def update(mo:Rep[MO], v:Rep[VT])(implicit o: Overloaded1) = field_update(x,mo,v)

    def apply(n : Rep[Int]) = field_apply(x, n)
    def update(n : Rep[Int], v : Rep[VT]) = field_update(x, n,v)
    def size = field_size(x)
  }

  def field_apply[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]], mo: Rep[MO]) : Rep[VT]
  def field_update[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]], mo: Rep[MO], v : Rep[VT]) : Rep[Unit]

  def label[MO<:MeshObj:Manifest,VT:Manifest](url: Rep[String]): Rep[Field[MO,VT]]
  def field_obj_new[MO<:MeshObj:Manifest,VT:Manifest](): Rep[Field[MO,VT]]

  def field_apply[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]], n: Rep[Int])(implicit o: Overloaded1): Rep[VT]
  def field_update[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]], n: Rep[Int], v: Rep[VT])(implicit o: Overloaded1): Rep[Unit]
  def field_size[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]]): Rep[Int]
}

trait FieldOpsExp extends FieldOps with VariablesExp with BaseFatExp {
  this: DeLisztExp =>

  def reflectPure[VT:Manifest](x: Def[VT]): Exp[VT] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class FieldApply[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], mo: Exp[MO]) extends Def[VT]
  case class FieldUpdate[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], mo: Exp[MO], v: Exp[VT]) extends Def[Unit]

  ////////////////////////////////
  // implemented via delite ops

  //////////////
  // mirroring

/*  override def mirror[VT:Manifest](e: Def[VT], f: Transformer): Exp[VT] = (e match {
    case FieldApply(x, n) => vec_apply(f(x), f(n))
    case Reflect(FieldApply(l,r), u, es) => reflectMirrored(Reflect(FieldApply(f(l),f(r)), mapOver(f,u), f(es)))
    case Reflect(FieldUpdate(l,i,r), u, es) => reflectMirrored(Reflect(FieldUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[VT]] */


  /////////////////////
  // object interface

  /////////////////////
  // class interface
  def field_apply[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO, VT]], mo: Exp[MO]) = reflectPure(FieldApply(x,mo))
  def field_update[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]], mo: Rep[MO], v : Rep[VT]) = reflectWrite(x)(FieldUpdate(x,mo,v))

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

  def field_apply[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int])(implicit o: Overloaded1) = FieldIntApply[MO,VT](x,n)
  def field_update[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int], v: Exp[VT])(implicit o: Overloaded1) = reflectWrite(x)(FieldIntUpdate[MO,VT](x,n,v))
  def field_size[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]]) = FieldSize(x)
}

trait FieldOpsExpOpt extends FieldOpsExp {
  this: DeLisztExp =>
}

trait ScalaGenFieldOps extends ScalaGenBase {
  val IR: FieldOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case FieldApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case FieldUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(v))

      case f@FieldObjectNew() => emitValDef(sym, remap(f.fM) + "()")
      case f@LabelField(url) => emitValDef(sym, "generated.scala.Mesh.mesh.label[" + remap(f.mM) + "](" + quote(url) + ")")
      case FieldIntApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case FieldIntUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(v))
      case FieldSize(x) => emitValDef(sym, quote(x) + ".size")

      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenFieldOps extends CudaGenBase with CudaGenDataStruct {
  val IR: FieldOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenFieldOps extends CGenBase {
  val IR: FieldOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
