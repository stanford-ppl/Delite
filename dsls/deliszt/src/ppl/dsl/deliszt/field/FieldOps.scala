package ppl.dsl.deliszt.field

import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import java.io.PrintWriter
import ppl.dsl.deliszt.datastruct.scala._

import ppl.delite.framework.DSLType
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait FieldOps extends DSLType with Variables {
  this: DeLiszt =>

  implicit def repFieldToFieldOps[MO <: MeshObj:Manifest, VT : Manifest](x: Rep[Field[MO, VT]]) = new fieldOpsCls[MO,VT](x)
  implicit def varToFieldOps[MO <: MeshObj:Manifest, VT : Manifest](x: Var[Field[MO, VT]]) = new fieldOpsCls[MO,VT](readVar(x))

  /**
   * This class defines the public interface for the Field[T] class.
   */
  class fieldOpsCls[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]]) {
    def apply(mo : Rep[MO]) = field_apply(x, mo)
    def update(mo:Rep[MO], v:Rep[VT]) = field_update(x,mo,v)
  }

  def field_apply[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]], mo: Rep[MO]) : Rep[VT]
  def field_update[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]], mo: Rep[MO], v : Rep[VT]) : Rep[Unit]
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
