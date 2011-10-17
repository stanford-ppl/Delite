package ppl.dsl.deliszt.field

import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import java.io.PrintWriter
import ppl.dsl.deliszt.capabilities._
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
  
  def FieldWithConst[MO<:MeshObj:Manifest, VT:Manifest](c : Rep[VT]) : Rep[Field[MO,VT]]
	def FieldWithLabel[MO<:MeshObj:Manifest, VT:Manifest](url : Rep[String]) = label[MO,VT](url)

  /**
   * This class defines the public interface for the Field[T] class.
   */
  class fieldOpsCls[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]]) {
    def apply(mo : Rep[MO]) = field_mo_apply(x, mo)
    def update(mo: Rep[MO], v: Rep[VT]) = field_mo_update(x,mo,v)

    def apply(n : Rep[Int])(implicit o: Overloaded1) = field_apply(x, n)
    def update(n : Rep[Int], v : Rep[VT])(implicit o: Overloaded1) = field_update(x,n,v)
    def size = field_size(x)
  }

  def field_mo_apply[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]], mo: Rep[MO]) : Rep[VT]
  def field_mo_update[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]], mo: Rep[MO], v : Rep[VT]) : Rep[Unit]

  def label[MO<:MeshObj:Manifest,VT:Manifest](url: Rep[String]): Rep[Field[MO,VT]]
  def field_obj_new[MO<:MeshObj:Manifest,VT:Manifest](): Rep[Field[MO,VT]]

  def field_apply[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]], n: Rep[Int]): Rep[VT]
  def field_update[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]], n: Rep[Int], v: Rep[VT]): Rep[Unit]
  def field_size[MO<:MeshObj:Manifest,VT:Manifest](x: Rep[Field[MO,VT]]): Rep[Int]
}

trait FieldOpsExp extends FieldOps with VariablesExp with BaseFatExp {
  this: DeLisztExp with FieldImplOps =>

  def reflectPure[VT:Manifest](x: Def[VT]): Exp[VT] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething

  ///////////////////////////////////////////////////
  // implemented via method on real data structure  
  case class FieldApply[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], mo: Exp[MO]) extends Def[VT] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }
  
  case class FieldUpdate[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], mo: Exp[MO], v: Exp[VT]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }
  
  case class FieldPlusUpdate[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], mo: Exp[MO], v: Exp[VT]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }
  
  case class FieldTimesUpdate[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], mo: Exp[MO], v: Exp[VT]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }
  
  case class FieldMinusUpdate[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], mo: Exp[MO], v: Exp[VT]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }
  
  case class FieldDivideUpdate[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO,VT]], mo: Exp[MO], v: Exp[VT]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }

  ////////////////////////////////
  // implemented via delite ops
  case class DeLisztFieldWithConst[MO<:MeshObj:Manifest,VT:Manifest](c: Exp[VT]) extends Def[Field[MO,VT]] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }
  
  case class FieldObjectNew[MO<:MeshObj:Manifest,VT:Manifest]() extends Def[Field[MO,VT]] {
    val fM = manifest[FieldImpl[MO,VT]]
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }

  case class LabelFieldNew[MO<:MeshObj:Manifest,VT:Manifest](url: Exp[String]) extends Def[Field[MO,VT]] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }
  
  case class FieldIntApply[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int]) extends Def[VT] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }
  
  case class FieldIntUpdate[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int], v: Exp[VT]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }
  
  case class FieldSize[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]]) extends Def[Int] {
    val moM = manifest[MO]
    val vtM = manifest[VT]
  }

  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case e@FieldApply(x, i) => field_mo_apply(f(x), f(i))(e.moM, e.vtM)
    case e@FieldIntApply(x, i) => field_apply(f(x), f(i))(e.moM, e.vtM)
    // Read/write effects
    case Reflect(e@FieldApply(l,r), u, es) => reflectMirrored(Reflect(FieldApply(f(l),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@FieldIntApply(l,r), u, es) => reflectMirrored(Reflect(FieldIntApply(f(l),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@FieldUpdate(l,i,r), u, es) => reflectMirrored(Reflect(FieldUpdate(f(l),f(i),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@FieldPlusUpdate(l,i,r), u, es) => reflectMirrored(Reflect(FieldPlusUpdate(f(l),f(i),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@FieldTimesUpdate(l,i,r), u, es) => reflectMirrored(Reflect(FieldTimesUpdate(f(l),f(i),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@FieldDivideUpdate(l,i,r), u, es) => reflectMirrored(Reflect(FieldDivideUpdate(f(l),f(i),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@FieldMinusUpdate(l,i,r), u, es) => reflectMirrored(Reflect(FieldMinusUpdate(f(l),f(i),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    // Effect with SingleTask and DeliteOpLoop
    // Allocation
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case DeLisztFieldWithConst(x) => Nil
    case FieldApply(a,i) => Nil
    case FieldIntApply(a,i) => Nil
    case FieldUpdate(a,i,x) => Nil
    case FieldIntUpdate(a,i,x) => Nil
    case FieldPlusUpdate(a,i,x) => Nil
    case FieldTimesUpdate(a,i,x) => Nil
    case FieldDivideUpdate(a,i,x) => Nil
    case FieldMinusUpdate(a,i,x) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case DeLisztFieldWithConst(x) => Nil
    case FieldApply(a,i) => Nil
    case FieldIntApply(a,i) => Nil
    case FieldUpdate(a,i,x) => syms(x)
    case FieldIntUpdate(a,i,x) => syms(x)
    case FieldPlusUpdate(a,i,x) => syms(x)
    case FieldTimesUpdate(a,i,x) => syms(x)
    case FieldDivideUpdate(a,i,x) => syms(x)
    case FieldMinusUpdate(a,i,x) => syms(x)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case DeLisztFieldWithConst(x) => Nil  
    case FieldApply(a,i) => syms(a)
    case FieldIntApply(a,i) => syms(a)
    case FieldUpdate(a,i,x) => Nil
    case FieldIntUpdate(a,i,x) => Nil
    case FieldPlusUpdate(a,i,x) => Nil
    case FieldTimesUpdate(a,i,x) => Nil
    case FieldDivideUpdate(a,i,x) => Nil
    case FieldMinusUpdate(a,i,x) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case DeLisztFieldWithConst(x) => Nil
    case FieldApply(a,i) => Nil
    case FieldIntApply(a,i) => Nil
    case FieldUpdate(a,i,x) => syms(a)
    case FieldIntUpdate(a,i,x) => syms(a)
    case FieldPlusUpdate(a,i,x) => syms(a)
    case FieldTimesUpdate(a,i,x) => syms(a)
    case FieldDivideUpdate(a,i,x) => syms(a)
    case FieldMinusUpdate(a,i,x) => syms(a)
    case _ => super.copySyms(e)
  }


  /////////////////////
  // object interface

  /////////////////////
  // class interface
  def field_mo_apply[MO<:MeshObj:Manifest, VT:Manifest](x: Exp[Field[MO, VT]], mo: Exp[MO]) = reflectEffect(FieldApply(x,mo))
  def field_mo_update[MO<:MeshObj:Manifest, VT:Manifest](x: Rep[Field[MO, VT]], mo: Rep[MO], v : Rep[VT]) = reflectWrite(x)(FieldUpdate(x,mo,v))

  def FieldWithConst[MO<:MeshObj:Manifest,VT:Manifest](c: Exp[VT]) = reflectMutable(DeLisztFieldWithConst[MO,VT](c))
  
  def label[MO<:MeshObj:Manifest,VT:Manifest](url: Exp[String]) = reflectMutable(LabelFieldNew[MO,VT](url))
//  def position[MO<:MeshObj:Manifest](mo: Exp[MO]) = PositionFor(mo)
  
  def field_obj_new[MO<:MeshObj:Manifest,VT:Manifest]() = reflectMutable(FieldObjectNew[MO,VT]())

  def field_apply[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int]) = FieldIntApply[MO,VT](x,n)
  def field_update[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], n: Exp[Int], v: Exp[VT]) = reflectWrite(x)(FieldIntUpdate[MO,VT](x,n,v))
  def field_size[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]]) = FieldSize(x)
}

trait FieldOpsExpOpt extends FieldOpsExp {
  this: DeLisztExp with ArithOpsExp =>
  
  override def field_mo_update[MO<:MeshObj:Manifest,VT:Manifest](x: Exp[Field[MO,VT]], mo: Rep[MO], v: Exp[VT]) = v match {
    case Def(ArithPlus(a, b)) => (a, b) match {
        case (a, Def(FieldApply(x, mo))) => FieldPlusUpdate(x, mo, a)
        case (Def(FieldApply(x, mo)), b) => FieldPlusUpdate(x, mo, b)
        case _ => super.field_mo_update(x, mo, v)
    }
    case Def(ArithTimes(a, b)) => (a, b) match {
        case (a, Def(FieldApply(x, mo))) => FieldTimesUpdate(x, mo, a)
        case (Def(FieldApply(x, mo)), b) => FieldTimesUpdate(x, mo, b)
        case _ => super.field_mo_update(x, mo, v)
    }
    case Def(ArithMinus(Def(FieldApply(x, mo)), b)) => FieldMinusUpdate(x, mo, b)
    case Def(ArithFractionalDivide(Def(FieldApply(x, mo)), b)) => FieldDivideUpdate(x, mo, b)
    case _ => super.field_mo_update(x, mo, v)
  }
}

trait ScalaGenFieldOps extends ScalaGenBase {
  val IR: FieldOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case FieldApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case FieldUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(v))
      case FieldPlusUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") += " + quote(v))
      case FieldTimesUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") *= " + quote(v))
      case FieldMinusUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") -= " + quote(v))
      case FieldDivideUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") /= " + quote(v))

      case f@DeLisztFieldWithConst(x) => emitValDef(sym, "generated.scala.FieldImpl.withConst[" + remap(f.moM) + "," + remap(f.vtM) + "](" + quote(x) + ")")
      case f@FieldObjectNew() => emitValDef(sym, remap(f.fM) + "()")
      case f@LabelFieldNew(url) => emitValDef(sym, "generated.scala.Mesh.label[" + remap(f.moM) + "," + remap(f.vtM) + "](" + quote(url) + ")")
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
