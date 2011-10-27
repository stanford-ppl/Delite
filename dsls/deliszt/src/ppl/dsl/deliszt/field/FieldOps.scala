package ppl.dsl.deliszt.field

import java.io.PrintWriter
import ppl.dsl.deliszt.capabilities._
import ppl.dsl.deliszt._

import ppl.delite.framework.DSLType
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenericFatCodegen, GenerationFailedException}

trait FieldOps extends DSLType with Variables with OverloadHack {
  this: DeLiszt =>

  implicit def repFieldToFieldOps[MO <: MeshObj:Manifest, T : Manifest](x: Rep[Field[MO, T]]) = new fieldOpsCls[MO,T](x)
  implicit def varToFieldOps[MO <: MeshObj:Manifest, T : Manifest](x: Var[Field[MO, T]]) = new fieldOpsCls[MO,T](readVar(x))

  object Field {
    def apply[MO<:Cell:Manifest,T:Manifest]()(implicit ev : MO =:= Cell) = field_obj_new_cell[T]()
    def apply[MO<:Edge:Manifest,T:Manifest]()(implicit ev : MO =:= Edge, o: Overloaded1) = field_obj_new_edge[T]()
    def apply[MO<:Face:Manifest,T:Manifest]()(implicit ev : MO =:= Face, o: Overloaded2) = field_obj_new_face[T]()
    def apply[MO<:Vertex:Manifest,T:Manifest]()(implicit ev : MO =:= Vertex, o: Overloaded3) = field_obj_new_vertex[T]()
  }
  
  def FieldWithConst[MO<:Cell:Manifest, T:Manifest](c: Rep[T])(implicit ev : MO =:= Cell) : Rep[Field[Cell,T]]
  def FieldWithConst[MO<:Edge:Manifest, T:Manifest](c: Rep[T])(implicit ev : MO =:= Edge, o: Overloaded1) : Rep[Field[Edge,T]]
  def FieldWithConst[MO<:Face:Manifest, T:Manifest](c: Rep[T])(implicit ev : MO =:= Face, o: Overloaded2) : Rep[Field[Face,T]]
  def FieldWithConst[MO<:Vertex:Manifest, T:Manifest](c: Rep[T])(implicit ev : MO =:= Vertex, o: Overloaded3) : Rep[Field[Vertex,T]]
  
  def FieldWithLabel[MO<:Cell:Manifest, T:Manifest](url : Rep[String])(implicit ev : MO =:= Cell) : Rep[Field[Cell,T]]
  def FieldWithLabel[MO<:Edge:Manifest, T:Manifest](url : Rep[String])(implicit ev : MO =:= Edge, o: Overloaded1) : Rep[Field[Edge,T]]
  def FieldWithLabel[MO<:Face:Manifest, T:Manifest](url : Rep[String])(implicit ev : MO =:= Face, o: Overloaded2) : Rep[Field[Face,T]]
  def FieldWithLabel[MO<:Vertex:Manifest, T:Manifest](url : Rep[String])(implicit ev : MO =:= Vertex, o: Overloaded3) : Rep[Field[Vertex,T]]
  
  /**
   * This class defines the public interface for the Field[T] class.
   */
  class fieldOpsCls[MO<:MeshObj:Manifest, T:Manifest](x: Rep[Field[MO, T]]) {
    def apply(mo : Rep[MO]) = field_mo_apply(x, mo)
    def update(mo: Rep[MO], v: Rep[T]) = field_mo_update(x,mo,v)

    def apply(n : Rep[Int])(implicit o: Overloaded1) = field_apply(x, n)
    def update(n : Rep[Int], v : Rep[T])(implicit o: Overloaded1) = field_update(x,n,v)
    def size = field_size(x)
  }

  def field_mo_apply[MO<:MeshObj:Manifest, T:Manifest](x: Rep[Field[MO, T]], mo: Rep[MO]) : Rep[T]
  def field_mo_update[MO<:MeshObj:Manifest, T:Manifest](x: Rep[Field[MO, T]], mo: Rep[MO], v : Rep[T]) : Rep[Unit]

  def field_obj_new_cell[T:Manifest]() : Rep[Field[Cell,T]]
  def field_obj_new_edge[T:Manifest]() : Rep[Field[Edge,T]]
  def field_obj_new_face[T:Manifest]() : Rep[Field[Face,T]]
  def field_obj_new_vertex[T:Manifest]() : Rep[Field[Vertex,T]]

  def field_apply[MO<:MeshObj:Manifest,T:Manifest](x: Rep[Field[MO,T]], n: Rep[Int]): Rep[T]
  def field_update[MO<:MeshObj:Manifest,T:Manifest](x: Rep[Field[MO,T]], n: Rep[Int], v: Rep[T]): Rep[Unit]
  def field_size[MO<:MeshObj:Manifest,T:Manifest](x: Rep[Field[MO,T]]): Rep[Int]
}

trait FieldOpsExp extends FieldOps with VariablesExp with BaseFatExp {
  this: DeLisztExp with FieldImplOps =>

  def reflectPure[T:Manifest](x: Def[T]): Exp[T] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething

  ///////////////////////////////////////////////////
  // implemented via method on real data structure  
  case class FieldApply[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[MO]) extends Def[T] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[MO], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldPlusUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[MO], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldTimesUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[MO], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldMinusUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[MO], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldDivideUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[MO], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }

  ////////////////////////////////
  // implemented via delite ops
  case class DeLisztFieldWithConstCell[T:Manifest](v: Exp[T]) extends Def[Field[Cell,T]] {
    val t = manifest[T]
  }
  
  case class DeLisztFieldWithConstEdge[T:Manifest](v: Exp[T]) extends Def[Field[Edge,T]] {
    val t = manifest[T]
  }
  
  case class DeLisztFieldWithConstFace[T:Manifest](v: Exp[T]) extends Def[Field[Face,T]] {
    val t = manifest[T]
  }
  
  case class DeLisztFieldWithConstVertex[T:Manifest](v: Exp[T]) extends Def[Field[Vertex,T]] {
    val t = manifest[T]
  }
  
  case class FieldObjectNewCell[T:Manifest]() extends Def[Field[Cell,T]] {
    val t = manifest[T]
  }
  
  case class FieldObjectNewEdge[T:Manifest]() extends Def[Field[Edge,T]] {
    val t = manifest[T]
  }
  
  case class FieldObjectNewFace[T:Manifest]() extends Def[Field[Face,T]] {
    val t = manifest[T]
  }
  
  case class FieldObjectNewVertex[T:Manifest]() extends Def[Field[Vertex,T]] {
    val t = manifest[T]
  }

  case class LabelFieldNewCell[T:Manifest](url: Exp[String], m: Exp[Mesh]) extends Def[Field[Cell,T]] {
    val t = manifest[T]
  }
  
  case class LabelFieldNewEdge[T:Manifest](url: Exp[String], m: Exp[Mesh]) extends Def[Field[Edge,T]] {
    val t = manifest[T]
  }
  
  case class LabelFieldNewFace[T:Manifest](url: Exp[String], m: Exp[Mesh]) extends Def[Field[Face,T]] {
    val t = manifest[T]
  }
  
  case class LabelFieldNewVertex[T:Manifest](url: Exp[String], m: Exp[Mesh]) extends Def[Field[Vertex,T]] {
    val t = manifest[T]
  }
  
  case class FieldIntApply[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]], n: Exp[Int]) extends Def[T] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldIntUpdate[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]], n: Exp[Int], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldSize[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]]) extends Def[Int] {
    val moM = manifest[MO]
    val vtM = manifest[T]
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
    case DeLisztFieldWithConstCell(x) => Nil
    case DeLisztFieldWithConstEdge(x) => Nil
    case DeLisztFieldWithConstFace(x) => Nil
    case DeLisztFieldWithConstVertex(x) => Nil
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
    case DeLisztFieldWithConstCell(x) => Nil
    case DeLisztFieldWithConstEdge(x) => Nil
    case DeLisztFieldWithConstFace(x) => Nil
    case DeLisztFieldWithConstVertex(x) => Nil
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
    case DeLisztFieldWithConstCell(x) => Nil
    case DeLisztFieldWithConstEdge(x) => Nil
    case DeLisztFieldWithConstFace(x) => Nil
    case DeLisztFieldWithConstVertex(x) => Nil
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
    case DeLisztFieldWithConstCell(x) => Nil
    case DeLisztFieldWithConstEdge(x) => Nil
    case DeLisztFieldWithConstFace(x) => Nil
    case DeLisztFieldWithConstVertex(x) => Nil
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
  def field_mo_apply[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO, T]], mo: Exp[MO]) = reflectEffect(FieldApply(x,mo))
  def field_mo_update[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO, T]], mo: Exp[MO], v : Exp[T]) = reflectWrite(x)(FieldUpdate(x,mo,v))

  def FieldWithConst[MO<:Cell:Manifest, T:Manifest](c: Exp[T])(implicit ev : MO =:= Cell) = reflectMutable(DeLisztFieldWithConstCell(c))
  def FieldWithConst[MO<:Edge:Manifest, T:Manifest](c: Exp[T])(implicit ev : MO =:= Edge, o: Overloaded1) = reflectMutable(DeLisztFieldWithConstEdge(c))
  def FieldWithConst[MO<:Face:Manifest, T:Manifest](c: Exp[T])(implicit ev : MO =:= Face, o: Overloaded2) = reflectMutable(DeLisztFieldWithConstFace(c))
  def FieldWithConst[MO<:Vertex:Manifest, T:Manifest](c: Exp[T])(implicit ev : MO =:= Vertex, o: Overloaded3) = reflectMutable(DeLisztFieldWithConstVertex(c))
  
  def FieldWithLabel[MO<:Cell:Manifest, T:Manifest](url : Exp[String])(implicit ev : MO =:= Cell) = reflectMutable(LabelFieldNewCell[T](url, mesh))
  def FieldWithLabel[MO<:Edge:Manifest, T:Manifest](url : Exp[String])(implicit ev : MO =:= Edge, o: Overloaded1) = reflectMutable(LabelFieldNewEdge[T](url, mesh))
  def FieldWithLabel[MO<:Face:Manifest, T:Manifest](url : Exp[String])(implicit ev : MO =:= Face, o: Overloaded2) = reflectMutable(LabelFieldNewFace[T](url, mesh))
  def FieldWithLabel[MO<:Vertex:Manifest, T:Manifest](url : Exp[String])(implicit ev : MO =:= Vertex, o: Overloaded3) = reflectMutable(LabelFieldNewVertex[T](url, mesh))

  def field_obj_new_cell[T:Manifest]() = reflectMutable(FieldObjectNewCell[T]())
  def field_obj_new_edge[T:Manifest]() = reflectMutable(FieldObjectNewEdge[T]())
  def field_obj_new_face[T:Manifest]() = reflectMutable(FieldObjectNewFace[T]())
  def field_obj_new_vertex[T:Manifest]() = reflectMutable(FieldObjectNewVertex[T]())

  def field_apply[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]], n: Exp[Int]) = FieldIntApply[MO,T](x,n)
  def field_update[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]], n: Exp[Int], v: Exp[T]) = reflectWrite(x)(FieldIntUpdate[MO,T](x,n,v))
  def field_size[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]]) = FieldSize(x)
}

trait FieldOpsExpOpt extends FieldOpsExp {
  this: DeLisztExp with ArithOpsExp =>
  
  override def field_mo_update[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]], mo: Rep[MO], v: Exp[T]) = v match {
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

      case f@DeLisztFieldWithConstCell(x) => emitValDef(sym, "generated.scala.Field.cellWithConst[" + remap(f.t) + "](" + quote(x) + ")")
      case f@DeLisztFieldWithConstEdge(x) => emitValDef(sym, "generated.scala.Field.edgeWithConst[" + remap(f.t) + "](" + quote(x) + ")")
      case f@DeLisztFieldWithConstFace(x) => emitValDef(sym, "generated.scala.Field.faceWithConst[" + remap(f.t) + "](" + quote(x) + ")")
      case f@DeLisztFieldWithConstVertex(x) => emitValDef(sym, "generated.scala.Field.vertexWithConst[" + remap(f.t) + "](" + quote(x) + ")")
      
      case f@FieldObjectNewCell() => emitValDef(sym, "generated.scala.Field.ofCell[" + remap(f.t) + "]()")
      case f@FieldObjectNewEdge() => emitValDef(sym, "generated.scala.Field.ofEdge[" + remap(f.t) + "]()")
      case f@FieldObjectNewFace() => emitValDef(sym, "generated.scala.Field.ofFace[" + remap(f.t) + "]()")
      case f@FieldObjectNewVertex() => emitValDef(sym, "generated.scala.Field.ofVertex[" + remap(f.t) + "]()")
      
      case f@LabelFieldNewCell(url, m) => emitValDef(sym, quote(m) + ".labelCells[" + remap(f.t) + "](" + quote(url) + ")")
      case f@LabelFieldNewEdge(url, m) => emitValDef(sym, quote(m) + ".labelEdges[" + remap(f.t) + "](" + quote(url) + ")")
      case f@LabelFieldNewFace(url, m) => emitValDef(sym, quote(m) + ".labelFaces[" + remap(f.t) + "](" + quote(url) + ")")
      case f@LabelFieldNewVertex(url, m) => emitValDef(sym, quote(m) + ".labelVertices[" + remap(f.t) + "](" + quote(url) + ")")
      
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
