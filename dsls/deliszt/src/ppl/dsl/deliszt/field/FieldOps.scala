package ppl.dsl.deliszt.field

import java.io.PrintWriter
import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenericFatCodegen, GenerationFailedException}

import ppl.delite.framework.datastruct.scala.DeliteCollection

import ppl.dsl.deliszt.capabilities._
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.vec.VecOpsExpOpt

trait FieldOps extends Variables with OverloadHack {
  this: DeLiszt with LiftNumeric =>

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
  this: DeLisztExp with LiftNumeric with FieldImplOps =>

  //def reflectPure[T:Manifest](x: Def[T]): Exp[T] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething

  ///////////////////////////////////////////////////
  // implemented via method on real data structure  
  case class FieldApply[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[Int]) extends Def[T] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldRawApply[MO<:MeshObj:Manifest,A:Manifest](x: Exp[Field[MO,DeliteCollection[A]]], mo: Exp[Int], off: Exp[Int]) extends Def[A] {
    val moM = manifest[MO]
    val vtM = manifest[A]
  }

  case class FieldUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[Int], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldRawUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[Int], off: Exp[Int], v: Exp[Any]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }

  case class FieldPlusUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[Int], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldTimesUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[Int], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldMinusUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[Int], v: Exp[T]) extends Def[Unit] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }
  
  case class FieldDivideUpdate[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO,T]], mo: Exp[Int], v: Exp[T]) extends Def[Unit] {
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
  
  case class FieldSize[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]]) extends Def[Int] {
    val moM = manifest[MO]
    val vtM = manifest[T]
  }

  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@FieldApply(x, i) => field_apply(f(x), f(i))(e.moM, e.vtM)
    // Read/write effects
    case Reflect(e@FieldApply(l,r), u, es) => reflectMirrored(Reflect(FieldApply(f(l),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@FieldRawApply(l,o,r), u, es) => reflectMirrored(Reflect(FieldRawApply(f(l),f(o),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@FieldUpdate(l,i,r), u, es) => reflectMirrored(Reflect(FieldUpdate(f(l),f(i),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@FieldRawUpdate(l,i,o,r), u, es) => reflectMirrored(Reflect(FieldRawUpdate(f(l),f(i),f(o),f(r))(e.moM, e.vtM), mapOver(f,u), f(es)))(mtype(manifest[A]))
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
    case FieldUpdate(a,i,x) => Nil
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
    case FieldUpdate(a,i,x) => syms(x)
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
    case FieldUpdate(a,i,x) => Nil
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
    case FieldUpdate(a,i,x) => syms(a)
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

  def field_mo_apply[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO, T]], mo: Exp[MO]) = field_apply(x, ID(mo))
  def field_mo_update[MO<:MeshObj:Manifest, T:Manifest](x: Exp[Field[MO, T]], mo: Exp[MO], v : Exp[T]) = field_update(x, ID(mo), v)

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

  def field_apply[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]], n: Exp[Int]) = reflectPure(FieldApply(x,n))
  def field_update[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]], n: Exp[Int], v: Exp[T]) = reflectWrite(x)(FieldUpdate(x,n,v))
  def field_size[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]]) = FieldSize(x)

  // internal
  def field_raw_apply[MO<:MeshObj:Manifest,A:Manifest](x: Exp[Field[MO,DeliteCollection[A]]], idx: Exp[Int], off: Exp[Int]) = reflectPure(FieldRawApply[MO,A](x,idx,off))
  def field_raw_update[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]], n: Exp[Int], off: Exp[Int], v: Exp[Any]) = reflectWrite(x)(FieldRawUpdate[MO,T](x,n,off,v))
}

trait FieldOpsExpOpt extends FieldOpsExp {
  this: DeLisztExp with LiftNumeric with ArithOpsExp =>
  
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
    case Def(Vec3New(a,b,c)) => field_raw_update(x,ID(mo),0,a); field_raw_update(x,ID(mo),1,b); field_raw_update(x,ID(mo),2,c)
    //case Def(Reify(Def(Reflect(Vec3New(a,b,c), u, es)),_,_)) => field_raw_update(x,ID(mo),0,a); field_raw_update(x,ID(mo),1,b); field_raw_update(x,ID(mo),2,c)
    case _ => super.field_mo_update(x, mo, v)
  }
  override def field_update[MO<:MeshObj:Manifest,T:Manifest](x: Exp[Field[MO,T]], n: Exp[Int], v: Exp[T]) = v match {
    case Def(Vec3New(a,b,c)) => field_raw_update(x,n,0,a); field_raw_update(x,n,1,b); field_raw_update(x,n,2,c)
    //case Def(Reify(Def(Reflect(Vec3New(a,b,c), u, es)),_,_)) => field_raw_update(x,n,0,a); field_raw_update(x,n,1,b); field_raw_update(x,n,2,c)
    case _ => super.field_update(x, n, v)
  }
}

trait ScalaGenFieldOps extends ScalaGenBase {
  val IR: FieldOpsExp with VecOpsExpOpt
  import IR._

  val fieldImplPath = "ppl.dsl.deliszt.datastruct.scala.FieldImpl"
  val labelImplPath = "ppl.dsl.deliszt.datastruct.scala.LabelFieldImpl"
  val vec3FieldImplPath = "ppl.dsl.deliszt.datastruct.scala.Vec3FieldImpl"
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case FieldApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case FieldRawApply(x,n,off) => emitValDef(sym, quote(x) + ".raw_apply(" + quote(n) + "," + quote(off) + ")")
      case FieldUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(v))
      case FieldRawUpdate(x,n,off,v) => emitValDef(sym, quote(x) + ".raw_update(" + quote(n) + "," + quote(off) + "," + quote(v) + ")")
      case FieldPlusUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") += " + quote(v))
      case FieldTimesUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") *= " + quote(v))
      case FieldMinusUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") -= " + quote(v))
      case FieldDivideUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") /= " + quote(v))

      case f@DeLisztFieldWithConstCell(x) => {
         if (isVec3(x)) {
           emitValDef(sym, remap(vec3FieldImplPath, ".cellWithConst", f.t) + "(" + quote(x) + ")")
         } else {
           emitValDef(sym, remap(fieldImplPath, ".cellWithConst", f.t) + "(" + quote(x) + ")")
         }
      }
      case f@DeLisztFieldWithConstEdge(x) => {
         if (isVec3(x)) {
             emitValDef(sym, remap(vec3FieldImplPath, ".edgeWithConst", f.t) + "(" + quote(x) + ")")
         } else {
             emitValDef(sym, remap(fieldImplPath, ".edgeWithConst", f.t) + "(" + quote(x) + ")")
         }
      }

      case f@DeLisztFieldWithConstFace(x) => {
         if (isVec3(x)) {
             emitValDef(sym, remap(vec3FieldImplPath, ".faceWithConst", f.t) + "(" + quote(x) + ")")
         } else {
             emitValDef(sym, remap(fieldImplPath, ".faceWithConst", f.t) + "(" + quote(x) + ")")
         }
      }
      case f@DeLisztFieldWithConstVertex(x) => {
         if (isVec3(x)) {
             emitValDef(sym, remap(vec3FieldImplPath, ".vertexWithConst", f.t) + "(" + quote(x) + ")")
         } else {
             emitValDef(sym, remap(fieldImplPath, ".vertexWithConst", f.t) + "(" + quote(x) + ")")
         }
      }
      case f@FieldObjectNewCell() => emitValDef(sym, remap(fieldImplPath, ".ofCell", f.t) + "()")
      case f@FieldObjectNewEdge() => emitValDef(sym, remap(fieldImplPath, ".ofEdge", f.t) + "()")
      case f@FieldObjectNewFace() => emitValDef(sym, remap(fieldImplPath, ".ofFace", f.t) + "()")
      case f@FieldObjectNewVertex() => emitValDef(sym, remap(fieldImplPath, ".ofVertex", f.t) + "()")
      
      case f@LabelFieldNewCell(url, m) => emitValDef(sym, remap(labelImplPath, ".ofCell", f.t) + "(" + quote(m) + "," + quote(url) + ")")
      case f@LabelFieldNewEdge(url, m) => emitValDef(sym, remap(labelImplPath, ".ofEdge", f.t) + "(" + quote(m) + "," + quote(url) + ")")
      case f@LabelFieldNewFace(url, m) => emitValDef(sym, remap(labelImplPath, ".ofFace", f.t) + "(" + quote(m) + "," + quote(url) + ")")
      case f@LabelFieldNewVertex(url, m) => emitValDef(sym, remap(labelImplPath, ".ofVertex", f.t) + "(" + quote(m) + "," + quote(url) + ")")
      
      //case FieldIntApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      //case FieldIntUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(v))
      case FieldSize(x) => emitValDef(sym, quote(x) + ".size")

      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenFieldOps extends CudaGenBase {
  val IR: FieldOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FieldApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case FieldRawApply(x,n,off) => emitValDef(sym, quote(x) + ".raw_apply(" + quote(n) + "," + quote(off) + ")")
    case FieldUpdate(x,n,v) => stream.println(addTab() + quote(x) + ".update(" + quote(n) + "," + quote(v) + ");")
    case FieldRawUpdate(x,n,off,v) => stream.println(addTab() + quote(x) + ".raw_update(" + quote(n) + "," + quote(off) + "," + quote(v) + ");")

    //case FieldPlusUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") += " + quote(v))
    //case FieldTimesUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") *= " + quote(v))
    //case FieldMinusUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") -= " + quote(v))
    //case FieldDivideUpdate(x,n,v) => emitValDef(sym, quote(x) + "(" + quote(n) + ") /= " + quote(v))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenFieldOps extends CGenBase {
  val IR: FieldOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
