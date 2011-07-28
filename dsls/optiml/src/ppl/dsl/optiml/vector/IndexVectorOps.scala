package ppl.dsl.optiml.vector

import ppl.dsl.optiml.datastruct.scala.{IndexVectorSeqImpl, IndexVectorRangeImpl, Vector, IndexVector}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter

trait IndexVectorOps extends DSLType with Base with OverloadHack { this: OptiML =>

  object IndexVector {
    def apply(len: Rep[Int]) = indexvector_obj_new(len)
    def apply(xs: Rep[Vector[Int]])(implicit o: Overloaded1) = indexvector_obj_fromvec(xs)
  }

  implicit def repIndexVectorToIndexVectorOps(x: Rep[IndexVector]) = new IndexVectorOpsCls(x)

  class IndexVectorOpsCls(x: Rep[IndexVector]){
    def apply(index: Rep[Int]) = vector_apply(x,index)
    def apply[A:Manifest](block: Rep[Int] => Rep[A]) = indexvector_construct(x, block)
  }

  // impl defs
  def indexvector_range(start: Rep[Int], end: Rep[Int]): Rep[IndexVector]
  def indexvector_obj_new(len: Rep[Int]): Rep[IndexVector]
  def indexvector_obj_fromvec(xs: Rep[Vector[Int]]): Rep[IndexVector]

  // class defs
  def indexvector_construct[A:Manifest](x: Rep[IndexVector], block: Rep[Int] => Rep[A]): Rep[Vector[A]]
}

trait IndexVectorOpsExp extends IndexVectorOps with EffectExp { this: OptiMLExp with IndexVectorImplOps =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class IndexVectorRange(start: Exp[Int], end: Exp[Int]) extends Def[IndexVector]
  case class IndexVectorObjectNew(len: Exp[Int]) extends Def[IndexVector]

  ////////////////////////////////
  // implemented via delite ops

  case class IndexVectorObjectFromVec(xs: Exp[Vector[Int]]) extends DeliteOpSingleTask[IndexVector](reifyEffectsHere(index_vector_obj_fromvec_impl(xs)))

  // Note: Construction from a discrete index vector set will curently return a contiguous (non-sparse) vector.
  // Is this what we want?
  case class IndexVectorConstruct[B:Manifest](in: Exp[IndexVector], func: Exp[Int] => Exp[B])
    extends DeliteOpMap[Int,B,Vector[B]] {

    val size = copyTransformedOrElse(_.size)(in.length)
    
    def alloc = Vector[B](in.length, in.isRow)

    def m = manifest[B]
  }
  
  // impl defs
  def indexvector_range(start: Exp[Int], end: Exp[Int]) = reflectPure(IndexVectorRange(start, end))
  def indexvector_obj_new(len: Exp[Int]) = reflectMutable(IndexVectorObjectNew(len))
  def indexvector_obj_fromvec(xs: Exp[Vector[Int]]) = reflectPure(IndexVectorObjectFromVec(xs))

  // class defs
  def indexvector_construct[A:Manifest](x: Exp[IndexVector], block: Exp[Int] => Exp[A]): Exp[Vector[A]] = {
    reflectPure(IndexVectorConstruct(x, block))
    // HACK -- better scheduling performance in our apps, forces some expensive dependencies to be hoisted
    // TR TODO: use effect summary of func
    //reflectEffect(IndexVectorConstruct(x, block))
  }

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case e@IndexVectorConstruct(in,b) => reflectPure(new { override val original = Some(f,e) } with IndexVectorConstruct(f(in),f(b))(e.m))(mtype(manifest[A]))
//    case Reflect(i@IndexVectorConstruct(in,b), u, es) => reflectMirrored(Reflect(new IndexVectorConstruct(f(in),b)(i.m) { override val transform = f }, mapOver(f,u), f(es)))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenIndexVectorOps extends ScalaGenBase {
  val IR: IndexVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case v@IndexVectorRange(start, end) =>
      emitValDef(sym, "new " + remap(manifest[IndexVectorRangeImpl]) + "(" + quote(start) +  "," + quote(end) + ")")
    case v@IndexVectorObjectNew(len) =>
      emitValDef(sym, "new " + remap(manifest[IndexVectorSeqImpl]) + "(" + quote(len) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}
