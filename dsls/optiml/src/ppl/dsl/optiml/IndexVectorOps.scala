package ppl.dsl.optiml

import datastruct.scala.{IndexVectorSeqImpl, IndexVectorRangeImpl, Vector, IndexVector}
import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}

trait IndexVectorOps extends DSLType with Base { this: OptiML =>

  implicit def repIndexVectorToIndexVectorOps(x: Rep[IndexVector]) = new IndexVectorOpsCls(x)

  class IndexVectorOpsCls(x: Rep[IndexVector]){
    def apply[A:Manifest](block: Rep[Int] => Rep[A]) = indexvector_construct(x, block)
  }

  // impl defs
  def indexvector_range(start: Rep[Int], end: Rep[Int]): Rep[IndexVector]
  def indexvector_seq(xs: Rep[Seq[Int]]): Rep[IndexVector]

  // class defs
  def indexvector_construct[A:Manifest](x: Rep[IndexVector], block: Rep[Int] => Rep[A]): Rep[Vector[A]]
}

trait IndexVectorOpsExp extends IndexVectorOps with EffectExp { this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class IndexVectorRange(start: Exp[Int], end: Exp[Int]) extends Def[IndexVector]
  case class IndexVectorSeq(xs: Exp[Seq[Int]]) extends Def[IndexVector]

  ////////////////////////////////
  // implemented via delite ops

  case class IndexVectorConstruct[B](in: Exp[IndexVector], v: Sym[Int], func: Exp[B])(implicit val mA: Manifest[Int], val mB: Manifest[B])
    extends DeliteOpMap[Int,B,Vector] {

    val alloc = reifyEffectsHere(Vector[B](in.length, in.isRow))
  }

  // impl defs
  def indexvector_range(start: Exp[Int], end: Exp[Int]) = IndexVectorRange(start, end)
  def indexvector_seq(xs: Exp[Seq[Int]]) = reflectEffect(IndexVectorSeq(xs))

  // class defs
  def indexvector_construct[A:Manifest](x: Exp[IndexVector], block: Exp[Int] => Exp[A]): Exp[Vector[A]] = {
    val v = fresh[Int]
    val func = reifyEffects(block(v))
    // TODO: right now only effectful dependencies are getting hoisted out of loops, when any free dep should
    //IndexVectorConstruct(x, v, func)
    reflectEffect(IndexVectorConstruct(x, v, func))
  }

}

trait ScalaGenIndexVectorOps extends ScalaGenBase {
  val IR: IndexVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case v@IndexVectorRange(start, end) =>
      emitValDef(sym, "new " + remap(manifest[IndexVectorRangeImpl]) + "(" + quote(start) +  "," + quote(end) + ")")
    case v@IndexVectorSeq(xs) =>
      emitValDef(sym, "new " + remap(manifest[IndexVectorSeqImpl]) + "(" + quote(xs) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}