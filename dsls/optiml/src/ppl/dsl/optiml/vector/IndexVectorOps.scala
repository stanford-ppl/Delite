package ppl.dsl.optiml.vector

import ppl.dsl.optiml.datastruct.scala.{IndexVectorSeqImpl, IndexVectorRangeImpl, Vector, IndexVector}
import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import ppl.dsl.optiml.{OptiMLExp, OptiML}

trait IndexVectorOps extends DSLType with Base { this: OptiML =>

  object IndexVector {
    def apply(len: Rep[Int]) = indexvector_seq(Vector[Int](len, unit(true)))
  }

  implicit def repIndexVectorToIndexVectorOps(x: Rep[IndexVector]) = new IndexVectorOpsCls(x)

  class IndexVectorOpsCls(x: Rep[IndexVector]){
    def apply(index: Rep[Int]) = vector_apply(x,index)
    def apply[A:Manifest](block: Rep[Int] => Rep[A]) = indexvector_construct(x, block)
  }

  // impl defs
  def indexvector_range(start: Rep[Int], end: Rep[Int]): Rep[IndexVector]
  def indexvector_seq(xs: Rep[Vector[Int]]): Rep[IndexVector]

  // class defs
  def indexvector_construct[A:Manifest](x: Rep[IndexVector], block: Rep[Int] => Rep[A]): Rep[Vector[A]]
}

trait IndexVectorOpsExp extends IndexVectorOps with EffectExp { this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class IndexVectorRange(start: Exp[Int], end: Exp[Int]) extends Def[IndexVector]
  case class IndexVectorSeq(xs: Exp[Vector[Int]]) extends Def[IndexVector]

  ////////////////////////////////
  // implemented via delite ops

  case class IndexVectorConstruct[B](in: Exp[IndexVector], v: Sym[Int], func: Exp[B])(implicit val mA: Manifest[Int], val mB: Manifest[B])
    extends DeliteOpMap[Int,B,Vector] {

    val alloc = reifyEffectsHere(Vector[B](in.length, in.isRow))
  }

  // impl defs
  def indexvector_range(start: Exp[Int], end: Exp[Int]) = IndexVectorRange(start, end)
  def indexvector_seq(xs: Exp[Vector[Int]]) = reflectEffect(IndexVectorSeq(xs))

  // class defs
  def indexvector_construct[A:Manifest](x: Exp[IndexVector], block: Exp[Int] => Exp[A]): Exp[Vector[A]] = {
    val v = fresh[Int]
    val func = reifyEffects(block(v))
    IndexVectorConstruct(x, v, func)
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