package ppl.dsl.optiml

import ppl.dsl.optiml.datastruct.scala._
import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}

trait IndexVector2Ops extends DSLType with Base { this: OptiML =>

  // chained implicits from LanguageOps
  implicit def tuple2ToIndexVectorOps(tup: (Rep[IndexVector], IndexWildcard))(implicit overloaded1 : Overloaded1)
    = repIndexVector2ToIndexVectorOps(indexvector2_new(tup._1, indexvector2_wildcard()))
  implicit def tuple2ToIndexVectorOps(tup: (IndexWildcard, Rep[IndexVector]))(implicit overloaded2 : Overloaded2)
    = repIndexVector2ToIndexVectorOps(indexvector2_new(indexvector2_wildcard(), tup._2))
  implicit def tuple2ToIndexVectorOps(tup: (Rep[IndexVector], Rep[IndexVector]))(implicit overloaded3 : Overloaded3)
    = repIndexVector2ToIndexVectorOps(indexvector2_new(tup._1, tup._2))

  implicit def repIndexVector2ToIndexVectorOps(x: Rep[IndexVector2]) = new IndexVector2OpsCls(x)

  class IndexVector2OpsCls(x: Rep[IndexVector2]){
    def apply[A:Manifest](block: Rep[Int] => Rep[Vector[A]]) = indexvector2_construct_vectors(x, block)
    def apply[A:Manifest](block: (Rep[Int],Rep[Int]) => Rep[A]) = indexvector2_construct(x, block)
    def rowInd = indexvector2_rowind(x)
    def colInd = indexvector2_colind(x)
  }

  // impl defs
  def indexvector2_new(rowInd: Rep[IndexVector], colInd: Rep[IndexVector]): Rep[IndexVector2]
  def indexvector2_wildcard(): Rep[IndexVector]

  // class defs
  def indexvector2_construct_vectors[A:Manifest](x: Rep[IndexVector2], block: Rep[Int] => Rep[Vector[A]]): Rep[Matrix[A]]
  def indexvector2_construct[A:Manifest](x: Rep[IndexVector2], block: (Rep[Int],Rep[Int]) => Rep[A]): Rep[Matrix[A]]
  def indexvector2_rowind(x: Rep[IndexVector2]): Rep[IndexVector]
  def indexvector2_colind(x: Rep[IndexVector2]): Rep[IndexVector]
}

trait IndexVector2OpsExp extends IndexVector2Ops with EffectExp { this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class IndexVector2New(rowInd: Exp[IndexVector], colInd: Exp[IndexVector]) extends Def[IndexVector2]
  case class IndexVector2Wildcard() extends Def[IndexVector]
  case class IndexVector2RowInd(x: Exp[IndexVector2]) extends Def[IndexVector]
  case class IndexVector2ColInd(x: Exp[IndexVector2]) extends Def[IndexVector]

  ////////////////////////////////
  // implemented via delite ops

  case class IndexVector2ConstructVectors[A:Manifest](in: Exp[IndexVector], block: Rep[Int] => Rep[Vector[A]])
    extends DeliteOpMap[Int,Vector[A],Vector] {

    val alloc = reifyEffects(Vector[Vector[A]](in.length, unit(true)))
    val v = fresh[Int]
    val func = reifyEffects(block(v))
  }

  case class IndexVector2Construct[A:Manifest](x: Exp[IndexVector2], block: (Rep[Int],Rep[Int]) => Rep[A])
    extends DeliteOpMap[Int,Vector[A],Vector] {

    val in = x.rowInd
    val alloc = reifyEffects(Vector[Vector[A]](in.length, unit(true)))
    val v = fresh[Int]
    val func = reifyEffects(x.colInd map { j => block(v, j) })
  }

  // impl defs
  def indexvector2_new(rowInd: Exp[IndexVector], colInd: Exp[IndexVector]) = IndexVector2New(rowInd, colInd)
  def indexvector2_wildcard() = IndexVector2Wildcard()

  // class defs
  def indexvector2_construct_vectors[A:Manifest](x: Exp[IndexVector2], block: Exp[Int] => Exp[Vector[A]]): Exp[Matrix[A]] = {
    if ((x.rowInd.isInstanceOfL[IndexVector]) && (x.colInd.isInstanceOfL[IndexVectorWC]))
      Matrix(IndexVector2ConstructVectors(x.rowInd, block))
    else if ((x.colInd.isInstanceOfL[IndexVector]) && (x.rowInd.isInstanceOfL[IndexVectorWC]))
      Matrix(IndexVector2ConstructVectors(x.colInd, block))
    else {
      println(unit("illegal matrix constructor"))
      exit(-1)
    }
  }
  def indexvector2_construct[A:Manifest](x: Exp[IndexVector2], block: (Exp[Int],Exp[Int]) => Exp[A]): Exp[Matrix[A]] = {
    Matrix(IndexVector2Construct(x, block))
  }
  def indexvector2_rowind(x: Exp[IndexVector2]) = IndexVector2RowInd(x)
  def indexvector2_colind(x: Exp[IndexVector2]) = IndexVector2ColInd(x)

}

trait ScalaGenIndexVector2Ops extends ScalaGenBase {
  val IR: IndexVector2OpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case IndexVector2New(rowInd, colInd) =>
      emitValDef(sym, "new " + remap(manifest[IndexVector2Impl]) + "(" + quote(rowInd) +  "," + quote(colInd) + ")")
    case IndexVector2Wildcard() => emitValDef(sym, "generated.scala.IndexVectorWCImpl")
    case IndexVector2RowInd(x) => emitValDef(sym, quote(x) + ".rowInd")
    case IndexVector2ColInd(x) => emitValDef(sym, quote(x) + ".colInd")
    case _ => super.emitNode(sym, rhs)
  }
}