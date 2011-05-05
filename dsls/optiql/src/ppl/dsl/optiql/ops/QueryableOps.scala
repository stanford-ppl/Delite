package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.DataTable
import java.io.PrintWriter
import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, Base}
import ppl.dsl.optiql.OptiQLExp

trait QueryableOps extends Base {

  //type TransparentProxy[+T] = Rep[T]

  implicit def repToQueryableOps[TSource:Manifest](r: Rep[DataTable[TSource]]) = new QOpsCls(r)

  //override def __forward[A,B,C](self: TransparentProxy[A], method: String, x: TransparentProxy[B]*): TransparentProxy[C] = throw new RuntimeException("forwarding to " + method)

  class QOpsCls[TSource:Manifest](s: Rep[DataTable[TSource]]) {
    def Where(predicate: Rep[TSource] => Rep[Boolean]) = queryable_where(s, predicate)
  }

  def queryable_where[TSource:Manifest](s: Rep[DataTable[TSource]], predicate: Rep[TSource] => Rep[Boolean]): Rep[DataTable[TSource]]

}

trait QueryableOpsExp extends QueryableOps with EffectExp {
  this: QueryableOps with OptiQLExp =>

  case class QueryableWhere[TSource:Manifest](s: Exp[DataTable[TSource]], predicate: Exp[TSource] => Exp[Boolean]) extends DeliteOpLoop[DataTable[TSource]] {
    val size = s.size
    val v = fresh[Int]
    val body : Def[DataTable[TSource]] = new DeliteCollectElem[TSource, DataTable[TSource]](
      alloc = reifyEffects(DataTable[TSource]()),
      func = reifyEffects(s(v)),
      cond = reifyEffects(predicate(s(v)))::Nil
    )
  }

  def queryable_where[TSource:Manifest](s: Exp[DataTable[TSource]], predicate: Exp[TSource] => Exp[Boolean]) = QueryableWhere(s,predicate)
}

trait ScalaGenQueryableOps extends ScalaGenEffect {
  val IR: QueryableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym,rhs)
  }
}