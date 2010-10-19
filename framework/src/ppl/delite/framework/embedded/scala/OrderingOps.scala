package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait OrderingOps extends DSLType { this: DeliteApplication =>
  def infix_<[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T) = ordering_lt(lhs,c(rhs))
  def infix_<=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T) = ordering_lteq(lhs,c(rhs))
  def infix_>[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T) = ordering_gt(lhs,c(rhs))
  def infix_>=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T) = ordering_gteq(lhs,c(rhs))
  def infix_equiv[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T) = ordering_equiv(lhs,c(rhs))
  def infix_max[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T) = ordering_max(lhs,c(rhs))
  def infix_min[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => T) = ordering_min(lhs,c(rhs))

  def ordering_lt[T:Ordering](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_lteq[T:Ordering](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_gt[T:Ordering](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_gteq[T:Ordering](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_equiv[T:Ordering](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_max[T:Ordering](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def ordering_min[T:Ordering](lhs: Rep[T], rhs: Rep[T]): Rep[T]
}


trait OrderingOpsExp { this: DeliteApplication =>
  case class OrderingLT[T:Ordering](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingLTEQ[T:Ordering](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingGT[T:Ordering](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingGTEQ[T:Ordering](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingEquiv[T:Ordering](lhs: Exp[T], rhs: Exp[T]) extends Def[Boolean]
  case class OrderingMax[T:Ordering](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class OrderingMin[T:Ordering](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  def ordering_lt[T:Ordering](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingLT(lhs,rhs)
  def ordering_lteq[T:Ordering](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingLTEQ(lhs,rhs)
  def ordering_gt[T:Ordering](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingGT(lhs,rhs)
  def ordering_gteq[T:Ordering](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingGTEQ(lhs,rhs)
  def ordering_equiv[T:Ordering](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingEquiv(lhs,rhs)
  def ordering_max[T:Ordering](lhs: Exp[T], rhs: Exp[T]): Rep[T] = OrderingMax(lhs,rhs)
  def ordering_min[T:Ordering](lhs: Exp[T], rhs: Exp[T]): Rep[T] = OrderingMin(lhs,rhs)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaOrdering { val intermediate: OrderingOpsExp.this.type = OrderingOpsExp.this })
}

trait CodeGeneratorScalaOrdering extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with OrderingOpsExp 
  import intermediate._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case OrderingLT(a,b) => emitValDef(sym, quote(a) + " < " + quote(b))
    case OrderingLTEQ(a,b) => emitValDef(sym, quote(a) + " <= " + quote(b))
    case OrderingGT(a,b) => emitValDef(sym, quote(a) + " > " + quote(b))
    case OrderingGTEQ(a,b) => emitValDef(sym, quote(a) + " >= " + quote(b))
    case OrderingEquiv(a,b) => emitValDef(sym, quote(a) + " equiv " + quote(b))
    case OrderingMax(a,b) => emitValDef(sym, quote(a) + " max " + quote(b))
    case OrderingMin(a,b) => emitValDef(sym, quote(a) + " min " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
