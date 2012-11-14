package ppl.dsl.opticvx.common

import scala.collection.immutable.Seq

sealed trait ArityOp {
  def apply(irpoly: IRPoly): IRPoly
}
case class ArityOpRemoveParam(idx: Int) extends ArityOp {
  if (idx < 0) throw new IRValidationException()
  def apply(irpoly: IRPoly): IRPoly = {
    if (idx >= irpoly.arity) throw new IRValidationException()
    // we can "remove" a parameter only if it has no effect on the polynomial
    // this will occur only if the derivative with respect to that param is 0
    val irpolydiff = irpoly.diff(idx)
    if (!irpolydiff.is0) throw new IRValidationException()
    // construct the inputs
    val inseq: Seq[IRPoly] = for (i <- 0 until irpoly.arity) yield {
      if(i < idx) IRPoly.param(i, irpoly.arity - 1)
      else if(i == idx) IRPoly.const(0, irpoly.arity - 1)
      else IRPoly.param(i - 1, irpoly.arity - 1)
    }
    // and evaluate the polynomial
    irpoly.eval[IRPoly](inseq)(new IntLikeIRPoly(irpoly.arity - 1))
  }
}
case class ArityOpAddParam(idx: Int) extends ArityOp {
  if (idx < 0) throw new IRValidationException()
  def apply(irpoly: IRPoly): IRPoly = {
    if (idx > irpoly.arity) throw new IRValidationException()
    // construct the inputs
    val inseq: Seq[IRPoly] = for (i <- 0 until irpoly.arity) yield {
      if(i < idx) IRPoly.param(i, irpoly.arity + 1)
      else IRPoly.param(i + 1, irpoly.arity + 1)
    }
    // and evaluate the polynomial
    irpoly.eval[IRPoly](inseq)(new IntLikeIRPoly(irpoly.arity + 1))
  }
}
case class ArityOpSubstituteAt(idx: Int, subval: IRPoly) extends ArityOp {
  def apply(irpoly: IRPoly): IRPoly = {
    if (idx >= irpoly.arity) throw new IRValidationException()
    if (subval.arity != irpoly.arity - 1) throw new IRValidationException()
    // construct the inputs
    val inseq: Seq[IRPoly] = for (i <- 0 until irpoly.arity) yield {
      if(i < idx) IRPoly.param(i, irpoly.arity - 1)
      else if(i == idx) subval
      else IRPoly.param(i - 1, irpoly.arity - 1)
    }
    // and evaluate the polynomial
    irpoly.eval[IRPoly](inseq)(new IntLikeIRPoly(irpoly.arity - 1))
  }
}
case class ArityOpSwap(idx1: Int, idx2: Int) extends ArityOp {
  if ((idx1 < 0)||(idx2 < 0)||(idx1 == idx2)) throw new IRValidationException()
  def apply(irpoly: IRPoly): IRPoly = {
    if ((idx1 >= irpoly.arity)||(idx2 >= irpoly.arity)) throw new IRValidationException()
    // construct the inputs
    val inseq: Seq[IRPoly] = for (i <- 0 until irpoly.arity) yield {
      if(i == idx1) IRPoly.param(idx2, irpoly.arity)
      else if(i == idx2) IRPoly.param(idx1, irpoly.arity)
      else IRPoly.param(i, irpoly.arity)
    }
    // and evaluate the polynomial
    irpoly.eval[IRPoly](inseq)(new IntLikeIRPoly(irpoly.arity))
  }
}
case class ArityOpPromoteBy(len: Int) extends ArityOp {
  if (len < 0) throw new IRValidationException()
  def apply(irpoly: IRPoly): IRPoly = {
    // construct the inputs
    val inseq: Seq[IRPoly] = for (i <- 0 until irpoly.arity) yield {
      IRPoly.param(i, irpoly.arity + len)
    }
    // and evaluate the polynomial
    irpoly.eval[IRPoly](inseq)(new IntLikeIRPoly(irpoly.arity + len))
  }
}


trait HasArity[T] {
  val arity: Int
  def demote: T = removeParam(arity - 1)
  def promote: T = addParam(arity)
  def promoteBy(len: Int): T = arityOp(ArityOpPromoteBy(len))
  def removeParam(idx: Int): T = arityOp(ArityOpRemoveParam(idx))
  def addParam(idx: Int): T = arityOp(ArityOpAddParam(idx))
  def substituteAt(idx: Int, irpoly: IRPoly) = arityOp(ArityOpSubstituteAt(idx, irpoly))
  def arityOp(op: ArityOp): T
}
