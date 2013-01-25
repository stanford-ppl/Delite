package ppl.dsl.opticvx.common

import scala.collection.immutable.Seq

/*
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
case class ArityOpSequential(op1: ArityOp, op2: ArityOp) extends ArityOp {
  def apply(irpoly: IRPoly): IRPoly = {
    return op2(op1(irpoly))
  }
}
*/

//Here, xa is the arity of each of the xs.  We need this upfront in case
//the xs is an empty sequence, in order to construct an expression of appropriate
//arity.
case class ArityOp(val arity: Int, val xs: Seq[IRPoly]) extends HasArity[ArityOp] {
  if(arity < 0) throw new IRValidationException()
  for(x <- xs) {
    if(x.arity != arity) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): ArityOp = ArityOp(op.arity, xs map (x => x.arityOp(op)))
}

trait HasArity[T] {
  val arity: Int

  // this will be invariant at index idx if and only if
  // substituting two different variables at that index produces
  // the same result
  def invariantAt(idx: Int): Boolean = {
    if((idx < 0)||(idx >= arity)) throw new IRValidationException()
    val op1 = ArityOp(
      arity + 1, 
      for(i <- 0 until arity) yield IRPoly.param(i, arity + 1)
    )
    val op2 = ArityOp(
      arity + 1, 
      for(i <- 0 until arity) yield IRPoly.param(if (i == idx) arity else i, arity + 1)
    )
    val sub1 = arityOp(op1)
    val sub2 = arityOp(op2)
    return sub1 == sub2
  }

  def demote: T = removeParam(arity - 1)
  def promote: T = addParam(arity)
  
  //def promoteBy(len: Int): T = arityOp(ArityOpPromoteBy(len))
  def promoteBy(len: Int): T = {
    val op = ArityOp(
      arity + len,
      for(i <- 0 until arity) yield IRPoly.param(i, arity + len))
    arityOp(op)
  }

  //def removeParam(idx: Int): T = arityOp(ArityOpRemoveParam(idx))
  def removeParam(idx: Int): T = {
    // no well-defined way to remove this parameter if the expression is not
    // invariant in the parameter
    if(!invariantAt(idx)) throw new IRValidationException()
    val op = ArityOp(
      arity - 1,
      for(i <- 0 until arity) yield {
        if(i < idx) IRPoly.param(i, arity - 1)
        else if(i == idx) IRPoly.const(0, arity - 1)
        else IRPoly.param(i - 1, arity - 1)
      })
    arityOp(op)
  }

  //def addParam(idx: Int): T = arityOp(ArityOpAddParam(idx))
  def addParam(idx: Int): T = {
    if((idx < 0)||(idx > arity)) throw new IRValidationException()
    val op = ArityOp(
      arity + 1,
      for(i <- 0 until arity) yield {
        if(i < idx) IRPoly.param(i, arity + 1)
        else IRPoly.param(i + 1, arity + 1)
      })
    arityOp(op)
  }

  //def substituteAt(idx: Int, irpoly: IRPoly): T = arityOp(ArityOpSubstituteAt(idx, irpoly))
  def substituteAt(idx: Int, irpoly: IRPoly): T = {
    if((idx < 0)||(idx >= arity)) throw new IRValidationException()
    if(irpoly.arity + 1 != arity) throw new IRValidationException()
    val op = ArityOp(
      arity - 1,
      for(i <- 0 until arity) yield {
        if(i < idx) IRPoly.param(i, arity - 1)
        else if(i == idx) irpoly
        else IRPoly.param(i - 1, arity - 1)
      })
    arityOp(op)
  } 

  def arityOp(op: ArityOp): T
}
