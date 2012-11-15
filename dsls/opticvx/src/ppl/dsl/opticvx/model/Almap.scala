// Almap = Abstract Linear MAP
// This represents a linear map as a composition of a set of linear mapping primitives

package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.solver._
import scala.collection.immutable.Seq


trait Almap extends HasArity[Almap] {
  //The size of the input parameters used by this map
  val input: IRPoly
  //The domain and codomain sizes of this map
  val domain: IRPoly
  val codomain: IRPoly

  //Constraints that all the shape properties must share the map's arity
  def arityVerify() {
    if (input.arity != arity) throw new IRValidationException()
    if (domain.arity != arity) throw new IRValidationException()
    if (codomain.arity != arity) throw new IRValidationException()
  }
  
  //The transpose
  def T: Almap

  //Amount of scratch space necessary for computation (assume 0)
  def scratch: IRPoly

  //Code generation for this matrix
  def genmmpy(
    context: SolverContext,
    src: IRPoly,
    dst: IRPoly,
    scratch: IRPoly,
    srcscale: SolverExpr,
    dstscale: SolverExpr
    ): Seq[SolverInstr]
}

//The identity map
case class AlmapIdentity(val input: IRPoly, val domain: IRPoly) extends Almap {
  val arity: Int = input.arity
  val codomain: IRPoly = domain
  
  def arityOp(op: ArityOp): Almap = AlmapIdentity(input.arityOp(op), domain.arityOp(op))

  def T: Almap = this

  def scratch: IRPoly = IRPoly.const(0, arity)

  arityVerify()
}


//The zero map
case class AlmapZero(val input: IRPoly, val domain: IRPoly, val codomain: IRPoly) extends Almap {
  val arity: Int = input.arity
  
  def arityOp(op: ArityOp): Almap = AlmapZero(input.arityOp(op), domain.arityOp(op), codomain.arityOp(op))

  def T: Almap = AlmapZero(input, codomain, domain)

  def scratch: IRPoly = IRPoly.const(0, arity)

  arityVerify()
}

//The sum of two linear maps
case class AlmapSum(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val input: IRPoly = arg1.input
  val domain: IRPoly = arg1.domain
  val codomain: IRPoly = arg1.codomain

  if (arg2.arity != arity) throw new IRValidationException()
  if (arg2.input != input) throw new IRValidationException()
  if (arg2.domain != domain) throw new IRValidationException()
  if (arg2.codomain != codomain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapSum(arg1.arityOp(op), arg2.arityOp(op))

  def T: Almap = AlmapSum(arg1.T, arg2.T)

  def scratch: IRPoly = IRPoly.pmax(arg1.scratch, arg2.scratch)
  
  arityVerify()
}

//Negation of a linear map
case class AlmapNeg(val arg: Almap) extends Almap {
  val arity: Int = arg.arity
  val input: IRPoly = arg.input
  val domain: IRPoly = arg.domain
  val codomain: IRPoly = arg.codomain
  
  def arityOp(op: ArityOp): Almap = AlmapNeg(arg.arityOp(op))

  def T: Almap = AlmapNeg(arg.T)

  def scratch: IRPoly = arg.scratch
  
  arityVerify()
}

//Scale of a linear map by some indexing of the input space
case class AlmapScale(val arg: Almap, val scale: IRPoly) extends Almap {
  val arity: Int = arg.arity
  val input: IRPoly = arg.input
  val domain: IRPoly = arg.domain
  val codomain: IRPoly = arg.codomain

  if (arity != scale.arity) throw new IRValidationException()
  
  def arityOp(op: ArityOp): Almap = AlmapScale(arg.arityOp(op), scale.arityOp(op))
  
  def T: Almap = AlmapScale(arg.T, scale)

  def scratch: IRPoly = arg.scratch
  
  arityVerify()
}

//Scale of a linear map by a constant
case class AlmapScaleConstant(val arg: Almap, val scale: Double) extends Almap {
  val arity: Int = arg.arity
  val input: IRPoly = arg.input
  val domain: IRPoly = arg.domain
  val codomain: IRPoly = arg.codomain
  
  def arityOp(op: ArityOp): Almap = AlmapScaleConstant(arg.arityOp(op), scale)
  
  def T: Almap = AlmapScaleConstant(arg.T, scale)

  def scratch: IRPoly = arg.scratch

  arityVerify()
}


//The vertical concatenation of two linear maps
case class AlmapVCat(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val input: IRPoly = arg1.input
  val domain: IRPoly = arg1.domain
  val codomain: IRPoly = arg1.codomain + arg2.codomain

  if (arg1.arity != arg2.arity) throw new IRValidationException()
  if (arg1.input != arg2.input) throw new IRValidationException()
  if (arg1.domain != arg2.domain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapVCat(arg1.arityOp(op), arg2.arityOp(op))

  def T: Almap = AlmapHCat(arg1.T, arg2.T)

  def scratch: IRPoly = IRPoly.pmax(arg1.scratch, arg2.scratch)
  
  arityVerify()
}


//The vertical concatenation of a number of linear maps, depending on problem size
case class AlmapVCatFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val input: IRPoly = body.input.demote
  val domain: IRPoly = body.domain.demote
  val codomain: IRPoly = body.codomain.sum(arity).substituteAt(arity, len)

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapVCatFor(len.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapHCatFor(len, body.T)

  // Since we'll be running all the bodies in parallel, we need to sum, not max
  def scratch: IRPoly = body.scratch.sum(arity).substituteAt(arity, len)
  
  arityVerify()
}

// "Puts" the given almap at the target index, all other entries are 0
case class AlmapVPut(val len: IRPoly, val at: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val input: IRPoly = body.input.demote
  val domain: IRPoly = body.domain.demote
  val codomain: IRPoly = body.codomain.sum(arity).substituteAt(arity, len)

  if (body.arity != (len.arity + 1)) throw new IRValidationException()
  if (len.arity != at.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapVPut(len.arityOp(op), at.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapHPut(len, at, body.T)

  def scratch: IRPoly = body.scratch.substituteAt(arity, at)

  arityVerify()
}

//The horizontal concatenation of two linear maps
case class AlmapHCat(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val input: IRPoly = arg1.input
  val domain: IRPoly = arg1.domain + arg2.domain
  val codomain: IRPoly = arg1.codomain

  if (arg1.arity != arg2.arity) throw new IRValidationException()
  if (arg1.input != arg2.input) throw new IRValidationException()
  if (arg1.codomain != arg2.codomain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapHCat(arg1.arityOp(op), arg2.arityOp(op))

  def T: Almap = AlmapVCat(arg1.T, arg2.T)

  def scratch: IRPoly = IRPoly.pmax(arg1.scratch, arg2.scratch)
  
  arityVerify()
}


//The horzontal concatenation of a number of linear maps, depending on problem size
case class AlmapHCatFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val input: IRPoly = body.input.demote
  val domain: IRPoly = body.domain.sum(arity).substituteAt(arity, len)
  val codomain: IRPoly = body.codomain.demote

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapHCatFor(len.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapVCatFor(len, body.T)
  
  //Since we'll be running all the bodies in parallel, we need to sum, not max
  def scratch: IRPoly = body.scratch.sum(arity).substituteAt(arity, len)

  arityVerify()
}

// "Puts" the given almap at the target index, all other entries are 0
case class AlmapHPut(val len: IRPoly, val at: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val input: IRPoly = body.input.demote
  val domain: IRPoly = body.domain.sum(arity).substituteAt(arity, len)
  val codomain: IRPoly = body.codomain.demote

  if (body.arity != (len.arity + 1)) throw new IRValidationException()
  if (len.arity != at.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapHPut(len.arityOp(op), at.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapVPut(len, at, body.T)

  def scratch: IRPoly = body.scratch.substituteAt(arity, at)

  arityVerify()
}

//The sum of a problem-size-dependent number of linear ops
case class AlmapSumFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val input: IRPoly = body.input.demote
  val domain: IRPoly = body.domain.demote
  val codomain: IRPoly = body.codomain.demote

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapSumFor(len.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapSumFor(len, body.T)

  def scratch: IRPoly = body.scratch.sum(arity).substituteAt(arity, len) + body.codomain.sum(arity).substituteAt(arity, len)
  
  arityVerify()
}

//Matrix multiply
case class AlmapProd(val argl: Almap, val argr: Almap) extends Almap {
  if (argl.arity != argr.arity) throw new IRValidationException()
  if (argl.input != argr.input) throw new IRValidationException()
  if (argl.domain != argr.codomain) throw new IRValidationException()

  val arity: Int = argl.arity
  val input: IRPoly = argl.input
  val domain: IRPoly = argr.domain
  val codomain: IRPoly = argl.codomain

  def arityOp(op: ArityOp): Almap = AlmapProd(argl.arityOp(op), argr.arityOp(op))

  def T: Almap = AlmapProd(argr.T, argl.T)

  def scratch: IRPoly = argl.domain + IRPoly.pmax(argl.scratch, argr.scratch)

  arityVerify()
}


