package ppl.dsl.opticvx.common

import scala.collection.immutable.Seq

/* Note that the "arity" used in this function is not the same as the arity used
   elsewhere in the code.  It refers to the number of signum inputs to the polynomial,
   not the number of integer inputs (there are no integer inputs). */

object SignumPoly {
  def const(c: Signum, arity: Int): SignumPoly = {
    if (arity < 0) throw new IRValidationException()
    if (arity == 0) {
      SignumPolyA0(c)
    }
    else {
      val cl0 = const(Signum.Zero, arity-1)
      SignumPolyN(const(c, arity-1), cl0, cl0)
    }
  }
  def param(idx: Int, arity: Int): SignumPoly = {
    if ((idx >= arity)||(idx < 0)) throw new IRValidationException()
    if (idx + 1 == arity) {
      val cl0 = const(Signum.Zero, arity-1)
      val cl1 = const(Signum.Positive, arity-1)
      SignumPolyN(cl0, cl1, cl0)
    }
    else {
      val cl0 = const(Signum.Zero, arity-1)
      SignumPolyN(param(idx, arity-1), cl0, cl0)
    }
  }
}

sealed trait SignumPoly {
  val arity: Int

  def +(y: SignumPoly): SignumPoly = {
    if(arity != y.arity) throw new IRValidationException()
    if(arity == 0) {
      val xa = this.asInstanceOf[SignumPolyA0]
      val ya = y.asInstanceOf[SignumPolyA0]
      SignumPolyA0(xa.c0 + ya.c0)
    }
    else {
      val xa = this.asInstanceOf[SignumPolyN]
      val ya = y.asInstanceOf[SignumPolyN]
      SignumPolyN(xa.c0 + ya.c0, xa.c1 + ya.c1, xa.c2 + ya.c2)
    }
  }

  def unary_-(): SignumPoly = {
    if(arity == 0) {
      val xa = this.asInstanceOf[SignumPolyA0]
      SignumPolyA0(-xa.c0)
    }
    else {
      val xa = this.asInstanceOf[SignumPolyN]
      SignumPolyN(-xa.c0, -xa.c1, -xa.c2)
    }
  }

  def -(y: SignumPoly): SignumPoly = this + (-y)

  def *(y: SignumPoly): SignumPoly = {
    if(arity != y.arity) throw new IRValidationException()
    if(arity == 0) {
      val xa = this.asInstanceOf[SignumPolyA0]
      val ya = y.asInstanceOf[SignumPolyA0]
      SignumPolyA0(xa.c0 * ya.c0)
    }
    else {
      val xa = this.asInstanceOf[SignumPolyN]
      val ya = y.asInstanceOf[SignumPolyN]
      SignumPolyN(
        xa.c0 * ya.c0,
        xa.c1 * (ya.c0 + ya.c2) + (xa.c0 + xa.c2) * ya.c1,
        xa.c2 * (ya.c0 + ya.c2) + (xa.c0 + xa.c2) * ya.c2 + xa.c1 * ya.c1)
    }
  }

  def +(y: Signum): SignumPoly = this + (SignumPoly.const(y, arity))
  def -(y: Signum): SignumPoly = this + (SignumPoly.const(-y, arity))
  def *(y: Signum): SignumPoly = this * (SignumPoly.const(y, arity))

  def eval(s: Seq[Signum]): Signum = {
    if(s.length != arity) throw new IRValidationException()
    if (arity == 0) {
      this.asInstanceOf[SignumPolyA0].c0
    }
    else {
      val xa = this.asInstanceOf[SignumPolyN]
      val sf: Seq[Signum] = s.take(s.length - 1)
      var sl: Signum = s(s.length - 1)
      xa.c0.eval(sf) + sl * xa.c1.eval(sf) + sl * sl * xa.c2.eval(sf)
    }
  }

  def evalpoly(s: Seq[SignumPoly]): Signum = {
    if(s.length != arity) throw new IRValidationException()
    if(arity == 0) throw new IRValidationException()
    for(i <- 0 until s.length) {
      if(s(i).arity != s(0).arity) throw new IRValidationException()
    }
    val xa = this.asInstanceOf[SignumPolyN]
    val sf: Seq[SignumPoly] = s.take(s.length - 1)
    var sl: SignumPoly = s(s.length - 1)
    xa.c0.evalpoly(sf) + sl * xa.c1.evalpoly(sf) + sl * sl * xa.c2.evalpoly(sf)
  }
}

case class SignumPolyA0(val c0: Signum) extends SignumPoly {
  val arity: Int = 0
}

/* We only need up to 2 coefficients because (X)^3 == X */
case class SignumPolyN(val c0: SignumPoly, val c1: SignumPoly, val c2: SignumPoly) extends SignumPoly {
  val arity: Int = (c0.arity + 1)
  if(c0.arity != c1.arity) throw new IRValidationException()
  if(c0.arity != c2.arity) throw new IRValidationException()
}
