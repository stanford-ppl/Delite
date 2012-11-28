package ppl.dsl.opticvx.common

import scala.collection.immutable.Seq

object IRPoly {
  def const(c: Int, arity: Int): IRPoly = {
    if (arity < 0) throw new IRValidationException()
    if (arity == 0) {
      IRPolyA0(c)
    }
    else {
      if(c == 0) IRPolyN(arity, Seq())
      else IRPolyN(arity, Seq(const(c, arity - 1)))
    }
  }
  def param(idx: Int, arity: Int): IRPoly = {
    if ((idx >= arity)||(idx < 0)) throw new IRValidationException()
    if (idx + 1 == arity) {
      IRPolyN(arity, Seq(const(0, arity - 1), const(1, arity - 1)))
    }
    else {
      IRPolyN(arity, Seq(param(idx, arity - 1)))
    }
  }
  // given two polynomials, returns their least upper bound respect to the partial ordering
  // that y >= x iff the coefficients of y are greater than those of x
  def pmax(x: IRPoly, y: IRPoly): IRPoly = {
    if(x.arity != y.arity) throw new IRValidationException()
    if(x.arity == 0) {
      IRPolyA0(math.max(x.asInstanceOf[IRPolyA0].c0, y.asInstanceOf[IRPolyA0].c0))
    }
    else {
      var xcfs: Seq[IRPoly] = x.asInstanceOf[IRPolyN].coeffs
      var ycfs: Seq[IRPoly] = y.asInstanceOf[IRPolyN].coeffs
      val poly0: IRPoly = if (x.arity == 1) IRPolyA0(0) else IRPolyN(x.arity - 1, Seq())
      while(xcfs.length < ycfs.length) {
        xcfs = xcfs :+ poly0
      }
      while(ycfs.length < xcfs.length) {
        ycfs = ycfs :+ poly0
      }
      var outcfs: Seq[IRPoly] = for (i <- 0 until xcfs.length) yield (pmax(xcfs(i), ycfs(i)))
      while((outcfs.length > 0)&&(outcfs(outcfs.length - 1).is0)) {
        outcfs = outcfs.take(outcfs.length - 1)
      }
      IRPolyN(x.arity, outcfs)
    }
  }
}

sealed trait IRPoly extends HasArity[IRPoly] {
  def is0: Boolean

  def +(y: IRPoly): IRPoly = {
    val x: IRPoly = this
    if (x.arity != y.arity) throw new IRValidationException()
    if (x.arity == 0) {
      IRPolyA0(x.asInstanceOf[IRPolyA0].c0 + y.asInstanceOf[IRPolyA0].c0)
    }
    else {
      var xcfs: Seq[IRPoly] = x.asInstanceOf[IRPolyN].coeffs
      var ycfs: Seq[IRPoly] = y.asInstanceOf[IRPolyN].coeffs
      val poly0: IRPoly = if (x.arity == 1) IRPolyA0(0) else IRPolyN(x.arity - 1, Seq())
      while(xcfs.length < ycfs.length) {
        xcfs = xcfs :+ poly0
      }
      while(ycfs.length < xcfs.length) {
        ycfs = ycfs :+ poly0
      }
      var outcfs: Seq[IRPoly] = for (i <- 0 until xcfs.length) yield (xcfs(i) + ycfs(i))
      while((outcfs.length > 0)&&(outcfs(outcfs.length - 1).is0)) {
        outcfs = outcfs.take(outcfs.length - 1)
      }
      IRPolyN(x.arity, outcfs)
    }
  }

  def unary_-(): IRPoly = {
    if (this.arity == 0) {
      IRPolyA0(-(this.asInstanceOf[IRPolyA0].c0))
    }
    else {
      IRPolyN(this.arity, this.asInstanceOf[IRPolyN].coeffs map ((r) => -r))
    }
  }

  def -(y: IRPoly): IRPoly = (this + (-y))

  // is this polynomial nonnegative for nonnegative inputs?
  def isNonNegative: Boolean = {
    if (this.arity == 0) {
      this.asInstanceOf[IRPolyA0].c0 >= 0
    }
    else {
      var rv: Boolean = true
      for(c <- this.asInstanceOf[IRPolyN].coeffs) {
        rv = rv && (c.isNonNegative)
      }
      rv
    }
  }

  // comparison operators, which produce a partial order over the space
  def >=(y: IRPoly): Boolean = (this - y).isNonNegative
  def <=(y: IRPoly): Boolean = (y - this).isNonNegative
  def >(y: IRPoly): Boolean = (this >= y)&&(this != y)
  def <(y: IRPoly): Boolean = (this <= y)&&(this != y)

  // product
  def *(y: IRPoly): IRPoly = {
    val x: IRPoly = this
    if (x.arity != y.arity) throw new IRValidationException()
    if (x.arity == 0) {
      IRPolyA0(x.asInstanceOf[IRPolyA0].c0 * y.asInstanceOf[IRPolyA0].c0)
    }
    else if((x.is0)||(y.is0)) {
      IRPolyN(arity, Seq())
    }
    else {
      val xcfs: Seq[IRPoly] = x.asInstanceOf[IRPolyN].coeffs
      val ycfs: Seq[IRPoly] = y.asInstanceOf[IRPolyN].coeffs
      val c0: IRPoly = xcfs(0) * ycfs(0)
      val dx: IRPoly = IRPolyN(arity, xcfs.drop(1))
      val dy: IRPoly = IRPolyN(arity, ycfs.drop(1))
      val dz: IRPoly = y*dx + x*dy + dx*dy
      val dzcfs: Seq[IRPoly] = dz.asInstanceOf[IRPolyN].coeffs
      IRPolyN(arity, Seq(c0) ++ dzcfs)
    }
  }

  // division by an integer
  def /(r: Int): IRPoly = {
    if(arity == 0) {
      if((this.asInstanceOf[IRPolyA0].c0 % r) != 0) throw new IRValidationException()
      IRPolyA0(this.asInstanceOf[IRPolyA0].c0 / r)
    }
    else {
      IRPolyN(arity, this.asInstanceOf[IRPolyN].coeffs map (c => c / r))
    }
  }

  // sums the polynomial over the variable at the given index
  def sum(idx: Int): IRPoly = {
    if (arity <= 0) throw new IRValidationException()
    if (idx == arity - 1) {
      IRPolyN(arity, Seq(IRPoly.const(0, arity - 1)) ++ this.asInstanceOf[IRPolyN].coeffs)
    }
    else if (idx < arity - 1) {
      IRPolyN(arity, this.asInstanceOf[IRPolyN].coeffs map (x => x.sum(idx)))
    }
    else {
      throw new IRValidationException()
    }
  }

  // computes the sequence difference of this polynomial over the variable at the given index
  def diff(idx: Int): IRPoly = {
    if (arity <= 0) throw new IRValidationException()
    if (idx == arity - 1) {
      if (this.is0) return this
      IRPolyN(arity, this.asInstanceOf[IRPolyN].coeffs.drop(1))
    }
    else if (idx < arity - 1) {
      var cfs: Seq[IRPoly] = this.asInstanceOf[IRPolyN].coeffs map (x => x.diff(idx))
      while((cfs.length > 0)&&(cfs(cfs.length - 1).is0)) {
        cfs = cfs.take(cfs.length - 1)
      }
      IRPolyN(arity, cfs)
    }
    else {
      throw new IRValidationException()
    }
  }

  // evaluate the polynomial at some value
  def eval[T](x: Seq[T])(implicit e: IntLike[T]): T = {
    import e._
    if (x.length != arity) throw new IRValidationException()
    if (arity == 0) return this.asInstanceOf[IRPolyA0].c0
    val cfs: Seq[IRPoly] = this.asInstanceOf[IRPolyN].coeffs
    val xf: Seq[T] = x.take(x.length - 1)
    var xl: T = x(x.length - 1)
    var rl: T = 1
    var acc: T = 0
    for (i <- 0 until cfs.length) {
      val c = cfs(i).eval(xf)
      acc += c * rl
      rl = (rl * xl) / (i + 1)
      xl -= 1
    }
    return acc
  }

  // next param
  def next: IRPoly = IRPoly.param(arity, arity+1)

  // arity ops
  def arityOp(op: ArityOp): IRPoly = op(this)

  // pretty print methods
  def toString(varnames: Seq[String]): String
  override def toString: String = {
    val varnames = Seq("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
    if (arity > varnames.length) throw new IRValidationException()
    toString(varnames.take(arity))
  }
}

case class IRPolyA0(val c0: Int) extends IRPoly {
  val arity: Int = 0
  def is0: Boolean = (c0 == 0)
  // pretty printing (for debugging)
  def toString(varnames: Seq[String]): String = {
    if(varnames.length != 0) throw new IRValidationException()
    c0.toString
  }
}
case class IRPolyN(val arity: Int, val coeffs: Seq[IRPoly]) extends IRPoly {
  // This case class can't represent arity-0 polynomials
  if (arity < 1) throw new IRValidationException()
  // All coefficients of this polynomial must have arity A-1
  for(c <- coeffs) {
    if (arity != c.arity + 1) throw new IRValidationException()
  }
  // The highest-order coefficient can't be zero
  if (coeffs.length > 0) {
    if (coeffs(coeffs.length - 1).is0) throw new IRValidationException()
  }
  // This is zero if it has no coefficients
  def is0: Boolean = (coeffs.length == 0)
  // pretty printing (for debugging)
  def toString(varnames: Seq[String]): String = {
    if(varnames.length != arity) throw new IRValidationException()
    if(coeffs.length == 0) {
      "0"
    }
    else {
      var rv: Seq[String] = Seq()
      for (i <- 0 until coeffs.length) yield {
        if (!(coeffs(i).is0)) {
          if (i == 0) {
            rv = rv :+ coeffs(i).toString(varnames.init)
          }
          else if (i == 1) {
            rv = rv :+ ("(" + coeffs(i).toString(varnames.init) + ")" + varnames.last)
          }
          else {
            rv = rv :+ ("(" + coeffs(i).toString(varnames.init) + ")" + varnames.last + "@" + i.toString)
          }
        }
      }
      rv mkString " + "
    }
  }
}

class IntLikeIRPoly(val arity: Int) extends IntLike[IRPoly] {
  def add(x: IRPoly, y: IRPoly): IRPoly = x + y
  def neg(x: IRPoly): IRPoly = -x
  def multiply(x: IRPoly, y: IRPoly): IRPoly = x * y
  def divide(x: IRPoly, r: Int): IRPoly = x / r

  implicit def int2T(x: Int): IRPoly = IRPoly.const(x, arity)
}

