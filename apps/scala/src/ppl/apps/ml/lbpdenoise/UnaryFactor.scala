package ppl.apps.ml.lbpdenoise

import ppl.delite.dsl.optiml.Vector

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 12/08/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 * Ported from GraphLab
 *
 * A unary factor is a table over a single variable and is associated
 * with edge variable in the pairwise markov random field.  Unary
 * factors are also used to represent messages. All data is
 * represented in log form.
 */

object UnaryFactor {
  def apply(v: Int, arity: Int): UnaryFactor = {
    new UnaryFactor(v, arity)
  }

  def copy(uf: UnaryFactor): UnaryFactor = {
    val nf = UnaryFactor(uf.arity, uf.arity)
    Array.copy(uf.data, 0, nf.data, 0, uf.arity)
    nf
  }

  def normalize(a: UnaryFactor) = {
    assert(a.arity > 0)

    // Scale and compute normalizing constant
    var Z = 0.0

    var asg = 0
    while (asg < a.arity) {
      Z += Math.exp(a.data(asg))
      asg += 1
    }

    assert(Z > 0)
    var logZ = Math.log(Z)

    asg = 0
    while (asg < a.arity) {
      a.data(asg) -= logZ
      asg += 1
    }
  }

  // Multiply elementwise by other factor
  def times(a: UnaryFactor, b: UnaryFactor) = {
    assert(a.arity == b.arity)

    var asg = 0
    while (asg < a.arity) {
      a.data(asg) += b.data(asg)
      asg += 1
    }
  }

  // Add other factor elementwise
  def plus(a: UnaryFactor, b: UnaryFactor) = {
    assert(a.arity == b.arity)

    var asg = 0
    while (asg < a.arity) {
      a.data(asg) = Math.log(Math.exp(a.data(asg)) + Math.exp(b.data(asg)))
      asg += 1
    }
  }

  // Divide elementwise by other factor
  def divide(a: UnaryFactor, b: UnaryFactor) = {
    assert(a.arity == b.arity)

    var asg = 0
    while (asg < a.arity) {
      a.data(asg) -= b.data(asg)
      asg += 1
    }
  }

  def convolve(uf: UnaryFactor, bf: BinaryFactor, other: UnaryFactor) = {
    var x = 0
    while (x < uf.arity) {
      var sum = 0.0

      var y = 0
      while (y < uf.arity) {
        sum += Math.exp(bf.data(bf.index4(uf.v, x, other.v, y)) + other.data(y))
        y += 1
      }

      assert(!(sum < 0))
      // Guard against zeros
      if (sum == 0)
        sum = Double.MinValue

      uf.data(x) = Math.log(sum)
      x += 1
    }
  }

  def condition(uf: UnaryFactor, bf: BinaryFactor, asg: Int) = {
    val other_var = if (uf.v == bf.v1) bf.v2 else bf.v1

    var x = 0
    while (x < uf.arity) {
       uf.data(x) += bf.data(bf.index4(uf.v, x, other_var, asg))
      x += 1
    }
  }

  /**This = other * damping + this * (1-damping) */
  def damp(a: UnaryFactor, b: UnaryFactor, damping: Double) = {
    assert(a.arity == b.arity)

    if (damping != 0) {
      assert(damping >= 0)
      assert(damping < 1)

      var asg = 0
      while (asg < a.arity) {
        a.data(asg) = Math.log(damping * Math.exp(b.data(asg)) +
                (1.0 - damping) * Math.exp(a.data(asg)))
        asg += 1
      }
    }
  }

  /**Compute the residual between two unary factors */
  def residual(a: UnaryFactor, b: UnaryFactor): Double = {
    assert(a.arity == b.arity)
    var residual = 0.0
    var asg = 0
    while (asg < a.arity) {
      residual += Math.abs(Math.exp(a.data(asg)) -
              Math.exp(b.data(asg)))
      asg += 1
    }
    residual / a.arity
  }

  // Max assignment
  def max_asg(a: UnaryFactor) : Int = {
    var max_asg = 0;
    var max_value = a.data(0);

    var asg = 0
    while (asg < a.arity) {
      if (a.data(asg) > max_value) {
        max_value = a.data(asg)
        max_asg = asg
      }
      asg += 1
    }

    max_asg
  }

  def expectation(a: UnaryFactor): Double = {
    var sum = 0.0
    var s2 = 0.0

    var asg = 0
    while (asg < a.arity) {
      sum += asg * Math.exp(a.data(asg))
      s2 += Math.exp(a.data(asg))
      asg += 1
    }

    sum / s2
  }
}

class UnaryFactor(var v: Int, val arity: Int) {
  val data = Vector.zeros(arity)
}
