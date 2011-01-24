package ppl.apps.ml.lbpdenoise

import ppl.dsl.optiml.datastruct.scala.{Matrix, Vector}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 01/19/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object UnaryFactor {
  def uniform(arity: Int) {
    UnaryFactor.normalize(Vector.zeros(arity))
  }

  def normalize(uf: Vector) : Vector = {
    logZ = Math.log(uf.exp.sum)
    uf map {_ - logZ}
  }

  // Multiply elementwise by other factor
  def times(a: Vector, b: Vector) : Vector = {
    assert(a.length == b.length)

    a + b
  }

  // Add other factor elementwise
  def plus(a: Vector, b: Vector) = {
    assert(a.length == b.length)

    (a.exp + b.exp) map {Math.log(_)}
  }

  // Divide elementwise by other factor
  def divide(a: Vector, b: Vector) : Vector = {
    assert(a.length == b.length)

    a - b
  }

  def convolve(bf: Matrix, other: Vector) : Vector = {
    val indices = Vector.range(0, bf.getCols)
    val colSums = indices map { (bf.getCol(_) + other).sum } map { if(_ == 0) Double.MinValue else _ }
    colSums map {Math.log(_)}
  }

  def condition(uf: Vector, bf: Matrix, asg: Int) : Vector = {
    val other_var = if (uf.v == bf.v1) bf.v2 else bf.v1

    var x = 0
    while (x < uf.arity) {
       uf.data(x) += bf.data(bf.index4(uf.v, x, other_var, asg))
      x += 1
    }
  }

  /**This = other * damping + this * (1-damping) */
  def damp(a: Vector, b: Vector, damping: Double) = {
    assert(a.arity == b.arity)

    if (damping != 0) {
      assert(damping >= 0)
      assert(damping < 1)

      (damping * b.exp + (1.0 - damping) * a.exp) map {Math.log(_)}
    }
    else {
    a
    }
  }

  /**Compute the residual between two unary factors */
  def residual(a: Vector, b: Vector): Double = {
    assert(a.arity == b.arity)
    
    (a.exp - b.exp).abs.sum
  }

  // Max assignment
  def max_asg(uf: Vector) : Int = {
    var max_asg = 0;
    var max_value = uf(0);

    var asg = 0
    while (asg < uf.length) {
      if (uf(asg) > max_value) {
        max_value = a(asg)
        max_asg = asg
      }
      asg += 1
    }

    max_asg
  }

  def expectation(uf: Vector): Double = {
    val indices = (0::uf.length)
    
    (indices * uf.exp).sum / uf.exp.sum
  }
}

object BinaryFactor {
  def setAgreement(bf: Matrix[Double], lambda: Double) = {
    for (i <- 0 until bf.numRows) {
      for (j <- 0 until bf.numCols) {
        if (i != j)
          bf(i, j) = -lambda
        else
          bf(i, j) = 0
      }
    }
  }

  def setLaplace(bf: Matrix[Double], lambda: Double) = {
    for (i <- 0 until bf.numRows) {
      for (j <- 0 until bf.numCols) {
        bf(i, j) = -Math.abs(i - j) * lambda;
      }
    }
  }
}