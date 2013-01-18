package ppl.dsl.optiml.baseline

import LanguageDefs._

object Vector {
  def rand(n: Int) = {
    val out = DenseVector[Double](n)
    for (i <- 0 until n) {
      out(i) = random()
    }
    out
  }
  
  def range(st: Int, en: Int) = {
    val out = DenseVector[Int](en-st)
    for (i <- 0 until (en-st)) {
      out(i) = st+i
    }
    out
  }
}

trait Vector[@specialized T] {
// abstract class Vector[T:Numeric:Manifest] {
  def length: Int
  def apply(i: Int): T
  def update(i: Int, x: T): Unit
  def newVector[B:Numeric:Manifest](len: Int): Vector[B]
  
  def *(s: T)(implicit m: Manifest[T], n: Numeric[T]) = {
    val out = newVector[T](length)
    for (i <- 0 until length) {
      out(i) = implicitly[Numeric[T]].times(this(i),s)
    }
    out
  }
  
  def map[B:Numeric:Manifest](f: T => B): Vector[B] = {
    val out = newVector[B](length)
    for (i <- 0 until length) {
      out(i) = f(this(i))
    }    
    out
  }
}