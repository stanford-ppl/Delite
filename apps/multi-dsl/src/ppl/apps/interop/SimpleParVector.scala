package ppl.apps.interop

import scala.collection.parallel.mutable.ParArray
import scala.math.Fractional.Implicits._

object ParVector {

  def apply[T:Numeric:Fractional:Manifest](length: Int, isRow: Boolean) = new ParVector[T](length, isRow)
  def apply[T:Numeric:Fractional:Manifest](length: Int) = new ParVector[T](length, false)
  def apply[T:Numeric:Fractional:Manifest](x: ParArray[T]) = {
    val result = new ParVector[T](x.length, false) 
    System.arraycopy(x,0,result.data,0,x.length)
    result
  }
  def ones[T:Numeric:Fractional:Manifest](length: Int) = {
    val data = ParArray.fill[T](length)(implicitly[Numeric[T]].one)
    val result = new ParVector[T](data, length, true)
    result
  }

  def fromParArray[T:Numeric:Fractional:Manifest](x: ParArray[T]) = new ParVector[T](x,x.length,true)
}

class ParVector[@specialized T:Numeric:Manifest:Fractional](val data: ParArray[T], val length: Int, val isRow: Boolean) {

  def this(_length: Int, _isRow: Boolean) {
    this(new ParArray[T](_length), _length, _isRow)
  }

  def apply(idx: Int): T = data(idx)
  def update(idx: Int, newVal: T) { data(idx) = newVal }

  def map[B:Manifest:Numeric:Fractional](func: T => B): ParVector[B] = {
    val resultParArray:ParArray[B] = data.map(func)
    new ParVector[B](resultParArray, length, isRow)
  }
  def zip[B:Manifest:Numeric:Fractional](v: ParVector[T], func: (T,T) => B): ParVector[B] = {
    val resultParArray:ParArray[B] = data.zip(v.data).map(t => func(t._1,t._2))
    new ParVector[B](resultParArray, length, isRow)
  }

  def +(x: T): ParVector[T] = map(e => e + x)
  def -(x: T): ParVector[T] = map(e => e - x)
  def *(x: T): ParVector[T] = map(e => e * x)
  def /(x: T): ParVector[T] = map(e => e / x)
  def log(): ParVector[Double] = map(e => Math.log(e.toDouble))
  def exp(): ParVector[Double] = map(e => Math.exp(e.toDouble))
  def square(): ParVector[Double] = map(e => e.toDouble * e.toDouble)
  def t() = new ParVector[T](data.map(e => e),length,!isRow)

  def +(v: ParVector[T]) : ParVector[T] = zip(v, (a,b) => a+b)
  def -(v: ParVector[T]) : ParVector[T] = zip(v, (a,b) => a-b)
  def *(v: ParVector[T]) : ParVector[T] = zip(v, (a,b) => a*b)
  def /(v: ParVector[T]) : ParVector[T] = zip(v, (a,b) => a/b)

  def sum(): T = data.reduce(_ + _)
  def mean(): Double = sum().toDouble / this.length

  def norm(): ParVector[Double] = {
    val sum = this.sum()
    val result = map(e => e.toDouble / sum.toDouble)
    result
  }

  def stddev(): Double = {
    val m = mean()
    val acc = this.map(e => e.toDouble - m).square.sum //(this - m).square.sum
    (Math.sqrt(acc) / this.length).toDouble
  }

}
