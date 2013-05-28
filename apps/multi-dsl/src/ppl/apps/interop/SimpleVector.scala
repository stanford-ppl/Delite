package ppl.apps.interop

import scala.math.Fractional.Implicits._

object Vector {

  def apply[T:Numeric:Fractional:Manifest](length: Int, isRow: Boolean) = new Vector[T](length, isRow)
  def apply[T:Numeric:Fractional:Manifest](length: Int) = new Vector[T](length, false)
  def apply[T:Numeric:Fractional:Manifest](x: Array[T]) = {
    val result = new Vector[T](x.length, false) 
    System.arraycopy(x,0,result.data,0,x.length)
    result
  }
  def ones[T:Numeric:Fractional:Manifest](length: Int) = {
    val data = Array.fill[T](length)(implicitly[Numeric[T]].one)
    val result = new Vector[T](data, length, true)
    result
  }

  def fromArray[T:Numeric:Fractional:Manifest](x: Array[T]) = new Vector[T](x,x.length,true)
}

class Vector[@specialized T:Numeric:Manifest:Fractional](val data: Array[T], val length: Int, val isRow: Boolean) {

  def this(_length: Int, _isRow: Boolean) {
    this(new Array[T](_length), _length, _isRow)
  }

  def apply(idx: Int): T = data(idx)
  def update(idx: Int, newVal: T) { data(idx) = newVal }

  def map[B:Manifest:Numeric:Fractional](func: T => B): Vector[B] = {
    val resultArray:Array[B] = data.map(func)
    new Vector[B](resultArray, length, isRow)
  }
  def zip[B:Manifest:Numeric:Fractional](v: Vector[T], func: (T,T) => B): Vector[B] = {
    val resultArray:Array[B] = data.zip(v.data).map(t => func(t._1,t._2))
    new Vector[B](resultArray, length, isRow)
  }

  def +(x: T): Vector[T] = map(e => e + x)
  def -(x: T): Vector[T] = map(e => e - x)
  def *(x: T): Vector[T] = map(e => e * x)
  def /(x: T): Vector[T] = map(e => e / x)
  def log(): Vector[Double] = map(e => Math.log(e.toDouble))
  def exp(): Vector[Double] = map(e => Math.exp(e.toDouble))
  def square(): Vector[Double] = map(e => e.toDouble * e.toDouble)
  def t() = new Vector[T](data.map(e => e),length,!isRow)

  def +(v: Vector[T]) : Vector[T] = zip(v, (a,b) => a+b)
  def -(v: Vector[T]) : Vector[T] = zip(v, (a,b) => a-b)
  def *(v: Vector[T]) : Vector[T] = zip(v, (a,b) => a*b)
  def /(v: Vector[T]) : Vector[T] = zip(v, (a,b) => a/b)

  def sum(): T = data.reduce(_ + _)
  def mean(): Double = sum().toDouble / this.length

  def norm(): Vector[Double] = {
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
