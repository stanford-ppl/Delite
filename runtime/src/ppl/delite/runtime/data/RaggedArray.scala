package ppl.delite.runtime.data

import ppl.delite.runtime.messages._
import ppl.delite.runtime.DeliteMesosScheduler


/**
 * RaggedArray - used for arrays with more than Int.MaxSize elements.
 *
 * It would be better to unify DeliteArray runtime abstractions into a single
 * data structure that encapsulates local, remote, and ragged, since the
 * combinations become unwieldly. It would be nice to be able to generate only
 * 2 variants from the framework (native array and DeliteArray), and have
 * all of the other functionality handled by DeliteArray.
 */

trait RaggedArray[T]

///////////////////////////////////////////////////////////////////////////////
// RaggedNativeArray (Uses native arrays underneath)
///////////////////////////////////////////////////////////////////////////////

object RaggedNativeArray {
  // Constructor used for string split
  def apply[T:Manifest](a: Array[T]) = {
    val out = new RaggedNativeArrayObject[T](a.length)
    for (i <- 0 until a.length) out(i) = a(i)
    out
  }
}

trait RaggedNativeArray[T] extends RaggedArray[T] {
  val elemsPerArray = 2e9.toInt //Int.MaxValue-2 (VM limit varies)
  val data: Array[Array[T]]

  def length: Long
  def apply(i: Long): T
  def update(i: Long, y: T)
  def mkString(s: String) = data.map(_.mkString(s)).mkString(s)

  def copy(_srcPos: Long, dest: RaggedNativeArray[T], _destPos: Long, len: Long) = {
    var srcPos = _srcPos
    var destPos = _destPos
    var elemsToCopy = len

    while (elemsToCopy > 0) {
      val srcArray = (srcPos / elemsPerArray).toInt
      val srcIndex = (srcPos % elemsPerArray).toInt
      val destArray = (destPos / elemsPerArray).toInt
      val destIndex = (destPos % elemsPerArray).toInt
      val space = Math.min(Math.min(elemsPerArray - srcIndex, elemsPerArray - destIndex), elemsToCopy.toInt)
      System.arraycopy(this.data(srcArray), srcIndex, dest.data(destArray), destIndex, space)
      srcPos += space
      destPos += space
      elemsToCopy -= space
    }
  }
}

final class RaggedNativeArrayInt(n: Long) extends RaggedNativeArray[Int] {
  val numArrays = (n / elemsPerArray).toInt+1
  val data = new Array[Array[Int]](numArrays)
  for (i <- 0 until data.length) {
    val size = if (i == data.length-1) (n % elemsPerArray).toInt else elemsPerArray
    data(i) = new Array[Int](size)
  }

  def length: Long = n

  def apply(i: Long): Int = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).apply(j)
  }

  def update(i: Long, y: Int) = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).update(j, y)
  }
}

final class RaggedNativeArrayDouble(n: Long) extends RaggedNativeArray[Double] {
  val numArrays = (n / elemsPerArray).toInt+1
  val data = new Array[Array[Double]](numArrays)
  for (i <- 0 until data.length) {
    val size = if (i == data.length-1) (n % elemsPerArray).toInt else elemsPerArray
    data(i) = new Array[Double](size)
  }

  def length: Long = n

  def apply(i: Long): Double = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).apply(j)
  }

  def update(i: Long, y: Double) = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).update(j, y)
  }
}

final class RaggedNativeArrayFloat(n: Long) extends RaggedNativeArray[Float] {
  val numArrays = (n / elemsPerArray).toInt+1
  val data = new Array[Array[Float]](numArrays)
  for (i <- 0 until data.length) {
    val size = if (i == data.length-1) (n % elemsPerArray).toInt else elemsPerArray
    data(i) = new Array[Float](size)
  }

  def length: Long = n

  def apply(i: Long): Float = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).apply(j)
  }

  def update(i: Long, y: Float) = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).update(j, y)
  }
}

final class RaggedNativeArrayBoolean(n: Long) extends RaggedNativeArray[Boolean] {
  val numArrays = (n / elemsPerArray).toInt+1
  val data = new Array[Array[Boolean]](numArrays)
  for (i <- 0 until data.length) {
    val size = if (i == data.length-1) (n % elemsPerArray).toInt else elemsPerArray
    data(i) = new Array[Boolean](size)
  }

  def length: Long = n

  def apply(i: Long): Boolean = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).apply(j)
  }

  def update(i: Long, y: Boolean) = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).update(j, y)
  }
}

class RaggedNativeArrayObject[T:Manifest](n: Long) extends RaggedNativeArray[T] {
  val numArrays = (n / elemsPerArray).toInt+1
  val data = new Array[Array[T]](numArrays)
  for (i <- 0 until data.length) {
    val size = if (i == data.length-1) (n % elemsPerArray).toInt else elemsPerArray
    data(i) = new Array[T](size)
  }

  def length: Long = n

  def apply(i: Long): T = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).apply(j)
  }

  def update(i: Long, y: T) = {
    val a = (i / elemsPerArray).toInt
    val j = (i % elemsPerArray).toInt
    data(a).update(j, y)
  }
}

final class RaggedNativeArrayLong(n: Long) extends RaggedNativeArrayObject[Long](n)
final class RaggedNativeArrayShort(n: Long) extends RaggedNativeArrayObject[Short](n)
final class RaggedNativeArrayByte(n: Long) extends RaggedNativeArrayObject[Byte](n)
final class RaggedNativeArrayChar(n: Long) extends RaggedNativeArrayObject[Char](n)


///////////////////////////////////////////////////////////////////////////////
// RaggedDeliteArray (Uses LocalDeliteArrays underneath (TODO)
///////////////////////////////////////////////////////////////////////////////

final class RaggedDeliteArray[T](n: Long) extends RaggedArray[T] with DeliteArray[T] {
  def length: Int = ???
  def readAt(i: Int): T = ???
  def data: Array[T] = ???

  var id: String = _
  var offsets: Array[Int] = _
  var offset: Int = _
}
