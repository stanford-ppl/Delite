package ppl.delite.runtime.data

import ppl.delite.runtime.messages._
import ppl.delite.runtime.DeliteMesosScheduler

trait DeliteArray[T] {
  def length: Int
  def apply(i: Int): T
  def update(i: Int, x: T): Unit

  def hasArray: Boolean
  def copy(srcPos: Int, dest: DeliteArray[T], destPos: Int, len: Int): Unit
  def data: Array[T]
  var offsets: Array[Int]
}

abstract class RemoteDeliteArray[T:Manifest] extends DeliteArray[T] {
  type L <: LocalDeliteArray[T]

  val id: String
  var chunkLengths: Array[Int]
  var offsets = {
    val off = new Array[Int](chunkLengths.length)
    off(0) = 0
    var sum = 0 
    for (i <- 1 until chunkLengths.length) {
      sum += chunkLengths(i-1)
      off(i) = sum
    }
    off
  }

  def apply(i: Int) = getLocal.apply(i)
  def update(i: Int, x: T) = getLocal.update(i,x)

  def data = getLocal.data

  private var local: L = _

  def hasArray = local ne null
  val length = chunkLengths.reduce(_ + _)

  def copy(srcPos: Int, dest: DeliteArray[T], destPos: Int, len: Int) = dest match {
    case d:RemoteDeliteArray[T] => System.arraycopy(this.getLocal.data, srcPos, d.getLocal.data, destPos, len) 
    case d:LocalDeliteArray[T] => System.arraycopy(this.getLocal.data, srcPos, d.data, destPos, len)
  }

  def take(n: Int) = {
    val res = createLocal(n)
    copy(0, res, 0, n)
    res
  }

  def getLocal = if (local eq null) retrieveArray else local

  private def retrieveArray = {
    if (!hasArray) {
      println("WARNING: transferring remote symbol " + id.split("_")(0))
      val returnResults = DeliteMesosScheduler.getData(this)
      val chunks = for (result <- returnResults) yield {
        Serialization.deserialize(specializedClass, result.getOutput(0)).asInstanceOf[LocalDeliteArray[T]]
      }
      val length = chunks.map(_.length).sum
      val result = createLocal(length)
      var offset = 0
      for (chunk <- chunks) {
        System.arraycopy(chunk.data, 0, result.data, offset, chunk.length)
        offset += chunk.length
      }
      local = result
    }
    local
  }

  def specializedClass: Class[_]
  def createLocal(len: Int): L

}

abstract class LocalDeliteArray[T:Manifest] extends DeliteArray[T] {
  type L <: LocalDeliteArray[T]

  val data: Array[T]
  var offset: Int
  var offsets: Array[Int] = _
  var id: String = _

  def hasArray = true
  val length = data.length

  def apply(i: Int): T = try { data(i-offset) } catch { case a: ArrayIndexOutOfBoundsException => 
    remoteRead(i)
  }

  def update(i: Int, x: T) = try { data(i-offset) = x } catch { case a: ArrayIndexOutOfBoundsException => 
    throw new UnsupportedOperationException("DeliteArray update: index " + (i-offset) + " is not local")
  }

  def remoteRead(i: Int): T = {
    var location = offsets.indexWhere(_ >= i) - 1
    if (location < 0) location = offsets.length - 1
    val result = ppl.delite.runtime.DeliteMesosExecutor.requestData(id, location, i)
    Serialization.deserialize(manifest[T].erasure.asInstanceOf[Class[T]], result.getOutput(0))
  }

  def copy(srcPos: Int, dest: DeliteArray[T], destPos: Int, len: Int) = dest match {
    case d: LocalDeliteArray[T] => System.arraycopy(this.data, srcPos, d.data, destPos, len)
    case d: RemoteDeliteArray[T] => System.arraycopy(this.data, srcPos, d.getLocal.data, destPos, len)
  }

  def take(n: Int) = {
    val res = createLocal(n, offset)
    copy(0, res, 0, n)
    res
  }

  def createLocal(len: Int, offset: Int): L

}

trait DeliteArrayInt extends DeliteArray[Int] {
  def apply(i: Int): Int
  def take(n: Int): DeliteArrayInt
}
trait DeliteArrayLong extends DeliteArray[Long] {
  def apply(i: Int): Long
  def take(n: Int): DeliteArrayLong
}
trait DeliteArrayFloat extends DeliteArray[Float] {
  def apply(i: Int): Float
  def take(n: Int): DeliteArrayFloat
}
trait DeliteArrayDouble extends DeliteArray[Double] {
  def apply(i: Int): Double
  def take(n: Int): DeliteArrayDouble
}
trait DeliteArrayChar extends DeliteArray[Char] {
  def apply(i: Int): Char
  def take(n: Int): DeliteArrayChar
}
trait DeliteArrayShort extends DeliteArray[Short] {
  def apply(i: Int): Short
  def take(n: Int): DeliteArrayShort
}
trait DeliteArrayByte extends DeliteArray[Byte] {
  def apply(i: Int): Byte
  def take(n: Int): DeliteArrayByte
}
trait DeliteArrayBoolean extends DeliteArray[Boolean] {
  def apply(i: Int): Boolean
  def take(n: Int): DeliteArrayBoolean
}
trait DeliteArrayObject[T] extends DeliteArray[T] {
  def apply(i: Int): T
  def take(n: Int): DeliteArrayObject[T]
}

object DeliteArrayDouble {
  def combine(lhs: DeliteArrayDouble, rhs: DeliteArrayDouble) = (lhs, rhs) match {
    case (l: RemoteDeliteArrayDouble, r: RemoteDeliteArrayDouble) => new RemoteDeliteArrayDouble(l.id, l.chunkLengths ++ r.chunkLengths)
  }
}

object DeliteArrayInt {
  def combine(lhs: DeliteArrayInt, rhs: DeliteArrayInt) = (lhs, rhs) match {
    case (l: RemoteDeliteArrayInt, r: RemoteDeliteArrayInt) => new RemoteDeliteArrayInt(l.id, l.chunkLengths ++ r.chunkLengths)
  }
}

object DeliteArrayChar {
  def combine(lhs: DeliteArrayChar, rhs: DeliteArrayChar) = (lhs, rhs) match {
    case (l: RemoteDeliteArrayChar, r: RemoteDeliteArrayChar) => new RemoteDeliteArrayChar(l.id, l.chunkLengths ++ r.chunkLengths)
  }
}

object DeliteArrayObject {
  def combine[T:Manifest](lhs: DeliteArrayObject[_], rhs: DeliteArrayObject[_]) = (lhs, rhs) match {
    case (l: RemoteDeliteArrayObject[_], r: RemoteDeliteArrayObject[_]) => new RemoteDeliteArrayObject[T](l.id, l.chunkLengths ++ r.chunkLengths)
  }
}

class RemoteDeliteArrayInt(val id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArray[Int] with DeliteArrayInt {
  type L = LocalDeliteArrayInt
  def specializedClass = classOf[DeliteArrayInt]
  def createLocal(len: Int) = new LocalDeliteArrayInt(len)
}
class RemoteDeliteArrayLong(val id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArray[Long] with DeliteArrayLong {
  type L = LocalDeliteArrayLong
  def specializedClass = classOf[DeliteArrayLong]
  def createLocal(len: Int) = new LocalDeliteArrayLong(len)
}
class RemoteDeliteArrayFloat(val id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArray[Float] with DeliteArrayFloat {
  type L = LocalDeliteArrayFloat
  def specializedClass = classOf[DeliteArrayFloat]
  def createLocal(len: Int) = new LocalDeliteArrayFloat(len)
}
class RemoteDeliteArrayDouble(val id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArray[Double] with DeliteArrayDouble {
  type L = LocalDeliteArrayDouble
  def specializedClass = classOf[DeliteArrayDouble]
  def createLocal(len: Int) = new LocalDeliteArrayDouble(len)
}
class RemoteDeliteArrayChar(val id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArray[Char] with DeliteArrayChar {
  type L = LocalDeliteArrayChar
  def specializedClass = classOf[DeliteArrayChar]
  def createLocal(len: Int) = new LocalDeliteArrayChar(len)
}
class RemoteDeliteArrayShort(val id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArray[Short] with DeliteArrayShort {
  type L = LocalDeliteArrayShort
  def specializedClass = classOf[DeliteArrayShort]
  def createLocal(len: Int) = new LocalDeliteArrayShort(len)
}
class RemoteDeliteArrayByte(val id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArray[Byte] with DeliteArrayByte {
  type L = LocalDeliteArrayByte
  def specializedClass = classOf[DeliteArrayByte]
  def createLocal(len: Int) = new LocalDeliteArrayByte(len)
}
class RemoteDeliteArrayBoolean(val id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArray[Boolean] with DeliteArrayBoolean {
  type L = LocalDeliteArrayBoolean
  def specializedClass = classOf[DeliteArrayBoolean]
  def createLocal(len: Int) = new LocalDeliteArrayBoolean(len)
}
class RemoteDeliteArrayObject[T:Manifest](val id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArray[T] with DeliteArrayObject[T] {
  type L = LocalDeliteArrayObject[T]
  def specializedClass = classOf[DeliteArrayObject[T]]
  def createLocal(len: Int) = new LocalDeliteArrayObject[T](len)
}

class LocalDeliteArrayInt(val data: Array[Int], var offset: Int) extends LocalDeliteArray[Int] with DeliteArrayInt {
  type L = LocalDeliteArrayInt
  def this(data: Array[Int]) = this(data, 0)
  def this(len: Int) = this(new Array[Int](len), 0)
  def this(len: Int, start: Int) = this(new Array[Int](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayInt(len, start)
}
class LocalDeliteArrayLong(val data: Array[Long], var offset: Int) extends LocalDeliteArray[Long] with DeliteArrayLong {  
  type L = LocalDeliteArrayLong
  def this(data: Array[Long]) = this(data, 0)
  def this(len: Int) = this(new Array[Long](len), 0)
  def this(len: Int, start: Int) = this(new Array[Long](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayLong(len, start)
}
class LocalDeliteArrayFloat(val data: Array[Float], var offset: Int) extends LocalDeliteArray[Float] with DeliteArrayFloat {
  type L = LocalDeliteArrayFloat
  def this(data: Array[Float]) = this(data, 0)
  def this(len: Int) = this(new Array[Float](len), 0)
  def this(len: Int, start: Int) = this(new Array[Float](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayFloat(len, start)
}
class LocalDeliteArrayDouble(val data: Array[Double], var offset: Int) extends LocalDeliteArray[Double] with DeliteArrayDouble {
  type L = LocalDeliteArrayDouble
  def this(data: Array[Double]) = this(data, 0)
  def this(len: Int) = this(new Array[Double](len), 0)
  def this(len: Int, start: Int) = this(new Array[Double](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayDouble(len, start)
}
class LocalDeliteArrayChar(val data: Array[Char], var offset: Int) extends LocalDeliteArray[Char] with DeliteArrayChar {
  type L = LocalDeliteArrayChar
  def this(data: Array[Char]) = this(data, 0)
  def this(len: Int) = this(new Array[Char](len), 0)
  def this(len: Int, start: Int) = this(new Array[Char](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayChar(len, start)
}
class LocalDeliteArrayShort(val data: Array[Short], var offset: Int) extends LocalDeliteArray[Short] with DeliteArrayShort {
  type L = LocalDeliteArrayShort
  def this(data: Array[Short]) = this(data, 0)
  def this(len: Int) = this(new Array[Short](len), 0)
  def this(len: Int, start: Int) = this(new Array[Short](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayShort(len, start)
} 
class LocalDeliteArrayByte(val data: Array[Byte], var offset: Int) extends LocalDeliteArray[Byte] with DeliteArrayByte {
  type L = LocalDeliteArrayByte
  def this(data: Array[Byte]) = this(data, 0)
  def this(len: Int) = this(new Array[Byte](len), 0)
  def this(len: Int, start: Int) = this(new Array[Byte](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayByte(len, start)
}
class LocalDeliteArrayBoolean(val data: Array[Boolean], var offset: Int) extends LocalDeliteArray[Boolean] with DeliteArrayBoolean {
  type L = LocalDeliteArrayBoolean
  def this(data: Array[Boolean]) = this(data, 0)
  def this(len: Int) = this(new Array[Boolean](len), 0)
  def this(len: Int, start: Int) = this(new Array[Boolean](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayBoolean(len, start)
}
class LocalDeliteArrayObject[T:Manifest](val data: Array[T], var offset: Int) extends LocalDeliteArray[T] with DeliteArrayObject[T] {
  type L = LocalDeliteArrayObject[T]
  def this(data: Array[T]) = this(data, 0)
  def this(len: Int) = this(new Array[T](len), 0)
  def this(len: Int, start: Int) = this(new Array[T](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayObject[T](len, start)
}
