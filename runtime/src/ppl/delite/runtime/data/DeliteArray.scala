package ppl.delite.runtime.data

import ppl.delite.runtime.messages._
import ppl.delite.runtime.DeliteMesosScheduler

trait DeliteArray[T] {
  def length: Int
  def readAt(i: Int): T

  var id: String
  var offsets: Array[Int]
  var offset: Int
}

trait RemoteDeliteArray[T] extends DeliteArray[T] {
  def chunkLengths: Array[Int]
}

abstract class RemoteDeliteArrayImpl[T:Manifest] extends DeliteArray[T] with RemoteDeliteArray[T] {
  type L <: LocalDeliteArray[T]

  var chunkLengths: Array[Int]
  var offset = 0
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
  def readAt(i: Int) = apply(i)

  def data = getLocal.data

  private var local: L = _

  def hasArray = local ne null
  val length = chunkLengths.reduce(_ + _)

  def copy(srcPos: Int, dest: DeliteArray[T], destPos: Int, len: Int) = dest match {
    case d:RemoteDeliteArrayImpl[T] => System.arraycopy(this.getLocal.data, srcPos, d.getLocal.data, destPos, len) 
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

final class RemoteDeliteArrayDouble(var id: String, var chunkLengths: Array[Int]) extends DeliteArrayDouble with RemoteDeliteArray[Double] {

  var offset = 0
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
  def update(i: Int, x: Double) = getLocal.update(i,x)
  def readAt(i: Int) = apply(i)
  def data = getLocal.data

  private var local: LocalDeliteArrayDouble = _

  def hasArray = local ne null
  val length = chunkLengths.reduce(_ + _)

  def copy(srcPos: Int, dest: DeliteArrayDouble, destPos: Int, len: Int) = dest match {
    case d:RemoteDeliteArrayDouble => System.arraycopy(this.getLocal.data, srcPos, d.getLocal.data, destPos, len) 
    case d:LocalDeliteArrayDouble => System.arraycopy(this.getLocal.data, srcPos, d.data, destPos, len)
  }

  def take(n: Int) = {
    val res = new LocalDeliteArrayDouble(n)
    copy(0, res, 0, n)
    res
  }

  def getLocal = if (local eq null) retrieveArray else local

  private def retrieveArray = {
    if (!hasArray) {
      println("WARNING: transferring remote symbol " + id.split("_")(0))
      val returnResults = DeliteMesosScheduler.getData(this.asInstanceOf[RemoteDeliteArray[_]])
      val chunks = for (result <- returnResults) yield {
        Serialization.deserialize(classOf[DeliteArrayDouble], result.getOutput(0)).asInstanceOf[LocalDeliteArrayDouble]
      }
      val length = chunks.map(_.length).sum
      val result = new LocalDeliteArrayDouble(length)
      var offset = 0
      for (chunk <- chunks) {
        System.arraycopy(chunk.data, 0, result.data, offset, chunk.length)
        offset += chunk.length
      }
      local = result
    }
    local
  }
}

final class RemoteDeliteArrayInt(var id: String, var chunkLengths: Array[Int]) extends DeliteArrayInt with RemoteDeliteArray[Int] {

  var offset = 0
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
  def update(i: Int, x: Int) = getLocal.update(i,x)
  def readAt(i: Int) = apply(i)

  def data = getLocal.data

  private var local: LocalDeliteArrayInt = _

  def hasArray = local ne null
  val length = chunkLengths.reduce(_ + _)

  def copy(srcPos: Int, dest: DeliteArrayInt, destPos: Int, len: Int) = dest match {
    case d:RemoteDeliteArrayInt => System.arraycopy(this.getLocal.data, srcPos, d.getLocal.data, destPos, len) 
    case d:LocalDeliteArrayInt => System.arraycopy(this.getLocal.data, srcPos, d.data, destPos, len)
  }

  def take(n: Int) = {
    val res = new LocalDeliteArrayInt(n)
    copy(0, res, 0, n)
    res
  }

  def getLocal = if (local eq null) retrieveArray else local

  private def retrieveArray = {
    if (!hasArray) {
      println("WARNING: transferring remote symbol " + id.split("_")(0))
      val returnResults = DeliteMesosScheduler.getData(this.asInstanceOf[RemoteDeliteArray[_]])
      val chunks = for (result <- returnResults) yield {
        Serialization.deserialize(classOf[DeliteArrayInt], result.getOutput(0)).asInstanceOf[LocalDeliteArrayInt]
      }
      val length = chunks.map(_.length).sum
      val result = new LocalDeliteArrayInt(length)
      var offset = 0
      for (chunk <- chunks) {
        System.arraycopy(chunk.data, 0, result.data, offset, chunk.length)
        offset += chunk.length
      }
      local = result
    }
    local
  }
}

final class RemoteDeliteArrayChar(var id: String, var chunkLengths: Array[Int]) extends DeliteArrayChar with RemoteDeliteArray[Char] {

  var offset = 0
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
  def update(i: Int, x: Char) = getLocal.update(i,x)
  def readAt(i: Int) = apply(i)

  def data = getLocal.data

  private var local: LocalDeliteArrayChar = _

  def hasArray = local ne null
  val length = chunkLengths.reduce(_ + _)

  def copy(srcPos: Int, dest: DeliteArrayChar, destPos: Int, len: Int) = {
    System.arraycopy(this.data, srcPos, dest.data, destPos, len) 
  }

  def take(n: Int) = {
    val res = new LocalDeliteArrayChar(n)
    copy(0, res, 0, n)
    res
  }

  def getLocal = if (local eq null) retrieveArray else local

  private def retrieveArray = {
    if (!hasArray) {
      println("WARNING: transferring remote symbol " + id.split("_")(0))
      val returnResults = DeliteMesosScheduler.getData(this.asInstanceOf[RemoteDeliteArray[_]])
      val chunks = for (result <- returnResults) yield {
        Serialization.deserialize(classOf[DeliteArrayInt], result.getOutput(0)).asInstanceOf[LocalDeliteArrayChar]
      }
      val length = chunks.map(_.length).sum
      val result = new LocalDeliteArrayChar(length)
      var offset = 0
      for (chunk <- chunks) {
        System.arraycopy(chunk.data, 0, result.data, offset, chunk.length)
        offset += chunk.length
      }
      local = result
    }
    local
  }
}

final class RemoteDeliteArrayBoolean(var id: String, var chunkLengths: Array[Int]) extends DeliteArrayBoolean with RemoteDeliteArray[Boolean] {

  var offset = 0
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
  def update(i: Int, x: Boolean) = getLocal.update(i,x)
  def readAt(i: Int) = apply(i)

  def data = getLocal.data

  private var local: LocalDeliteArrayBoolean = _

  def hasArray = local ne null
  val length = chunkLengths.reduce(_ + _)

  def copy(srcPos: Int, dest: DeliteArrayBoolean, destPos: Int, len: Int) = {
    System.arraycopy(this.data, srcPos, dest.data, destPos, len) 
  }

  def take(n: Int) = {
    val res = new LocalDeliteArrayBoolean(n)
    copy(0, res, 0, n)
    res
  }

  def getLocal = if (local eq null) retrieveArray else local

  private def retrieveArray = {
    if (!hasArray) {
      println("WARNING: transferring remote symbol " + id.split("_")(0))
      val returnResults = DeliteMesosScheduler.getData(this.asInstanceOf[RemoteDeliteArray[_]])
      val chunks = for (result <- returnResults) yield {
        Serialization.deserialize(classOf[DeliteArrayInt], result.getOutput(0)).asInstanceOf[LocalDeliteArrayBoolean]
      }
      val length = chunks.map(_.length).sum
      val result = new LocalDeliteArrayBoolean(length)
      var offset = 0
      for (chunk <- chunks) {
        System.arraycopy(chunk.data, 0, result.data, offset, chunk.length)
        offset += chunk.length
      }
      local = result
    }
    local
  }
}

abstract class LocalDeliteArray[T:Manifest] extends DeliteArray[T] {
  type L <: LocalDeliteArray[T]

  val data: Array[T]
  var offset: Int
  var offsets: Array[Int] = _
  var id: String = _

  val length = data.length

  def apply(i: Int): T = data(i-offset) /* try { data(i-offset) } catch { case a: ArrayIndexOutOfBoundsException => 
    remoteRead(i)
  }*/
  def readAt(i: Int) = data(i-offset)

  def update(i: Int, x: T) = data(i-offset) = x /*try { data(i-offset) = x } catch { case a: ArrayIndexOutOfBoundsException => 
    throw new UnsupportedOperationException("DeliteArray update: index " + (i-offset) + " is not local")
  }*/

  private def remoteRead(i: Int): T = {
    var location = offsets.indexWhere(_ >= i) - 1
    if (location < 0) location = offsets.length - 1
    val result = ppl.delite.runtime.DeliteMesosExecutor.requestData(id, location, i)
    Serialization.deserialize(manifest[T].erasure.asInstanceOf[Class[T]], result.getOutput(0))
  }

  def copy(srcPos: Int, dest: DeliteArray[T], destPos: Int, len: Int) = dest match {
    case d: LocalDeliteArray[T] => System.arraycopy(this.data, srcPos, d.data, destPos, len)
    case d: RemoteDeliteArrayImpl[T] => System.arraycopy(this.data, srcPos, d.data, destPos, len)
  }

  def take(n: Int) = {
    val res = createLocal(n, offset)
    copy(0, res, 0, n)
    res
  }

  def createLocal(len: Int, offset: Int): L

}


trait DeliteArrayLong extends DeliteArray[Long] {
  def apply(i: Int): Long
  def take(n: Int): DeliteArrayLong
}
trait DeliteArrayFloat extends DeliteArray[Float] {
  def apply(i: Int): Float
  def take(n: Int): DeliteArrayFloat
}
trait DeliteArrayShort extends DeliteArray[Short] {
  def apply(i: Int): Short
  def take(n: Int): DeliteArrayShort
}
trait DeliteArrayByte extends DeliteArray[Byte] {
  def apply(i: Int): Byte
  def take(n: Int): DeliteArrayByte
}
trait DeliteArrayObject[T] extends DeliteArray[T] {
  def data: Array[T]
  def apply(i: Int): T
  def update(i: Int, x: T)
  def take(i: Int): DeliteArrayObject[T]
  def copy(srcPos: Int, dest: DeliteArray[T], destPos: Int, len: Int)
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

class RemoteDeliteArrayLong(var id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArrayImpl[Long] with DeliteArrayLong {
  type L = LocalDeliteArrayLong
  def specializedClass = classOf[DeliteArrayLong]
  def createLocal(len: Int) = new LocalDeliteArrayLong(len)
}
class RemoteDeliteArrayFloat(var id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArrayImpl[Float] with DeliteArrayFloat {
  type L = LocalDeliteArrayFloat
  def specializedClass = classOf[DeliteArrayFloat]
  def createLocal(len: Int) = new LocalDeliteArrayFloat(len)
}
class RemoteDeliteArrayShort(var id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArrayImpl[Short] with DeliteArrayShort {
  type L = LocalDeliteArrayShort
  def specializedClass = classOf[DeliteArrayShort]
  def createLocal(len: Int) = new LocalDeliteArrayShort(len)
}
class RemoteDeliteArrayByte(var id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArrayImpl[Byte] with DeliteArrayByte {
  type L = LocalDeliteArrayByte
  def specializedClass = classOf[DeliteArrayByte]
  def createLocal(len: Int) = new LocalDeliteArrayByte(len)
}
class RemoteDeliteArrayObject[T:Manifest](var id: String, var chunkLengths: Array[Int]) extends RemoteDeliteArrayImpl[T] with DeliteArrayObject[T] {
  type L = LocalDeliteArrayObject[T]
  def specializedClass = classOf[DeliteArrayObject[T]]
  def createLocal(len: Int) = new LocalDeliteArrayObject[T](len)
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

final class LocalDeliteArrayDouble(val data: Array[Double], var offset: Int) extends DeliteArrayDouble {
  def this(data: Array[Double]) = this(data, 0)
  def this(len: Int) = this(new Array[Double](len), 0)
  def this(len: Int, start: Int) = this(new Array[Double](len), start)

  var id: String = _
  var offsets: Array[Int] = _

  val length = data.length
  def apply(i: Int): Double = data(i-offset)
  def readAt(i: Int) = data(i-offset)
  def update(i: Int, x: Double) = data(i-offset) = x
  
  def copy(srcPos: Int, dest: DeliteArrayDouble, destPos: Int, len: Int) = {
    System.arraycopy(this.data, srcPos, dest.data, destPos, len)
  }

  def take(n: Int) = {
    val res = new LocalDeliteArrayDouble(n, offset)
    copy(0, res, 0, n)
    res
  }

}
abstract class DeliteArrayDouble extends DeliteArray[Double] {
  def data: Array[Double]
  def apply(i: Int): Double
  def update(i: Int, x: Double)
  def take(i: Int): DeliteArrayDouble
  def copy(srcPos: Int, dest: DeliteArrayDouble, destPos: Int, len: Int)
}

final class LocalDeliteArrayInt(val data: Array[Int], var offset: Int) extends DeliteArrayInt {
  def this(data: Array[Int]) = this(data, 0)
  def this(len: Int) = this(new Array[Int](len), 0)
  def this(len: Int, start: Int) = this(new Array[Int](len), start)

  var id: String = _
  var offsets: Array[Int] = _

  val length = data.length
  def apply(i: Int): Int = data(i-offset)
  def readAt(i: Int) = data(i-offset)
  def update(i: Int, x: Int) = data(i-offset) = x

  def copy(srcPos: Int, dest: DeliteArrayInt, destPos: Int, len: Int) = {
    System.arraycopy(this.data, srcPos, dest.data, destPos, len)
  }

  def take(n: Int) = {
    val res = new LocalDeliteArrayInt(n, offset)
    copy(0, res, 0, n)
    res
  }

}
abstract class DeliteArrayInt extends DeliteArray[Int] {
  def data: Array[Int]
  def apply(i: Int): Int
  def update(i: Int, x: Int)
  def take(i: Int): DeliteArrayInt
  def copy(srcPos: Int, dest: DeliteArrayInt, destPos: Int, len: Int)
}

final class LocalDeliteArrayChar(val data: Array[Char], var offset: Int) extends DeliteArrayChar {
  def this(data: Array[Char]) = this(data, 0)
  def this(len: Int) = this(new Array[Char](len), 0)
  def this(len: Int, start: Int) = this(new Array[Char](len), start)

  var id: String = _
  var offsets: Array[Int] = _

  val length = data.length
  def apply(i: Int): Char = data(i-offset)
  def readAt(i: Int) = data(i-offset)
  def update(i: Int, x: Char) = data(i-offset) = x

  def copy(srcPos: Int, dest: DeliteArrayChar, destPos: Int, len: Int) = {
    System.arraycopy(this.data, srcPos, dest.data, destPos, len)
  }

  def take(n: Int) = {
    val res = new LocalDeliteArrayChar(n, offset)
    copy(0, res, 0, n)
    res
  }

}
abstract class DeliteArrayChar extends DeliteArray[Char] {
  def data: Array[Char]
  def apply(i: Int): Char
  def update(i: Int, x: Char)
  def take(i: Int): DeliteArrayChar
  def copy(srcPos: Int, dest: DeliteArrayChar, destPos: Int, len: Int)
}

final class LocalDeliteArrayBoolean(val data: Array[Boolean], var offset: Int) extends DeliteArrayBoolean {
  def this(data: Array[Boolean]) = this(data, 0)
  def this(len: Int) = this(new Array[Boolean](len), 0)
  def this(len: Int, start: Int) = this(new Array[Boolean](len), start)

  var id: String = _
  var offsets: Array[Int] = _

  val length = data.length
  def apply(i: Int): Boolean = data(i-offset)
  def readAt(i: Int) = data(i-offset)
  def update(i: Int, x: Boolean) = data(i-offset) = x

  def copy(srcPos: Int, dest: DeliteArrayBoolean, destPos: Int, len: Int) = {
    System.arraycopy(this.data, srcPos, dest.data, destPos, len)
  }

  def take(n: Int) = {
    val res = new LocalDeliteArrayBoolean(n, offset)
    copy(0, res, 0, n)
    res
  }

}
abstract class DeliteArrayBoolean extends DeliteArray[Boolean] {
  def data: Array[Boolean]
  def apply(i: Int): Boolean
  def update(i: Int, x: Boolean)
  def take(i: Int): DeliteArrayBoolean
  def copy(srcPos: Int, dest: DeliteArrayBoolean, destPos: Int, len: Int)
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
class LocalDeliteArrayObject[T:Manifest](val data: Array[T], var offset: Int) extends LocalDeliteArray[T] with DeliteArrayObject[T] {
  type L = LocalDeliteArrayObject[T]
  def this(data: Array[T]) = this(data, 0)
  def this(len: Int) = this(new Array[T](len), 0)
  def this(len: Int, start: Int) = this(new Array[T](len), start)
  def createLocal(len: Int, start: Int) = new LocalDeliteArrayObject[T](len, start)
  //TODO: Get rid of dc_update. Used for GPU data transfer and avoid manifest.
  def dc_update(idx: Int, newVal: T) { data(idx) = newVal }
}
