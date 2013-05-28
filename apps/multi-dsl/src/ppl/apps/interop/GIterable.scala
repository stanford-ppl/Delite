package ppl.apps.interop

import scala.collection.mutable.ArrayBuffer
import Types._

/**
 * Iterable collection of graph items (nodes or edges)
 */


class GIterable[T] extends ArrayBuffer[T]

/*
class GIterable[@specialized T: ClassManifest](var data: Array[T], __offset: Int, var size: Int) {

  val _offset = __offset

  def this(){
    this(new Array[T](0), 0, 0)
  }
  def this(data: Array[T]){
    this(data, 0, data.length)
  }

  def apply(i: Int) = {
    //println("i = " + i + " _offset = " + _offset + " data = " + data(_offset+i) + " size = " + length)
    data(_offset+i)
  }
  def length = size//data.length

  def contains(n: T): Boolean = {
    var i = 0
    var found = false
    while(i < size && !found) {
      if(data(i) == n) found = true
      i += 1
    }
    found
  }

  def filter(cond: T => Boolean) = {
    val result = ArrayBuffer[T]()
    var i = 0
    while(i < size) {
      if(cond(data(i))) result += data(i)
      i += 1
    }
    new GIterable(result.toArray)
  }

  
  def foreach(block: T => Unit) { 
    var i = 0
    while(i < size) {
      block(data(i))
      i += 1
    }
  }
  //def foreach(block: T => Unit) { data.foreach(block) }

  def insert(x: T) {
    dcInsert(size, x)
  }

  // required by DeliteCollection
  def dcSize = length
  def dcApply(i: Int) = apply(i)
  def dcUpdate(i: Int, n: T) = { data(_offset+i) = n }
  def dcInsert(pos: Int, x: T) {
    insertSpace(pos,1)
    dcUpdate(pos,x)
  }

  protected def insertSpace(pos: Int, len: Int) {
    ensureExtra(len)
    //println("insertSpace trying to use pos,len,length: " + pos + "," + len + "," + length)
    System.arraycopy(data, pos, data, pos+len, length-pos)
    size += len
  }

  protected def ensureExtra(extra: Int) = {
    if (data.length - length < extra) {
      realloc(length + extra)
    }
  }

  protected def realloc(minLen: Int) {
    var n = Math.max(4, data.length * 2)
    while (n < minLen) n = n*2
    val d = new Array[T](n)
    //println("realloc trying to use length: " + length)
    System.arraycopy(data, 0, d, 0, length)
    data = d
  }
}
*/
