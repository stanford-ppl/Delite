/*

package ppl.apps.interop

import Types._

/**
 * Iterable collection of graph items (nodes or edges)
 */

class NodeGIterable extends ArrayBuffer[Node] {
  def 
}

class NodeGIterable(var data: Array[Node], __offset: Int, var size: Int) {

  val _offset = __offset

  def this(){
    this(new Array[Node](0), 0, 0)
  }
  def this(data: Array[Node]){
    this(data, 0, data.length)
  }

  def apply(i: Int) = {
    //println("i = " + i + " _offset = " + _offset + " data = " + data(_offset+i) + " size = " + length)
    data(_offset+i)
  }
  def length = size//data.length

  def contains(n: Node): Boolean = {
    var i = 0
    var found = false
    while(i < size && !found) {
      if(data(i) == n) found = true
      i += 1
    }
    found
  }

  def filter(cond: Node => Boolean) = new GIterable(data.filter(cond))

  def foreach(block: Node => Unit) { data.foreach(block) }

  // required by DeliteCollection
  def dcSize = length
  def dcApply(i: Int) = apply(i)
  def dcUpdate(i: Int, n: Node) = { data(_offset+i) = n }
  def dcInsert(pos: Int, x: Node) {
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
    val d = new Array[Node](n)
    //println("realloc trying to use length: " + length)
    System.arraycopy(data, 0, d, 0, length)
    data = d
  }
}

*/
