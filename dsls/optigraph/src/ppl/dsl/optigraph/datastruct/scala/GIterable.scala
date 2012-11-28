package ppl.dsl.optigraph.datastruct.scala

/**
 * Iterable collection of graph items (nodes or edges)
 */

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
  def toList:List[T] = data.toList
  def toSet:GSet[T] = {
    val ns: GSet[T] = new GSet[T]
    var i = 0
    while (i < length) {
      ns.add(data(_offset+i))
      i += 1
    }
    ns
  }

  def unsafeSetData(xs: Array[T], len: Int) {
    data = new Array[T](len)
    var i = 0
    // TODO: use Array copy
    while (i < len) {
      data(i) = xs(i)
      i += 1
    }
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