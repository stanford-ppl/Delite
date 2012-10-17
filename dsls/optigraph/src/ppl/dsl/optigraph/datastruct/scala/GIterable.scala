package ppl.dsl.optigraph.datastruct.scala

/** 
 * Iterable collection of graph items (nodes or edges)
 */

class GIterable[@specialized T: ClassManifest](var data: Array[T], offset: Int, var size: Int) {
  
  def this(){
    this(new Array[T](0), 0, 0)
  }
  def this(data: Array[T]){
    this(data, 0, data.length)
  }
  
  def apply(i: Int) = {
    //println("i = " + i + " offset = " + offset + " data = " + data(offset+i) + " size = " + length)
    data(offset+i)
  }
  def length = size//data.length
  def toList:List[T] = data.toList
  def toSet:GSet[T] = {
    val ns: GSet[T] = new GSet[T]
    var i = 0
    while (i < length) {
      ns.add(data(offset+i))
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
  def dcUpdate(i: Int, n: T) = { data(offset+i) = n }
  def dcInsert(pos: Int, x: T) {
    insertSpace(pos,1)
    dcUpdate(pos,x)
  }

  protected def insertSpace(pos: Int, len: Int) {
    ensureExtra(len)
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
    System.arraycopy(data, 0, d, 0, length)
    data = d
  }  
}