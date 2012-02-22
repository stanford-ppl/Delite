package ppl.dsl.optigraph.datastruct.scala

/** 
 * Iterable collection of graph items (nodes or edges)
 */

class GIterable[@specialized T: ClassManifest](var data: Array[T]) {
  
  def this(){
    this(new Array[T](0))
  }
  
  def apply(i: Int) = data(i)
  def length = data.length
  def toList:List[T] = data.toList
  def toSet:GSet[T] = {
    val ns: GSet[T] = new GSet[T]
    var i = 0
    while (i < length) {
      ns.add(data(i))
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
  def dcSize = data.length
  def dcApply(i: Int) = data(i)
  def dcUpdate(i: Int, n: T) = { data(i) = n }
}