package ppl.dsl.optiql.baseline

/*
import collection.mutable.ArrayBuffer
import util.Sorting
import QueryUtils._



class QVector[A](n: Int)(implicit exposeAlloc: QueryUtils.ExposureFlag) extends Queryable[A] {

  private var alloc = n
  var size = if (exposeAlloc.flag) alloc else 0
  private var elems = new Array[A](alloc)
  private var firstElem:A = _
  private var lastElem:A = _

  def this() = this(16)

  def this(contents: Array[A]) = {
    this(contents.size)
    for (e <- contents) this += e
  }

  def +=(newElem: A) = {
    if (size >= alloc) allocate()
    if (size == 0) firstElem = newElem
    lastElem = newElem
    elems(size) = newElem
    size += 1
  }

  def apply(index: Int) = {
    if (index < 0 || index > size - 1) throw new IndexOutOfBoundsException("QVector index out of bounds")
    if (index == 0) firstElem
    else if (index == size - 1) lastElem
    else elems(index)
  }

  def clear = {
    alloc = 16
    size = 0
    elems = new Array[A](alloc)
  }

  def filter(p: A => Boolean): Queryable[A] = {
    val result = new QVector[A]
    var index = 0
    while (index < size) {
      val e = elems(index)
      if (p(e)) result += e
      index += 1
    }
    result
  }

  def flatMap[B](f: A => Queryable[B]): Queryable[B] = {
    val result = new QVector[B]
    var i = 0
    while (i < size) {
      val outer = elems(i)
      val coll = f(outer)
      for(e <- coll) result += e
      i += 1
    }
    result
  }

  def foreach(f: A => Unit): Unit = {
    var i = 0
    while (i < size) {
      f(elems(i))
      i += 1
    }
  }

  def head(): A = { if (size < 1) throw new NoSuchElementException("Cannot access head of empty QVector"); firstElem }

  def map[B](f: A => B): Queryable[B] = {
    val result = new QVector[B]
    var i = 0
    while (i < size) {
      result += f(elems(i))
      i += 1
    }
    result
  }

  def sort(start: Int, end: Int, pred: (A, A) => Boolean) = {
    val sub:Array[A]  = Sorting.stableSort(elems.slice(start, end), pred)
    var elemIndex = 0
    for (e <- sub) {
      elems(start + elemIndex) = e
      elemIndex += 1
    }
    if (start == 0) firstElem = elems(0)
    if (end == size) lastElem = elems(size - 1)
  }

  def tail(): A = { if (size < 1) throw new NoSuchElementException("Cannot access tail of empty QVector"); lastElem }

  def update(index: Int, e: A) = {
    if (index < 0 || index > size - 1 ) throw new IndexOutOfBoundsException("QVector index out of bounds")
    elems(index) = e
  }


  /* PRIVATE METHODS */

  private def allocate() {
    alloc *= 2
    val bigger = new Array[A](alloc)
    if (exposeAlloc.flag) size = alloc
    val it = elems.elements
    var index = 0
    while (it.hasNext) {
      bigger(index) = it.next
      index += 1
    }
    elems = bigger
  }
}

*/