package ppl.dsl.optiql.baseline


/*
import collection.mutable
import collection.mutable.{ArrayBuffer, HashMap}
import QueryUtils._


//TODO: Revise type safety of sum and average
//TODO: Better solution for elementAt (is there some way to check for an apply method?)
//TODO: Return value of groupBy is a pain to work with
//TODO: Fix sequenceEqual
//TODO: Review design decision of implicit parameter in QVector ctor



trait Queryable[A] {

  /*Stores intervals across which elements are "equal" after a call to an ordering operator.  Each interval
    represents the range of values to re-sort on the next call to thenBy() or thenByDesc() as [lower, upper)
    (upper bound exceeds the last element to re-sort by one)*/
  var eqIntervals: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]

  def filter(p: A => Boolean): Queryable[A]
  def flatMap[B](f: A => Queryable[B]): Queryable[B]
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): Queryable[B]
  def head(): A
  def tail(): A
  def size(): Int

  def select[B](proj: (A) => B): Queryable[B] = map(proj)

  def selectMany[B](proj: (A) => Queryable[B]): Queryable[B] = flatMap(proj)

  def selectMany[B, C](projSel: (A) => Queryable[B], projRes: (B) => C): Queryable[C] = flatMap(projSel).map(projRes)

  def take(count: Int): Queryable[A] = {
    if (count >= size) this

    val result = new QVector[A]
    if (count <= 0) result

    var remaining = count
    for (e <- this if remaining > 0) {
      remaining -= 1
      result += e
      if (remaining == 0) return result
    }
    result
  }

  def skip(count: Int): Queryable[A] = {
    if (count <= 0) this

    val result = new QVector[A]
    if (count >= size) result

    var remaining = count
    for (e <- this) {
      if (remaining > 0) remaining -= 1
      else result += e
    }
    result
  }

  def takeWhile(pred: (A) => Boolean): Queryable[A] = {
    val result = new QVector[A]
    for (e <- this) {
      if (pred(e)) result += e
      else return result
    }
    result
  }

  def skipWhile(pred: (A) => Boolean): Queryable[A] = {
    val result = new QVector[A]
    var done = false
    for (e <- this) {
      if (!pred(e)) done = true
      if (done) result += e
    }
    result
  }

  def join[B, C, D](inner: Queryable[B], projOuter: (A) => C, projInner: (B) => C, projRes: (A, B) => D): Queryable[D] /*Tuple2[A, B]*/ = {
    val innerKeys = buildHash(inner, projInner)
    for (elem <- this; key = projOuter(elem); if innerKeys.contains(key); keyMatch <- innerKeys(key)) yield projRes(elem, keyMatch) //(elem, keyMatch)
  }

  def groupJoin[B, C, D](inner: Queryable[B], projOuter: (A) => C, projInner: (B) => C, projRes: (A, Queryable[B]) => D): Queryable[D] = {
    val innerKeys = buildHash(inner, projInner)
    for (elem <- this; key = projOuter(elem); if innerKeys.contains(key)) yield projRes(elem, innerKeys(key))
  }

  def concat(other: Queryable[A]): Queryable[A] = {
    val result = new QVector[A]
    for (e <- this) result += e
    for (e <- other) result += e
    result
  }

  def orderby[B](proj: (A) => B)(implicit cmp: QueryUtils.ComparisonFunction[B]): Queryable[A] = {
    val count = size
    if (size < 2) this
    else {
      val sorted = new QVector[A](count)
      val realPred = (one: A, two: A) => cmp.pred(proj(one), proj(two))
      for (e <- this) sorted += e
      sorted.sort(0, sorted.size, realPred)
      sorted.eqIntervals = countEqIntervals(sorted, realPred)
      sorted
    }
  }

  def thenby[B](proj: (A) => B)(implicit cmp: QueryUtils.ComparisonFunction[B]): Queryable[A] = {
    val count = size
    if (size < 2 || eqIntervals.isEmpty) this
    else {
      val sorted = new QVector[A](count)
      val realPred = (one: A, two: A) => cmp.pred(proj(one), proj(two))
      for (e <- this) sorted += e
      for (i <- eqIntervals) sorted.sort(i._1, i._2, realPred)
      sorted.eqIntervals = countEqIntervals(sorted, realPred)
      sorted
    }
  }

  def orderbyDesc[B](proj: (A) => B)(implicit cmp: QueryUtils.ComparisonFunction[B]): Queryable[A] = {
    val desc = QueryUtils.ComparisonFunction((one:B, two:B) => !cmp.pred(one, two) && one != two)
    orderby[B](proj)(desc)
  }

  def thenbyDesc[B](proj: (A) => B)(implicit cmp: QueryUtils.ComparisonFunction[B]): Queryable[A] = {
    val desc = QueryUtils.ComparisonFunction((one:B, two:B) => !cmp.pred(one, two) && one != two)
    thenby[B](proj)(desc)
  }

  def reverse(): Queryable[A] = {
    var count = size.asInstanceOf[Int]
    if (count < 2) this
    else {
      val result = new QVector[A](count)(QueryUtils.ExposureFlag(true))
      for (e <- this) {
        count -= 1
        result(count) = e
      }
      result
    }
  }

  def groupBy[B](proj: (A) => B): Queryable[Tuple2[B, Queryable[A]]] = {
    val table = buildHash(this, proj)
    val result = new QVector[Tuple2[B, Queryable[A]]]
    for (key <- table.keySet) result += (key, table(key))
    result
  }

  def groupBy[B, C](proj: (A) => B, sel: (A) => C): Queryable[Tuple2[B, Queryable[C]]] = {
    val table = buildHash(this, proj, sel)
    val result = new QVector[Tuple2[B, Queryable[C]]]
    for (key <- table.keySet) result += (key, table(key))
    result
  }

  def distinct(): Queryable[A] = {
    val src = new mutable.HashSet[A]
    val result = new QVector[A]
    for (e <- this) src += e
    for (e <- src) result += e
    result
  }

  def union(other: Queryable[A]): Queryable[A] = {
    val src = new mutable.HashSet[A]
    val arg = new mutable.HashSet[A]
    val result = new QVector[A]
    for (e <- this) src += e
    for (e <- other)arg += e
    src ++= arg
    for (e <- src) result += e
    result
  }

  def intersect(other: Queryable[A]): Queryable[A] = {
    val src = new mutable.HashSet[A]
    val arg = new mutable.HashSet[A]
    val result = new QVector[A]
    for (e <- this) src += e
    for (e <- other)arg += e
    src intersect arg
    for (e <- src) result += e
    result
  }

  def except(other: Queryable[A]): Queryable[A] = {
    val src = new mutable.HashSet[A]
    val arg = new mutable.HashSet[A]
    val result = new QVector[A]
    for (e <- this) src += e
    for (e <- other)arg += e
    for (e <- src -- arg) result += e
    result
  }

  def sequenceEqual(other: Queryable[A]): Boolean = this == other

  def first(): A = head

  def firstOrDefault(): Option[A] = if (size > 0) Some(head) else None

  def last(): A = tail

  def lastOrDefault(): Option[A] = if (size > 0) Some(tail) else None

  def single(): A = {
    if (size > 1) throw new NoSuchElementException("Cannot access single element of multiple-element Queryable")
    head
  }

  def singleOrDefault(): Option[A] = {
    val numElems = size
    if (numElems > 1) throw new NoSuchElementException("Cannot access single element of multiple-element Queryable")
    else if (numElems < 1) None
    else Some(head)
  }

  def elementAt(index: Int): A = {
    if (index < 0 || index > size - 1) throw new IndexOutOfBoundsException("Queryable index out of bounds")
    this match {
      case v: QVector[A] => v(index)
      case s: RandomAccessSeq[A] => s(index)
      case _ => {
        var i = 0
        for (e <- this) {
          if (i == index) return e
          i += 1
        }
        throw new RuntimeException("Panic: failed to access valid index in a Queryable") // this is designed to be unreachable
      }
    }
  }

  def elementAtOrDefault(index : Int): Option[A] = {
    if (index < 0 || index > size - 1) None
    else this match {
      case v: QVector[A] => Some(v(index))
      case s: RandomAccessSeq[A] => Some(s(index))
      case _ => {
        var i = 0
        for (e <- this) {
          if (i == index) return Some(e)
          i += 1
        }
        throw new RuntimeException("Panic: failed to access valid index in a Queryable") // this is designed to be unreachable
      }
    }
  }

  def defaultIfEmpty(): Queryable[Option[A]] = {
    if (size > 0) this.map(Some(_))
    else {
      val result = new QVector[Option[A]]
      result += None
      result
    }
  }

  def any(): Boolean =  size > 0

  def all(pred: (A) => Boolean): Boolean = {
    if (size < 1) true
    else for (e <- this) if (!pred(e)) return false
    true
  }

  def contains(elem:A): Boolean = {
    for (e <- this) if (e == elem) return true
    false
  }

  def count() =  size

  def longCount():Long = {
    var count:Long = 0;
    for (e <- this) count += 1
    count
  }

  def sum() = {
    var sum = 0.0
    val typ = recoverType
    typ match {
      case "Int" => for (e <- this) sum += e.asInstanceOf[Int]
      case "Double" => for (e <- this) sum += e.asInstanceOf[Double]
      case "Float" => for (e <- this) sum += e.asInstanceOf[Float]
      case "Long" => for (e <- this) sum += e.asInstanceOf[Long]
      case "Short" => for (e <- this) sum += e.asInstanceOf[Short]
      case _ => throw new RuntimeException("Cannot compute sum of elements of Queryable storing non-numeric type")
    }
    sum
  }

  def min()(implicit orderer: (A) => Ordered[A]): A = {
    var min = head
    var first = true
    for (e <- this) {
      if (first) first = false
      else if (e < min) min = e
    }
    min
  }

  def max()(implicit orderer: (A) => Ordered[A]): A = {
    var max = head
    var first = true
    for (e <- this) {
      if (first) first = false
      else if (e > max) max = e
    }
    max
  }

  def average() = {
    var sum = 0.0
    var count = 0
    val typ = recoverType
    typ match {
      case "Int" => for (e <- this) { sum += e.asInstanceOf[Int]; count += 1 }
      case "Double" => for (e <- this) { sum += e.asInstanceOf[Double]; count += 1 }
      case "Float" => for (e <- this) { sum += e.asInstanceOf[Float]; count += 1 }
      case "Long" => for (e <- this) { sum += e.asInstanceOf[Long]; count += 1 }
      case "Short" => for (e <- this) { sum += e.asInstanceOf[Short]; count += 1 }
      case _ => throw new RuntimeException("Cannot compute average of elements of Queryable storing non-numeric type")
    }
    sum / count
  }

  def aggregate(f: (A, A) => A): A = {
    var acc = head
    var first = true
    for (e <- this) {
      if (first) first = false
      else acc = f(acc, e)
    }
    acc
  }

  /* PRIVATE METHODS */

  //for simple join, groupJoin and groupBy
  private def buildHash[B, C](coll:Queryable[B], proj: (B) => C) = {
    val hash = mutable.HashMap[C, QVector[B]]()
    for (elem <- coll; key = proj(elem)) {
      if (hash.contains(key)) hash(key) += elem
      else {
        val fresh = new QVector[B]()
        fresh += elem
        hash += (key -> fresh)
      }
    }
    hash
  }

  //for two-argument groupBy (which is necessary for query syntax)
  private def buildHash[B, C, D](coll:Queryable[B], proj: (B) => C, sel: (B) => D) = {
    val hash = mutable.HashMap[C, QVector[D]]()
    for (elem <- coll; key = proj(elem); selected = sel(elem)) {
      if (hash.contains(key)) hash(key) += selected
      else {
        val fresh = new QVector[D]()
        fresh += selected
        hash += (key -> fresh)
      }
    }
    hash
  }

  /*Records intervals for which elements are "equal" to one another in terms of PRED.
    Properly nests the equality checks so that previous calls to an ordering operator
    are respected.*/
  private def countEqIntervals[B](src: QVector[A], pred: (A, A) => Boolean): ArrayBuffer[(Int, Int)] = {
    var result = new ArrayBuffer[(Int, Int)]()
    var intervals = new ArrayBuffer[(Int, Int)]()

    if (eqIntervals.isEmpty) intervals += Tuple2[Int, Int](0, src.size) else intervals = eqIntervals
    for (int <- intervals) {
      var i = int._1
      while(i < int._2 - 1) {
        if (!pred(src(i), src(i + 1))) {
          var first = i
          i += 1
          while (i < int._2 - 1 && !pred(src(i), src(i + 1))) i += 1
          result += Tuple2[Int, Int](first, i + 1)
        }
        i += 1
      }
    }
    result
  }

  private def recoverType = {
    if (size < 1) "Empty"
    head match {
      case i:Int => "Int"
      case d:Double => "Double"
      case f:Float => "Float"
      case l:Long => "Long"
      case s:Short => "Short"
      case _ => "Non-numeric"
    }
  }

}

*/