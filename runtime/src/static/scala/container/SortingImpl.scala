package generated.scala.container

import java.util.Arrays


object SortingImpl {

  /**
   * Generic index array sorting
   */
  def sort(a: Array[Int], c: Comparator) = TimSort.sort(a,c)
  def sort(a: Array[Int], lo: Int, hi: Int, c: Comparator) = TimSort.sort(a,lo,hi,c)

  /**
   * Specialized ascending order sorts for primitive types
   */
  def sort(a: Array[Int]) = Arrays.sort(a)
  def sort(a: Array[Int], lo: Int, hi: Int) = Arrays.sort(a,lo,hi)

  def sort(a: Array[Long]) = Arrays.sort(a)
  def sort(a: Array[Long], lo: Int, hi: Int) = Arrays.sort(a,lo,hi)

  def sort(a: Array[Float]) = Arrays.sort(a)
  def sort(a: Array[Float], lo: Int, hi: Int) = Arrays.sort(a,lo,hi)

  def sort(a: Array[Double]) = Arrays.sort(a)
  def sort(a: Array[Double], lo: Int, hi: Int) = Arrays.sort(a,lo,hi)

  def sort(a: Array[Byte]) = Arrays.sort(a)
  def sort(a: Array[Byte], lo: Int, hi: Int) = Arrays.sort(a,lo,hi)

  def sort(a: Array[Char]) = Arrays.sort(a)
  def sort(a: Array[Char], lo: Int, hi: Int) = Arrays.sort(a,lo,hi)

  def sort(a: Array[Short]) = Arrays.sort(a)
  def sort(a: Array[Short], lo: Int, hi: Int) = Arrays.sort(a,lo,hi)


  /**
   * Parallel sort testing cdoe
  */

  /*
   * commented because not jdk 1.6 compatible

  def main(args: Array[String]) {
    val len = 30000000

    def time[T](f: => T): Double = {
      val start = System.currentTimeMillis
      val res = f
      val stop = System.currentTimeMillis
      val elapsed = (stop-start)/1e3
      elapsed
    }

    def avg(amt: Double*) = amt.sum / amt.length

    def init(len: Int) = {
      val a = new Array[Double](len)
      val b = new Array[Long](len)
      val c = new Array[Int](len)
      val r = new java.util.Random
      for (i <- 0 until len) { a(i) = r.nextDouble; b(i) = r.nextDouble.toLong; c(i) = i }
      (a,b,c)
    }

    val (a,b,idx) = init(len)

    val comp = new Comparator {
      def compare(li: Int, ri: Int): Int = {
        val first = java.lang.Double.compare(a(li), a(ri))
        if (first != 0) return first

        val second = java.lang.Long.compare(b(li), b(ri))
        if (second != 0) return second

        //val third = ...

        second
      }
    }

    //primitive
    val t = a.clone
    println("primitive time: " + time(sort(t)))


    def merge(arr: Array[Int], startA: Int, startB: Int, endB: Int) = {
      //println("merging: " + startA + " , " + startB + " , " + endB)
      var posA = startA
      var posB = startB
      var posS = 0
      val swap = new Array[Int](endB - startA)

      while (posA < startB && posB < endB) {
        val elemA = arr(posA)
        val elemB = arr(posB)
        if (comp.compare(elemA, elemB) > 0) {
          swap(posS) = elemB
          posB += 1
        }
        else {
          swap(posS) = elemA
          posA += 1
        }
        posS += 1
      }

      val remainder = startB - posA //check lhs remainder only (any rhs remainder is already correct)
      System.arraycopy(arr, posA, arr, endB-remainder, remainder) //move lhs remainder (if any)
      System.arraycopy(swap, 0, arr, startA, posS) //copy back merged result
    }

    def tree_reduce(arr: Array[Int], numThreads: Int) = {
      println("tree level: " + numThreads)
      val len: Long = arr.length
      var level = numThreads
      var totalTime = 0.0
      while (level > 0) {
        val localTimes = new Array[Double](level)
        for (i <- 0 until level) {
          if (level == numThreads) {
            localTimes(i) = time(sort(arr, (len*i/level).toInt, (len*(i+1)/level).toInt, comp))
          }
          else {
            localTimes(i) = time(merge(arr, (len*i/level).toInt, (len*(2*i+1)/(2*level)).toInt, (len*(i+1)/level).toInt))
          }
        }
        val avgTime = localTimes.reduce(_ + _) / localTimes.length
        val maxTime = localTimes.reduce((a:Double,b:Double) => java.lang.Math.max(a,b))
        totalTime += maxTime
        println("level " + level + " avg time: " + avgTime)
        //println("level " + level + " max time: " + maxTime)
        level = level / 2 //if (level > 1) 1 else 0 //without tree-reduce gets *slower* with more threads
      }
      println("total time: " + totalTime)
      println("")
      arr
    }

    def check(ind: Array[Int]) = {
      for (i <- 0 until len) {
        assert(t(i) == a(ind(i)))
      }
      ind
    }

    check(tree_reduce(idx.clone, 1))
    check(tree_reduce(idx.clone, 2))
    check(tree_reduce(idx.clone, 4))
    check(tree_reduce(idx.clone, 8))
    check(tree_reduce(idx.clone, 16))
    check(tree_reduce(idx.clone, 32))
    check(tree_reduce(idx.clone, 64))
    val j = check(tree_reduce(idx.clone, 128))
    println("sorted time: " + time(sort(j,comp)))
  }

  */

}
