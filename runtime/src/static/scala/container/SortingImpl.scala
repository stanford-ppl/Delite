package generated.scala.container

import java.util.Arrays


object SortingImpl {

  /**
   * Generic index array sorting
   */
  def sort(a: Array[Int], c: IntComparator) = IntTimSort.sort(a,0,a.length,c,null,0,0)
  def sort(a: Array[Int], lo: Int, hi: Int, c: IntComparator) = IntTimSort.sort(a,lo,hi,c,null,0,0)
  def merge(a: Array[Int], lo: Int, mid: Int, hi: Int, c: IntComparator) = IntTimSort.merge(a,lo,mid,hi,c)

  def sort(a: Array[Long], c: LongComparator) = LongTimSort.sort(a,0,a.length,c,null,0,0)
  def sort(a: Array[Long], lo: Int, hi: Int, c: LongComparator) = LongTimSort.sort(a,lo,hi,c,null,0,0)
  def merge(a: Array[Long], lo: Int, mid: Int, hi: Int, c: LongComparator) = LongTimSort.merge(a,lo,mid,hi,c)

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

  def sort(a: Array[String]) = Arrays.sort(a.asInstanceOf[Array[Object]])
  def sort(a: Array[String], lo: Int, hi: Int) = Arrays.sort(a.asInstanceOf[Array[Object]],lo,hi)

}

/*object TestSort { self =>
  import java.util._
  
  type Elem = Double //Int
  val random = new Random
  def rand = {
    random.nextDouble
    //random.nextInt
  }

  def time[T](name: String)(block: => T) = {
    val start = System.currentTimeMillis
    val res = block
    val end = System.currentTimeMillis
    println(name + ": " + (end-start)/1e3)
    res
  }

  def main(args: Array[String]) {
    val size = if (args.length > 0) args(0).toInt else 10000000
    val src = Array.fill[Elem](size)(rand)
    val dst = Array.fill[Elem](size)(rand)

    val indices = Array.tabulate[Int](size)(i => i)
    val indicesO = indices.map(i => Integer.valueOf(i))

    def compareEdges(i: Int, j: Int) = {
      if (src(i) == src(j)) {
        if (dst(i) == dst(j)) 0
        else if (dst(i) < dst(j)) 1
        else -1
      }
      else if (src(i) > src(j)) 1
      else -1
    }

    val comp = new IntComparator {
      def compare(o1: Int, o2: Int) = compareEdges(o1, o2)
    }

    val edges = src.zip(dst)

    val compO = new Comparator[Integer] {
      def compare(o1: Integer, o2: Integer) = compareEdges(o1.intValue, o2.intValue)
    }

    val compJ = new Comparator[(Elem,Elem)] {
      def compare(o1: (Elem,Elem), o2: (Elem,Elem)) = {
        if (o1._1 == o2._1) {
          if (o1._2 == o2._2) 0
          else if (o1._2 > o2._2) 1
          else -1
        }
        else if (o1._1 > o2._1) 1
        else -1
      }
    }

    def arrayEq(a: Array[Int], b: Array[Int]): Boolean = {
      for (i <- 0 until a.length) {
        if (a(i) != b(i)) return false
      }
      true
    }

    val indicesA = indices.clone
    val indicesB = indices.clone
    val indicesOA = indicesO.clone
    val indicesOB = indicesO.clone
    val edgesA = edges.clone
    val edgesB = edges.clone

    time("sequential delite")(SortingImpl.sort(indicesA,comp))
    val chunks = 4
    time("parallel delite ("+chunks+")") {
      Array.tabulate[Int](chunks)(i => i).par.foreach{ i => 
        val start = i*size/chunks
        val end = (i+1)*size/chunks
        SortingImpl.sort(indicesB, start, end, comp)
      }
      var half = chunks / 2
      while (half > 0) {
        Array.tabulate[Int](half)(i => i).par.foreach{ i =>
          val start = i*size/half
          val end = (i+1)*size/half
          val mid = start + (end-start)/2
          SortingImpl.merge(indicesB, start, mid, end, comp)
        }
        half /= 2
      }
    }
    assert(arrayEq(indicesA, indicesB), "parallel sort incorrect!")

    time("sequential java tuples")(Arrays.sort(edgesA, compJ))
    time("parallel java tuples (all)")(Arrays.parallelSort(edgesB, compJ))

    time("sequential java boxed")(Arrays.sort(indicesOA, compO))
    time("parallel java boxed (all)")(Arrays.parallelSort(indicesOB, compO))
  }
}*/
