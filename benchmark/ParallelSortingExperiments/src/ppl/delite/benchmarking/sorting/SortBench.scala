package ppl.delite.benchmarking.sorting
import ppl.delite.benchmarking.sorting.tpch.TPCH
import ppl.delite.benchmarking.sorting.tpch.util.PerformanceTimer
import java.util.Comparator
import ppl.delite.benchmarking.sorting.tpch.LineItem

object SortBench {

  def main(args: Array[String]): Unit = {
    println("PPL Delite Team Sorting Benchmark\n")
    //load input 
    val lineItems = TPCH.loadTPCHTable(TPCH.LINEITEMS, 2000000)
    
    val comparator = new Comparator[AnyRef] {
      def compare(_o1: AnyRef, _o2:AnyRef): Int = {
        val o1 = _o1.asInstanceOf[LineItem]
        val o2 = _o2.asInstanceOf[LineItem]
        if (o1.l_extendedprice < o2.l_extendedprice)
          return -1
        else if (o1.l_extendedprice > o2.l_extendedprice)
          return 1
        else
          return 0        
      }
    }
     
    
    var needSorting = lineItems.getArray.clone
    println("performing timsort of loaded data")
    PerformanceTimer.start("timsort" , false)
    //call timsort on some key and time it
    java.util.Arrays.sort(needSorting, 0, lineItems.size, comparator)
    PerformanceTimer.stop("timsort" , false)
    PerformanceTimer.print("timsort")                
    
    
    needSorting = lineItems.getArray.clone
    val threads = new Array[SortingThread](Config.num_procs)
    //do timesort + parallel merge sort
    for(i <- 0 until Config.num_procs) {
      threads(i) = new SortingThread(i, needSorting, comparator)
    }
    
    //launch threads
    PerformanceTimer.start("par-timsort", false)
    for(i <- 0 until Config.num_procs) {
      threads(i).start
    }
    
    //join on threads
    for(i <- 0 until Config.num_procs) {
      threads(i).join(0)
    }
    PerformanceTimer.stop("par-timsort", false)
    PerformanceTimer.print("par-timsort")
    
  }

}