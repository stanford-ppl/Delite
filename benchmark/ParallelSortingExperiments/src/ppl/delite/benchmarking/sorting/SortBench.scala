package ppl.delite.benchmarking.sorting
import ppl.delite.benchmarking.sorting.tpch.TPCH
import ppl.delite.benchmarking.sorting.tpch.util.PerformanceTimer
import java.util.Comparator
import ppl.delite.benchmarking.sorting.tpch.LineItem
import java.util.concurrent.CyclicBarrier
import java.util.concurrent.atomic.AtomicIntegerArray
import java.util.concurrent.CountDownLatch

object SortBench {

  def main(args: Array[String]): Unit = {
    println("PPL Delite Team Sorting Benchmark\n")
    //load input 
    val lineItems = TPCH.loadTPCHTable(TPCH.LINEITEMS, Config.tpch_num_elems)
  
    
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
     
    
    val sortedSeq = lineItems.getArray.clone
    println("performing timsort of loaded data")
    PerformanceTimer.start("timsort" , false)
    //call timsort on some key and time it
    java.util.Arrays.sort(sortedSeq, 0, lineItems.size, comparator)
    PerformanceTimer.stop("timsort" , false)
    PerformanceTimer.print("timsort") 
    
    
    //latches for the merge sort
    val perThread = new Array[PerThreadState](Config.num_procs)
    for(i <- 0 until Config.num_procs) {
      perThread(i) = new PerThreadState
    }
    
    val state = new SharedThreadState(
    		lineItems.getArray.clone,
    		comparator,
    		lineItems.size,
    		perThread)
    
    val threads = new Array[SortingThread](Config.num_procs)
    //do timesort + parallel merge sort
    for(i <- 0 until Config.num_procs) {
      threads(i) = new SortingThread(i, state)
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
    
    //compare the sorted arrays to ensure proper sorting
    println("Comparing array sorted sequentially and in parrallel")
    var errors = 0
    for(i <- 0 until lineItems.size; if errors < 5) {
    	if(sortedSeq(i) != state.arrayToSort(i)) {
    	  println("Sequential[" + i + "]:" + sortedSeq(i) + " doesn't match Parallel[" + i + "]:" + state.arrayToSort(i))
    	  errors += 1
    	}
    }
    if(errors == 0) println("arrays are the same")
    
    
  }

}