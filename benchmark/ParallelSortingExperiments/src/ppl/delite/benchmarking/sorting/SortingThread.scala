package ppl.delite.benchmarking.sorting
import java.util.Comparator
import java.util.concurrent.CyclicBarrier

class SortingThread(id: Int, state: SharedThreadState) extends Thread {
  override def run() = {
    val chunk_size = state.arrayLength / Config.num_procs
    val remainder = state.arrayLength % Config.num_procs
    val sz = if (id < remainder) chunk_size + 1 else chunk_size
    val off = if (id < remainder) 0 else id - remainder
    var start = id*(chunk_size+1) - off
    var end = start + sz
    //println("sorting thread[" + id + "] will sort from " + idx + " until " + (end-1))  
    
    //sort my chunck using TimSort
    java.util.Arrays.sort(state.arrayToSort, start, end, state.comparator)
        
    //tree based merge sort
    //println("sorting thread[" + id + "] proceeding to parallel merge portion")  
    var log2Idx = id
    var step = 1
    while((log2Idx % 2 == 0) && (log2Idx + step < Config.num_procs)) {
      log2Idx /= 2
      val neighbor = id + step
      step *= 2
      //wait for for neighbor
      state.perThread(neighbor).lock.lock()
      try {
        while(state.perThread(neighbor).done == false) {
        	state.perThread(neighbor).condition.await()
        }          
      } finally {
        state.perThread(neighbor).lock.unlock()  
      }    
      //other indices
      val ostart = state.perThread(neighbor).start
      val oend = state.perThread(neighbor).end
      //do the merge
      //println("Thread[" + id + "]-" + start + ":" + end + " will merge with thread[" + neighbor + "]-" + ostart + ":" + oend ) 
      assert(end == ostart) 
      end = oend
      java.util.Arrays.sort(state.arrayToSort, start, end, state.comparator)
    }
    
    //finished, set my latch
    //println("Thread[" + id + "] is finished")
    val myState = state.perThread(id) 
    myState.lock.lock()
    try{
    	myState.start = start
    	myState.end = end
    	myState.done = true
    	myState.condition.signal()
    } finally {
    	state.perThread(id).lock.unlock()  
    }
    
  }
}
