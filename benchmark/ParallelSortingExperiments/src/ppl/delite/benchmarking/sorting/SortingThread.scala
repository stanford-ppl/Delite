package ppl.delite.benchmarking.sorting
import java.util.Comparator

class SortingThread(id: Int, needSorting: Array[_], comparator: Comparator[AnyRef]) extends Thread {
  override def run() = {
    val chunk_size = needSorting.size / Config.num_procs
    val remainder = needSorting.size % Config.num_procs
    val sz = if (id < remainder) chunk_size + 1 else chunk_size
    val off = if (id < remainder) 0 else id - remainder
    var idx = id*(chunk_size+1) - off
    val end = idx + sz
    println("sorting thread[" + id + "] will sort from " + idx + " until " + end)  
    
  }
}