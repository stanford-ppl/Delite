package ppl.delite.benchmarking.sorting
import java.util.Comparator
import java.util.concurrent.CountDownLatch
import java.util.concurrent.locks.ReentrantLock

class SharedThreadState (  
  var arrayToSort: Array[AnyRef],
  val comparator: Comparator[AnyRef],
  val arrayLength: Int,
  val perThread: Array[PerThreadState]
) 

class PerThreadState{
  var start = 0
  var end = 0
  var done = false
  
  val lock = new ReentrantLock()
  val condition = lock.newCondition()
  
  
}