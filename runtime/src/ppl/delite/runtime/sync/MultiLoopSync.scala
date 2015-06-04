package ppl.delite.runtime.sync

import java.lang.Math
import java.util.concurrent.CyclicBarrier
import java.util.concurrent.atomic.{AtomicInteger, AtomicReferenceArray}

import ppl.delite.runtime.Config
import generated.scala.ResourceInfo

/**
 * Contains all the synchronous containers, barrier, and dynamic scheduling logic required by a multiloop
 */

final class MultiLoopSync[T <: AnyRef](loopSize: Long, numChunksHint: Int, resourceInfo: ResourceInfo) {
 
  //dynamic scheduler
  val threads = {
    val availableThreads = resourceInfo.availableThreads
    val startId = resourceInfo.threadId
    val r = if (loopSize >= availableThreads) Range(startId, startId+availableThreads) //contiguous threads
    else if (loopSize == 0) Range(startId, startId+1) //1 thread
    else Range(startId, startId+availableThreads, availableThreads/loopSize.toInt) //spread threads out over available range
    //TODO: this wastes threads when availableThreads/loopSize does not divide evenly the rather than assigning an uneven number of threads in the future nested level(s)
  
    val newAvailable = availableThreads / r.length
    //println("loopSize: " + loopSize + " start: " + startId + " available: " + newAvailable + " assigned: " + r.mkString("(",", ",")"))
    r.map(i => resourceInfo.copySync(i, newAvailable))
  }

  val numChunks = {
    val defaultChunks = if (numChunksHint == -1) (loopSize/(Math.log10(loopSize)*(500.0/numThreads))).toInt else numChunksHint
    if (numThreads == 1 || defaultChunks < numThreads || loopSize < defaultChunks) numThreads else defaultChunks
  }

  def numThreads = threads.length

  def localThreadId(info: ResourceInfo) = {
    threads.indexOf(info)
  }

  def getThreadResource(idx: Int) = threads(idx)

  private[this] val offset = new AtomicInteger(numThreads)
  def getNextChunkIdx(): Int = offset.getAndAdd(1)

  //barrier between stages
  private[this] val barrier = new CyclicBarrier(numThreads)
  def awaitBarrier() = barrier.await()

  //synchronous buffers for passing activation records between threads
  private[this] val results = new AtomicReferenceArray[T](numChunks)
  
  def get(i: Int): T = {
    while (results.get(i) eq null) { } //spin 
    results.get(i)
  }

  def set(i: Int, res: T): Unit = {
    results.set(i, res)
  }

  private[this] val resultsC = new AtomicReferenceArray[T](numThreads)
  
  def getC(i: Int): T = {
    while (resultsC.get(i) eq null) { } //spin 
    resultsC.get(i)
  }

  def setC(i: Int, res: T): Unit = {
    resultsC.set(i, res)
  }

  private[this] val resultsP = new AtomicReferenceArray[T](numThreads)
  
  def getP(i: Int): T = {
    while (resultsP.get(i) eq null) { } //spin 
    resultsP.get(i)
  }

  def setP(i: Int, res: T): Unit = {
    resultsP.set(i, res)
  }

}
