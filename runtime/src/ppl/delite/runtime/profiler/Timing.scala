package ppl.delite.runtime.profiler

/**
 * Timing of a Delite op.
 * 
 * @author Philipp Haller
 */
class Timing(val threadName: String, val startTime: Long, val component: String) {
  private var _endTime: Long = 0L
  private var done = false
  
  def endTime: Long =
    _endTime
  
  def endTime_=(time: Long) {
    _endTime = time
    done = true
  }
  
  def isDone: Boolean =
    done
    
  def elapsedMicros: Long =
    ((endTime - startTime) / 1000)
  
  override def toString =
    /*"Timing for " + component + " (" +*/ elapsedMicros + "us"
}

/**
 * A set of timings for the chunks of a data-parallel operation.
 * 
 * Note that System.nanoTime() can only be used to measure the elapsed time of a piece of code.
 * 
 * @author Philipp Haller
 */
class MultiTiming(thread: String, start: Long, val numChunks: Int, component: String) extends Timing(thread, start, component) {
  val timings = Array.ofDim[Timing](numChunks)
  
  def apply(i: Int): Timing =
    timings(i)
  
  /**
   * Creates and initializes a new timing for a chunk of this operation.
   * 
   * @param chunk      the index of the chunk
   * @param threadName the name of the thread running the chunk
   */
  def start(chunk: Int, threadName: String) {
    val time = System.nanoTime()
    timings(chunk) = new Timing(threadName, time, component)
  }
  
  /**
   * Stops the current timing for a chunk of this operation.
   * The compound timing of this operation is stopped if all chunk timings are done.
   * 
   * @param chunk  the index of the chunk
   */
  def stop(chunk: Int) {
    val time = System.nanoTime()
    timings(chunk).endTime = time
    // if all chunks are done, this compound timing is done
    if (timings.forall(t => (t != null && t.isDone)))
      this.endTime = time
  }
  
  def loadImbalance: Double = {
    val totalTime = timings.foldLeft(0L)((elapsed: Long, t: Timing) => elapsed + t.elapsedMicros)
    val idealPerThread = totalTime / numChunks
    val shortest = timings.map(t => t.elapsedMicros).min
    val imbalance = (idealPerThread - shortest).toDouble / idealPerThread.toDouble
    imbalance
  }
  
  /*override def toString =
    "MultiTiming for " + component
   */
}
