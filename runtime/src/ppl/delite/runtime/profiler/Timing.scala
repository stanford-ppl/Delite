package ppl.delite.runtime.profiler

/**
 * Timing of a Delite op.
 * 
 * @author Philipp Haller
 */

class Timing(val startTime: Long, val component: String) {
  private var _endTime: Long = 0L
  private var done = false
  
  def endTime: Long = _endTime
  def endTime_=(time: Long) {
    _endTime = time
    done = true
  }
  
  def isDone: Boolean = done
  def elapsedMillis: Long = endTime - startTime
  override def toString = elapsedMillis + "ms"
}
