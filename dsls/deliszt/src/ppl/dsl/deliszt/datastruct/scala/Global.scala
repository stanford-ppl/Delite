package ppl.dsl.deliszt.datastruct.scala

import java.util.Date

object Global {
  var wall_start = 0L
  
  def wall_time() : Double = (System.currentTimeMillis - wall_start) / 1000.0
  def processor_time() : Double = System.nanoTime / 1000000000.0
}