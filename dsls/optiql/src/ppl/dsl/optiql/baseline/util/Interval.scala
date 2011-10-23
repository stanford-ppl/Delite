package ppl.dsl.optiql.baseline.util

/**
 * Defines an interval which is a common SQL type
 */
object Interval {
  def apply(n: Int) = new IntervalBuilder(n)
}

class IntervalBuilder(n:Int) {
  def years: Interval = new Interval(n,0,0)
  def months: Interval = new Interval(0,n,0)
  def days: Interval = new Interval(0,0,n)
}

class Interval(val years: Int, val months: Int, val days: Int)