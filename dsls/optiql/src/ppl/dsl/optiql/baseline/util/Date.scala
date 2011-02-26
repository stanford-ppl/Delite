package ppl.dsl.optiql.baseline.util;


object Date {
  def apply(year:Int, month:Int, day:Int):SimpleDate = SimpleDate(year, month, day)
  def apply(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int) = DateTime(year, month, day, hour, minute, second)
  def apply(s: String): Date = {
    val tokens = s.split("-")
    assert(tokens.size == 3, "expected 3 tokens in date, got: " + tokens)
    Date(tokens(0).toInt, tokens(1).toInt, tokens(2).toInt)
  }
}

abstract class Date;
case class SimpleDate(year:Int, month:Int, day:Int) extends Date {
  assert(month > 0 && month <= 12, "invalid month in date")
  assert(day > 0 && day <= 31, "invalid day in date")
}
case class DateTime(year: Int, month:Int, day:Int, hour:Int, minute:Int, second:Int)  extends Date
