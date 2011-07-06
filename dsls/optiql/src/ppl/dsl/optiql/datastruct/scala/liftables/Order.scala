package ppl.dsl.optiql.datastruct.scala.liftables

import ppl.dsl.optiql.datastruct.scala.util.Date

class Order (
  val key: Int,
  val customerKey: Int,
  val status: Char,
  val totalPrice: Float,
  val date: Date,
  val priority: String,
  val clerk: String,
  val shipPriority: Int,
  val comment: String
)































