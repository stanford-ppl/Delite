package ppl.dsl.optiql.datastruct.scala.liftables

import ppl.dsl.optiql.datastruct.scala.util.Date

class LineItem (
  val orderKey: Int,
  val partKey: Int,
  val supplierKey: Int,
  val lineNumber: Int,
  val quantity: Float,
  val extendedPrice: Float,
  val discount: Float,
  val tax: Float,
  val returnFlag: Char,
  val lineStatus: Char,
  val shipDate: Date,
  val commitDate: Date,
  val receiptDate: Date,
  val shipInstruct: String,
  val shipMode: String,
  val comment: String
)































