package ppl.delite.benchmarking.sorting.tpch

import ppl.delite.benchmarking.sorting.tpch.util.Date

class LineItem (
  val l_orderkey: Int,
  val l_partkey: Int,
  val l_suppkey: Int,
  val l_linenumber: Int,
  val l_quantity: Double,
  val l_extendedprice: Double,
  val l_discount: Double,
  val l_tax: Double,
  val l_returnflag: Char,
  val l_linestatus: Char,
  val l_shipdate: Date,
  val l_commitdate: Date,
  val l_receiptdate: Date,
  val l_shipinstruct: String,
  val l_shipmode: String,
  val l_comment: String
)