package ppl.dsl.optiql.baseline.benchmarks.tpch.schema

import ppl.dsl.optiql.baseline.util.Date
import collection.mutable.{ArrayBuffer, BufferLike}

//this includes functionality for loading TPCH style data
abstract class DataTable[T] extends Iterable[T] {

  var data: ArrayBuffer[T]

  def iterator = data.iterator

  override def filter(p: T => Boolean ) = {
    val res = instantiateTable()
    res.data = data.filter(p)
    res
  }

  def instantiateTable(): DataTable[T]


  def addRecord(fields: Array[String])

  implicit def cStrToInt(s: String) = Integer.parseInt(s)
  implicit def cStrToFloat(s: String) = java.lang.Float.parseFloat(s)
  implicit def cStrToChar(s: String) = {
    assert(s.size == 1, "expected char, got: " + s)
    s(0)
  }
  implicit def cStrToDate(s: String) = Date(s)

  //todo clean this up
  override def toString() = {
    val tableStr = new StringBuilder
    tableStr append( "Table: " + this.getClass.getSimpleName + "\n" )
    for(i <- 0 until numCols )
      tableStr append("================")
    tableStr append("=\n|")
    for(i <- 0 until numCols)
      tableStr append( "               |")



    tableStr.toString
  }

  def numCols: Int
  def getColSize(idx: Int): Int
  def getColHeader(idx: Int): String
}