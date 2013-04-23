package ppl.dsl.optiql.datastruct.scala.container

import ppl.delite.runtime.data._

object Table {

  def printAsTable(m: AnyRef, max_rows: Int = 0) { // FIXME: max_rows not used!

    implicit val tableStr = new StringBuilder
    val numRows = m.getClass.getMethod("size").invoke(m).asInstanceOf[Integer].intValue
    val fields = m.getClass.getMethod("data").invoke(m)
    val fieldStrs = fields.getClass.getDeclaredMethods.filter(_.getName.endsWith("_$eq")).map(_.getName.stripSuffix("_$eq"))
    val columnSizes = getTableColSizes(fields, fieldStrs)

    // Check if Table is empty
    if(numRows == 0) {
      println("=====================================================")
      println("|                  EMPTY TABLE                      |")
      println("=====================================================")
      return
    }

    def repeat(s: String, n:Int) {
      //assert(n < 0 || n > 300, "Incorrect value supplied for n in repeat")
      //todo for now, just ignore bad value of n
      if(n < 0 || n > 300)
        return
      var idx = 0
      while(idx != n) {
        tableStr.append(s);
        idx += 1
      }
    }

    def horizontalRule = {
      for(i <- 0 until columnSizes.size )
        repeat("=",columnSizes(i)+1)
      tableStr append("=\n")
    }

    def newLine = {
      tableStr append("\n")
    }

    horizontalRule
    tableStr append("|")
    for(i <- 0 until columnSizes.size) {
      tableStr append( " " + fieldStrs(i))
      repeat(" " , columnSizes(i) - fieldStrs(i).size - 1  )
      tableStr append("|")
    }
    newLine
    horizontalRule
    print(tableStr.toString)
    tableStr.clear

    for(r <- 0 until numRows) {
      emitRecordAsRow(r, columnSizes)
    }

    print(tableStr.toString)
    tableStr.clear

    horizontalRule
    println(tableStr.toString)

    def emitRecordAsRow(r: Int, columnSizes: Array[Int]) {
      tableStr append("| ")
      var str = ""

      var idx = 0
      for(f <- fieldStrs) {
        str = readArray(fields.getClass.getMethod(f).invoke(fields), r)
        tableStr append(str); repeat(" ", columnSizes(idx) - str.size - 1); tableStr append("| ")
        idx += 1
      }
      tableStr append("\n")
    }
  }

  private def max(a: Int, b: Int) = if(a > b) a else b

  private def readArray(x: Any, idx: Int) = x match {
    case d: DeliteArray[_] => d.readAt(idx).toString
    case a: Array[_] => a(idx).toString
    case _ => throw new IllegalArgumentException(x.getClass.getSimpleName + " cannot be printed as a table")
  }

  private def readArrayLength(x: Any) = x match {
    case d: DeliteArray[_] => d.length
    case a: Array[_] => a.length
    case _ => throw new IllegalArgumentException(x.getClass.getSimpleName + " cannot be printed as a table")
  }

  private def getTableColSizes(fields: AnyRef, fieldStrs: Array[String]) = {
    val colSizes = new Array[Int](fieldStrs.length)

    //Columns should be at least the size of the headers
    var idx = 0
    for(f <- fieldStrs) {
      colSizes(idx) = max(colSizes(idx), f.length + 2)
      idx += 1
    }
    //columns should be at least the size of maximal element
    idx = 0
    for (f <- fieldStrs) {
      val array = fields.getClass.getMethod(f).invoke(fields)
      val length = readArrayLength(array)
      for (j <- 0 until length) {
        val d = readArray(array, j)
        colSizes(idx) = max(colSizes(idx), d.length + 2)
      }
      idx += 1
    }

    colSizes
  }
}
